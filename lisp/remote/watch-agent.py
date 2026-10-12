"""Linux file events for Emacs Remote without inotify-tools.

Usage: python3 -u -c SOURCE PATH [flat].  A directory is watched recursively,
or with `flat' only itself, without descending.  Any other path is one file:
its directory is watched, not the file itself, and only events for that name
are reported, so a writer that replaces the file by renaming a temporary one
over it does not end the watch.

The agent exits when its standard input closes.  Its owner holds that pipe
open; a remote process started without a terminal gets no hangup when the
owner goes away, and in a quiet directory would otherwise never notice.

Standard output consists of pairs of NUL-terminated event names and absolute
paths.  The first pair is READY, sent once the watches are installed; in place
of a path it carries how many seconds ago the watched path last changed, or
nothing when the path does not exist.  Both ends of that interval are read
from this machine's clock, so the owner can tell a change it could not have
seen from one it did without comparing clocks.

The agent uses only Python's standard library and libc; it is sent as source
on process startup, so no target-side installation or package manager is
needed.
"""

import ctypes
import os
import select
import struct
import sys
import time


IN_MODIFY = 0x00000002
IN_ATTRIB = 0x00000004
IN_CLOSE_WRITE = 0x00000008
IN_MOVED_FROM = 0x00000040
IN_MOVED_TO = 0x00000080
IN_CREATE = 0x00000100
IN_DELETE = 0x00000200
IN_DELETE_SELF = 0x00000400
IN_MOVE_SELF = 0x00000800
IN_UNMOUNT = 0x00002000
IN_Q_OVERFLOW = 0x00004000
IN_IGNORED = 0x00008000
IN_ISDIR = 0x40000000
MASK = (IN_MODIFY | IN_ATTRIB | IN_CLOSE_WRITE | IN_MOVED_FROM |
        IN_MOVED_TO | IN_CREATE | IN_DELETE | IN_DELETE_SELF |
        IN_MOVE_SELF | IN_UNMOUNT | IN_Q_OVERFLOW)
HEADER = struct.Struct("iIII")


def run(root, flat=False):
    root = os.path.abspath(root)
    libc = ctypes.CDLL(None, use_errno=True)
    libc.inotify_init1.argtypes = [ctypes.c_int]
    libc.inotify_init1.restype = ctypes.c_int
    libc.inotify_add_watch.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_uint32]
    libc.inotify_add_watch.restype = ctypes.c_int
    libc.inotify_rm_watch.argtypes = [ctypes.c_int, ctypes.c_int]
    libc.inotify_rm_watch.restype = ctypes.c_int
    fd = libc.inotify_init1(os.O_NONBLOCK | os.O_CLOEXEC)
    if fd < 0:
        raise OSError(ctypes.get_errno(), "inotify_init1")
    by_wd = {}
    by_path = {}

    def emit(name, path):
        payload = name.encode("ascii") + b"\0" + os.fsencode(path) + b"\0"
        while payload:
            payload = payload[os.write(1, payload):]

    def walk_error(error):
        raise error

    def add_tree(directory):
        for current, dirs, _files in os.walk(
                directory, followlinks=False, onerror=walk_error):
            dirs[:] = [name for name in dirs
                       if not os.path.islink(os.path.join(current, name))]
            if current in by_path:
                continue
            wd = libc.inotify_add_watch(fd, os.fsencode(current), MASK)
            if wd < 0:
                raise OSError(ctypes.get_errno(), "inotify_add_watch", current)
            by_wd[wd] = current
            by_path[current] = wd

    def remove_tree(directory):
        prefix = directory + os.sep
        for path, wd in list(by_path.items()):
            if path == directory or path.startswith(prefix):
                by_path.pop(path, None)
                by_wd.pop(wd, None)
                libc.inotify_rm_watch(fd, wd)

    single = not os.path.isdir(root)
    try:
        if single:
            parent = os.path.dirname(root)
            wd = libc.inotify_add_watch(fd, os.fsencode(parent), MASK)
            if wd < 0:
                raise OSError(ctypes.get_errno(), "inotify_add_watch", parent)
            by_wd[wd] = parent
            by_path[parent] = wd
        elif flat:
            wd = libc.inotify_add_watch(fd, os.fsencode(root), MASK)
            if wd < 0:
                raise OSError(ctypes.get_errno(), "inotify_add_watch", root)
            by_wd[wd] = root
            by_path[root] = wd
        else:
            add_tree(root)
            if root not in by_path:
                raise NotADirectoryError(root)
        try:
            status = os.stat(root)
            age = "%.3f" % max(0.0, time.time() - max(status.st_mtime, status.st_ctime))
        except OSError:
            age = ""
        emit("READY", age)
        # Standard input that is already at end of file (closed, or the
        # null device) carries no signal about the owner.
        owner = 0
        try:
            ready, _, _ = select.select([owner], [], [], 0)
            if ready and not os.read(owner, 4096):
                owner = None
        except (OSError, ValueError):
            owner = None
        while True:
            waiting = [fd] if owner is None else [fd, owner]
            readable, _, _ = select.select(waiting, [], [])
            if owner in readable:
                try:
                    if not os.read(owner, 4096):
                        return
                except OSError:
                    return
                if fd not in readable:
                    continue
            try:
                data = os.read(fd, 1024 * 1024)
            except BlockingIOError:
                continue
            offset = 0
            while offset + HEADER.size <= len(data):
                wd, mask, _cookie, size = HEADER.unpack_from(data, offset)
                offset += HEADER.size
                name = data[offset:offset + size].split(b"\0", 1)[0]
                offset += size
                if mask & IN_Q_OVERFLOW:
                    emit("Q_OVERFLOW", root)
                    continue
                directory = by_wd.get(wd)
                if not directory:
                    continue
                path = os.path.join(directory, os.fsdecode(name)) if name else directory
                if single:
                    if not name:
                        # The directory itself went away; so did the file.
                        if mask & (IN_IGNORED | IN_DELETE_SELF |
                                   IN_MOVE_SELF | IN_UNMOUNT):
                            emit("IGNORED", root)
                            return
                        continue
                    if path != root:
                        continue
                    names = [label for bit, label in (
                        (IN_CREATE, "CREATE"), (IN_MOVED_TO, "MOVED_TO"),
                        (IN_MOVED_FROM, "MOVED_FROM"), (IN_DELETE, "DELETE"),
                        (IN_MODIFY, "MODIFY"), (IN_CLOSE_WRITE, "CLOSE_WRITE"),
                        (IN_ATTRIB, "ATTRIB")) if mask & bit]
                    if names:
                        emit(",".join(names), path)
                    continue
                if mask & IN_IGNORED:
                    by_path.pop(directory, None)
                    by_wd.pop(wd, None)
                    if directory == root:
                        emit("IGNORED", root)
                        return
                    continue
                if not flat and mask & (IN_MOVED_FROM | IN_DELETE) and mask & IN_ISDIR:
                    remove_tree(path)
                if not flat and mask & (IN_MOVED_TO | IN_CREATE) and mask & IN_ISDIR:
                    try:
                        add_tree(path)
                    except OSError:
                        emit("Q_OVERFLOW", root)
                names = [label for bit, label in (
                    (IN_CREATE, "CREATE"), (IN_MOVED_TO, "MOVED_TO"),
                    (IN_MOVED_FROM, "MOVED_FROM"), (IN_DELETE, "DELETE"),
                    (IN_MODIFY, "MODIFY"), (IN_CLOSE_WRITE, "CLOSE_WRITE"),
                    (IN_ATTRIB, "ATTRIB"), (IN_DELETE_SELF, "DELETE_SELF"),
                    (IN_MOVE_SELF, "MOVE_SELF"), (IN_UNMOUNT, "UNMOUNT"))
                    if mask & bit]
                if mask & IN_ISDIR:
                    names.append("ISDIR")
                if names:
                    emit(",".join(names), path)
                if path == root and mask & (IN_DELETE_SELF | IN_MOVE_SELF | IN_UNMOUNT):
                    return
    finally:
        os.close(fd)


if __name__ == "__main__":
    try:
        run(sys.argv[1], len(sys.argv) > 2 and sys.argv[2] == "flat")
    except (OSError, IndexError) as error:
        print(error, file=sys.stderr)
        sys.exit(1)
