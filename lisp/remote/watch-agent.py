"""Recursive Linux file events for Emacs Remote without inotify-tools.

Usage: python3 -u -c SOURCE DIRECTORY.  Standard output consists of pairs of
NUL-terminated event names and absolute paths.  The agent uses only Python's
standard library and libc; it is sent as source on process startup, so no
target-side installation or package manager is needed.
"""

import ctypes
import os
import select
import struct
import sys


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


def run(root):
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

    try:
        add_tree(root)
        if root not in by_path:
            raise NotADirectoryError(root)
        emit("READY", root)
        while True:
            select.select([fd], [], [])
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
                if mask & IN_IGNORED:
                    by_path.pop(directory, None)
                    by_wd.pop(wd, None)
                    if directory == root:
                        emit("IGNORED", root)
                        return
                    continue
                if mask & (IN_MOVED_FROM | IN_DELETE) and mask & IN_ISDIR:
                    remove_tree(path)
                if mask & (IN_MOVED_TO | IN_CREATE) and mask & IN_ISDIR:
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
        run(sys.argv[1])
    except (OSError, IndexError) as error:
        print(error, file=sys.stderr)
        sys.exit(1)
