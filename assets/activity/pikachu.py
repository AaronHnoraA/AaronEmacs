#!/usr/bin/env python3
"""Draw the activity indicator's sprite frames.

The XPM files beside this script are what Emacs loads, and they are plain
text, so a one-pixel fix can be made in them directly.  Anything bigger is
easier here, because this script also writes a scaled PNG of every frame
side by side -- the only way to actually see what a 28x20 grid looks like.

    python3 pikachu.py            # rewrite the .xpm frames in place
    python3 pikachu.py -p out.png # ... and a magnified preview to look at
"""
import os, sys, zlib, struct

PALETTE = {
    ' ': None,            # transparent
    'o': (0x3a, 0x2c, 0x14),  # outline, dark brown
    'k': (0x1a, 0x14, 0x0a),  # black: ear tips, eyes
    'Y': (0xf8, 0xd0, 0x30),  # pikachu yellow
    'D': (0xd4, 0xa0, 0x18),  # shaded yellow
    'R': (0xe8, 0x40, 0x30),  # cheek red
    'W': (0xff, 0xff, 0xff),  # eye highlight
    'B': (0xa0, 0x5a, 0x20),  # tail base brown
}

W, H = 28, 20

# A full silhouette at mode-line scale: long dark-tipped ears, a separate
# head and body, cheek disks, back stripes, paws, and a zigzag tail.  Tail
# pixels are drawn first so its brown base sits naturally behind the body.
BODY = [
    "     kkk        kk",
    "     kkko      kkk",
    "      kYo      oYk",
    "      oYYo     oYo",
    "      oYYo    oYYo",
    "       oYYooooYYo",
    "      oYYYYYYYYYYYo",
    "     oYYYYYYYYYYYYYo",
    "     oYkWYYYYYYkWYYo",
    "     oYkkYYYYYYkkYYo",
    "    oYRRYYYYYYYYRRYYo",
    "    oYRRYYYYoYYYRRYYo",
    "     oYYYYYYoooYYYYYo",
    "      oYYYYYYYYYBBYYo",
    "       oYYYYYYYYYYYYo",
    "       oYYYYYYYDBBYYo",
    "       oYYYYYYYYYYYYo",
    "       oYYYooYYYYYYYo",
    "      oYYYo  oo oYYYo",
    "      oooo       oooo",
]

TAIL = [
    (4, 26, 27), (5, 24, 27), (6, 23, 26), (7, 24, 26),
    (8, 22, 25), (9, 20, 24), (10, 21, 24), (11, 19, 23),
    (12, 20, 23), (13, 18, 22), (14, 18, 21), (15, 17, 20),
]


def sprite(body, tail_lift=0):
    """Layer a dark-edged lightning tail behind the hand-drawn body."""
    pixels = [[' '] * W for _ in range(H)]
    filled = set()
    for y, start, end in TAIL:
        y -= tail_lift
        for x in range(start, end + 1):
            if 0 <= y < H:
                filled.add((x, y))
    for x, y in filled:
        for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            nx, ny = x + dx, y + dy
            if 0 <= nx < W and 0 <= ny < H and (nx, ny) not in filled:
                pixels[ny][nx] = 'o'
    for x, y in filled:
        pixels[y][x] = 'B' if y >= 14 else 'Y'
    for y, line in enumerate(body):
        for x, ch in enumerate(line):
            if ch != ' ':
                pixels[y][x] = ch
    return [''.join(row) for row in pixels]


STAND = sprite(BODY)
RUN_A_BODY = BODY.copy()
RUN_A_BODY[17:] = [
    "      oYYYooYYYYYYYo",
    "     oYYYo      oYYYo",
    "     oooo        oooo",
]
RUN_B_BODY = BODY.copy()
RUN_B_BODY[17:] = [
    "       oYYYooYYYYYYYo",
    "         oYYoooYYYo",
    "         ooo   oooo",
]
RUN_A = sprite(RUN_A_BODY, 1)
RUN_B = sprite(RUN_B_BODY)


def bob(frame, dy):
    """Shift FRAME down by DY pixels, keeping the canvas size."""
    if dy == 0:
        return frame
    blank = " " * W
    return [blank] * dy + frame[:-dy]


def check(name, frame):
    assert len(frame) == H, f"{name}: {len(frame)} rows, want {H}"
    for i, row in enumerate(frame):
        assert len(row) == W, f"{name} row {i}: {len(row)} cols, want {W}"
        for ch in row:
            assert ch in PALETTE, f"{name} row {i}: unknown char {ch!r}"


def to_xpm(name, frame):
    used = sorted({ch for row in frame for ch in row})
    lines = [f'/* XPM */', f'static char *{name}[] = {{',
             f'"{W} {H} {len(used)} 1",']
    for ch in used:
        rgb = PALETTE[ch]
        colour = "None" if rgb is None else "#%02X%02X%02X" % rgb
        lines.append(f'"{ch} c {colour}",')
    for i, row in enumerate(frame):
        comma = "" if i == H - 1 else ","
        lines.append(f'"{row}"{comma}')
    lines.append("};")
    return "\n".join(lines) + "\n"


def png(path, frames, scale=10, gap=2):
    """Write a scaled side-by-side preview so the sprite can be eyeballed."""
    cols = W * len(frames) + gap * (len(frames) - 1)
    pw, ph = cols * scale, H * scale
    bg = (0x1c, 0x1c, 0x22)
    rows = []
    for y in range(H):
        line = bytearray()
        for x in range(cols):
            fi, fx = divmod(x, W + gap)
            if fx >= W or fi >= len(frames):
                rgb = bg
            else:
                rgb = PALETTE[frames[fi][y][fx]] or bg
            line += bytes(rgb) * scale
        rows.extend([bytes(line)] * scale)
    raw = b"".join(b"\x00" + r for r in rows)

    def chunk(tag, data):
        c = struct.pack(">I", len(data)) + tag + data
        return c + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)

    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n")
        f.write(chunk(b"IHDR", struct.pack(">IIBBBBB", pw, ph, 8, 2, 0, 0, 0)))
        f.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        f.write(chunk(b"IEND", b""))


FRAMES = [("stand", STAND),
          ("run-1", RUN_A),
          ("run-2", bob(RUN_B, 1)),
          ("run-3", RUN_A),
          ("run-4", bob(RUN_B, 1))]

if __name__ == "__main__":
    args = sys.argv[1:]
    preview = None
    if "-p" in args:
        i = args.index("-p")
        preview = args[i + 1]
        del args[i:i + 2]
    outdir = args[0] if args else os.path.dirname(os.path.abspath(__file__))
    os.makedirs(outdir, exist_ok=True)
    for name, frame in [("stand", STAND), ("run-1", RUN_A), ("run-2", RUN_B)]:
        check(name, frame)
    for name, frame in FRAMES:
        check(name, frame)
        with open(os.path.join(outdir, f"pikachu-{name}.xpm"), "w") as f:
            f.write(to_xpm("pikachu_" + name.replace("-", "_"), frame))
    if preview:
        png(preview, [f for _, f in FRAMES])
        print("preview:", preview)
    print("wrote", len(FRAMES), "frames to", outdir)
