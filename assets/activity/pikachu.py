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
    'o': (0x77, 0x4b, 0x24),  # warm outline, legible on a dark mode line
    'k': (0x28, 0x22, 0x29),  # charcoal ear tips and eyes
    'Y': (0xff, 0xd5, 0x42),  # golden yellow
    'L': (0xff, 0xe9, 0x85),  # forehead and belly light
    'D': (0xe9, 0xa5, 0x29),  # amber shade
    'R': (0xf2, 0x62, 0x4c),  # coral cheeks
    'W': (0xff, 0xff, 0xff),  # eye highlight
    'B': (0xa0, 0x5a, 0x20),  # tail base brown
}

W, H = 28, 20

# Front-facing chibi silhouette: splayed ears, round cheeks, little paws.
# Keep the last row empty so the running bob never clips the feet.
# Stripes belong on the back, not across this front-facing belly.
BODY = [
    "    kk             kk",
    "    kkk           kkk",
    "     kYo         oYk",
    "     oYYo       oYYo",
    "      oYYoooooooYYo",
    "      oYYLLLLLYYYYo",
    "     oYLLLLYYYYYYYYo",
    "     oYYkWYYYYYkWYYo",
    "    oYYYkkYYYYYkkYYYo",
    "    oYRYYYYYkYYYYYRYo",
    "    oRRRYYYoYoYYYRRRo",
    "     oRYYYYYoYYYYRo",
    "      oYYYYYYYYYYo",
    "       oDYYYYYYDo",
    "      oYYoLLLLoYYo",
    "      oYDoLLLLoDYo",
    "       oYYLLLLYYo",
    "      oYYDooooDYYo",
    "      oooo    oooo",
    "",
]

TAIL = [
    (5, 24, 26), (6, 23, 26), (7, 22, 25),
    (8, 23, 24), (9, 23, 25), (10, 22, 24),
    (11, 21, 23), (12, 22, 23), (13, 21, 22),
    (14, 19, 21), (15, 18, 20),
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
RUN_A_BODY[17:19] = [
    "     oYYYDooooDYYo",
    "     oooo     oooo",
]
RUN_B_BODY = BODY.copy()
RUN_B_BODY[17:19] = [
    "       oYDooooDYYYo",
    "       ooo    ooooo",
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
          ("run-2", bob(RUN_A, 1)),
          ("run-3", RUN_B),
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
