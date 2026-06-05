#!/usr/bin/env bash
# split-roam-repo.sh
# 一次性脚本：把 roam/ 物理迁到 ~/Documents/roam，symlink 回来，
# 并把顶层 Org 仓 orphan-reinit 成干净历史。
#
# 用法：bash Aaronnote/split-roam-repo.sh
# 运行前确保 Aaronnote 应用已关闭，工作区干净。
# 脚本不会自动 git push，push 留给你手动确认。

set -euo pipefail

ORG_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ROAM_SRC="$ORG_DIR/roam"
ROAM_DST="$HOME/Documents/AaronNote"

echo "=== Roam 仓库物理分离脚本 ==="
echo "  Org 目录: $ORG_DIR"
echo "  roam 迁移: $ROAM_SRC  →  ~/Documents/AaronNote"
echo ""

# ---- 前置检查 -----------------------------------------------------------

if [ ! -d "$ROAM_SRC" ] || [ -L "$ROAM_SRC" ]; then
  echo "✓ roam/ 已经是 symlink 或不存在，跳过迁移步骤。"
  ALREADY_SPLIT=1
else
  ALREADY_SPLIT=0
fi

if [ -d "$ROAM_DST" ] && [ "$ALREADY_SPLIT" = "0" ]; then
  echo "❌ ~/Documents/roam 已存在，请先手动处理后重试。"
  exit 1
fi

# ---- 步骤 1：Org 仓最后一次快照 -----------------------------------------

if [ "$ALREADY_SPLIT" = "0" ]; then
  echo "【步骤 1】在旧历史里留最后一个快照 commit..."
  cd "$ORG_DIR"
  git add -A
  git diff --cached --quiet && echo "  (无改动，跳过)" || \
    git commit -m "pre-split snapshot: $(date '+%Y-%m-%d %H:%M:%S')"
fi

# ---- 步骤 2：物理迁移 roam → ~/Documents/roam ---------------------------

if [ "$ALREADY_SPLIT" = "0" ]; then
  echo ""
  echo "【步骤 2】物理迁移 roam/ → ~/Documents/roam"
  read -r -p "  继续？ [y/N] " yn
  [[ "$yn" =~ ^[Yy]$ ]] || { echo "已取消。"; exit 0; }

  mv "$ROAM_SRC" "$ROAM_DST"
  ln -s "$ROAM_DST" "$ROAM_SRC"
  echo "  ✓ 迁移完成，symlink 已建立。"
fi

# ---- 步骤 3：roam 仓初始化 -----------------------------------------------

if [ ! -d "$ROAM_DST/.git" ]; then
  echo ""
  echo "【步骤 3】初始化 ~/Documents/roam 为独立 git 仓..."
  cd "$ROAM_DST"

  git init -b master

  cat > .gitignore <<'GITIGNORE'
roam.db
.aaronnote-sync-state.json
GITIGNORE

  git add -A
  git commit -m "initial roam snapshot: $(date '+%Y-%m-%d %H:%M:%S')"
  echo "  ✓ roam 仓初始化完成（$(git rev-parse --short HEAD)）"
else
  echo "  ✓ ~/Documents/roam/.git 已存在，跳过初始化。"
fi

# ---- 步骤 4：Org 仓 orphan reinit ----------------------------------------

echo ""
echo "【步骤 4】Org 仓 orphan-reinit（清除旧历史）"
echo "  ⚠️  此操作不可逆：旧历史 90 天后 gc 彻底丢失。"
echo "  远程仓需之后手动 git push --force origin master。"
read -r -p "  继续？ [y/N] " yn
[[ "$yn" =~ ^[Yy]$ ]] || { echo "已取消。旧历史未修改。"; exit 0; }

cd "$ORG_DIR"

# 确保 roam 在 .gitignore 里
if ! grep -qxF 'roam' .gitignore 2>/dev/null; then
  echo 'roam' >> .gitignore
  echo "  + 追加 'roam' 到 .gitignore"
fi

# 确保 roam.db 也被忽略（symlink 指向 Documents，但双重保险）
if ! grep -qxF 'roam/roam.db' .gitignore 2>/dev/null && ! grep -qxF 'roam.db' .gitignore 2>/dev/null; then
  echo 'roam/roam.db' >> .gitignore
fi

REMOTE_URL=$(git config --get remote.origin.url 2>/dev/null || echo "")

git checkout --orphan __fresh
git add -A
git commit -m "fresh start: split roam to its own repo ($(date '+%Y-%m-%d'))"
git branch -D master 2>/dev/null || true
git branch -m master
git gc --prune=now --aggressive

echo ""
echo "  ✓ Org 仓历史已清除。当前 commit: $(git rev-parse --short HEAD)"

if [ -n "$REMOTE_URL" ]; then
  echo ""
  echo "  远程仓库: $REMOTE_URL"
  echo "  清除完成后需要运行："
  echo ""
  echo "    git push --force origin master"
  echo ""
  echo "  （脚本不自动执行 push，请确认后手动运行）"
fi

echo ""
echo "=== 完成 ==="
echo "  roam 真实路径: $ROAM_DST"
echo "  roam symlink:  $ROAM_SRC → $ROAM_DST"
echo "  Aaronnote 应用直接重启即可，路径透明无需改配置。"
