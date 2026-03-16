\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{default}[2026/03/16 Default Org XeLaTeX class]

\LoadClass[11pt]{article}

\RequirePackage{iftex}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\ifXeTeX
  \RequirePackage{xeCJK}
  \IfFontExistsTF{Songti SC}{\setCJKmainfont{Songti SC}}{}
  \IfFontExistsTF{PingFang SC}
    {\setCJKsansfont{PingFang SC}}
    {\IfFontExistsTF{Hiragino Sans GB}{\setCJKsansfont{Hiragino Sans GB}}{}}
\fi

\endinput
