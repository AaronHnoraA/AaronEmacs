;;; "Compiled" snippets and support files for `org-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("zzzz"
                        "\\zeta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "Zeta" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/zzzz_Zeta" nil nil)
                       ("xx"
                        "\\times `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "Times" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/xx_Times" nil nil)
                       ("while"
                        "\\While{$1}\n	\\State $0\n\\EndWhile `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm:While" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/while_Algorithm_While"
                        nil nil)
                       ("vmat"
                        "\\begin{vmatrix}\n$1\n\\end{vmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Vmatrix" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/vmat_Vmatrix" nil
                        nil)
                       ("vb"
                        "#+begin_src verb -n :wrap src ob-verb-response\n\n#+end_src `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "vb" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/verb" nil nil)
                       ("vec"
                        "\\vec{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Vector" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/vec_Vector" nil
                        nil)
                       ("v("
                        "(${VISUAL}) `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Parentheses (visual)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/v_Parentheses_visual_"
                        nil nil)
                       ("v["
                        "[${VISUAL}] `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Brackets (visual)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/v_Brackets_visual_"
                        nil nil)
                       ("v{"
                        "{${VISUAL}} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Braces (visual)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/v_Braces_visual_"
                        nil nil)
                       ("uuuu"
                        "\\upsilon `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Upsilon (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/uuuu_Upsilon_lowercase_"
                        nil nil)
                       ("und"
                        "\\underline{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Underline" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/und_Underline" nil
                        nil)
                       ("tttt"
                        "\\theta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Theta (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/tttt_Theta_lowercase_"
                        nil nil)
                       ("tr"
                        "#+transclude: [[][]] :level 2 :lines 1-2 :src java :rest \"-n\" `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "tr" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/transclude" nil
                        nil)
                       ("trace"
                        "\\mathrm{Tr} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Trace" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/trace_Trace" nil
                        nil)
                       ("tt"
                        "#+title: `(file-name-sans-extension (buffer-name))`\n#+subtitle: this is subtitle\n#+author: `(getenv \"USER\")`\n#+date: `(format-time-string \"<%Y-%m-%d %H:%M>\")`\n#+SETUPFILE: ~/.doom.d/org-classic-head.setup `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "title" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/title" nil nil)
                       ("timest"
                        "`(substring (number-to-string (* (time-to-seconds) 1000)) 0 13)` `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "timest" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/timest" nil nil)
                       ("tilde"
                        "\\tilde{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Tilde Accent" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/tilde_Tilde_Accent"
                        nil nil)
                       ("thml"
                        "\\begin{theorem}{$1}\\label{thm:$1}\n	$2\n\\end{theorem}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Theorem (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/thml_Theorem_with_label_"
                        nil nil)
                       ("theorem"
                        "\\begin{theorem}{$1}\n	$2\n\\end{theorem}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Theorem (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/theorem_Theorem_no_label_"
                        nil nil)
                       ("text"
                        "\\text{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Text Environment" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/text_Text_Environment"
                        nil nil)
                       ("tayl"
                        "$1($2 + $3) = $1($2) + $1'($2)$3 + $1''($2) \\frac{$3^{2}}{2!} + \\dots$4 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Taylor Expansion" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/tayl_Taylor_Expansion"
                        nil nil)
                       ("table:ref"
                        "${1:Table}~\\ref{tab:$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Table:Ref" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/table_ref_Table_Ref"
                        nil nil)
                       ("table:acm:*"
                        "\\begin{table*}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{tabular}{${3:ccl}}\n		\\toprule\n		$4\n		a & b & c \\\\\\\\\n		\\midrule\n		d & e & f \\\\\\\\\n		\\bottomrule\n	\\end{tabular}\n\\end{table*}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Table:ACM:*" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/table_acm_Table_ACM_"
                        nil nil)
                       ("table:acm"
                        "\\begin{table}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{tabular}{${3:ccl}}\n		\\toprule\n		$4\n		a & b & c \\\\\\\\\n		\\midrule\n		d & e & f \\\\\\\\\n		\\bottomrule\n	\\end{tabular}\n\\end{table}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Table:ACM" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/table_acm_Table_ACM"
                        nil nil)
                       ("table"
                        "\\begin{table}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{center}\n		\\begin{tabular}[c]{l|l}\n			\\hline\n			\\multicolumn{1}{c|}{\\textbf{$3}} & \n			\\multicolumn{1}{c}{\\textbf{$4}} \\\\\\\\\n			\\hline\n			a & b \\\\\\\\\n			c & d \\\\\\\\\n			$5\n			\\hline\n		\\end{tabular}\n	\\end{center}\n\\end{table}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Table" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/table_Table" nil
                        nil)
                       ("tabl"
                        "\\begin{tabular}{${1:c}}\\label{tab:$2}\n$0\n\\end{tabular} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Tabular (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/tabl_Tabular_with_label_"
                        nil nil)
                       ("tab"
                        "\\begin{tabular}{${1:c}}\n$0\n\\end{tabular} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Tabular (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/tab_Tabular_no_label_"
                        nil nil)
                       ("sup="
                        "\\supseteq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Superset Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sup_Superset_Equal"
                        nil nil)
                       ("sum"
                        "\\sum `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sum" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sum_Sum" nil nil)
                       ("subsl"
                        "\\subsubsection{$1}\\label{sec:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Sub Section (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subsl_Sub_Sub_Section_with_label_"
                        nil nil)
                       ("subs"
                        "\\subsubsection{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Sub Section (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subs_Sub_Sub_Section_no_label_"
                        nil nil)
                       ("subpl"
                        "\\subparagraph{$1}\\label{subp:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Paragraph (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subpl_Sub_Paragraph_with_label_"
                        nil nil)
                       ("subp"
                        "\\subparagraph{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Paragraph (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subp_Sub_Paragraph_no_label_"
                        nil nil)
                       ("subl"
                        "\\subsection{$1}\\label{sub:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Section (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subl_Sub_Section_with_label_"
                        nil nil)
                       ("subfile"
                        "\\subfile{$1}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Subfile" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/subfile_Subfile"
                        nil nil)
                       ("sub="
                        "\\subseteq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Subset Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sub_Subset_Equal"
                        nil nil)
                       ("sub"
                        "\\subsection{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sub Section (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sub_Sub_Section_no_label_"
                        nil nil)
                       ("sts"
                        "_\\text{$1} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Text Subscript" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sts_Text_Subscript"
                        nil nil)
                       ("state"
                        "\\State $1 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm:State" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/state_Algorithm_State"
                        nil nil)
                       ("ssss"
                        "\\sigma `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sigma (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ssss_Sigma_lowercase_"
                        nil nil)
                       ("sr"
                        "^{2} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Square" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sr_Square" nil nil)
                       ("sql"
                        "#+BEGIN_SRC sql\nselect * from $0 limit 1\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "org-sql" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sql" nil nil)
                       ("sq"
                        "\\sqrt{ $1 }$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Square Root" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sq_Square_Root"
                        nil nil)
                       ("spl"
                        "\\begin{split}\n	$0\n\\end{split} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Split" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/spl_Split" nil nil)
                       ("solution"
                        "\\begin{solution}\n	$1\n\\end{solution}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Solution" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/solution_Solution"
                        nil nil)
                       ("simm"
                        "\\sim `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Similar To" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/simm_Similar_To"
                        nil nil)
                       ("sim="
                        "\\simeq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Approx Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sim_Approx_Equal"
                        nil nil)
                       ("setsubfile"
                        "\\documentclass[$1]{subfiles}\n\\graphicspath{{$2}}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "SetSubfile" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/setsubfile_SetSubfile"
                        nil nil)
                       ("set"
                        "\\{ $1 \\}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Set" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/set_Set" nil nil)
                       ("section:ref"
                        "${1:Section}~\\ref{sec:$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Section:Ref" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/section_ref_Section_Ref"
                        nil nil)
                       ("secl"
                        "\\section{$1}\\label{sec:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Section (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/secl_Section_with_label_"
                        nil nil)
                       ("sec"
                        "\\section{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Section (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/sec_Section_no_label_"
                        nil nil)
                       ("rm"
                        "\\mathrm{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Roman" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/rm_Roman" nil nil)
                       ("remark"
                        "\\begin{remark}\n	$1\n\\end{remark}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Remark" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/remark_Remark" nil
                        nil)
                       ("ref"
                        "\\ref{$1: $2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Reference" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ref_Reference" nil
                        nil)
                       ("readme"
                        "Porgram kernel\n============\n\nThere are several guides for developers and users. These guides can\nbe rendered in a number of formats, like HTML and PDF. Please read\nDocumentation/admin-guide/README.rst first.\n\nPlease read the Documentation/process/changes.rst file, as it contains the\nrequirements for building and running this program, and information about\nthe problems which may result by upgrading your program. `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "readme" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/readme" nil nil)
                       ("rd"
                        "^{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Raise to Power" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/rd_Raise_to_Power"
                        nil nil)
                       ("qu"
                        "#+begin_quote\n注意：这是一个特殊的说明\n#+end_quote `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "qu" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/quote" nil nil)
                       ("pu"
                        "\\pu{ $1 } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Physical Units" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/pu_Physical_Units"
                        nil nil)
                       ("proposition"
                        "\\begin{proposition}{$1}\n		$2\n\\end{proposition}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Proposition (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/proposition_Proposition_no_label_"
                        nil nil)
                       ("propl"
                        "\\begin{proposition}{$1}\\label{pro:$1}\n		$2\n\\end{proposition}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Proposition (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/propl_Proposition_with_label_"
                        nil nil)
                       ("prop"
                        "\\propto `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Proportional To" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/prop_Proportional_To"
                        nil nil)
                       ("pd"
                        "■`(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "proof end" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/proof_end" nil nil)
                       ("proof"
                        "\\begin{proof}\n	$1\n\\end{proof}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Proof" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/proof_Proof" nil
                        nil)
                       ("prod"
                        "\\prod `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Product" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/prod_Product" nil
                        nil)
                       ("problemset"
                        "\\begin{problemset}\n	$1\n\\end{problemset}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Problemset" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/problemset_Problemset"
                        nil nil)
                       ("problem"
                        "\\begin{problem}\n	$1\n\\end{problem}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Problem" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/problem_Problem"
                        nil nil)
                       ("postulate"
                        "\\begin{postulate}{$1}\n		$2\n\\end{postulate}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Postulate (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/postulate_Postulate_no_label_"
                        nil nil)
                       ("postl"
                        "\\begin{postulate}{$1}\\label{pos:$1}\n		$2\n\\end{postulate}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Postulate (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/postl_Postulate_with_label_"
                        nil nil)
                       ("pmat"
                        "\\begin{pmatrix}\n$1\n\\end{pmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Pmatrix" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/pmat_Pmatrix" nil
                        nil)
                       ("ps"
                        "#DARKORANGE/LIGHTORANGE/DARKBLUE/LIGHTBLUE/DARKRED/LIGHTRED/DARKGREEN/LIGHTGREEN\n!define DARKBLUE\n!includeurl ~/org/org-roam/emacs/C4-PlantUML/juststyle.puml `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "plantuml-style" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/plantuml-style"
                        nil nil)
                       ("plaininline"
                        "\\lstinline{$1}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "lstinline" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/plaininline_lstinline"
                        nil nil)
                       ("plain"
                        "\\begin{lstlisting}\n	$1\n\\end{lstlisting}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "lstlisting" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/plain_lstlisting"
                        nil nil)
                       ("part"
                        "\\begin{part}\n	$0\n\\end{part} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Part" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/part_Part" nil nil)
                       ("parl"
                        "\\paragraph{$1}\\label{par:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Paragraph (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/parl_Paragraph_with_label_"
                        nil nil)
                       ("para"
                        "\\parallel `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Parallel" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/para_Parallel" nil
                        nil)
                       ("par"
                        "\\frac{ \\partial $1 }{ \\partial $2 } $3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Partial Derivative" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/par_Partial_Derivative"
                        nil nil)
                       ("par"
                        "\\paragraph{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Paragraph (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/par_Paragraph_no_label_"
                        nil nil)
                       ("page"
                        "${1:page}~\\pageref{$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Page" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/page_Page" nil nil)
                       ("packages.el"
                        ";;; packages.el --- Description -*- lexical-binding: t; -*-\n;;\n;; Copyright (C) 2023 自杰\n;;\n;; Author: 自杰 <van@windos99.local>\n;; Maintainer: 自杰 <van@windos99.local>\n;; Created: September 27, 2023\n;; Modified: September 27, 2023\n;; Version: 0.0.1\n;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp\n;; Homepage: https://github.com/van/packages\n;; Package-Requires: ((emacs \"24.3\"))\n;;\n;; This file is not part of GNU Emacs.\n;;\n;;; Commentary:\n;;\n;;  Description\n;;\n;;; Code:\n\n\n\n(provide 'packages)\n;;; packages.el ends here `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "packages.el" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/packages.el" nil
                        nil)
                       ("ox"
                        "\\otimes `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Tensor Product" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ox_Tensor_Product"
                        nil nil)
                       ("outlineexp"
                        "\\\\[\n	$1\n\\\\]\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "OutlineExp" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/outlineexp_OutlineExp"
                        nil nil)
                       ("outer"
                        "\\ket{$1} \\bra{$1} $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Outer Product" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/outer_Outer_Product"
                        nil nil)
                       ("orr"
                        "\\cup `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Union" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/orr_Union" nil nil)
                       ("order"
                        "#+BEGIN_SRC plantuml :file ./image/time.svg\n!define LIGHTGREEN\n!includeurl /Users/van/org/org-roam/emacs/C4-PlantUML/juststyle.puml\nskinparam backgroundColor transparent\n\nA -> B: reqest\nB -> B: handle\nB -> A: response\n\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "order" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/order" nil nil)
                       ("openlink"
                        "http://10.31.2.53/openlink.html?link=$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "NasOpenlink" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/openlink_NasOpenlink"
                        nil nil)
                       ("oooo"
                        "\\omega `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Omega (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/oooo_Omega_lowercase_"
                        nil nil)
                       ("ooo"
                        "\\infty `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Infinity" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ooo_Infinity" nil
                        nil)
                       ("ome"
                        "\\omega `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Omega (alt)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ome_Omega_alt_"
                        nil nil)
                       ("oint"
                        "\\oint `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Contour Integral" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/oint_Contour_Integral"
                        nil nil)
                       ("oinf"
                        "\\int_{0}^{\\infty} $1 \\, d$2 $3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Integral 0 to Infinity" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/oinf_Integral_0_to_Infinity"
                        nil nil)
                       ("o+"
                        "\\oplus `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Direct Sum" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/o+_Direct_Sum" nil
                        nil)
                       ("notin"
                        "\\not\\in `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Not Element Of" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/notin_Not_Element_Of"
                        nil nil)
                       ("note"
                        "\\begin{note}\n	$1\n\\end{note}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Note" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/note_Note" nil nil)
                       ("norm"
                        "\\lvert $1 \\rvert $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Absolute Value" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/norm_Absolute_Value"
                        nil nil)
                       ("nabl"
                        "\\nabla `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Nabla" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/nabl_Nabla" nil
                        nil)
                       ("msun"
                        "M_{\\odot} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Solar Mass" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/msun_Solar_Mass"
                        nil nil)
                       ("mod"
                        "|$1|$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Modulus" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/mod_Modulus" nil
                        nil)
                       ("mind"
                        "#+BEGIN_SRC plantuml :file ./image/mind.svg\n@startmindmap\n,* A\n,**[#Orange] C\n,**[#Orange] D\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "mind" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/mind" nil nil)
                       ("math"
                        "\\begin{math}\n	$1\n\\end{math}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Math" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/math_Math" nil nil)
                       ("mat"
                        "\\begin{${1:p/b/v/V/B/small}matrix}\n	$0\n\\end{${1:p/b/v/V/B/small}matrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Matrix" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/mat_Matrix" nil
                        nil)
                       ("me"
                        "\\langle ${1:\\phi}\\rvert ${2:A}\\lvert ${3:\\psi}\\rangle $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "matrix element" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/martix_braket" nil
                        nil)
                       ("marginpar"
                        "\\marginpar{$1}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Marginpar" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/marginpar_Marginpar"
                        nil nil)
                       ("lra"
                        "\\left< $1 \\right> $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Angle" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lra_Left-Right_Angle"
                        nil nil)
                       ("lr("
                        "\\left( $1 \\right) $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Parentheses" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lr_Left-Right_Parentheses"
                        nil nil)
                       ("lr["
                        "\\left[ $1 \\right] $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Brackets" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lr_Left-Right_Brackets"
                        nil nil)
                       ("lr{"
                        "\\left\\{ $1 \\right\\} $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Braces" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lr_Left-Right_Braces"
                        nil nil)
                       ("lr|"
                        "\\left| $1 \\right| $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Absolute" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lr_Left-Right_Absolute"
                        nil nil)
                       ("llll"
                        "\\lambda `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Lambda (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/llll_Lambda_lowercase_"
                        nil nil)
                       ("listing:ref"
                        "${1:Listing}~\\ref{lst:$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Listing:Ref" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/listing_ref_Listing_Ref"
                        nil nil)
                       ("lim"
                        "\\lim_{ $1 \\to $2 } $3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Limit" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lim_Limit" nil nil)
                       ("lemmal"
                        "\\begin{lemma}{$1}\\label{lem:$1}\n	$2\n\\end{lemma}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Lemma (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lemmal_Lemma_with_label_"
                        nil nil)
                       ("lemma"
                        "\\begin{lemma}{$1}\n	$2\n\\end{lemma}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Lemma (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/lemma_Lemma_no_label_"
                        nil nil)
                       ("lax"
                        "$$\\begin{aligned}\nA &= B \\times C \\\\\nDelay_{A} &= \\frac{A（bit）}{B（bit/s）} \\\\\n\\end{aligned} $$ `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "latex" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/latex" nil nil)
                       ("kkkk"
                        "\\kappa `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Kappa" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/kkkk_Kappa" nil
                        nil)
                       ("ket1"
                        "\\lvert 1\\rangle $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "ket1" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ket1" nil nil)
                       ("ket0"
                        "\\lvert 0\\rangle $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "ket0" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ket0" nil nil)
                       ("ket"
                        "\\lvert ${1:\\psi}\\rangle $0`(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "ket" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ket" nil nil)
                       ("kbt"
                        "k_{B}T `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Boltzmann Constant" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/kbt_Boltzmann_Constant"
                        nil nil)
                       ("json"
                        "#+BEGIN_SRC plantuml :file json-t.png\n@startjson\n<style>\ndocument {\n  BackGroundColor transparent\n}\n</style>\n#highlight \"lastName\"\n#highlight \"address\" / \"city\"\n#highlight \"phoneNumbers\" / \"0\" / \"number\"\n{\n  \"lastName\": \"Smith\",\n  \"address\": {\n    \"streetAddress\": \"21 2nd Street\",\n    \"city\": \"New York\",\n  }\n}\n@endjson\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "json" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/json" nil nil)
                       ("item"
                        "\\item $1 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "item" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/item_item" nil nil)
                       ("item"
                        "\\\\begin{itemize}\n	\\item $0\n\\\\end{itemize} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Itemize" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/item_Itemize" nil
                        nil)
                       ("iso"
                        "{}^{$1}_{$2}$3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Isotope" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/iso_Isotope" nil
                        nil)
                       ("invs"
                        "^{-1} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Inverse" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/invs_Inverse" nil
                        nil)
                       ("introduction"
                        "\\begin{introduction}\n	$1\n\\end{introduction}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Introduction" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/introduction_Introduction"
                        nil nil)
                       ("int"
                        "\\int $1 \\, d$2 $3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Integral" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/int_Integral" nil
                        nil)
                       ("inn"
                        "\\in `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Element Of" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/inn_Element_Of"
                        nil nil)
                       ("inlineexp"
                        "\\\\($1\\\\)$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "InlineExp" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/inlineexp_InlineExp"
                        nil nil)
                       ("infi"
                        "\\int_{-\\infty}^{\\infty} $1 \\, d$2 $3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Integral -Inf to Inf" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/infi_Integral_-Inf_to_Inf"
                        nil nil)
                       ("iint"
                        "\\iint `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Double Integral" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/iint_Double_Integral"
                        nil nil)
                       ("iiint"
                        "\\iiint `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Triple Integral" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/iiint_Triple_Integral"
                        nil nil)
                       ("iiii"
                        "\\iota `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Iota" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/iiii_Iota" nil nil)
                       ("if"
                        "\\If{$1}\n\\ElsIf{$2}\n\\Else\n\\EndIf `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm:If" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/if_Algorithm_If"
                        nil nil)
                       ("iden"
                        "\\begin{pmatrix}\n1 & 0 & \\dots & 0 \\\\\n0 & 1 & \\dots & 0 \\\\\n\\vdots & \\vdots & \\ddots & \\vdots \\\\\n0 & 0 & \\dots & 1\n\\end{pmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Identity Matrix" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/iden_Identity_Matrix"
                        nil nil)
                       ("bg"
                        "#+ATTR_HTML: :style background: #3498db `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "htmlbg" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/htmlbg" nil nil)
                       ("hide"
                        "\\begin{hide}\n	$1\n\\end{hide}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "hide" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/hide_hide" nil nil)
                       ("he4"
                        "{}^{4}_{2}He `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Helium-4" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/he4_Helium-4" nil
                        nil)
                       ("he3"
                        "{}^{3}_{2}He `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Helium-3" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/he3_Helium-3" nil
                        nil)
                       ("hat"
                        "\\hat{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Hat Accent" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/hat_Hat_Accent"
                        nil nil)
                       ("gk"
                        "* 发版\n|       时间 | 系统 | 版本 | 说明                   | 核心开发 |\n|            |      |      |                        |          | `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "gk" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/gk" nil nil)
                       ("gggg"
                        "\\gamma `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Gamma (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/gggg_Gamma_lowercase_"
                        nil nil)
                       ("gat"
                        "\\begin{gather}\n	$0\n\\end{gather} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Gather(ed)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/gat_Gather_ed_"
                        nil nil)
                       ("frac"
                        "\\frac{$1}{$2}$3 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Fraction" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/frac_Fraction" nil
                        nil)
                       ("for"
                        "\\For{i=0:$1}\n	\\State $0\n\\EndFor `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm:For" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/for_Algorithm_For"
                        nil nil)
                       ("floor"
                        "\\lfloor $1 \\rfloor $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Floor" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/floor_Floor" nil
                        nil)
                       ("figure:ref"
                        "${1:Figure}~\\ref{fig:$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Figure:Ref" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/figure_ref_Figure_Ref"
                        nil nil)
                       ("figure:acm:*"
                        "\\begin{figure*}\n	\\includegraphics[width=0.45\\textwidth]{figures/$1}\n	\\caption{$2}\\label{fig:$3}\n\\end{figure*}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Figure:ACM:*" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/figure_acm_Figure_ACM_"
                        nil nil)
                       ("figure:acm"
                        "\\begin{figure}\n	\\includegraphics[width=0.45\\textwidth]{figures/$1}\n	\\caption{$2}\\label{fig:$3}\n\\end{figure}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Figure:ACM" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/figure_acm_Figure_ACM"
                        nil nil)
                       ("figure"
                        "\\begin{figure}\n	\\begin{center}\n		\\includegraphics[width=0.95\\textwidth]{figures/$1}\n	\\end{center}\n	\\caption{$3}\\label{fig:$4}\n\\end{figure}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Figure" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/figure_Figure" nil
                        nil)
                       ("exercise"
                        "\\begin{exercise}\n	$1\n\\end{exercise}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Exercise" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/exercise_Exercise"
                        nil nil)
                       ("example"
                        "\\begin{example}\n	$1\n\\end{example}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Example" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/example_Example"
                        nil nil)
                       ("eset"
                        "\\emptyset `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Empty Set" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/eset_Empty_Set"
                        nil nil)
                       ("equation"
                        "\\begin{equation}\n	$0\n	\\label{eq:$1}\n\\end{equation} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Equation" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/equation_Equation"
                        nil nil)
                       ("equ"
                        "\\begin{equation*}\n	$1\n\\end{equation*} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Equ" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/equ_Equ" nil nil)
                       ("enumerate"
                        "\\\\begin{enumerate}\n	\\item $0\n\\\\end{enumerate} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Enumerate" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/enumerate_Enumerate"
                        nil nil)
                       ("ent"
                        "entity $0 {\nID\n--\n,* Name\n}\nnote left #5DADE2 : comment `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "entityt" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/entityt" nil nil)
                       ("entity"
                        "#+BEGIN_SRC plantuml :file ./image/db.svg\n!define LIGHTGREEN\n!includeurl /Users/van/org/org-roam/emacs/C4-PlantUML/juststyle.puml\nscale 550 width\nskinparam backgroundColor transparent\nentity t_credit_apply_log {\n  * id\n  --\n  * 客户id\n  * 客户类型\n  * ...\n}\nnote left #red: 申请表\\n1.获取合同\\t插入\\n2.签署合同\\t更新\n\nentity t_product_info {\n  * id\n  -\n  * 产品名称\n  * 产品利率\n  * 资方id\n  * ...\n}\nnote right #6495ED: 产品信息表\n\nt_credit_apply_log }o--|| t_product_info\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "entity" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/entity" nil nil)
                       ("empty"
                        "\\null\\thispagestyle{empty}\n\\newpage\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "EmptyPage" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/empty_EmptyPage"
                        nil nil)
                       ("eeee"
                        "\\epsilon `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Epsilon" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/eeee_Epsilon" nil
                        nil)
                       ("ee"
                        "e^{ $1 }$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Exponential" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ee_Exponential"
                        nil nil)
                       ("e\\xi sts"
                        "\\exists `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Exists" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/e_xi_sts_Exists"
                        nil nil)
                       ("dot"
                        "\\dot{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Dot Accent" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/dot_Dot_Accent"
                        nil nil)
                       ("dot"
                        "#+BEGIN_SRC DOT :file image/dotgra.svg\n    digraph G {\n        node [shape=\"box\",fontcolor=\"0xfffff\"]\n        bgcolor=\"transparent\"\n        node [shape=\"box\",fontcolor=\"#c475db\"]\n        edge [color=\"#a69fe0\",fontcolor=white]\n        rankdir = TD\n         NC -> SlaughterServer1 [dir=both,minlen=2,label=\"ϟ\"]\n         NC -> SlaughterServer2 [dir=both,minlen=2,label=\"ϟ\"]\n\n        subgraph clusterD {\n            label = \"Local\";\n            SlaughterServer2 -> LocalDB2 [splines=ortho]\n            SlaughterServer2 -> SlaughterClient2 [minlen=1]\n            {rank=same;  SlaughterServer2 , LocalDB2 }\n        }\n\n        subgraph clusterM {\n            label = \"Local\";\n            SlaughterServer1 -> LocalDB1 [splines=ortho]\n            SlaughterServer1 -> SlaughterClient1 [minlen=1]\n            {rank=same;  SlaughterServer1 , LocalDB1 }\n        }\n    }\n#+END_SRC `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "dot" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/dot" nil nil)
                       (";"
                        "\\\\\\($1\\\\\\)$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "display inline math" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/disply-inline-math"
                        nil nil)
                       ("displaymath"
                        "\\begin{displaymath}\n	$1\n\\end{displaymath}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "DisplayMath" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/displaymath_DisplayMath"
                        nil nil)
                       (";;"
                        "\\[\n$1\n\\] $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "display math" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/display-math" nil
                        nil)
                       ("dint"
                        "\\int_{$1}^{$2} $3 \\, d$4 $5 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Definite Integral" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/dint_Definite_Integral"
                        nil nil)
                       ("desc"
                        "\\\\begin{description}\n	\\item[$1] $0\n\\\\end{description} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Description" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/desc_Description"
                        nil nil)
                       ("del"
                        "\\nabla `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Del" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/del_Del" nil nil)
                       ("defl"
                        "\\begin{definition}{$1}\\label{def:$1}\n	$2\n\\end{definition}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Definition (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/defl_Definition_with_label_"
                        nil nil)
                       ("definition"
                        "\\begin{definition}{$1}\n	$2\n\\end{definition}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Definition (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/definition_Definition_no_label_"
                        nil nil)
                       ("ddt"
                        "\\frac{d}{dt} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Time Derivative" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ddt_Time_Derivative"
                        nil nil)
                       ("ddot"
                        "\\ddot{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Double Dot Accent" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ddot_Double_Dot_Accent"
                        nil nil)
                       ("dddd"
                        "\\delta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Delta (lowercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/dddd_Delta_lowercase_"
                        nil nil)
                       ("datechange"
                        "\\datechange{$1}{$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Datechange" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/datechange_Datechange"
                        nil nil)
                       ("daily"
                        "* 汇报日期： `(format-time-string \"%Y-%m-%d\")`\n本周工作：\n\n下周工作： `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "daily" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/daily" nil nil)
                       ("dag"
                        "^{\\dagger} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Dagger" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/dag_Dagger" nil
                        nil)
                       ("d2m"
                        "# -*- mode: snippet -*-\n# name:\n# key: trigger-key\n# condition: t\n# -- `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "d2m" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/d2m" nil nil)
                       ("d2"
                        "#+begin_src d2 :file demo.svg\ndirection  : right\nstyle.fill : transparent\nvars: {\n  nodecolor          : \"#E67E22\"\n  style-stroke       : \"#17202A\"\n  style-stroke-width : 2\n  style-fill-pattern : dots\n  style-shadow       : true\n  line-style-fill    : \"#884EA0\"\n}\n\nclasses: {\n    2dn: {\n        style.multiple     : true\n    }\n    3dn: {\n        style.3d           : true\n    }\n    2de: {\n        style.animated     : true\n        style.stroke-width : \\${style-stroke-width}\n        style.stroke       : \\${line-style-fill}\n    }\n}\n\n\nA :    { class : 2dn }\nB :    { class : 3dn }\nA -> B { class : 2de }\n\n*.style.fill         : \\${nodecolor}\n*.style.stroke       : \\${style-stroke}\n*.style.stroke-width : \\${style-stroke-width}\n*.style.fill-pattern : \\${style-fill-pattern}\n*.style.shadow       : \\${style-shadow}\n\n#+end_src `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "d2" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/d2" nil nil)
                       ("corollary"
                        "\\begin{corollary}{$1}\n	$2\n\\end{corollary}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Corollary (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/corollary_Corollary_no_label_"
                        nil nil)
                       ("corl"
                        "\\begin{corollary}{$1}\\label{cor:$1}\n	$2\n\\end{corollary}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Corollary (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/corl_Corollary_with_label_"
                        nil nil)
                       ("conclusion"
                        "\\begin{conclusion}\n	$1\n\\end{conclusion}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Conclusion" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/conclusion_Conclusion"
                        nil nil)
                       ("compactitem"
                        "\\begin{compactitem}\n	\\item $1\n\\end{compactitem}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Compactitem" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/compactitem_Compactitem"
                        nil nil)
                       ("ct"
                        "#+title: `(file-name-sans-extension (buffer-name))`\n#+subtitle: this is subtitle\n#+author: `(getenv \"USER\")`\n#+SETUPFILE: ~/.doom.d/org-classic-head.setup `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "ctitle" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/classic-title" nil
                        nil)
                       ("cite"
                        "\\cite{$1}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Cite" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cite_Cite" nil nil)
                       ("change"
                        "\\begin{change}\n	$1\n\\end{change}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "change" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/change_change" nil
                        nil)
                       ("chal"
                        "\\chapter{$1}\\label{chap:$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Chapter (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/chal_Chapter_with_label_"
                        nil nil)
                       ("cha"
                        "\\chapter{$1}\n${0:$TM_SELECTED_TEXT} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Chapter (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cha_Chapter_no_label_"
                        nil nil)
                       ("ceil"
                        "\\lceil $1 \\rceil $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Ceiling" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ceil_Ceiling" nil
                        nil)
                       ("cee"
                        "\\ce{ $1 } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Chemical Equation" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cee_Chemical_Equation"
                        nil nil)
                       ("cdot"
                        "\\cdot `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Dot Product" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cdot_Dot_Product"
                        nil nil)
                       ("cb"
                        "^{3} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Cube" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cb_Cube" nil nil)
                       ("cas"
                        "\\begin{cases}\n	${1:equation}, &\\text{ if }${2:case}\\\\\\\\\n	$0\n\\end{cases} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Cases" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/cas_Cases" nil nil)
                       ("cp"
                        "#+CAPTION: caption `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "cp" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/caption" nil nil)
                       ("bk"
                        "\\langle ${1:\\phi}\\rvert ${2:\\psi}\\rangle $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "Braket" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/brk_Braket" nil
                        nil)
                       ("bra"
                        "\\langle ${1:\\psi}\\rvert $0`(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "bra" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/bra" nil nil)
                       ("bmat"
                        "\\begin{bmatrix}\n$1\n\\end{bmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Bmatrix" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/bmat_Bmatrix" nil
                        nil)
                       (";b"
                        "`(let* ((blk (yas-choose-value\n              '(\"definition\" \"defn\" \"theorem\" \"lemma\" \"cor\"\n                \"prop\" \"property\" \"proof\" \"example\"\n                \"attention\" \"note\" \"warning\"))))\n   (insert (concat \"#+begin_\" blk \"\\n\\n#+end_\" blk)))`\n\n"
                        "org special block (scripted; cached choice)" nil nil
                        nil
                        "/Users/hc/.emacs.d/snippets/org-mode/block-special-all"
                        nil nil)
                       (";bb"
                        "#+begin_${1:block}\n$0\n#+end_${1} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "org special block (definition/theorem/...)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/block" nil nil)
                       ("bf"
                        "\\mathbf{$1} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Bold Face" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/bf_Bold_Face" nil
                        nil)
                       ("begin"
                        "\\\\begin{${1:env}}\n$2\n\\\\end{${1:env}} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "\\begin{}…\\end{}" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/begin_begin_end_"
                        nil nil)
                       ("beg"
                        "\\begin{$1}\n$2\n\\end{$1} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Begin-End Environment" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/beg_Begin-End_Environment"
                        nil nil)
                       ("bbbb"
                        "\\beta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Beta" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/bbbb_Beta" nil nil)
                       ("bar"
                        "\\bar{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Bar Accent" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/bar_Bar_Accent"
                        nil nil)
                       ("bd"
                        "[[https://img.shields.io/badge/supports-Emacs_27.1_to_29.1-red.svg?logo=gnuemacs&color=7F5AB6]] `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "badge" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/badge" nil nil)
                       ("axioml"
                        "\\begin{axiom}{$1}\\label{axi:$1}\n		$2\n\\end{axiom}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Axiom (with label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/axioml_Axiom_with_label_"
                        nil nil)
                       ("axiom"
                        "\\begin{axiom}{$1}\n		$2\n\\end{axiom}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Axiom (no label)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/axiom_Axiom_no_label_"
                        nil nil)
                       ("avg"
                        "\\langle $1 \\rangle $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Angle Brackets" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/avg_Angle_Brackets"
                        nil nil)
                       ("img"
                        "[[file:/Users/van/org/org-roam/图片附件/]] `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "img" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/attach" nil nil)
                       ("assumption"
                        "\\begin{assumption}\n	$1\n\\end{assumption}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Assumption" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/assumption_Assumption"
                        nil nil)
                       ("array"
                        "\\begin{array}\n$1\n\\end{array} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Array" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/array_Array" nil
                        nil)
                       ("and"
                        "\\cap `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Intersection" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/and_Intersection"
                        nil nil)
                       ("ali"
                        "\\begin{align*}\n$1\n\\end{align*} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "Align" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ali_Align" nil nil)
                       ("algo:ref"
                        "${1:Algorithm}~\\ref{algo:$2}$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm:Ref" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/algo_ref_Algorithm_Ref"
                        nil nil)
                       ("algo"
                        "% \\usepackage{algorithm,algorithmicx,algpseudocode}\n\\begin{algorithm}\n	\\floatname{algorithm}{${1:Algorithm}}\n	\\algrenewcommand\\algorithmicrequire{\\textbf{${2:Input: }}}\n	\\algrenewcommand\\algorithmicensure{\\textbf{${3:Output: }}}\n	\\caption{$4}\\label{alg:$5}\n	\\begin{algorithmic}[1]\n		\\Require \\$input\\$\n		\\Ensure \\$output\\$\n		$6\n		\\State \\textbf{return} \\$state\\$\n	\\end{algorithmic}\n\\end{algorithm}\n$0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Algorithm" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/algo_Algorithm"
                        nil nil)
                       ("aaaa"
                        "\\alpha $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`\n"
                        "Alpha" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/aaaa_Alpha" nil
                        nil)
                       ("a.py"
                        "import os\n\n# 定义要查找和替换的特征字符串\nSEARCH_TEXT = '(when (and (not (eobp)) (looking-at \"\\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-blank-lines))))'\nREPLACE_TEXT = '(when (and (not (eobp)) (looking-at \"\\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))'\n\ndef batch_replace():\n    # 获取脚本自身的文件名，避免自残\n    script_name = os.path.basename(__file__)\n    count = 0\n\n    for root, dirs, files in os.walk('.'):\n        for file_name in files:\n            # 跳过脚本自己\n            if file_name == script_name:\n                continue\n\n            file_path = os.path.join(root, file_name)\n            \n            try:\n                # 读取内容\n                with open(file_path, 'r', encoding='utf-8') as f:\n                    content = f.read()\n\n                # 如果内容中包含目标字符串，则进行替换\n                if SEARCH_TEXT in content:\n                    new_content = content.replace(SEARCH_TEXT, REPLACE_TEXT)\n                    \n                    with open(file_path, 'w', encoding='utf-8') as f:\n                        f.write(new_content)\n                    \n                    print(f\"已处理: {file_path}\")\n                    count += 1\n            except Exception as e:\n                print(f\"无法处理 {file_path}: {e}\")\n\n    print(f\"\\n任务完成！共修改了 {count} 个文件。\")\n\nif __name__ == \"__main__\":\n    batch_replace()\n"
                        "a.py" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/a.py" nil nil)
                       (":t"
                        "\\vartheta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Vartheta" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_t_Vartheta" nil
                        nil)
                       ("#region"
                        "%#Region $0 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Region Start" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_region_Region_Start"
                        nil nil)
                       ("#endregion"
                        "%#Endregion `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Region End" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_endregion_Region_End"
                        nil nil)
                       (":e"
                        "\\varepsilon `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Varepsilon" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_e_Varepsilon" nil
                        nil)
                       ("\""
                        "\\text{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Text Environment (short)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Text_Environment_short_"
                        nil nil)
                       ("_"
                        "_{$1}$2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Subscript" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Subscript" nil
                        nil)
                       ("\\\\\\"
                        "\\setminus `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Set Minus" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Set_Minus" nil
                        nil)
                       ("!="
                        "\\neq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Not Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Not_Equal" nil
                        nil)
                       ("<<"
                        "\\ll `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Much Less" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Much_Less" nil
                        nil)
                       (">>"
                        "\\gg `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Much Greater" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Much_Greater" nil
                        nil)
                       ("!>"
                        "\\mapsto `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Maps To" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Maps_To" nil nil)
                       ("<="
                        "\\leq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Less or Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Less_or_Equal"
                        nil nil)
                       ("=>"
                        "\\implies `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Implies" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Implies" nil nil)
                       ("=<"
                        "\\impliedby `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Implied By" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Implied_By" nil
                        nil)
                       (">="
                        "\\geq `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Greater or Equal" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Greater_or_Equal"
                        nil nil)
                       ("==="
                        "\\equiv `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Equiv" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Equiv" nil nil)
                       ("**"
                        "\\cdot `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Dot" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_Dot" nil nil)
                       ("<->"
                        "\\leftrightarrow  `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Left-Right Arrow" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/_-_Left-Right_Arrow"
                        nil nil)
                       ("ZZ"
                        "\\mathbb{Z} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Integers" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/ZZ_Integers" nil
                        nil)
                       ("Vmat"
                        "\\begin{Vmatrix}\n$1\n\\end{Vmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Vmatrix (double)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Vmat_Vmatrix_double_"
                        nil nil)
                       ("U"
                        "\\underbrace{ ${VISUAL} }_{ $1 } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Underbrace" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/U_Underbrace" nil
                        nil)
                       ("UUUU"
                        "\\Upsilon `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Upsilon (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/UUUU_Upsilon_uppercase_"
                        nil nil)
                       ("TTTT"
                        "\\Theta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Theta (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/TTTT_Theta_uppercase_"
                        nil nil)
                       ("S"
                        "\\sqrt{ ${VISUAL} } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sqrt (visual)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/S_Sqrt_visual_"
                        nil nil)
                       ("SSSS"
                        "\\Sigma `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Sigma (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/SSSS_Sigma_uppercase_"
                        nil nil)
                       ("Re"
                        "\\mathrm{Re} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Real Part" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Re_Real_Part" nil
                        nil)
                       ("RR"
                        "\\mathbb{R} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Real Numbers" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/RR_Real_Numbers"
                        nil nil)
                       ("Ome"
                        "\\Omega `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Omega uppercase (alt)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Ome_Omega_uppercase_alt_"
                        nil nil)
                       ("O"
                        "\\overbrace{ ${VISUAL} }^{ $1 } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Overbrace" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/O_Overbrace" nil
                        nil)
                       ("OOOO"
                        "\\Omega `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Omega (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/OOOO_Omega_uppercase_"
                        nil nil)
                       ("Norm"
                        "\\lVert $1 \\rVert $2 `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Norm" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Norm_Norm" nil nil)
                       ("NN"
                        "\\mathbb{N} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Natural Numbers" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/NN_Natural_Numbers"
                        nil nil)
                       ("LL"
                        "\\mathcal{L} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Lagrangian" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/LL_Lagrangian" nil
                        nil)
                       ("LLLL"
                        "\\Lambda `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Lambda (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/LLLL_Lambda_uppercase_"
                        nil nil)
                       ("K"
                        "\\cancelto{ $1 }{ ${VISUAL} } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Cancel To" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/K_Cancel_To" nil
                        nil)
                       ("Im"
                        "\\mathrm{Im} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Imaginary Part" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Im_Imaginary_Part"
                        nil nil)
                       ("HH"
                        "\\mathcal{H} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Hamiltonian" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/HH_Hamiltonian"
                        nil nil)
                       ("GGGG"
                        "\\Gamma `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Gamma (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/GGGG_Gamma_uppercase_"
                        nil nil)
                       ("DDDD"
                        "\\Delta `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Delta (uppercase)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/DDDD_Delta_uppercase_"
                        nil nil)
                       ("C"
                        "\\cancel{ ${VISUAL} } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Cancel" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/C_Cancel" nil nil)
                       ("CC"
                        "\\mathbb{C} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Complex Numbers" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/CC_Complex_Numbers"
                        nil nil)
                       ("Bmat"
                        "\\begin{Bmatrix}\n$1\n\\end{Bmatrix} `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Bmatrix (curly)" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/Bmat_Bmatrix_curly_"
                        nil nil)
                       ("B"
                        "\\underset{ $1 }{ ${VISUAL} } `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Underset" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/B_Underset" nil
                        nil)
                       ("-_To"
                        " `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "-_To" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/-_To" nil nil)
                       ("-+_Minus-Plus"
                        " `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "-+_Minus-Plus" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/-+_Minus-Plus" nil
                        nil)
                       ("+-"
                        "\\pm `(when (and (not (eobp)) (looking-at \"\n\")) (let ((inhibit-modification-hooks t)) (ignore-errors (delete-char 1))))`"
                        "Plus-Minus" nil nil nil
                        "/Users/hc/.emacs.d/snippets/org-mode/+-_Plus-Minus" nil
                        nil)))


;;; Do not edit! File generated at Fri Feb 13 20:32:31 2026
