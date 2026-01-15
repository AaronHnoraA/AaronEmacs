;;; "Compiled" snippets and support files for `tex-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'tex-mode
					 '(("zzzz" "\\zeta\n" "Zeta" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/zzzz_Zeta"
						nil nil)
					   ("xx" "\\times\n" "Times" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/xx_Times"
						nil nil)
					   ("xelatex" "%! TeX program = xelatex\n$0\n"
						"Xelatex" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/xelatex_Xelatex"
						nil nil)
					   ("while"
						"\\While{$1}\n	\\State $0\n\\EndWhile\n"
						"Algorithm:While" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/while_Algorithm_While"
						nil nil)
					   ("vmat"
						"\\begin{vmatrix}\n$1\n\\end{vmatrix}\n"
						"Vmatrix" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/vmat_Vmatrix"
						nil nil)
					   ("vec" "\\vec{$1}$2\n" "Vector" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/vec_Vector"
						nil nil)
					   ("v(" "(${VISUAL})\n" "Parentheses (visual)"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/v_Parentheses_visual_"
						nil nil)
					   ("v[" "[${VISUAL}]\n" "Brackets (visual)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/v_Brackets_visual_"
						nil nil)
					   ("v{" "{${VISUAL}}\n" "Braces (visual)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/v_Braces_visual_"
						nil nil)
					   ("uuuu" "\\upsilon\n" "Upsilon (lowercase)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/uuuu_Upsilon_lowercase_"
						nil nil)
					   ("und" "\\underline{$1}$2\n" "Underline" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/und_Underline"
						nil nil)
					   ("tttt" "\\theta\n" "Theta (lowercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/tttt_Theta_lowercase_"
						nil nil)
					   ("trace" "\\mathrm{Tr}\n" "Trace" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/trace_Trace"
						nil nil)
					   ("tilde" "\\tilde{$1}$2\n" "Tilde Accent" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/tilde_Tilde_Accent"
						nil nil)
					   ("thml"
						"\\begin{theorem}{$1}\\label{thm:$1}\n	$2\n\\end{theorem}\n$0\n"
						"Theorem (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/thml_Theorem_with_label_"
						nil nil)
					   ("theorem"
						"\\begin{theorem}{$1}\n	$2\n\\end{theorem}\n$0\n"
						"Theorem (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/theorem_Theorem_no_label_"
						nil nil)
					   ("text" "\\text{$1}$2\n" "Text Environment" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/text_Text_Environment"
						nil nil)
					   ("tayl"
						"$1($2 + $3) = $1($2) + $1'($2)$3 + $1''($2) \\frac{$3^{2}}{2!} + \\dots$4\n"
						"Taylor Expansion" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/tayl_Taylor_Expansion"
						nil nil)
					   ("table:ref" "${1:Table}~\\ref{tab:$2}$0\n"
						"Table:Ref" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/table_ref_Table_Ref"
						nil nil)
					   ("table:acm:*"
						"\\begin{table*}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{tabular}{${3:ccl}}\n		\\toprule\n		$4\n		a & b & c \\\\\\\\\n		\\midrule\n		d & e & f \\\\\\\\\n		\\bottomrule\n	\\end{tabular}\n\\end{table*}\n$0\n"
						"Table:ACM:*" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/table_acm_Table_ACM_"
						nil nil)
					   ("table:acm"
						"\\begin{table}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{tabular}{${3:ccl}}\n		\\toprule\n		$4\n		a & b & c \\\\\\\\\n		\\midrule\n		d & e & f \\\\\\\\\n		\\bottomrule\n	\\end{tabular}\n\\end{table}\n$0\n"
						"Table:ACM" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/table_acm_Table_ACM"
						nil nil)
					   ("table"
						"\\begin{table}\n	\\caption{$1}\\label{tab:$2}\n	\\begin{center}\n		\\begin{tabular}[c]{l|l}\n			\\hline\n			\\multicolumn{1}{c|}{\\textbf{$3}} & \n			\\multicolumn{1}{c}{\\textbf{$4}} \\\\\\\\\n			\\hline\n			a & b \\\\\\\\\n			c & d \\\\\\\\\n			$5\n			\\hline\n		\\end{tabular}\n	\\end{center}\n\\end{table}\n$0\n"
						"Table" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/table_Table"
						nil nil)
					   ("tabl"
						"\\begin{tabular}{${1:c}}\\label{tab:$2}\n$0\n\\end{tabular}\n"
						"Tabular (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/tabl_Tabular_with_label_"
						nil nil)
					   ("tab"
						"\\begin{tabular}{${1:c}}\n$0\n\\end{tabular}\n"
						"Tabular (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/tab_Tabular_no_label_"
						nil nil)
					   ("sup=" "\\supseteq\n" "Superset Equal" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sup_Superset_Equal"
						nil nil)
					   ("sum" "\\sum\n" "Sum" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sum_Sum"
						nil nil)
					   ("subsl"
						"\\subsubsection{$1}\\label{sec:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Sub Section (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subsl_Sub_Sub_Section_with_label_"
						nil nil)
					   ("subs"
						"\\subsubsection{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Sub Section (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subs_Sub_Sub_Section_no_label_"
						nil nil)
					   ("subpl"
						"\\subparagraph{$1}\\label{subp:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Paragraph (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subpl_Sub_Paragraph_with_label_"
						nil nil)
					   ("subp"
						"\\subparagraph{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Paragraph (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subp_Sub_Paragraph_no_label_"
						nil nil)
					   ("subl"
						"\\subsection{$1}\\label{sub:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Section (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subl_Sub_Section_with_label_"
						nil nil)
					   ("subfile" "\\subfile{$1}\n$0\n" "Subfile" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/subfile_Subfile"
						nil nil)
					   ("sub=" "\\subseteq\n" "Subset Equal" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sub_Subset_Equal"
						nil nil)
					   ("sub"
						"\\subsection{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Sub Section (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sub_Sub_Section_no_label_"
						nil nil)
					   ("sts" "_\\text{$1}\n" "Text Subscript" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sts_Text_Subscript"
						nil nil)
					   ("state" "\\State $1\n" "Algorithm:State" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/state_Algorithm_State"
						nil nil)
					   ("ssss" "\\sigma\n" "Sigma (lowercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ssss_Sigma_lowercase_"
						nil nil)
					   ("sr" "^{2}\n" "Square" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sr_Square"
						nil nil)
					   ("sq" "\\sqrt{ $1 }$2\n" "Square Root" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sq_Square_Root"
						nil nil)
					   ("spl" "\\begin{split}\n	$0\n\\end{split}\n"
						"Split" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/spl_Split"
						nil nil)
					   ("solution"
						"\\begin{solution}\n	$1\n\\end{solution}\n$0\n"
						"Solution" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/solution_Solution"
						nil nil)
					   ("simm" "\\sim\n" "Similar To" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/simm_Similar_To"
						nil nil)
					   ("sim=" "\\simeq\n" "Approx Equal" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sim_Approx_Equal"
						nil nil)
					   ("setsubfile"
						"\\documentclass[$1]{subfiles}\n\\graphicspath{{$2}}\n$0\n"
						"SetSubfile" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/setsubfile_SetSubfile"
						nil nil)
					   ("set" "\\{ $1 \\}$2\n" "Set" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/set_Set"
						nil nil)
					   ("section:ref" "${1:Section}~\\ref{sec:$2}$0\n"
						"Section:Ref" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/section_ref_Section_Ref"
						nil nil)
					   ("secl"
						"\\section{$1}\\label{sec:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Section (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/secl_Section_with_label_"
						nil nil)
					   ("sec"
						"\\section{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Section (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/sec_Section_no_label_"
						nil nil)
					   ("rm" "\\mathrm{$1}$2\n" "Roman" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/rm_Roman"
						nil nil)
					   ("remark"
						"\\begin{remark}\n	$1\n\\end{remark}\n$0\n"
						"Remark" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/remark_Remark"
						nil nil)
					   ("ref" "\\ref{$1: $2}$0\n" "Reference" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ref_Reference"
						nil nil)
					   ("rd" "^{$1}$2\n" "Raise to Power" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/rd_Raise_to_Power"
						nil nil)
					   ("pu" "\\pu{ $1 }\n" "Physical Units" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/pu_Physical_Units"
						nil nil)
					   ("proposition"
						"\\begin{proposition}{$1}\n		$2\n\\end{proposition}\n$0\n"
						"Proposition (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/proposition_Proposition_no_label_"
						nil nil)
					   ("propl"
						"\\begin{proposition}{$1}\\label{pro:$1}\n		$2\n\\end{proposition}\n$0\n"
						"Proposition (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/propl_Proposition_with_label_"
						nil nil)
					   ("prop" "\\propto\n" "Proportional To" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/prop_Proportional_To"
						nil nil)
					   ("proof"
						"\\begin{proof}\n	$1\n\\end{proof}\n$0\n"
						"Proof" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/proof_Proof"
						nil nil)
					   ("prod" "\\prod\n" "Product" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/prod_Product"
						nil nil)
					   ("problemset"
						"\\begin{problemset}\n	$1\n\\end{problemset}\n$0\n"
						"Problemset" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/problemset_Problemset"
						nil nil)
					   ("problem"
						"\\begin{problem}\n	$1\n\\end{problem}\n$0\n"
						"Problem" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/problem_Problem"
						nil nil)
					   ("postulate"
						"\\begin{postulate}{$1}\n		$2\n\\end{postulate}\n$0\n"
						"Postulate (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/postulate_Postulate_no_label_"
						nil nil)
					   ("postl"
						"\\begin{postulate}{$1}\\label{pos:$1}\n		$2\n\\end{postulate}\n$0\n"
						"Postulate (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/postl_Postulate_with_label_"
						nil nil)
					   ("pmat"
						"\\begin{pmatrix}\n$1\n\\end{pmatrix}\n"
						"Pmatrix" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/pmat_Pmatrix"
						nil nil)
					   ("plaininline" "\\lstinline{$1}$0\n"
						"lstinline" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/plaininline_lstinline"
						nil nil)
					   ("plain"
						"\\begin{lstlisting}\n	$1\n\\end{lstlisting}\n$0\n"
						"lstlisting" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/plain_lstlisting"
						nil nil)
					   ("part" "\\begin{part}\n	$0\n\\end{part}\n"
						"Part" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/part_Part"
						nil nil)
					   ("parl"
						"\\paragraph{$1}\\label{par:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Paragraph (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/parl_Paragraph_with_label_"
						nil nil)
					   ("para" "\\parallel\n" "Parallel" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/para_Parallel"
						nil nil)
					   ("par"
						"\\frac{ \\partial $1 }{ \\partial $2 } $3\n"
						"Partial Derivative" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/par_Partial_Derivative"
						nil nil)
					   ("par"
						"\\paragraph{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Paragraph (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/par_Paragraph_no_label_"
						nil nil)
					   ("page" "${1:page}~\\pageref{$2}$0\n" "Page"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/page_Page"
						nil nil)
					   ("ox" "\\otimes \n" "Tensor Product" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ox_Tensor_Product"
						nil nil)
					   ("outlineexp" "\\\\[\n	$1\n\\\\]\n$0\n"
						"OutlineExp" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/outlineexp_OutlineExp"
						nil nil)
					   ("outer" "\\ket{$1} \\bra{$1} $2\n"
						"Outer Product" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/outer_Outer_Product"
						nil nil)
					   ("orr" "\\cup\n" "Union" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/orr_Union"
						nil nil)
					   ("openlink"
						"http://10.31.2.53/openlink.html?link=$0\n"
						"NasOpenlink" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/openlink_NasOpenlink"
						nil nil)
					   ("oooo" "\\omega\n" "Omega (lowercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/oooo_Omega_lowercase_"
						nil nil)
					   ("ooo" "\\infty\n" "Infinity" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ooo_Infinity"
						nil nil)
					   ("ome" "\\omega\n" "Omega (alt)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ome_Omega_alt_"
						nil nil)
					   ("oint" "\\oint\n" "Contour Integral" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/oint_Contour_Integral"
						nil nil)
					   ("oinf" "\\int_{0}^{\\infty} $1 \\, d$2 $3\n"
						"Integral 0 to Infinity" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/oinf_Integral_0_to_Infinity"
						nil nil)
					   ("o+" "\\oplus \n" "Direct Sum" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/o+_Direct_Sum"
						nil nil)
					   ("notin" "\\not\\in\n" "Not Element Of" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/notin_Not_Element_Of"
						nil nil)
					   ("note" "\\begin{note}\n	$1\n\\end{note}\n$0\n"
						"Note" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/note_Note"
						nil nil)
					   ("norm" "\\lvert $1 \\rvert $2\n"
						"Absolute Value" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/norm_Absolute_Value"
						nil nil)
					   ("nabl" "\\nabla\n" "Nabla" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/nabl_Nabla"
						nil nil)
					   ("msun" "M_{\\odot}\n" "Solar Mass" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/msun_Solar_Mass"
						nil nil)
					   ("mod" "|$1|$2\n" "Modulus" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/mod_Modulus"
						nil nil)
					   ("math" "\\begin{math}\n	$1\n\\end{math}\n$0\n"
						"Math" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/math_Math"
						nil nil)
					   ("mat"
						"\\begin{${1:p/b/v/V/B/small}matrix}\n	$0\n\\end{${1:p/b/v/V/B/small}matrix}\n"
						"Matrix" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/mat_Matrix"
						nil nil)
					   ("marginpar" "\\marginpar{$1}\n$0\n"
						"Marginpar" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/marginpar_Marginpar"
						nil nil)
					   ("lra" "\\left< $1 \\right> $2\n"
						"Left-Right Angle" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lra_Left-Right_Angle"
						nil nil)
					   ("lr(" "\\left( $1 \\right) $2\n"
						"Left-Right Parentheses" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lr_Left-Right_Parentheses"
						nil nil)
					   ("lr[" "\\left[ $1 \\right] $2\n"
						"Left-Right Brackets" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lr_Left-Right_Brackets"
						nil nil)
					   ("lr{" "\\left\\{ $1 \\right\\} $2\n"
						"Left-Right Braces" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lr_Left-Right_Braces"
						nil nil)
					   ("lr|" "\\left| $1 \\right| $2\n"
						"Left-Right Absolute" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lr_Left-Right_Absolute"
						nil nil)
					   ("llll" "\\lambda\n" "Lambda (lowercase)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/llll_Lambda_lowercase_"
						nil nil)
					   ("listing:ref" "${1:Listing}~\\ref{lst:$2}$0\n"
						"Listing:Ref" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/listing_ref_Listing_Ref"
						nil nil)
					   ("lim" "\\lim_{ $1 \\to $2 } $3\n" "Limit" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lim_Limit"
						nil nil)
					   ("lemmal"
						"\\begin{lemma}{$1}\\label{lem:$1}\n	$2\n\\end{lemma}\n$0\n"
						"Lemma (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lemmal_Lemma_with_label_"
						nil nil)
					   ("lemma"
						"\\begin{lemma}{$1}\n	$2\n\\end{lemma}\n$0\n"
						"Lemma (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/lemma_Lemma_no_label_"
						nil nil)
					   ("kkkk" "\\kappa\n" "Kappa" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/kkkk_Kappa"
						nil nil)
					   ("ket" "\\ket{$1} $2\n" "Ket" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ket_Ket"
						nil nil)
					   ("kbt" "k_{B}T\n" "Boltzmann Constant" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/kbt_Boltzmann_Constant"
						nil nil)
					   ("item" "\\item $1\n" "item" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/item_item"
						nil nil)
					   ("item"
						"\\\\begin{itemize}\n	\\item $0\n\\\\end{itemize}\n"
						"Itemize" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/item_Itemize"
						nil nil)
					   ("iso" "{}^{$1}_{$2}$3\n" "Isotope" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/iso_Isotope"
						nil nil)
					   ("invs" "^{-1}\n" "Inverse" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/invs_Inverse"
						nil nil)
					   ("introduction"
						"\\begin{introduction}\n	$1\n\\end{introduction}\n$0\n"
						"Introduction" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/introduction_Introduction"
						nil nil)
					   ("int" "\\int $1 \\, d$2 $3\n" "Integral" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/int_Integral"
						nil nil)
					   ("inn" "\\in\n" "Element Of" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/inn_Element_Of"
						nil nil)
					   ("inlineexp" "\\\\($1\\\\)$0\n" "InlineExp" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/inlineexp_InlineExp"
						nil nil)
					   ("infi"
						"\\int_{-\\infty}^{\\infty} $1 \\, d$2 $3\n"
						"Integral -Inf to Inf" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/infi_Integral_-Inf_to_Inf"
						nil nil)
					   ("iint" "\\iint\n" "Double Integral" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/iint_Double_Integral"
						nil nil)
					   ("iiint" "\\iiint\n" "Triple Integral" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/iiint_Triple_Integral"
						nil nil)
					   ("iiii" "\\iota\n" "Iota" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/iiii_Iota"
						nil nil)
					   ("if"
						"\\If{$1}\n\\ElsIf{$2}\n\\Else\n\\EndIf\n"
						"Algorithm:If" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/if_Algorithm_If"
						nil nil)
					   ("iden"
						"\\begin{pmatrix}\n1 & 0 & \\dots & 0 \\\\\n0 & 1 & \\dots & 0 \\\\\n\\vdots & \\vdots & \\ddots & \\vdots \\\\\n0 & 0 & \\dots & 1\n\\end{pmatrix}\n"
						"Identity Matrix" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/iden_Identity_Matrix"
						nil nil)
					   ("hide" "\\begin{hide}\n	$1\n\\end{hide}\n$0\n"
						"hide" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/hide_hide"
						nil nil)
					   ("he4" "{}^{4}_{2}He \n" "Helium-4" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/he4_Helium-4"
						nil nil)
					   ("he3" "{}^{3}_{2}He \n" "Helium-3" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/he3_Helium-3"
						nil nil)
					   ("hat" "\\hat{$1}$2\n" "Hat Accent" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/hat_Hat_Accent"
						nil nil)
					   ("gggg" "\\gamma\n" "Gamma (lowercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/gggg_Gamma_lowercase_"
						nil nil)
					   ("gat"
						"\\begin{gather}\n	$0\n\\end{gather}\n"
						"Gather(ed)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/gat_Gather_ed_"
						nil nil)
					   ("frac" "\\frac{$1}{$2}$3\n" "Fraction" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/frac_Fraction"
						nil nil)
					   ("for"
						"\\For{i=0:$1}\n	\\State $0\n\\EndFor\n"
						"Algorithm:For" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/for_Algorithm_For"
						nil nil)
					   ("floor" "\\lfloor $1 \\rfloor $2\n" "Floor"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/floor_Floor"
						nil nil)
					   ("figure:ref" "${1:Figure}~\\ref{fig:$2}$0\n"
						"Figure:Ref" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/figure_ref_Figure_Ref"
						nil nil)
					   ("figure:acm:*"
						"\\begin{figure*}\n	\\includegraphics[width=0.45\\textwidth]{figures/$1}\n	\\caption{$2}\\label{fig:$3}\n\\end{figure*}\n$0\n"
						"Figure:ACM:*" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/figure_acm_Figure_ACM_"
						nil nil)
					   ("figure:acm"
						"\\begin{figure}\n	\\includegraphics[width=0.45\\textwidth]{figures/$1}\n	\\caption{$2}\\label{fig:$3}\n\\end{figure}\n$0\n"
						"Figure:ACM" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/figure_acm_Figure_ACM"
						nil nil)
					   ("figure"
						"\\begin{figure}\n	\\begin{center}\n		\\includegraphics[width=0.95\\textwidth]{figures/$1}\n	\\end{center}\n	\\caption{$3}\\label{fig:$4}\n\\end{figure}\n$0\n"
						"Figure" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/figure_Figure"
						nil nil)
					   ("exercise"
						"\\begin{exercise}\n	$1\n\\end{exercise}\n$0\n"
						"Exercise" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/exercise_Exercise"
						nil nil)
					   ("example"
						"\\begin{example}\n	$1\n\\end{example}\n$0\n"
						"Example" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/example_Example"
						nil nil)
					   ("eset" "\\emptyset\n" "Empty Set" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/eset_Empty_Set"
						nil nil)
					   ("equation"
						"\\begin{equation}\n	$0\n	\\label{eq:$1}\n\\end{equation}\n"
						"Equation" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/equation_Equation"
						nil nil)
					   ("equ"
						"\\begin{equation*}\n	$1\n\\end{equation*}\n"
						"Equ" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/equ_Equ"
						nil nil)
					   ("enumerate"
						"\\\\begin{enumerate}\n	\\item $0\n\\\\end{enumerate}\n"
						"Enumerate" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/enumerate_Enumerate"
						nil nil)
					   ("empty"
						"\\null\\thispagestyle{empty}\n\\newpage\n$0\n"
						"EmptyPage" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/empty_EmptyPage"
						nil nil)
					   ("eeee" "\\epsilon\n" "Epsilon" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/eeee_Epsilon"
						nil nil)
					   ("ee" "e^{ $1 }$2\n" "Exponential" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ee_Exponential"
						nil nil)
					   ("e\\xi sts" "\\exists\n" "Exists" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/e_xi_sts_Exists"
						nil nil)
					   ("dot" "\\dot{$1}$2\n" "Dot Accent" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/dot_Dot_Accent"
						nil nil)
					   ("displaymath"
						"\\begin{displaymath}\n	$1\n\\end{displaymath}\n$0\n"
						"DisplayMath" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/displaymath_DisplayMath"
						nil nil)
					   ("dint" "\\int_{$1}^{$2} $3 \\, d$4 $5\n"
						"Definite Integral" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/dint_Definite_Integral"
						nil nil)
					   ("desc"
						"\\\\begin{description}\n	\\item[$1] $0\n\\\\end{description}\n"
						"Description" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/desc_Description"
						nil nil)
					   ("del" "\\nabla\n" "Del" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/del_Del"
						nil nil)
					   ("defl"
						"\\begin{definition}{$1}\\label{def:$1}\n	$2\n\\end{definition}\n$0\n"
						"Definition (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/defl_Definition_with_label_"
						nil nil)
					   ("definition"
						"\\begin{definition}{$1}\n	$2\n\\end{definition}\n$0\n"
						"Definition (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/definition_Definition_no_label_"
						nil nil)
					   ("ddt" "\\frac{d}{dt} \n" "Time Derivative" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ddt_Time_Derivative"
						nil nil)
					   ("ddot" "\\ddot{$1}$2\n" "Double Dot Accent"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ddot_Double_Dot_Accent"
						nil nil)
					   ("dddd" "\\delta\n" "Delta (lowercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/dddd_Delta_lowercase_"
						nil nil)
					   ("datechange" "\\datechange{$1}{$2}$0\n"
						"Datechange" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/datechange_Datechange"
						nil nil)
					   ("dag" "^{\\dagger}\n" "Dagger" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/dag_Dagger"
						nil nil)
					   ("corollary"
						"\\begin{corollary}{$1}\n	$2\n\\end{corollary}\n$0\n"
						"Corollary (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/corollary_Corollary_no_label_"
						nil nil)
					   ("corl"
						"\\begin{corollary}{$1}\\label{cor:$1}\n	$2\n\\end{corollary}\n$0\n"
						"Corollary (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/corl_Corollary_with_label_"
						nil nil)
					   ("conclusion"
						"\\begin{conclusion}\n	$1\n\\end{conclusion}\n$0\n"
						"Conclusion" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/conclusion_Conclusion"
						nil nil)
					   ("compactitem"
						"\\begin{compactitem}\n	\\item $1\n\\end{compactitem}\n$0\n"
						"Compactitem" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/compactitem_Compactitem"
						nil nil)
					   ("cite" "\\cite{$1}$0\n" "Cite" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cite_Cite"
						nil nil)
					   ("change"
						"\\begin{change}\n	$1\n\\end{change}\n$0\n"
						"change" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/change_change"
						nil nil)
					   ("chal"
						"\\chapter{$1}\\label{chap:$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Chapter (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/chal_Chapter_with_label_"
						nil nil)
					   ("cha"
						"\\chapter{$1}\n${0:$TM_SELECTED_TEXT}\n"
						"Chapter (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cha_Chapter_no_label_"
						nil nil)
					   ("ceil" "\\lceil $1 \\rceil $2\n" "Ceiling" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ceil_Ceiling"
						nil nil)
					   ("cee" "\\ce{ $1 }\n" "Chemical Equation" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cee_Chemical_Equation"
						nil nil)
					   ("cdot" "\\cdot\n" "Dot Product" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cdot_Dot_Product"
						nil nil)
					   ("cb" "^{3}\n" "Cube" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cb_Cube"
						nil nil)
					   ("cas"
						"\\begin{cases}\n	${1:equation}, &\\text{ if }${2:case}\\\\\\\\\n	$0\n\\end{cases}\n"
						"Cases" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/cas_Cases"
						nil nil)
					   ("brk" "\\braket{ $1 | $2 } $3\n" "Braket" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/brk_Braket"
						nil nil)
					   ("bra" "\\bra{$1} $2\n" "Bra" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/bra_Bra"
						nil nil)
					   ("bmat"
						"\\begin{bmatrix}\n$1\n\\end{bmatrix}\n"
						"Bmatrix" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/bmat_Bmatrix"
						nil nil)
					   ("bf" "\\mathbf{$1}\n" "Bold Face" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/bf_Bold_Face"
						nil nil)
					   ("begin"
						"\\\\begin{${1:env}}\n$2\n\\\\end{${1:env}}\n"
						"\\begin{}…\\end{}" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/begin_begin_end_"
						nil nil)
					   ("beg" "\\begin{$1}\n$2\n\\end{$1}\n"
						"Begin-End Environment" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/beg_Begin-End_Environment"
						nil nil)
					   ("bbbb" "\\beta\n" "Beta" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/bbbb_Beta"
						nil nil)
					   ("bar" "\\bar{$1}$2\n" "Bar Accent" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/bar_Bar_Accent"
						nil nil)
					   ("axioml"
						"\\begin{axiom}{$1}\\label{axi:$1}\n		$2\n\\end{axiom}\n$0\n"
						"Axiom (with label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/axioml_Axiom_with_label_"
						nil nil)
					   ("axiom"
						"\\begin{axiom}{$1}\n		$2\n\\end{axiom}\n$0\n"
						"Axiom (no label)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/axiom_Axiom_no_label_"
						nil nil)
					   ("avg" "\\langle $1 \\rangle $2\n"
						"Angle Brackets" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/avg_Angle_Brackets"
						nil nil)
					   ("assumption"
						"\\begin{assumption}\n	$1\n\\end{assumption}\n$0\n"
						"Assumption" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/assumption_Assumption"
						nil nil)
					   ("array" "\\begin{array}\n$1\n\\end{array}\n"
						"Array" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/array_Array"
						nil nil)
					   ("and" "\\cap\n" "Intersection" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/and_Intersection"
						nil nil)
					   ("ali" "\\begin{align}\n$1\n\\end{align}\n"
						"Align" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ali_Align"
						nil nil)
					   ("algo:ref" "${1:Algorithm}~\\ref{algo:$2}$0\n"
						"Algorithm:Ref" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/algo_ref_Algorithm_Ref"
						nil nil)
					   ("algo"
						"% \\usepackage{algorithm,algorithmicx,algpseudocode}\n\\begin{algorithm}\n	\\floatname{algorithm}{${1:Algorithm}}\n	\\algrenewcommand\\algorithmicrequire{\\textbf{${2:Input: }}}\n	\\algrenewcommand\\algorithmicensure{\\textbf{${3:Output: }}}\n	\\caption{$4}\\label{alg:$5}\n	\\begin{algorithmic}[1]\n		\\Require \\$input\\$\n		\\Ensure \\$output\\$\n		$6\n		\\State \\textbf{return} \\$state\\$\n	\\end{algorithmic}\n\\end{algorithm}\n$0\n"
						"Algorithm" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/algo_Algorithm"
						nil nil)
					   ("aaaa" "\\alpha\n" "Alpha" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/aaaa_Alpha"
						nil nil)
					   (":t" "\\vartheta\n" "Vartheta" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_t_Vartheta"
						nil nil)
					   ("#region" "%#Region $0\n" "Region Start" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_region_Region_Start"
						nil nil)
					   ("#endregion" "%#Endregion\n" "Region End" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_endregion_Region_End"
						nil nil)
					   (":e" "\\varepsilon\n" "Varepsilon" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_e_Varepsilon"
						nil nil)
					   ("\"" "\\text{$1}$2\n"
						"Text Environment (short)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Text_Environment_short_"
						nil nil)
					   ("_" "_{$1}$2\n" "Subscript" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Subscript"
						nil nil)
					   ("\\\\\\" "\\setminus\n" "Set Minus" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Set_Minus"
						nil nil)
					   ("!=" "\\neq\n" "Not Equal" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Not_Equal"
						nil nil)
					   ("<<" "\\ll\n" "Much Less" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Much_Less"
						nil nil)
					   (">>" "\\gg\n" "Much Greater" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Much_Greater"
						nil nil)
					   ("!>" "\\mapsto\n" "Maps To" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Maps_To"
						nil nil)
					   ("<=" "\\leq\n" "Less or Equal" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Less_or_Equal"
						nil nil)
					   ("=>" "\\implies\n" "Implies" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Implies"
						nil nil)
					   ("=<" "\\impliedby\n" "Implied By" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Implied_By"
						nil nil)
					   (">=" "\\geq\n" "Greater or Equal" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Greater_or_Equal"
						nil nil)
					   ("===" "\\equiv\n" "Equiv" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Equiv"
						nil nil)
					   ("**" "\\cdot\n" "Dot" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_Dot"
						nil nil)
					   ("<->" "\\leftrightarrow \n" "Left-Right Arrow"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/_-_Left-Right_Arrow"
						nil nil)
					   ("ZZ" "\\mathbb{Z}\n" "Integers" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/ZZ_Integers"
						nil nil)
					   ("Vmat"
						"\\begin{Vmatrix}\n$1\n\\end{Vmatrix}\n"
						"Vmatrix (double)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Vmat_Vmatrix_double_"
						nil nil)
					   ("U" "\\underbrace{ ${VISUAL} }_{ $1 }\n"
						"Underbrace" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/U_Underbrace"
						nil nil)
					   ("UUUU" "\\Upsilon\n" "Upsilon (uppercase)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/UUUU_Upsilon_uppercase_"
						nil nil)
					   ("TTTT" "\\Theta\n" "Theta (uppercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/TTTT_Theta_uppercase_"
						nil nil)
					   ("S" "\\sqrt{ ${VISUAL} }\n" "Sqrt (visual)"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/S_Sqrt_visual_"
						nil nil)
					   ("SSSS" "\\Sigma\n" "Sigma (uppercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/SSSS_Sigma_uppercase_"
						nil nil)
					   ("Re" "\\mathrm{Re}\n" "Real Part" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Re_Real_Part"
						nil nil)
					   ("RR" "\\mathbb{R}\n" "Real Numbers" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/RR_Real_Numbers"
						nil nil)
					   ("Ome" "\\Omega\n" "Omega uppercase (alt)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Ome_Omega_uppercase_alt_"
						nil nil)
					   ("O" "\\overbrace{ ${VISUAL} }^{ $1 }\n"
						"Overbrace" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/O_Overbrace"
						nil nil)
					   ("OOOO" "\\Omega\n" "Omega (uppercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/OOOO_Omega_uppercase_"
						nil nil)
					   ("Norm" "\\lVert $1 \\rVert $2\n" "Norm" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Norm_Norm"
						nil nil)
					   ("NN" "\\mathbb{N}\n" "Natural Numbers" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/NN_Natural_Numbers"
						nil nil)
					   ("LL" "\\mathcal{L}\n" "Lagrangian" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/LL_Lagrangian"
						nil nil)
					   ("LLLL" "\\Lambda\n" "Lambda (uppercase)" nil
						nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/LLLL_Lambda_uppercase_"
						nil nil)
					   ("K" "\\cancelto{ $1 }{ ${VISUAL} }\n"
						"Cancel To" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/K_Cancel_To"
						nil nil)
					   ("Im" "\\mathrm{Im}\n" "Imaginary Part" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Im_Imaginary_Part"
						nil nil)
					   ("HH" "\\mathcal{H}\n" "Hamiltonian" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/HH_Hamiltonian"
						nil nil)
					   ("GGGG" "\\Gamma\n" "Gamma (uppercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/GGGG_Gamma_uppercase_"
						nil nil)
					   ("DDDD" "\\Delta\n" "Delta (uppercase)" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/DDDD_Delta_uppercase_"
						nil nil)
					   ("C" "\\cancel{ ${VISUAL} }\n" "Cancel" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/C_Cancel"
						nil nil)
					   ("CC" "\\mathbb{C}\n" "Complex Numbers" nil nil
						nil
						"/Users/hc/.emacs.d/snippets/tex-mode/CC_Complex_Numbers"
						nil nil)
					   ("Bmat"
						"\\begin{Bmatrix}\n$1\n\\end{Bmatrix}\n"
						"Bmatrix (curly)" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/Bmat_Bmatrix_curly_"
						nil nil)
					   ("B" "\\underset{ $1 }{ ${VISUAL} }\n"
						"Underset" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/B_Underset"
						nil nil)
					   ("->" "\\to\n" "To" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/-_To"
						nil nil)
					   ("-+" "\\mp\n" "Minus-Plus" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/-+_Minus-Plus"
						nil nil)
					   ("+-" "\\pm\n" "Plus-Minus" nil nil nil
						"/Users/hc/.emacs.d/snippets/tex-mode/+-_Plus-Minus"
						nil nil)))


;;; Do not edit! File generated at Wed Jan 14 13:15:18 2026
