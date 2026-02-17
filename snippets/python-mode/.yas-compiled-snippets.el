;;; "Compiled" snippets and support files for `python-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("year"
                        "ps.add_argument(\"--year\", choices=[\"16\", \"17\", \"18\"])\nps.add_argument(\"--pol\", choices=[\"Down\", \"Up\"])\nyear = args.year\npol = args.pol\n"
                        "yearpol" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/year_yearpol"
                        nil nil)
                       ("th2d"
                        "ROOT.TH2D(\"${1:h}\", \"${2:h}\", ${3:binning})\n"
                        "th2d" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/th2d_th2d" nil
                        nil)
                       ("th1d"
                        "ROOT.TH1D(\"${1:h}\", \"${2:h}\", ${3:binning})\n"
                        "th1d" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/th1d_th1d" nil
                        nil)
                       ("ryaml"
                        "with open(${1:yamlfile}, 'r') as f:\n	${2:content} = yaml.safe_load(f)\n	${0}\n"
                        "Read Yaml" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/ryaml_Read_Yaml"
                        nil nil)
                       ("root" "import ROOT\nROOT.gROOT.SetBatch(True)\n"
                        "Import ROOT" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/root_Import_ROOT"
                        nil nil)
                       ("paras"
                        "import os\nimport sys\nsys.path.append(os.environ[\"MAJORANA\"])\nfrom paras import *\n"
                        "paras" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/paras_paras"
                        nil nil)
                       ("numcpu" "ROOT.RooFit.NumCPU(${1})\n" "numcpu" nil nil
                        nil
                        "/Users/hc/.emacs.d/snippets/python-mode/numcpu_numcpu"
                        nil nil)
                       ("mtd" "ROOT.DisableImplicitMT()\n"
                        "Multithreading Disable" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/mtd_Multithreading_Disable"
                        nil nil)
                       ("mt" "ROOT.EnableImplicitMT()\n" "Multithreading" nil
                        nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/mt_Multithreading"
                        nil nil)
                       ("mpl" "import matplotlib as mpl\nmpl.use(\"Agg\")\n"
                        "import matplotlib" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/mpl_import_matplotlib"
                        nil nil)
                       ("main"
                        "def main():\n	${0}\n\n\nif __name__ == \"__main__\":\n	main()\n"
                        "main" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/main_main" nil
                        nil)
                       ("jraw" "# %% [code]\n%%raw\n${1:raw}\n" "Jupyter:Raw"
                        nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/jraw_Jupyter_Raw"
                        nil nil)
                       ("jmd" "# %% [code]\n%%markdown\n${1:md}\n" "Jupyter:Md"
                        nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/jmd_Jupyter_Md"
                        nil nil)
                       ("jcode" "# %% [code]\n${1:code}\n" "Jupyter:Code" nil
                        nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/jcode_Jupyter_Code"
                        nil nil)
                       ("histo1d"
                        "df.Histo1D((${1:binning}), \"${2:branch}\")\n"
                        "df.Histo1D" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/histo1d_df.Histo1D"
                        nil nil)
                       ("cwd" "cwd = get_cwd(__file__)\n" "cwd" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/cwd_cwd" nil
                        nil)
                       ("canvas"
                        "c = ROOT.TCanvas(\"c\", \"c\", 800, 600)\n${2}\nc.SaveAs(\"${1:test.pdf}\")\n"
                        "canvas" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/canvas_canvas"
                        nil nil)
                       ("argg"
                        "from argparse import ArgumentParser as AP\nfrom argparse import ArgumentDefaultsHelpFormatter as ADHF\nps = AP(formatter_class=ADHF)\nps.add_argument(\"--test\", action=\"store_true\")\nargs = ps.parse_args()\n"
                        "argg" nil nil nil
                        "/Users/hc/.emacs.d/snippets/python-mode/argg_argg" nil
                        nil)))


;;; Do not edit! File generated at Fri Feb 13 20:32:31 2026
