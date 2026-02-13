;;; "Compiled" snippets and support files for `fundamental-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'fundamental-mode
                     '(("thm" "theorem $1 : $2 := by\n  $3\n" "thm" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/thm_thm"
                        nil nil)
                       ("str" "structure $1 where\n  $2 : $3\n" "str" nil nil
                        nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/str_str"
                        nil nil)
                       ("openlink" "http://10.31.2.53/openlink.html?link=$0\n"
                        "NasOpenlink" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/openlink_NasOpenlink"
                        nil nil)
                       ("obs"
                        "%${1}\n@article{obs-concept-${2},\n    title = {{OBS:$3}},\n    url = {http://10.31.2.53/openlink.html?link=$1},\n}$0\n"
                        "obs" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/obs_obs"
                        nil nil)
                       ("mod" "import $1\n\nnamespace $2\n  $3\nend $2\n" "mod"
                        nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/mod_mod"
                        nil nil)
                       ("let" "let $1 := $2\n$3\n" "let" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/let_let"
                        nil nil)
                       ("lem" "lemma $1 : $2 := by\n  $3\n" "lem" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/lem_lem"
                        nil nil)
                       ("lam" "(fun $1 : $2 => $3)\n" "lam" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/lam_lam"
                        nil nil)
                       ("io" "def main : IO Unit := do\n  $1\n  return ()\n"
                        "io" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/io_io" nil
                        nil)
                       ("inst" "instance : $1 $2 := $3\n" "inst" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/inst_inst"
                        nil nil)
                       ("ind" "inductive $1\n  | $2 : $1\n  | $3 : $1\n" "ind"
                        nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/ind_ind"
                        nil nil)
                       ("imp" "import $1\n" "imp" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/imp_imp"
                        nil nil)
                       ("ex" "example $1 : $2 := by\n  $3\n" "ex" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/ex_ex" nil
                        nil)
                       ("def" "def $1 : $2 := $3\n" "def" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/def_def"
                        nil nil)
                       ("@unpublished"
                        "@unpublished{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    year = {${4:?_year}},\n    month = {${5:?_month}},\n    note = {${6:note}},\n}\n"
                        "@unpublished" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_unpublished_unpublished"
                        nil nil)
                       ("@techreport"
                        "@techreport{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    institution = {${4:institution}},\n    type = {${5:?_type}},\n    number = {${6:?_number}},\n    address = {${7:?_address}},\n    year = {${8:year}},\n    month = {${9:?_month}},\n    note = {${10:?_note}},\n}\n"
                        "@techreport" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_techreport_techreport"
                        nil nil)
                       ("@proceedings"
                        "@proceedings{${1:name},\n    title = {${2:title}},\n    editor = {${3:?_editor}},\n    volume = {${4:?_volume}},\n    number = {${5:?_number}},\n    series = {${6:?_series}},\n    address = {${7:?_address}},\n    organization = {${8:?_organization}},\n    publisher = {${9:?_publisher}},\n    year = {${10:year}},\n    month = {${11:?_month}},\n    note = {${12:?_note}},\n}\n"
                        "@proceedings" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_proceedings_proceedings"
                        nil nil)
                       ("@phdthesis"
                        "@phdthesis{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    school = {${4:school}},\n    address = {${5:?_address}},\n    year = {${6:year}},\n    month = {${7:?_month}},\n    keywords = {${8:?_keywords}},\n    note = {${9:?_note}},\n}\n"
                        "@phdthesis" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_phdthesis_phdthesis"
                        nil nil)
                       ("@misc"
                        "@misc{${1:name},\n    author = {${2:?_author}},\n    title = {${3:?_title}},\n    howpublished = {${4:?_howpublished}},\n    year = {${5:?_year}},\n    month = {${6:?_month}},\n    note = {${7:?_note}},\n}\n"
                        "@misc" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_misc_misc"
                        nil nil)
                       ("@mastersthesis"
                        "@mastersthesis{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    school = {${4:school}},\n    type = {${5:?_type}},\n    address = {${6:?_address}},\n    year = {${7:year}},\n    month = {${8:?_month}},\n    note = {${9:?_note}},\n}\n"
                        "@mastersthesis" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_mastersthesis_mastersthesis"
                        nil nil)
                       ("@manual"
                        "@manual{${1:name},\n    title = {${2:title}},\n    author = {${3:?_author}},\n    organization = {${4:?_organization}},\n    address = {${5:?_address}},\n    edition = {${6:?_edition}},\n    year = {${7:year}},\n    month = {${8:?_month}},\n    note = {${9:?_note}},\n}\n"
                        "@manual" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_manual_manual"
                        nil nil)
                       ("@inproceedings"
                        "@inproceedings{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    booktitle = {${4:booktitle}},\n    editor = {${5:?_editor}},\n    volume = {${6:?_volume}},\n    number = {${7:?_number}},\n    series = {${8:?_series}},\n    pages = {${9:?_pages}},\n    address = {${10:?_address}},\n    organization = {${11:?_organization}},\n    publisher = {${12:?_publisher}},\n    year = {${13:year}},\n    month = {${14:?_month}},\n    note = {${15:?_note}},\n}\n"
                        "@inproceedings" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_inproceedings_inproceedings"
                        nil nil)
                       ("@incollection"
                        "@incollection{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    booktitle = {${4:booktitle}},\n    publisher = {${5:publisher}},\n    editor = {${6:?_editor}},\n    volume = {${7:?_volume}},\n    number = {${8:?_number}},\n    series = {${9:?_series}},\n    type = {${10:?_type}},\n    chapter = {${11:?_chapter}},\n    pages = {${12:?_pages}},\n    address = {${13:?_address}},\n    edition = {${14:?_edition}},\n    year = {${15:year}},\n    month = {${16:?_month}},\n    note = {${17:?_note}},\n}\n"
                        "@incollection" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_incollection_incollection"
                        nil nil)
                       ("@inbook"
                        "@inbook{${1:name},\n    author = {${2:author}},\n    editor = {${3:editor}},\n    title = {${4:title}},\n    chapter = {${5:chapter}},\n    pages = {${6:pages}},\n    publisher = {${7:publisher}},\n    volume = {${8:?_volume}},\n    number = {${9:?_number}},\n    series = {${10:?_series}},\n    type = {${11:?_type}},\n    address = {${12:?_address}},\n    edition = {${13:?_edition}},\n    year = {${14:year}},\n    month = {${15:?_month}},\n    note = {${16:?_note}},\n}\n"
                        "@inbook" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_inbook_inbook"
                        nil nil)
                       ("@conference"
                        "@conference{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    booktitle = {${4:booktitle}},\n    editor = {${5:?_editor}},\n    volume = {${6:?_volume}},\n    number = {${7:?_number}},\n    series = {${8:?_series}},\n    pages = {${9:?_pages}},\n    address = {${10:?_address}},\n    year = {${11:year}},\n    month = {${12:?_month}},\n    publisher = {${13:?_publisher}},\n    note = {${14:?_note}},\n}\n"
                        "@conference" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_conference_conference"
                        nil nil)
                       ("@booklet"
                        "@booklet{${1:name},\n    author = {${1:?_author}},\n    title = {${2:title}},\n    howpublished = {${3:?_howpublished}},\n    address = {${4:?_address}},\n    year = {${5:?_year}},\n    month = {${6:?_month}},\n    note = {${7:?_note}},\n}\n"
                        "@booklet" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_booklet_booklet"
                        nil nil)
                       ("@book"
                        "@book{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    publisher = {${4:publisher}},\n    volume = {${5:?_volume}},\n    number = {${6:?_number}},\n    series = {${7:?_series}},\n    address = {${8:?_address}},\n    edition = {${9:?_edition}},\n    year = {${10:year}},\n    month = {${11:?_month}},\n    note = {${12:?_note}},\n}\n"
                        "@book" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_book_book"
                        nil nil)
                       ("@article"
                        "@article{${1:name},\n    author = {${2:author}},\n    title = {${3:title}},\n    journal = {${4:journal}},\n    volume = {${5:?_volume}},\n    number = {${6:?_number}},\n    pages = {${7:?_pages}},\n    year = {${8:year}},\n    month = {${9:?_month}},\n    note = {${10:?_note}},\n}\n"
                        "@article" nil nil nil
                        "/Users/hc/.emacs.d/snippets/fundamental-mode/_article_article"
                        nil nil)))


;;; Do not edit! File generated at Fri Feb 13 19:51:59 2026
