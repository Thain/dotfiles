(TeX-add-style-hook
 "prelude"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("enumitem" "shortlabels")))
   (TeX-run-style-hooks
    "amsfonts"
    "amsmath"
    "amsthm"
    "mathtools"
    "amssymb"
    "csquotes"
    "inputenc"
    "enumitem"
    "tikz-cd"
    "CJKutf8")
   (TeX-add-symbols
    '("norm" 1)
    "no"
    "m"
    "vs"
    "R"
    "Z"
    "N"
    "im"
    "ker"
    "deg"
    "inv"
    "Set"
    "id"
    "op"
    "yo"
    "wan")
   (LaTeX-add-environments
    '("problem" 1))
   (LaTeX-add-amsthm-newtheorems
    "theorem"
    "corollary"
    "lemma"
    "remark"
    "definition"
    "proposition"))
 :latex)

