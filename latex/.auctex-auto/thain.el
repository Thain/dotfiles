(TeX-add-style-hook
 "thain"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("enumitem" "shortlabels") ("geometry" "margin=1in")))
   (TeX-run-style-hooks
    "amsfonts"
    "amsmath"
    "amsthm"
    "mathtools"
    "amssymb"
    "csquotes"
    "inputenc"
    "enumitem"
    "geometry"
    "tikz-cd"
    "CJKutf8")
   (TeX-add-symbols
    "no"
    "vs"
    "R"
    "Z"
    "N"
    "im"
    "inv"
    "set"
    "id"
    "op"
    "yo"
    "wan")
   (LaTeX-add-environments
    '("problem" 1)))
 :latex)

