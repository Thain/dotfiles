(TeX-add-style-hook
 "quiver"
 (lambda ()
   (TeX-run-style-hooks
    "tikz-cd"
    "amssymb")
   (TeX-add-symbols
    "pv"))
 :latex)

