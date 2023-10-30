(map! :leader
      (:prefix ("TAB" . "workspace")
        :desc "load workspace"              "SPC" #'+workspace/load
        :desc "workspace forward"             "l" #'+workspace/switch-right
        :desc "workspace back"                "h" #'+workspace/switch-left))

(map! :leader
      (:prefix ("b" . "buffer")
        :desc "Kill buffer and window"        "k" #'kill-buffer-and-window))

(map! :leader
      (:prefix ("d" . "dired")
        :desc "Open dired in specified dir"   "." #'dired
        :desc "Open dired in $HOME"           "h" (lambda () (interactive) (dired "~"))
        :desc "Open dired in root"            "r" (lambda () (interactive) (dired "/"))
        :desc "Open dired in org dir"         "o" (lambda () (interactive) (dired "~/documents/org"))
        :desc "Close all dired buffers"       "q" #'+dired/quit-all
        :desc "Open dired in PWD"             "d" #'dired-jump))

(map! :leader
      (:prefix ("o" . "open")
        :desc "Open agenda"                   "a" #'org-agenda
        :desc "Open shell"                    "s" #'vterm
        :desc "Open media player"             "e" #'emms
        :desc "Open calendar"                 "c" #'=calendar
        :desc "Open notes (notes.org)"        "n" (lambda () (interactive) (find-file "~/documents/org/notes.org"))
        :desc "Open inbox"                    "i" (lambda () (interactive) (find-file "~/documents/org/gtd/inbox.org"))
        :desc "Open config (conf.org)"        "C" (lambda () (interactive) (find-file "~/.config/doom/conf.org"))
        :desc "Open $HOME in dired"           "h" (lambda () (interactive) (dired "~"))
        :desc "Open root in dired"            "r" (lambda () (interactive) (dired "/"))
        :desc "Open todo"                     "t" #'org-todo-list))

(map! :leader
      (:prefix ("w" . "window")
        :desc "open file in other window"    "." #'find-file-other-window
        :desc "vsplit and follow"            "v" #'+evil/window-vsplit-and-follow
        :desc "split and follow"             "s" #'+evil/window-split-and-follow))

(map! :leader
    ;; notes. not really necessary for what little i do,
    ;; so i've bound the ones i need over to the "open" and "dired" prefixes
    "n" nil
    ;; git, get to it when i need it
    "g" nil
    ;; actions; a wild list of things. "embark" is a recurring word
    "a" nil
    ;; resume last search
    "'" nil
    ;; toggle last popup
    "~" nil
    ;; universal arg?
    "u" nil
    ;; search. should get a dict/thesaur backend
    "s" nil
    ;; code. need modules for it?
    "c" nil
    ;; toggle. good for toggling modes and variables, but i dont have
    ;; specific things in mind yet. would like one for visual line mode
    "t" nil
    (:prefix ("o" . "open")
        "b" nil "-" nil "E" nil
        "l" nil "L" nil "u" nil "R" nil
        "U" nil "A" nil "d" nil)
    (:prefix ("h" . "help")
        "l" #'+lookup/definition
        ;; apropos. seems like a documentation thing
        ;"a" nil
        "A" nil
        ;; describe coding system. no idea
        "C" nil
        ;; describe various things I don't need
        ;; face, gnu,   input,   news,   syntax, manorwoman, lang
        "g" nil "l" nil "n" nil "s" nil "W" nil     "L" nil
        ;; autodef, profiler, info other window
        "u" nil     "T" nil   "4" nil "h" nil
        ;; alternate versions of things i already got rid of
        ;; and will certainly never use these keybinds for em
        "C-a" nil "C-c" nil "C-d" nil "C-e" nil "C-f" nil "C-k" nil
        "C-l" nil "C-n" nil "C-o" nil "C-p" nil "C-s" nil "C-t" nil
        "C-w" nil "<f1>" nil "q" nil  "C-\\" nil
        ;; various things maybe I will eventually want, but not yet
        ;; command, goto docs key, sandbox, local help, char
        "x" nil     "K" nil        "E" nil  "." nil     "'" nil
        ;; info, symbol, help packages, library
        "i" nil "o" nil "p" nil "P" nil
        ;; input history, online lookup, load theme, whereis
        "I" nil          "O" nil        "t" nil     "w" nil)
    (:prefix ("b" . "buffer")
        "n" nil "z" nil "Z" nil
        "x" nil "u" nil "p" nil
        "N" nil "r" nil "O" nil)
    (:prefix ("f" . "file")
        "c" nil "e" nil "E" nil "l" nil
        "P" nil "F" nil "u" nil "U" nil)
    (:prefix ("w" . "window")
        "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil
        "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-l" nil "C-w" nil "C-v" nil
        "C-n" nil "C-o" nil "C-p" nil "C-r" nil "C-s" nil "C-t" nil "C-u" nil "C-k" nil
        "C-q" nil "C-x" nil "C-_" nil "<left>" nil "<right>" nil
        "C-<left>" nil "C-<right>" nil "<up>" nil "C-<down>" nil "<down>" nil "C-<up>" nil
        "c" nil "t" nil "b" nil "n" nil "m" nil
        ;; I don't really separate windows. should i?
        "T" nil
        ;; TODO a bunch of evil binds. what are these supposed to do
        "g" nil "f" nil
        ;; don't need these since i made split and follow the default
        "S" nil "V" nil
        ;; TODO what are these supposed to do
        "r" nil "R" nil "p" nil
        ;; TODO what is the difference between these two and spc w d
        "q" nil "c" nil
        ;; TODO why do these do weird shit "_" nil
        "|" nil))

(map! :leader :desc "Open org capture"   "c" #'org-capture )

(map! :leader
      (:prefix ("e" . "emms")
        :desc "Open emms buffer"         "o" #'emms
        :desc "Open emms buffer"         "e" #'emms
        :desc "Open smart browser"       "B" #'emms-smart-browse
        :desc "Open normal browser"      "b" #'emms-browser
        :desc "Clear playlist"           "c" #'emms-playlist-current-clear
        :desc "Pause / Resume"           "p" #'emms-pause
        :desc "Start"                    "s" #'emms-start
        :desc "Stop"                     "t" #'emms-stop
        :desc "Next track"               "." #'emms-next
        :desc "Prev track"               "," #'emms-previous
        (:prefix ("a" . "add")
                :desc "Add dir"                  "d" #'emms-add-directory
                :desc "Add file"                 "f" #'emms-add-file
                :desc "Cache all files in dir"   "c" #'emms-add-directory-tree
                :desc "Add playlist"             "p" #'emms-add-playlist)))

(evil-select-search-module 'evil-search-module 'isearch)

;;(after! evil (setq evil-want-minibuffer 1))
;; TODO bind an ex search and replace
(map! :map evil-motion-state-map :prefix "g"
      :desc "goto line number"                 "o" 'evil-goto-line
      :desc "cursor to end of line"            "l" 'evil-end-of-line
      :desc "nonblank start of line"           "h" 'evil-first-non-blank
      "$" nil "^" nil "e" nil "E" nil ;; removing binds
      "K" nil "n" nil "N" nil "t" nil "T" nil "u" nil "U" nil "C-]" nil
      "C-g" nil "<down>" nil "<up>" nil "<home>" nil "<end>" nil "#" nil "*" nil
      "0" nil "_" nil
      ;; a lot of stuff in here, esp related to avy it seems. worth exploring eventually
      "s" nil
      )

(map! :map evil-visual-state-map :prefix "g"
      :desc "cursor to end of line"            "l" 'evil-end-of-line
      :desc "nonblank start of line"           "h" 'evil-first-non-blank
      )

(map! :map evil-normal-state-map :prefix "g"
      :desc "invert case of selection"         "`" 'evil-invert-case
      "8" nil "&" nil "@" nil "?" nil "~" nil "," nil "a" nil "A" nil "f" nil "F" nil
      "I" nil "J" nil "l" nil "L" nil "P" nil "y" nil
      ;; maybe bring these back when i'm ready. lots of lookup for files and references
      "d" nil "D" nil "f" nil "O" nil
      ;; what do these do? evil-fill
      "w" nil "q" nil
      ;; how does eval work? both for selection and whole buffer
      "r" nil "R" nil
      ;; some workspace stuff?
      "t" nil "T" nil
      ;; a lot of stuff in here, esp related to avy it seems. worth exploring eventually
      "s" nil
      )
