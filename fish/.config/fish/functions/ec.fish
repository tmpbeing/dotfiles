function ec
    emacsclient -nc --socket=main-emacs --frame-parameters='(quote (name . "main-emacs"))' -a "emacs"
end
