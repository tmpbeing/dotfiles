function ec
    emacsclient --socket-name=main -nc --frame-parameters='(quote (name . "main-emacs"))'
end
