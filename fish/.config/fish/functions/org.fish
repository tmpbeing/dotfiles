function org
    emacsclient --socket-name=org -nc --frame-parameters='(quote (name . "org-emacs"))' --eval '(+org/open-todo-file)' > /dev/null
end
