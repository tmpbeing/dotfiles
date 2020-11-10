function magit
	emacsclient -nw --socket=main-emacs -e "(progn (magit-status) (delete-other-windows))"
end
