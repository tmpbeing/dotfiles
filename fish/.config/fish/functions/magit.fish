function magit
	emacsclient -nw -e "(progn (magit-status) (delete-other-windows))"
end
