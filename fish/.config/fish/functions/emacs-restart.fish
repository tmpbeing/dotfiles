function emacs-restart
	if count $argv > /dev/null
		systemctl --user restart emacs-$argv
	else
		systemctl --user restart emacs-main
		systemctl --user restart emacs-org
	end
end
