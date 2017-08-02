function ls
	command ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F $argv
end

