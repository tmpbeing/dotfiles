function ls
	command ls -h --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F $argv
end

