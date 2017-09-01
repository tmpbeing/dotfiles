function conf
	if count $argv > /dev/null
		switch $argv
			case i3
				vim ~/.config/i3/config
			case nvim
				vim ~/.config/nvim/init.vim
			case xinit
				vim ~/.xinitrc
			case xresources
				vim ~/.Xresources
			case pacman
				sudo vim /etc/pacman.conf
			case dunst
				vim ~/.config/dunst/dunstrc
			case compton
				vim ~/.config/compton/compton.conf
			case termite
				vim ~/.config/termite/config
			case rofi
				vim ~/.config/rofi/config
			case polybar
				vim ~/.config/polybar/config
			case zathura
				vim ~/.config/zathura/zathurarc
			case ranger
				vim ~/.config/ranger/rc.conf
			case fish
				vim ~/.config/fish/config.fish
			case tmux
				vim ~/.tmux.conf
		end
	else
		return 1
	end
end

