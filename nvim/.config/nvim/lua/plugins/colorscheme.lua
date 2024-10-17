return {
	{
		"kuuote/elly.vim",
		name = "elly",
		priority = 1000,
		config = function ()
			vim.cmd([[colorscheme elly]])
		end
	},
	{
		"folke/tokyonight.nvim",
		name = "tokyonight",
		priority = 1000,
		-- config = function ()
		-- 	vim.cmd([[colorscheme tokyonight-moon]])
		-- end
	}
}
