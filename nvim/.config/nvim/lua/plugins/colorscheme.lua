return {
	{
		"kuuote/elly.vim",
		enabled = false,
		priority = 1000,
		config = function ()
			vim.cmd([[colorscheme elly]])
		end
	},
	{
		"webhooked/kanso.nvim",
		enabled = true,
		lazy = false,
		priority = 1000,
		opts = {
			background = { dark = "zen", light = "pearl"},
		},
		config = function ()
			vim.cmd([[colorscheme kanso-zen]])
		end
	}
}
