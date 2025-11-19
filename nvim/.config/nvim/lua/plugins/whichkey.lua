return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts_extend = { "spec" },
		opts = {
			defaults = {},
			spec = {
				{
					mode = { "n", "x"},
					{ "<leader><tab>", groups = "tabs" },
					{ "<leader>b", group="buffer" },
					{ "<leader>c", group="code" },
					{ "<leader>f", group="file" },
					{ "<leader>o", group="open" },
					{ "<leader>g", group="git" },
					{ "<leader>gh", group="git hunks" },
					{ "<leader>s", group="search" },
					{ "<leader>w", group="window" },
					{ "<leader>x", group="diagnostics" },
					{ "[", group = "prev" },
					{ "]", group = "next" },
					{ "g", group = "goto" },
					{ "gs", group = "surround" },
					{ "z", group = "fold" }
				}
			}
		}
	}
}
