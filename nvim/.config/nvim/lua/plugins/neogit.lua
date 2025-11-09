return {
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"sindrets/diffview.nvim",
			"nvim-telescope/telescope.nvim"
		},
		init = function ()
			local wk = require("which-key")
			wk.add({
                {"<leader>g", group = "git"},
                {"<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit status"}
            })
		end
	}
}
