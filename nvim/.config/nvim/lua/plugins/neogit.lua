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
			wk.register({
				["gg"] = { "<cmd>Neogit<cr>", "Neogit status" }
			}, { prefix = "<leader>" })
		end
	}
}
