return {
  {
    "folke/which-key.nvim",
    keys = { "<leader>", "<c-r>", "<c-w>", '"', "'", "c", "v", "g" },
    cmd = "WhichKey",
	config = function ()
		local wk = require("which-key")
		wk.add({
			{"<leader>f", group = "find"},
			{"<leader>c", group = "code"},
			{"<leader>g", group = "git"},
			{"<leader>x", group = "diagnostic"}
		})
	end
  }
}
