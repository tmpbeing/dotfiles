return {
  {
    "folke/which-key.nvim",
    keys = { "<leader>", "<c-r>", "<c-w>", '"', "'", "c", "v", "g" },
    cmd = "WhichKey",
	config = function ()
		local wk = require("which-key")
	end
  }
}
