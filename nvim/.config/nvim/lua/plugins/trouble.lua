return {
	{
		"folke/trouble.nvim",
		cmd = "Trouble",
		init = function ()
			local wk = require("which-key")
			wk.add({
                {"<leader>x", group = "diagnostics"},
				{"<leader>xx", ":Trouble diagnostics toggle<CR>", desc = "Diagnostics" },
				{"<leader>xX", ":Trouble diagnostics toggle filter.buf=0<CR>", desc = "Buffer Diagnostics" },
				{"<leader>cs", ":Trouble symbols toggle focus=false<CR>", desc = "Symbols (Trouble)" },
				{"<leader>cl", ":Trouble lsp toggle focus=false win.position=right<CR>", desc = "LSP Definitions / references / ... (Trouble)" },
				{"<leader>xL", ":Trouble loclist toggle<CR>", desc = "Location List (Trouble)" },
				{"<leader>xQ", ":Trouble qflist toggle<CR>", desc = "Quickfix List (Trouble)" }
			})
		end
	}
}
