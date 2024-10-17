return {
	{
		"folke/trouble.nvim",
		cmd = "Trouble",
		init = function ()
			local wk = require("which-key")
			wk.register({
				["xx"] = { "<cmd>Trouble diagnostics toggle<cr>", "Diagnostics" },
				["xX"] = { "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", "Buffer Diagnostics" },
				["cs"] = { "<cmd>Trouble symbols toggle focus=false<cr>", "Symbols (Trouble)" },
				["cl"] = { "<cmd>Trouble lsp toggle focus=false win.position=right<cr>", "LSP Definitions / references / ... (Trouble)" },
				["xL"] = { "<cmd>Trouble loclist toggle<cr>", "Location List (Trouble)" },
				["xQ"] = { "<cmd>Trouble qflist toggle<cr>", "Quickfix List (Trouble)" }
			}, { prefix= "<leader>" })
		end
	}
}
