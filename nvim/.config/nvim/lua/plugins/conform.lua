return {
	{
		"stevearc/conform.nvim",
		dependencies = { "mason.nvim" },
		lazy = true,
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		opts = {
			default_format_opts = {
				lsp_format = "fallback"
			},
			format_on_save = function(bufnr)
				if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
					return
				end
				return { timeout_ms = 500 }
			end,
			notify_on_error = true,
			notify_no_formatters = true
		},
		config = function()
			local confrom = require("conform")

			vim.api.nvim_create_user_command("FormatDisable", function(args)
				if args.bang then
					vim.b.disable_autoformat = true
				else
					vim.g.disable_autoformat = true
				end
			end, {
					desc = "Disable autoformat-on-save", 
					bang = true
			})

			vim.api.nvim_create_user_command("FormatEnable", function()
				vim.b.disable_autoformat = false
				vim.g.disable_autoformat = false
			end, { 
				desc = "Re-enable autoformat-on-save" 
			})

			vim.api.nvim_create_user_command("Format", function(args)
				local range = nil
				if args.count ~= -1 then
					local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
					range = {
						start = { args.line1, 0 },
						["end"] = { args.line2, end_line:len() },
					}
				end
				conform.format({ async = true, lsp_format = "fallback", range = range })
			end, {
				range = true,
				desc = "Run async formatting"
			})

		end,
		keys = {
			{
				"<leader>cF",
				function()
					require("conform").format({ async = true, timeout_ms = 5000 })
				end,
				mode = { "n", "x" },
				desc = "Format buffer"
			}
		},
	}
}
