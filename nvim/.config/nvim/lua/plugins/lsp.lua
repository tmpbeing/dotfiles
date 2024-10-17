return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/nvim-cmp",
    "j-hui/fidget.nvim"
  },

  config = function()
    local cmp = require("cmp")
    local cmp_lsp = require("cmp_nvim_lsp")

    require("fidget").setup({})
    require("mason").setup()
    require("mason-lspconfig").setup({
        ensure_installed = {
          "bashls",
          "lua_ls",
          "rust_analyzer",
          "gopls",
          "lexical",
          "basedpyright",
        }
    })

    cmp.setup({
        mapping = cmp.mapping.preset.insert({
          ["<C-p>"] = cmp.mapping.select_prev_item(),
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ["<C-d>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-y>"] = cmp.mapping.confirm({ select = true }),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e"] = cmp.mapping.close()
        }),
        sources = {
          { name = "nvim_lsp" },
          { name = "buffer" },
          { name = "path" }
        }
    })

    vim.diagnostic.config({
        float = {
          focusable = false,
          style = "minimal",
          border = "rounded",
          source = "always",
          header = "",
          prefix = ""
        }
    })
  end
}
