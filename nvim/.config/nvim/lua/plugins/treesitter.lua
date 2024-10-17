return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPost", "BufNewFile" },
    cmd = { "TSInstall", "TSBufEnable", "TSBufDisable", "TSModuleInfo"},
    build = ":TSUpdate",
    dependencies = {},
    config = function()
      require("nvim-treesitter.configs").setup({
      ensure_installed = {
        "bash",
        "python",
        "elixir"
      },
      highlight = {
        enable = true,
        use_languagetree = true,
      },
      indent = { enable = true },
    })
    end
  }
}
