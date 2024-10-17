return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      -- Deps from neotest doc
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter",
      -- Adapters
      "nvim-neotest/neotest-python",
      "jfpedroza/neotest-elixir",
      "marilari88/neotest-vitest"
    },

    config = function()
      local neotest = require("neotest")
      neotest.setup({
          adapters = {
            require("neotest-python"),
            require("neotest-elixir"),
            require("neotest-vitest")
          }
      })
    end
  }
}
