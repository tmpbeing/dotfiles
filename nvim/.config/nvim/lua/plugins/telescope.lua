return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    cmd = "Telescope",
    init = function()
      local builtin = require('telescope.builtin')
      local wk = require('which-key')
      wk.add({
          {"<leader>f", group = "find"},
          {"<leader>ff", builtin.find_files, desc = "Find File"},
          {"<leader>fb", builtin.buffers, desc = "Find Buffer"},
          {"<leader>fg", builtin.live_grep, desc = "Find with Grep"},
          {"<leader>fh", builtin.help_tags, desc = "Find Help"},
          {"<leader>fn", ":Telescope file_browser path=%:p:h select_buffer=true<CR>", desc= "File Browser"}
      })
    end
  }
}
