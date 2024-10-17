return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    cmd = "Telescope",
    init = function()
      local builtin = require('telescope.builtin')
      local wk = require('which-key')
      wk.register({
          ['ff'] = { builtin.find_files, "Find File" },
          ['fb'] = { builtin.buffers, "Find Buffer" },
          ['fg'] = { builtin.live_grep, "Find with Grep"},
          ['fh'] = { builtin.help_tags, "Find Help" },
          ['fn'] = {":Telescope file_browser path=%:p:h select_buffer=true<CR>", "File Browser"},
      }, { prefix = "<leader>" })
    end
  }
}
