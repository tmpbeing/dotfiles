-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = {
    -- import your plugins
    { "LazyVim/LazyVim", import = "lazyvim.plugins"},

    -- LazyVim lang modules
    { import = "lazyvim.plugins.extras.lang.clojure"},
    { import = "lazyvim.plugins.extras.lang.cmake"},
    { import = "lazyvim.plugins.extras.lang.docker"},
    { import = "lazyvim.plugins.extras.lang.elixir"},
    { import = "lazyvim.plugins.extras.lang.erlang"},
    { import = "lazyvim.plugins.extras.lang.gleam"},
    { import = "lazyvim.plugins.extras.lang.json"},
    { import = "lazyvim.plugins.extras.lang.markdown"},
    { import = "lazyvim.plugins.extras.lang.nix"},
    { import = "lazyvim.plugins.extras.lang.python"},
    { import = "lazyvim.plugins.extras.lang.ocaml"},
    { import = "lazyvim.plugins.extras.lang.rust"},
    { import = "lazyvim.plugins.extras.lang.sql"},
    { import = "lazyvim.plugins.extras.lang.svelte"},
    { import = "lazyvim.plugins.extras.lang.tailwind"},
    { import = "lazyvim.plugins.extras.lang.tex"},
    { import = "lazyvim.plugins.extras.lang.toml"},
    { import = "lazyvim.plugins.extras.lang.typescript"},
    { import = "lazyvim.plugins.extras.lang.typst"},
    { import = "lazyvim.plugins.extras.lang.vue"},
    { import = "lazyvim.plugins.extras.lang.yaml"},
    { import = "lazyvim.plugins.extras.lang.zig"},

    { import = "lazyvim.plugins.extras.editor.refactoring" },
    { import = "lazyvim.plugins.extras.editor.snacks_explorer" },
    { import = "lazyvim.plugins.extras.editor.snacks_picker" },
    { import = "lazyvim.plugins.extras.formatting.biome" },
    { import = "lazyvim.plugins.extras.dap.core" },
    { import = "lazyvim.plugins.extras.test.core" },

    { import = "plugins" },
  },
  install = { colorscheme = { "kanso" } },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  -- install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
  checker = { enabled = true, notify = false },
  performance = {
      rtp = {
          disabled_plugins = {
              "gzip",
              "tokionight.nvim",
              "tarPlugin",
              "tohtml",
              "tutor",
              "zipPlugin"
          }
      }
  }
})
