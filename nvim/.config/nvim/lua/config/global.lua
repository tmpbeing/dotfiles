vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.o.nu = true
vim.o.relativenumber = true

vim.o.tabstop = 4
vim.o.expandtab = false
vim.o.shiftwidth = 4

-- Sync clipboard between OS and Neovim
vim.o.clipboard = "unnamedplus"

vim.o.backup = false
vim.o.swapfile = false
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.cache/vim/undodir"

-- Search is case insensitive unless a capital is used
vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.colorcolumn = "80"
