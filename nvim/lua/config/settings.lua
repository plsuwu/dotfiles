local home = os.getenv("HOME")
vim.g.python3_host_prog = home .. "/.pyenv/shims/python"
vim.opt.undodir = home .. "/.vim/undodir"

vim.diagnostic.config({ virtual_text = false })

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true

-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1\

vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.wrap = false
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

vim.opt.termguicolors = true
vim.opt.scrolloff = 10
vim.opt.isfname:append("@-@")
vim.opt.updatetime = 50
