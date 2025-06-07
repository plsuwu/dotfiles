vim.opt.mouse = a

local home = os.getenv("HOME")

yls = home .. "/.pyenv/shims/yls"

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

-- vim.api.nvim_create_autocmd("TextYankPost", {
-- 	group = vim.api.nvim_create_augroup("HighlightYank", {}),
-- 	pattern = "*",
-- 	callback = function()
-- 		vim.highlight.on_yank({
-- 			higroup = "IncSearch",
-- 			timeout = 40,
-- 		})
-- 	end,
-- })
--
-- vim.api.nvim_create_autocmd({ "BufWritePre" }, {
-- 	group = vim.api.nvim_create_augroup("Main", {}),
-- 	pattern = "*",
-- 	command = [[%s/\s\+$//e]],
-- })
