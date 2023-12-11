vim.g.mapleader = " "

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<leader>vv", ":NvimTreeToggle<CR>", { silent = true })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z") -- remove line break at end of line

-- move cursor up/down while keeping viewport constant
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")


vim.keymap.set("x", "<leader>p", [["_dP]]) -- (vis bl mode) > replace selection with vim clip
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]]) -- yank selection to sys clipboard
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]]) -- send selection to black hole reg

vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
vim.keymap.set("n", "<leader>f", function()
    vim.lsp.buf.format()
end)

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]]) -- find/replace word under cursor with input
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true }) -- mark current file as executable
vim.keymap.set("n", "<leader><leader>", function() -- source current file
    vim.cmd("so")
end)
vim.keymap.set('n', '<leader>bd', function()
    vim.cmd("b#|bd#")
end)

