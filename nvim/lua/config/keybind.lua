vim.keymap.set("n", "<leader>vt", ":NvimTreeToggle<CR>", { silent = true })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-f>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("x", "<leader>p", [["_dP]])
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set("n", "<leader>sr", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
vim.keymap.set("n", "<leader>X", "<cmd>!chmod -x %<CR>", { silent = true })

-- vim.keymap.set("n", "<leader>f", function()
--     vim.lsp.buf.format()
-- end)

vim.keymap.set("n", "<F10>", function()
	vim.cmd("set wrap!")
end)

local brackets = {
	["("] = "(",
	["{"] = "{",
	["["] = "[",
	["<"] = "<",
	["'"] = "'",
	['"'] = '"',
}
for key, bracket in pairs(brackets) do
	vim.keymap.set("n", "<leader>d" .. key .. "p", "di" .. bracket .. "va" .. bracket .. "p")
end
