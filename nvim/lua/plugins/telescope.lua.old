return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.5",
	dependencies = { "nvim-lua/plenary.nvim" },

	config = function()
		local telescope = require("telescope")
		telescope.setup({})

		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>zff", builtin.find_files, {})
		vim.keymap.set("n", "<leader>zfg", builtin.live_grep, {})
		vim.keymap.set("n", "<leader>zfb", builtin.buffers, {})
		vim.keymap.set("n", "<leader>zfh", builtin.help_tags, {})
	end,
}
