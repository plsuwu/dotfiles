return {
	"nvim-tree/nvim-tree.lua",
	version = "*",
	lazy = false,
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},

	config = function()
		require("nvim-tree").setup({
			respect_buf_cwd = true,
			disable_netrw = true,
			view = {
				centralize_selection = true,
                signcolumn = "auto",
				side = "right",
				number = true,
                relativenumber = true,
				width = 38,
			},
			renderer = {
				icons = {
					padding = " ",
				},
			},
			filters = {
				enable = false,
			},
		})
	end,
}
