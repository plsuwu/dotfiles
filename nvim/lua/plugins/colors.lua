return {
	"folke/tokyonight.nvim",
	lazy = false,
	priority = 1000,
	opts = {},

	config = function()
		require("tokyonight").setup({
			style = "moon",
			light_style = "day",
			terminal_colors = "true",
		})
		vim.cmd([[colorscheme tokyonight]])
	end,
}
