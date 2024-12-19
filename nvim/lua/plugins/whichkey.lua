return {
	"folke/which-key.nvim",
	config = function()
		require("which-key").setup({
			preset = "modern",
			icons = {
				mappings = false,
			},
		})
	end,
}
