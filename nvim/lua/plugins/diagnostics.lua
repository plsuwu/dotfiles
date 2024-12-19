return {
	"rachartier/tiny-inline-diagnostic.nvim",
	event = "VeryLazy", -- / LspAttach
	priority = 1000, -- needs to be loaded in first
	config = function()
		require("tiny-inline-diagnostic").setup({
			preset = "powerline",
			options = {
				multilines = true,
				enable_on_inset = true,
			},
		})
	end,
}
