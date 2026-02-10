return {
	"nvim-treesitter/nvim-treesitter",
	lazy = false,
	builder = ":TSUpdate",

	config = function()
		vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
			pattern = { "*" },
			callback = function()
				vim.treesitter.start()
			end,
		})
	end,
}
