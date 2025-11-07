return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
	opts = {},
	config = function()
        

		require("nvim-treesitter.configs").setup({
			ensure_installed = {
				"bash",
				"c",
				"java",
				"haskell",
				"typescript",
				"rust",
				"python",
				"lua",
				"vim",
			},
			auto_install = true,
			sync_install = false,
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
				-- additional_vim_regex_highlighting = { 'ruby' },
			},
		})
	end,

	-- config = function()
	-- 	require("nvim-treesitter.configs").setup({
	-- 		ensure_installed = {
	-- 			"bash",
	-- 			"haskell",
	-- 			"javascript",
	-- 			"typescript",
	-- 			"lua",
	--                "vim",
	--                "vimdoc",
	-- 			"python",
	-- 			"rust",
	-- 		},
	-- 		sync_install = false,
	-- 		auto_install = true,
	-- 		highlight = {
	-- 			enable = true,
	-- 		},
	-- 	})
	-- end,
}
