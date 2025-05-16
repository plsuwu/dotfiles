return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
    opts = {
        ensure_installed = {
            "bash",
            "c",
            "csharp",
            "java",
            "haskell",
            "typescript",
            "rust",
            "python",
            "lua",
            "vim",
        },
        auto_install = true,
        highlighht = {
            enable = true,
            additional_vim_regex_highlighting = { 'ruby' },
        },
        indent = { enable = true, disable = { 'ruby' } },
    },

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
