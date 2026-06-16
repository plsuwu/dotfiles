return {
	"nvim-treesitter/nvim-treesitter",
	lazy = false,
	builder = ":TSUpdate",

	config = function()
		vim.filetype.add({
			extension = {
				alloy = "alloy",
			},
			filename = {
				["grafana.alloy"] = "alloy",
			},
			pattern = {
				["*.alloy"] = "alloy",
			},
		})

		vim.api.nvim_create_autocmd("User", {
			pattern = "TSUpdate",
			callback = function()
				require("nvim-treesitter.parsers").alloy = {
					install_info = {
						url = "https://github.com/mattsre/tree-sitter-alloy",
					},
				}
			end,
		})

		vim.api.nvim_create_autocmd("FileType", {
			pattern = { "alloy" },
			callback = function(args)
				vim.treesitter.start(args.buf, "alloy")
			end,
		})

		vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
			pattern = { "*" },
			callback = function()
				vim.treesitter.start()
			end,
		})
	end,
}
