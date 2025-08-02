return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	keys = {
		{
			"<leader>f",
			function()
				require("conform").format({
					async = true,
					lsp_format = "fallback",
				})
			end,
			mode = "",
			desc = "[f]ormat buffer",
		},
	},

	opts = {
		notify_on_error = true,
		lsp_format = "fallback",

		formatters_by_ft = {
			lua = { "stylua" },
			python = { "black" },
			nix = { "nixfmt" },
			javascript = { "prettierd" },
			typescript = { "prettierd" },
			typescriptreact = { "prettierd" },
			json = { "prettierd" },
			-- rust = { "rustfmt" },
		},

		formatters = {
			nixfmt = {
				prepend_args = { "--width=80" },
			},
		},
	},
}
