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
			nix = { "nixfmt" },
			python = { "ruff_fix", "ruff_format", "ruff_organize_imports" },
			rust = { "rustfmt" },
			-- c = { "clang-format" },
			c = { "ccls" },

			javascript = { "prettierd" },
			typescript = { "prettierd" },
			typescriptreact = { "prettierd" },
			json = { "prettierd" },

			bash = { "shfmt" },
			zsh = { "shfmt" }, -- this is pretty unreliable for zsh formatting but it is what it is
			haskell = { "fourmolu" },
		},

		formatters = {
			nixfmt = {
				prepend_args = { "--width=80" },
			},
		},
	},
}
