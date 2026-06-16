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

			htmldjango = { "djlint" },

			html = { "prettierd", "prettier", stop_after_first = true },
			astro = { "prettierd", "prettier", stop_after_first = true },

			javascript = { "prettierd", "prettier", stop_after_first = true },
			typescript = { "prettierd", "prettier", stop_after_first = true },
			typescriptreact = { "prettierd", "prettier", stop_after_first = true },
			json = { "prettierd", "prettier", stop_after_first = true },

			scala = { "scalafmt" },

			bash = { "shfmt" },
			zsh = { "shfmt" }, -- this is pretty unreliable for zsh formatting but it is what it is
			haskell = { "fourmolu" },
			solidity = { "forge_fmt", "prettierd", "prettier", stop_after_first = true },
		},

		formatters = {
			nixfmt = {
				prepend_args = { "--width=80" },
			},
		},
	},
}
