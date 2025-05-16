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

    ---@module "conform"
    ---@type conform.setupOpts
	opts = {
		notify_on_error = true,
        lsp_format = "fallback",
		-- format_on_save = function(bufnum)
		-- 	local lsp_format_opt
		--
		-- 	lsp_format_opt = "fallback"
		--
		-- 	return {
		-- 		timeout_ms = 500,
		-- 		lsp_format = lsp_format_opt,
		-- 	}
		-- end,

		formatters_by_ft = {
			lua = { "stylua" },
			python = { "isort", "black" },
            ruast = { "rustfmt" },
			javascript = { "prettierd", "prettier", stop_after_first = true },
			typescript = { "prettierd", "prettier", stop_after_first = true },
            cs = { "clang-format", fallback = "lsp_format" },
            c = { "clang-format" },
		},
	},
}
