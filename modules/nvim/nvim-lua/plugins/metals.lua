return {
	"scalameta/nvim-metals",
	ft = { "scala", "sbt", "java" },
	opts = function()
		local metals_config = require("metals").bare_config()
		metals_config.settings = {
			showImplicitArguments = true,
		}

		metals_config.init_options.statusBarProvider = "off"
		metals_config.capabilities = require("blink.cmp").get_lsp_capabilities()

		metals_config.on_attach = function(client, bufnr)
			local keymap = require("config.lsp-keymap")
			local map = function(k, f, d, m)
				m = m or "n"
				vim.keymap.set(m, k, f, { buffer = client.buf, desc = "LSP: " .. d })
			end

			for _, mapping in ipairs(keymap.keys) do
				local k, f, d, m = table.unpack(mapping)
				map(k, f, d, m)
			end
		end

		return metals_config
	end,

	config = function(self, metals_config)
		local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
		vim.api.nvim_create_autocmd("FileType", {
			pattern = self.ft,
			callback = function()
				require("metals").initialize_or_attach(metals_config)
			end,
			group = nvim_metals_group,
		})
	end,
}
