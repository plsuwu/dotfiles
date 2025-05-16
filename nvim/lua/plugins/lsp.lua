return {
	{
		"folke/lazydev.nvim",
		ft = "lua",
		opts = {
			library = {
				{ path = "${3rd}/luv/library", words = { "vim%.uv" } },
			},
		},
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			"mason-org/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
			{ "j-hui/fidget.nvim", opts = {} },

			-- Allows extra capabilities provided by blink.cmp
			"saghen/blink.cmp",
		},

		config = function()
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("def-lsp-attach", { clear = true }),
				callback = function(event)
					local map = function(keys, func, desc, mode)
						mode = mode or "n"
						vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
					end

					map("gd", require("telescope.builtin").lsp_definitions, "[g]oto [d]efinition")
					map("gr", require("telescope.builtin").lsp_references, "[g]oto [r]eferences")
					map("gI", require("telescope.builtin").lsp_implementations, "[g]oto [I]mpletementation")
					map("<leader>D", require("telescope.builtin").lsp_type_definitions, "type [D]efinition")
					map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[d]ocument [s]ymbols")
					map(
						"<leader>ws",
						require("telescope.builtin").lsp_dynamic_workspace_symbols,
						"[w]orkspace [s]ymbols"
					)
					map("<leader>rn", vim.lsp.buf.rename, "[r]e[n]ame buffer")
					map("<leader>ca", vim.lsp.buf.code_action, "[c]ode [a]ctions", { "n", "x" })
					map("gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")

					-- This function resolves a difference between neovim nightly (ver 0.11) and stable (ver 0.10)
					--- @param client vim.lsp.Client
					--- @param method vim.lsp.protocol.Method
					--- @param bufnr? integer some lsp support methods only in specific files
					--- @return boolean
					local function client_supports_method(client, method, bufnr)
						if vim.fn.has("nvim-0.11") == 1 then
							return client:supports_method(method, bufnr)
						else
							return client.supports_method(method, { bufnr = bufnr })
						end
					end

					local client = vim.lsp.get_client_by_id(event.data.client_id)
					if
						client
						and client_supports_method(
							client,
							vim.lsp.protocol.Methods.textDocument_documentHighlight,
							event.buf
						)
					then
						local highlight_group = vim.api.nvim_create_augroup("lsp-highlight-group", { clear = false })
						vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
							buffer = event.buf,
							group = highlight_group,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
							buffer = event.buf,
							group = highlight_group,
							callback = vim.lsp.buf.clear_references,
						})

						vim.api.nvim_create_autocmd("LspDetach", {
							group = vim.api.nvim_create_augroup("lsp-detach-group", { clear = true }),
							callback = function(event2)
								vim.lsp.buf.clear_references()
								vim.api.nvim_clear_autocmds({ group = "lsp-highlight-group", buffer = event2.buf })
							end,
						})
					end

					if
						client
						and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf)
					then
						map("<leader>th", function()
							vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufner = event.buf }))
						end, "[t]oggle inlay [h]ints")
					end
				end,
			})

			local capabilities = require("blink.cmp").get_lsp_capabilities()
			local servers = {
				ts_ls = {},
				clangd = {},
				rust_analyzer = {},
				pyright = {},

				lua_ls = {
					settings = {
						Lua = {
							diagnostics = {
								-- globals = { "vim" },
								diagnostics = { disable = { "missing-fields" } },
							},
							completion = {
								callSnippet = "Replace",
							},
						},
					},
				},

                -- hacked in by renaming `OmniSharp` to `omnisharp` for now
                -- hopefully i can be bothered to fix this when i have the time.....
				omnisharp = {
					cmd = { "/home/please/.local/share/nvim/mason/bin/OmniSharp" },
					root_dir = require("lspconfig").util.root_pattern("*sln", "*.csproj"),
				},
			}

			local ensure_installed = vim.tbl_keys(servers or {})
			vim.list_extend(ensure_installed, {
				"stylua",
			})
			require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

			require("mason-lspconfig").setup({
				ensure_installed = {},
				automatic_installation = false,
				automatic_enable = true,

				handlers = {
					function(server_name)
						local server = servers[server_name] or {}
						server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
						require("lspconfig")[server_name].setup(server)
					end,
				},
			})
		end,
	},
}
