return {
	{
		'folke/lazydev.nvim',
		ft = 'lua',
		opts = {
			library = {
				{
					path = '${3rd}/luv/library',
					words = { 'vim%.uv' },
				},
			},
		},
	},
	{

		'neovim/nvim-lspconfig',
		dependencies = {
			{ 'mason-org/mason.nvim', opts = {} },
			{ 'mason-org/mason-lspconfig.nvim' },
			{ 'WhoIsSethDaniel/mason-tool-installer.nvim' },
			{ 'j-hui/fidget.nvim', opts = {} },
			{ 'saghen/blink.cmp' },
		},

		config = function()
			-- LuaJIT compat bullshit - this yell at me in one way or another
			-- and it already works and there isn't a good solution it would seem
			---@diagnostic disable-next-line: deprecated
			table.unpack = table.unpack or unpack

			vim.api.nvim_create_autocmd('LspAttach', {
				group = vim.api.nvim_create_augroup('lsp-attach-main', { clear = true }),
				callback = function(e)
					local keymap = require 'config.lsp-keymap'
					local map = function(k, f, d, m)
						m = m or 'n'
						vim.keymap.set(m, k, f, { buffer = e.buf, desc = 'LSP: ' .. d })
					end

					for _, mapping in ipairs(keymap.keys) do
						local k, f, d, m = table.unpack(mapping)
						map(k, f, d, m)
					end

					local client = vim.lsp.get_client_by_id(e.data.client_id)
					if
						client
						and client:supports_method(
							vim.lsp.protocol.Methods.textDocument_documentHighlight,
							e.buf
						)
					then
						local highlight_group =
							vim.api.nvim_create_augroup('lsp-highlight-group', { clear = false })
						vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
							buffer = e.buf,
							group = highlight_group,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
							buffer = e.buf,
							group = highlight_group,
							callback = vim.lsp.buf.clear_references,
						})

						vim.api.nvim_create_autocmd('LspDetach', {
							group = vim.api.nvim_create_augroup(
								'lsp-detach-group',
								{ clear = true }
							),
							callback = function(e2)
								vim.lsp.buf.clear_references()
								vim.api.nvim_clear_autocmds {
									group = 'lsp-highlight-group',
									buffer = e2.buf,
								}
							end,
						})
					end

					if
						client
						and client:supports_method(
							vim.lsp.protocol.Methods.textDocument_inlayHint,
							e.buf
						)
					then
						map('<leader>th', function()
							vim.lsp.inlay_hint.enable(
								not vim.lsp.inlay_hint.is_enabled { bufnr = e.buf }
							)
						end, '[t]oggle inlay [h]ints')
					end
				end,
			})

			local servers = {
				ts_ls = {},
				clangd = {},
				rust_analyzer = {},
				pyright = {},

				lua_ls = {
					settings = {
						Lua = {
							runtime = {
								version = 'LuaJIT',
							},
							completion = {
								callSnippet = 'Replace',
							},
							diagnostics = {
								globals = { 'vim' },
								disable = { 'missing-fields' },
							},
						},
					},
				},

				omnisharp = { },
			}

			---@type MasonLspconfigSettings
			---@diagnostic disable-next-line: missing-fields
			require('mason-lspconfig').setup {
				-- automatic_enable = vim.tbl_keys(servers),
                ensure_installed = vim.tbl_keys(servers)
			}

			local ensure_installed = vim.tbl_keys(servers)
			vim.list_extend(ensure_installed, {
				'stylua',
			})
			require('mason-tool-installer').setup { ensure_installed = ensure_installed }

			-- ----------------
			-- Configuration for some servers may require an old setup call until they have been updated to use
			-- the new mason-lspconfig setup process
			--      https://github.com/neovim/nvim-lspconfig/issues/3705
			--
			-- use `require("lspconfig').server_name.setup{}` for these.
			-- ----------------
			for server_name, config in pairs(servers) do
				vim.lsp.config(server_name, config)
			end

			-- require('lspconfig').omnisharp.setup {}
		end,
	},
}
