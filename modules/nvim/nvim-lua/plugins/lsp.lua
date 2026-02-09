return {
	{
		"folke/lazydev.nvim",
		ft = "lua",
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "j-hui/fidget.nvim", opts = {} },
			{ "saghen/blink.cmp" },
		},

		config = function()
			-- LuaJIT SCREAMS at me regardless of what i do here
			---@diagnostic disable-next-line: deprecated
			table.unpack = table.unpack or unpack

			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("lsp-attach-main", { clear = true }),
				callback = function(e)
					local keymap = require("config.lsp-keymap")
					local map = function(k, f, d, m)
						m = m or "n"
						vim.keymap.set(m, k, f, { buffer = e.buf, desc = "LSP: " .. d })
					end

					for _, mapping in ipairs(keymap.keys) do
						local k, f, d, m = table.unpack(mapping)
						map(k, f, d, m)
					end

					local client = vim.lsp.get_client_by_id(e.data.client_id)
					if
						client
						and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, e.buf)
					then
						local highlight_group = vim.api.nvim_create_augroup("lsp-highlight-group", { clear = false })
						vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
							buffer = e.buf,
							group = highlight_group,
							callback = vim.lsp.buf.document_highlight,
						})

						vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
							buffer = e.buf,
							group = highlight_group,
							callback = vim.lsp.buf.clear_references,
						})

						vim.api.nvim_create_autocmd("LspDetach", {
							group = vim.api.nvim_create_augroup("lsp-detach-group", { clear = true }),
							callback = function(e2)
								vim.lsp.buf.clear_references()
								vim.api.nvim_clear_autocmds({
									group = "lsp-highlight-group",
									buffer = e2.buf,
								})
							end,
						})
					end

					if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, e.buf) then
						map("<leader>th", function()
							vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = e.buf }))
						end, "[t]oggle inlay [h]ints")
					end
				end,
			})

			local servers = {
				svelte = {},
				tailwindcss = {},
				ruff = {},
				ccls = {},
				nixd = {},
				rust_analyzer = {},
				ts_ls = {},

				denols = {
					root_dir = function(bufnr, on_dir)
						local root_markers = { "deno.lock" }
						root_markers = vim.fn.has("nvim-0.11.3") == 1 and { root_markers, { ".git" } }
							or vim.list_extend(root_markers, { ".git " })

						local ignore_path = vim.fs.root(
							bufnr,
							{ "package-lock.json", "yarn.lock", "pnpm-lock.yaml", "bun.lock", "bun.lockb" }
						)

						local project_root = vim.fs.root(bufnr, root_markers)
						if ignore_path and (not project_root or #ignore_path >= #project_root) then
							return
						end
						on_dir(project_root or vim.fn.getcwd())
					end,
				},
				pylsp = {
					settings = {
						pylsp = {
							plugins = {
								pycodestyle = {
									ignore = { "W391" },
									maxLineLength = 80,
								},
							},
						},
					},
				},

				lua_ls = {
					on_init = function(client)
						if client.workspace_folders then
							local path = client.workspace_folders[1].name
							if
								path ~= vim.fn.stdpath("config")
								-- `vim.uv.fs_stat` is allegedly undefined ://
								---@diagnostic disable-next-line: undefined-field
								and (vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc"))
							then
								return
							end
						end

						client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
							runtime = {
								version = "LuaJIT",
								path = {
									"lua/?.lua",
									"lua/?/init.lua",
								},
							},
							workspace = {
								checkThirdParty = false,
								library = {
									vim.env.VIMRUNTIME,
								},
							},
						})
					end,
					settings = {
						Lua = {},
					},
				},
			}

			for name, conf in pairs(servers) do
				vim.lsp.config(name, conf)
				vim.lsp.enable(name)
			end

			-- vim.lsp.enable('air') -- R fmt/lsp

			-- vim.lsp.enable("texlab")
			--
			-- vim.lsp.enable("ts_ls")
			-- vim.lsp.config("ts_ls", {
			-- 	workspace_required = true,
			-- 	root_markers = { "package.json" },
			-- })
			--
			-- vim.lsp.enable("svelte")
			-- vim.lsp.enable("tailwindcss")
			--
			-- vim.lsp.enable("postgres-lsp")
			--
			-- vim.lsp.enable("nixd")
			-- vim.lsp.enable("rust-analyzer")
			--
			-- vim.lsp.enable("cssls")
			-- vim.lsp.enable("gopls")
			-- vim.lsp.enable("ruff")
			--
			-- vim.lsp.enable("ccls")
			-- vim.lsp.enable("gleam")
			--
			-- vim.lsp.enable("denols")
			-- vim.lsp.config("denols", {
			-- 	cmd = { "deno", "lsp" },
			-- 	root_markers = { "deno.json", "deno.jsonc" },
			-- 	workspace_required = true,
			-- })
			--
			-- -- vim.lsp.enable("metals")  -- scala LSP
			-- vim.lsp.enable("pylsp")
			-- vim.lsp.config("pylsp", {
			-- 	settings = {
			-- 		pylsp = {
			-- 			plugins = {
			-- 				pycodestyle = {
			-- 					ignore = { "W391" },
			-- 					maxLineLength = 80,
			-- 				},
			-- 			},
			-- 		},
			-- 	},
			-- })
			-- -- vim.lsp.enable("pyright")
			-- -- vim.lsp.config("pyright", {
			-- -- 	settings = {
			-- -- 		pyright = {
			-- -- 			-- Using Ruff's import organizer
			-- -- 			disableOrganizeImports = true,
			-- -- 		},
			-- -- 		python = {
			-- -- 			analysis = {
			-- -- 				-- Ignore all files for analysis to exclusively use Ruff for linting
			-- -- 				ignore = { "*" },
			-- -- 			},
			-- -- 		},
			-- -- 	},
			-- -- })
			--
			-- vim.lsp.enable("hls")
			-- -- vim.lsp.enable("htmx")
			--
			-- vim.lsp.enable("jdtls") -- java
			--
			-- vim.lsp.config("hls", {
			-- 	filetypes = { "haskell", "lhaskell", "cabal" },
			-- })
			--
			-- vim.lsp.enable("lua_ls")
			-- vim.lsp.config("lua_ls", {
			-- 	on_init = function(client)
			-- 		if client.workspace_folders then
			-- 			local path = client.workspace_folders[1].name
			-- 			if
			-- 				path ~= vim.fn.stdpath("config")
			-- 				-- `vim.uv.fs_stat` is allegedly undefined ://
			-- 				---@diagnostic disable-next-line: undefined-field
			-- 				and (vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc"))
			-- 			then
			-- 				return
			-- 			end
			-- 		end
			--
			-- 		client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
			-- 			runtime = {
			-- 				version = "LuaJIT",
			-- 				path = {
			-- 					"lua/?.lua",
			-- 					"lua/?/init.lua",
			-- 				},
			-- 			},
			-- 			workspace = {
			-- 				checkThirdParty = false,
			-- 				library = {
			-- 					vim.env.VIMRUNTIME,
			-- 				},
			-- 			},
			-- 		})
			-- 	end,
			-- 	settings = {
			-- 		Lua = {},
			-- 	},
			-- })
		end,
	},
}
