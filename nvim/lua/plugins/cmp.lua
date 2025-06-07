return {
	'saghen/blink.cmp',
	event = 'VimEnter',
	version = '1.*',
	dependencies = {
		-- snippet engine
		{
			'L3MON4D3/LuaSnip',
			version = '2.*',
			build = (function()
				-- Build Step is needed for regex support in snippets.
				-- This step is not supported in many windows environments.
				-- Remove the below condition to re-enable on windows.
				if vim.fn.has 'win32' == 1 or vim.fn.executable 'make' == 0 then
					return
				end
				return 'make install_jsregexp'
			end)(),
			dependencies = {
				{
					'rafamadriz/friendly-snippets',
					config = function()
						require('luasnip.loaders.from_vscode').lazy_load()
					end,
				},
			},
			opts = {},
		},
		'folke/lazydev.nvim',
	},

	--- @module 'blink.cmp'
	--- @type blink.cmp.Config
	opts = {
		keymap = {
			preset = 'default',
		},
		appearance = {
			-- nerd_font_variant = 'mono',
			nerd_font_variant = 'normal',
		},

		completion = {
			documentation = { auto_show = false, auto_show_delay_ms = 300 },
			-- documentation = { auto_show = true },
		},

		sources = {
			default = { 'lazydev', 'lsp', 'path', 'snippets', 'buffer' },
			providers = {
				lazydev = {
					name = 'LazyDev',
					module = 'lazydev.integrations.blink',
					score_offset = 100,
				},
			},
		},

		snippets = { preset = 'luasnip' },
		fuzzy = { implementation = 'prefer_rust_with_warning' },
		signature = { enabled = true },
	},

	opts_extend = { 'sources.default' },
}
