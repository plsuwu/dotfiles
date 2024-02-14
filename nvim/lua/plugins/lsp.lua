return {
    "neovim/nvim-lspconfig",
    dependencies = {
        { "williamboman/mason.nvim", config = true },
        "williamboman/mason-lspconfig.nvim",

        {
            "jay-babu/mason-null-ls.nvim",
            event = {"BufReadPre", "BufNewFile"},
            dependencies = {
                "williamboman/mason.nvim",
                "nvimtools/none-ls.nvim"
            }
        },

        "jose-elias-alvarez/null-ls.nvim",
        'folke/neodev.nvim',

        {
            'hrsh7th/nvim-cmp',
            dependencies = {
                'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer',
                'hrsh7th/cmp-path', 'hrsh7th/cmp-cmdline', 'L3MON4D3/LuaSnip',
                'saadparwaiz1/cmp_luasnip', 'rafamadriz/friendly-snippets'
            }
        },
        {
            "j-hui/fidget.nvim",
            opts = {
                integration = {
                    ["nvim-tree"] = {
                        enable = true
                    }
                }
            }
        }
    },
    config = function()

        local cmp = require("cmp")
        local cmp_select = {behavior = cmp.SelectBehavior.Select}
        local cmp_lsp = require("cmp_nvim_lsp")
        local capabilities = vim.tbl_deep_extend(
            "force",
            {},
            vim.lsp.protocol.make_client_capabilities(),
            cmp_lsp.default_capabilities()
        )

        require("fidget").setup({})
        require("mason").setup()
        require("mason-lspconfig").setup({
            ensure_installed = {"lua_ls", "tsserver", "rust_analyzer", "ruff_lsp"},
            automatic_installation = true,
            handlers = {
                function(server_name)
                    require("lspconfig")[server_name].setup {
                        capabilities = capabilities
                    }
                end,

                ["lua_ls"] = function()
                    local lspconfig = require("lspconfig")
                    lspconfig.lua_ls.setup {
                        capabilities = capabilities,
                        settings = {
                            Lua = {
                                diagnostics = {
                                    globals = {"vim"}
                                }
                            }
                        }
                    }
                end

            }

        })

        require("mason-null-ls").setup({handlers = {}})
        require("null-ls").setup()
        require("neodev").setup()
        require('luasnip.loaders.from_vscode').lazy_load()

        local luasnip = require("luasnip")
        luasnip.config.setup {}
        cmp.setup({
            snippet = {
                expand = function(args)
                    luasnip.lsp_expand(args.body)
                end
            },
            window = {
                completion = cmp.config.window.bordered(),
                documentation = cmp.config.window.bordered()
            },
            mapping = cmp.mapping.preset.insert({
                ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
                ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
                ['<C-y>'] = cmp.mapping.confirm({select = true}),
                ["<C-Space>"] = cmp.mapping.complete()
            }),
            sources = cmp.config.sources({
                {name = "nvim_lsp"},
                {name = "luasnip"},
                {name = "friendly-snippets"},
                {name = "neodev"},
                {name = "path"},
            }, {
                {name = "buffer"}
            })
        })
        vim.diagnostic.config({
            -- update_in_insert = true,
            float = {
                focusable = false,
                style = "minimal",
                border = "rounded",
                source = "always",
                header = "",
                prefix = ""
            }
        })
    end
}
