return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		local lualine = require("lualine")
		local lualine_options = {
			options = {
				theme = "tokyonight",
			},
			sections = {
				lualine_a = {},
				lualine_b = {},
				lualine_c = {
					{
						"diagnostics",
						update_in_insert = true, -- Update diagnostics in insert mode.
						always_visible = false, -- Show diagnostics even if there are none.
					},
				},
				lualine_x = {},
				lualine_y = { "branch",
					{
						"diff",
						colored = true,
					},
				},
				lualine_z = { "location", "progress" },
			},

			tabline = {
				lualine_a = {
					{
						"buffers",
						show_filename_only = true,
                        hide_filename_extension = true,
						mode = 2,
						symbols = {
							modified = "*: ",
							alternate_file = ":: ",
							directory = " ",
						},
					},
				},
				lualine_b = {},
				-- lualine_c = { "filename" },
				lualine_c = {},
				lualine_x = {},
				lualine_y = {},
				lualine_z = { "mode" },
			},
            extensions = { "nvim-tree", "fugitive", "trouble", "mason", "lazy" }
		}

		local function insert_component(component)
			table.insert(lualine_options.tabline.lualine_x, component)
		end

		insert_component({
			function()
				local msg = "no LSP on buffer >_<"
				local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
				local clients = vim.lsp.get_active_clients()
				if next(clients) == nil then
					return msg
				end
				for _, client in ipairs(clients) do
					local filetypes = client.config.filetypes
					if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
						return client.name
					end
				end
				return msg
			end,
			icon = "",
			color = { gui = "italic", "bold" },
		})

		lualine.setup(lualine_options)
	end,
}
