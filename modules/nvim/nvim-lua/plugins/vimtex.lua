return {
	"lervag/vimtex",
	lazy = false,

	init = function()
		vim.g.vimtex_view_general_viewer = "okular"
		vim.g.vimtex_view_general_options = "--unique file:@pdf#src:@line@tex"
		vim.g.vimtex_syntax_enabled = 0
		vim.g.vimtext_compiler_latexmk = {
			aux_dir = "./.latexmk/aux-" .. vim.fn.expand("%:t:r"),
			out_dir = "./.latexmk/out-" .. vim.fn.expand("%:t:r"),
		}
		-- vim.g.vimtex_compiler_latexmk_engines = { _ = "-pdf" }
	end,
}
