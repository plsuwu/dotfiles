return {
	"iamcco/markdown-preview.nvim",
	cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
	-- build = "cd app && npm install",
    build = ':call mkdp#util#install()',
	-- init = function()
	-- 	vim.g.mkdp_filetypes = { "markdown" }
	-- end,
	ft = { "markdown" },
}
