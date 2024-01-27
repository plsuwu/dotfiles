return {
    "tpope/vim-fugitive",
    "tpope/vim-rhubarb",
    "github/copilot.vim",
    "nvim-lua/plenary.nvim",
    {
        'lukas-reineke/indent-blankline.nvim',
        main = 'ibl',
        opts = {},
    },
    { 'numToStr/Comment.nvim', opts = {} },
    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim"
        }
    },
}
