return {

    "tpope/vim-fugitive",
    "tpope/vim-rhubarb",
    "github/copilot.vim",
    "nvim-lua/plenary.nvim",
    -- "gc" in visual to comment lines out -> this is surely a remappable plugin
    { 'numToStr/Comment.nvim', opts = {} },

    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim"
        }
    },
    require("plugins.debugger")
}

