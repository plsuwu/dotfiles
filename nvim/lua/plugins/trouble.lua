return {
    "folke/trouble.nvim",
    config = function()
        require("trouble").setup({
            icons = false, -- might want these idk
        })
        vim.keymap.set("n", "<leadertt", function()
            require("trouble").toggle()
        end)

        vim.keymap.set("n", "[d", function()
            require("trouble").next({ skip_groups = true, jump = true });
        end)
        vim.keymap.set("n", "]d", function()
            require("trouble").previous({ skip_groups = true, jump = true });
        end)
    end
}
