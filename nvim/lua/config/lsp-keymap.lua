return {
    keys = {
        { "gd", require("telescope.builtin").lsp_definitions, "[g]oto [d]efinition" },
        { "gr", require("telescope.builtin").lsp_references, "[g]oto [r]eferences" },
        { "gI", require("telescope.builtin").lsp_implementations, "[g]oto [I]mpletementation" },
        { "<leader>D", require("telescope.builtin").lsp_type_definitions, "type [D]efinition" },
        { "<leader>ds", require("telescope.builtin").lsp_document_symbols, "[d]ocument [s]ymbols" },
        { "<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[w]orkspace [s]ymbols" },
        { "<leader>rn", vim.lsp.buf.rename, "[r]e[n]ame buffer" },
        { "<leader>ca", vim.lsp.buf.code_action, "[c]ode [a]ctions", { "n", "x" } },
        { "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration" }
    }
}
