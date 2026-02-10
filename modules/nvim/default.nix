{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.nvim;
  plugins = with pkgs.vimPlugins; [
    lazy-nvim
    lazydev-nvim
    nvim-treesitter.withAllGrammars
    blink-cmp
    friendly-snippets
    luasnip
    conform-nvim
    plenary-nvim
    fzf-lua
    harpoon2
    tiny-inline-diagnostic-nvim
    trouble-nvim
    telescope-nvim
    telescope-fzf-native-nvim
    telescope-ui-select-nvim
    nvim-tree-lua
    tokyonight-nvim
    nvim-lspconfig
    nvim-web-devicons
    nvim-lightbulb
    nvim-code-action-menu
    nvim-lsp-notify
    nvim-metals
    nvim-notify
    crates-nvim
    fidget-nvim
    todo-comments-nvim
    typescript-tools-nvim
  ];

  packDir = pkgs.vimUtils.packDir {
    nvim-plugin-dir = {
      start = plugins;
    };
  };
in
{
  options.modules.nvim = {
    enable = lib.mkEnableOption "nvim";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ttfautohint
      fd

      tree-sitter

      gcc
      cmake
      luajit
      bun
      nodejs
      ghc

      ccls
      clang-tools
      typescript
      coursier

      nixfmt
      stylua
      prettierd
      shfmt
      ruff
      djlint
      fourmolu

      python314FreeThreading
      python314Packages.python-lsp-server
      pyright

      nixd
      metals
      lua-language-server
      typescript-language-server
      svelte-language-server
      haskell-language-server
      htmx-lsp

      vscode-langservers-extracted
      tailwindcss-language-server
      rust-analyzer
      # rustc
      # rustfmt
      # cargo
    ];

    programs.neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;

      inherit plugins;

      initLua = ''
        vim.g.mapleader = " "
        vim.g.maplocalleader = " "
        require("lazy").setup({
            performance = {
                reset_packpath = false,
                rtp = { reset = false }
            },
            dev = {
                path = "${packDir}/pack/nvim-plugin-dir/start",
                patterns = {""},
            },
            spec = {
                { import = "plugins" },
            },
            install = {
                missing = false,
            },
        })

        require("config.keybind")
        require("config.settings")
      '';
    };

    home.file."./.config/nvim/lua" = {
      recursive = true;
      source = ./nvim-lua;
    };
  };
}
