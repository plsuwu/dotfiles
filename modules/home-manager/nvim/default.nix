{
  pkgs,
  config,
  lib,
  ...
}:
let
  luaPath = "src/new/modules/home-manager/nvim/lua";

  # TODO trim down this plugin list (if bothered)
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
    nvim-notify
    fidget-nvim
    todo-comments-nvim
    typescript-tools-nvim
  ];

  packDir = pkgs.vimUtils.packDir {
    nvim-plugin-dir = {
      start = plugins;
    };
  };

  cfg = config.systemModules.nvim;
in
{
  options.systemModules.nvim = {
    enable = lib.mkEnableOption "nvim";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      fd
      jq
      ripgrep

      python314
      python314Packages.python-lsp-server
      pyright

      tree-sitter
      luajit
      nodejs
      typescript
      bear
      clang-tools

      nixfmt
      stylua
      prettierd
      ruff

      nixd
      lua-language-server
      typescript-language-server
      svelte-language-server
      rust-analyzer

      tailwindcss-language-server
      vscode-langservers-extracted

      lemminx
    ];

    programs.neovim = {
      inherit plugins;
      enable = true;
      package = pkgs.neovim-unwrapped;

      viAlias = true;
      vimAlias = true;
      withRuby = true;
      withPython3 = true;
      withNodeJs = true;

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

    xdg.configFile."nvim/lua" = {
      recursive = true;
      source = ./lua;
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let
          editor = "nvim.desktop";
        in
        {
          "text/plain" = "${editor}";
          "text/markdown" = "${editor}";
          "text/x-log" = "${editor}";
          "application/x-yaml" = "${editor}";
        };
    };

    xdg.desktopEntries.nvim = {
      name = "Neovim";
      genericName = "Text Editor";
      exec = "${pkgs.alacritty}/bin/alacritty --class nvim -e ${pkgs.neovim}/bin/nvim %F";
      terminal = false;
      icon = "neovim";
      type = "Application";
      categories = [
        "Utility"
        "TextEditor"
      ];

      settings = {
        Keywords = "neovim;vim;";
      };

      mimeType = [
        "text/plain"
        "text/markdown"
        "text/x-log"
        "application/x-yaml"
      ];
    };
  };
}
