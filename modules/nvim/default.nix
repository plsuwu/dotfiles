{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.nvim;
  texlivePkg = pkgs.texliveFull;
in
{
  options.modules.nvim = {
    enable = lib.mkEnableOption "nvim";
    extraTexlivePkgs = lib.mkEnableOption "texlive";
  };

  config = lib.mkIf cfg.enable {

    # TODO: tidy this up a little :)
    home.packages =
      with pkgs;
      [
        gcc
        luajit

        djlint

        stylua
        prettierd

        python314FreeThreading
        python313Packages.python-lsp-server
        ruff

        shfmt

        cmake
        ccls
        clang-tools

        bun
        nodejs
        ttfautohint

        nixd
        nixfmt-rfc-style

        pyright
        lua-language-server

        gopls
        coursier
        metals

        postgres-language-server
        typescript
        typescript-language-server
        vscode-langservers-extracted
        svelte-language-server
        tailwindcss-language-server

        jdt-language-server

        texlab

        rust-analyzer
        rustc
        rustfmt
        cargo

        gleam

        ghc
        fourmolu
        haskell-language-server
        htmx-lsp

        kdePackages.okular
      ]
      ++ lib.optional cfg.extraTexlivePkgs texlivePkg;

    programs.neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;

      plugins = with pkgs.vimPlugins; [
        lazy-nvim
        lazydev-nvim
        nvim-treesitter
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
        rustaceanvim
        nvim-web-devicons
        nvim-lightbulb
        nvim-code-action-menu
        nvim-lsp-notify
        nvim-metals
        nvim-notify
        crates-nvim
        fidget-nvim
        todo-comments-nvim

        vimtex
      ];

      extraLuaConfig =
        let
          utilsPackDir = pkgs.vimUtils.packDir;
          cfgPackDir = config.programs.neovim.finalPackage.passthru.packpathDirs;
        in
        ''
          vim.g.mapleader = " "
          vim.g.maplocalleader = " "
          require("lazy").setup({
              performance = {
                  reset_packpath = false;
                  rtp = {
                      reset = false,
                  }
              },
              dev = {
                  path = "${utilsPackDir cfgPackDir}/pack/myNeovimPackages/start",
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
