{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.nvim;
in
{
  options.modules.nvim = {
    enable = lib.mkEnableOption "nvim";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gcc
      luajit

      stylua
      prettierd
      black

      shfmt

      cmake
      ccls
      clang-tools

      nixd
      nixfmt-rfc-style

      lua-language-server
      postgres-lsp
      gopls
      typescript
      typescript-language-server
      vscode-langservers-extracted
      nodejs_22

      rust-analyzer
      cargo
    ];

    # home.sessionVariables = {
    #   # `ccls` to auto-generate a `compile_commands.json`
    #   CMAKE_EXPORT_COMPILE_COMMANDS = "YES";
    # };

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
        nvim-notify
        crates-nvim
        fidget-nvim
        todo-comments-nvim
      ];

      extraLuaConfig =
        let
          utilsPackDir = pkgs.vimUtils.packDir;
          cfgPackDir = config.programs.neovim.finalPackage.passthru.packpathDirs;
        in
        ''
          vim.g.mapleader = " "
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
