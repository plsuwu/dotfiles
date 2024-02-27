{ pkgs, config, lib, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    extraPackages = with pkgs; [
      openssl
      pkg-config
      asm-lsp
    ];

    # plugins = with pkgs.vimPlugins; [
    #
    # ];

    # extraLuaConfig = ''
    #     ${builtins.readFile ./init.lua}
    # '';
  };

  xdg.configFile.nvim = {
    source = ./nvim-config;
    recursive = true;
  };
}
