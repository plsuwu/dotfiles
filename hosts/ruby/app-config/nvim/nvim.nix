{ config, pkgs, inputs, ... }:

# move nvim dir to nix/common -> want this specifically separate so
# running nvim as root will use this config.
# doesnt work tho bc idk how to get my lua in here without going insane!!
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };
}
