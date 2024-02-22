{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      colors = {
        primary = {
          background = "#1a1b26";
          foreground = "#a9b1d6";
        };
        normal = {
          black = "#32344a";
          red = "#f7768e";
          green = "#9ece6a";
          yellow = "#e0af68";
          blue = "#7aa2f7";
          magenta = "#ad8ee6";
          cyan = "#449dab";
          white = "#787c99";
        };
        bright = {
          black = "#444b6a";
          red = "#ff7a93";
          green = "#b9f27c";
          yellow = "#ff9e64";
          blue = "#7da6ff";
          magenta = "#bb9af7";
          cyan = "#0db9d7";
          white = "#acb0d0";
        };
      };
      scrolling = {
        history = 50000;
        multipler = 3;
      };
      window = {
        decorations = "full";
        dynamic_padding = true;
        dynamic_title = true;
        opacity = 1.0;
        padding = {
          x = 25;
          y = 25;
        };
        class = {
          general = "Alacritty";
        };
      };
      font = {
        size = 10.0;
        bold = {
          family = "JetBrainsMonoNerdFont";
          style = "Bold";
        };
        bold_italic = {
          family = "JetBrainsMonoNerdFont";
          style = "Bold Italic";
        };
        italic = {
          family = "JetBrainsMonoNerdFont";
          style = "Italic";
        };
        normal = {
          family = "JetBrainsMonoNerdFont";
          style = "Regular";
        };
      };
      shell = {
        program = "zsh";
        args = [ "--login" "-c" "tmux" ];
      };
    };
  };
}
