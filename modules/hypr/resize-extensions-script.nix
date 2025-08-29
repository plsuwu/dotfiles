pkgs:

pkgs.writeScript "hyprland-bitwarden-resize" ''
  #!/usr/bin/env sh

  handle() {
    case $1 in
      windowtitle*)

        window_id=''${1#*>>}
        window_info=$(hyprctl clients -j | ${pkgs.jq}/bin/jq --arg id "0x$window_id" '.[] | select(.address == ($id))')
        window_title=$(echo "$window_info" | ${pkgs.jq}/bin/jq '.title')

        if [[ "$window_title" == *"(Bitwarden Password Manager) - Bitwarden"* ]]; then

          # echo $window_id, $window_title
          # hyprctl dispatch togglefloating address:0x$window_id
          # hyprctl dispatch resizewindowpxel exact 20% 40%,address:0x$window_id
          # hyprctl dispatch movewindowpxel exact 40% 30%,address0x$window_id

          hyprctl --batch "dispatch togglefloating address:0x$window_id ; dispatch resizewindowpixel exact 20% 40%,address:0x$window_id ; dispatch movewindowpixel exact 40% 30%,address:0x$window_id"
        fi
        ;;
      esac
  }

  ${pkgs.socat}/bin/socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do handle "$line"; done
''
