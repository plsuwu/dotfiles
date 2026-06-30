{
  pkgs,
  ...
}:
let
  inherit (pkgs) stdenv;
  owner = "plsuwu";
  repo = "dotfiles";
  rev = "d5c878cbecaf23b6491a69bc2eea90670c0354d0";
  url = "https://github.com/${owner}/${repo}/raw/${rev}/myramors_cursor_linux.tar.gz";

  pname = "myramors";

  themeFileContent = ''
    [Icon Theme]
    Name=myramors
    Inherit=myramors
  '';
in

stdenv.mkDerivation {
  inherit pname;
  version = "1.2";
  src = pkgs.fetchurl {
    inherit url;
    hash = "sha256-JEgOuk6L5nVelqTqvE/bz2BzDtgBDrFGRAYFews0dvQ=";
  };

  nativeBuildInputs = [
    pkgs.win2xcur
  ];

  buildPhase = ''
    runHook preBuild

    win_cursors="$PWD"
    base_filename="MyraMors_"
    conversions="$win_cursors/conversions"

    mkdir -p "$conversions"

    declare -a x_cursors
    x_cursors=(
      "default"       # 1   ->  Arrow
      "help"          # 2   ->  Help
      "progress"      # 3   ->  AppStarting
      "wait"          # 4   ->  Wait
      "crosshair"     # 5   ->  Crosshair
      "text"          # 6   ->  IBeam
      "pencil"        # 7   ->  NWPen
      "no-drop"       # 8   ->  No
      "row-resize"    # 9   ->  SizeNS
      "col-resize"    # 10  ->  SizeWE
      "nwse-resize"   # 11  ->  SizeNWSE
      "nesw-resize"   # 12  ->  SizeNESW
      "move"          # 13  ->  SizeAll
      "up-arrow"      # 14  ->  UpArrow
      "pointer"       # 15  ->  Hand
      "pin"           # 16  ->  Pin (non-standard)
      "person"        # 17  ->  Person (non-standard)
    )

    cat <<< "${themeFileContent}" > 'cursor.theme'
    cat <<< "${themeFileContent}" > 'index.theme'

    for index in "''${!x_cursors[@]}"; do
      num=$((index + 1))
      original="$win_cursors/$base_filename$num.ani"
      mv "$original" "''${x_cursors[$index]}"
      win2xcur "''${x_cursors[$index]}" -o "$conversions/"
    done

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    theme_root="$out/share/icons/${pname}"
    install_dir="$theme_root/cursors"

    mkdir -p "$install_dir"
    cp -r conversions/* "$install_dir"/
    cp cursor.theme index.theme "$theme_root/"

    cd "$install_dir"

    declare -A aliases=(
      # default
      [left_ptr]=default
      [arrow]=default
      [top_left_arrow]=default

      # pointer
      [hand2]=pointer
      [hand1]=pointer
      [pointing_hand]=pointer
      [e29285e634086352946a0e7090d73106]=pointer
      [9d800788f1b08800ae810202380a0822]=pointer

      # text
      [xterm]=text
      [ibeam]=text

      # wait
      [watch]=wait
      [clock]=wait

      # progress
      [left_ptr_watch]=progress
      [3ecb610c1bf2410f44200f48c40d3599]=progress
      [00000000000000020006000e7e9ffc3f]=progress

      # help
      [question_arrow]=help
      [whats_this]=help
      [left_ptr_help]=help
      [d9ce0ab605698f320427677b458ad60b]=help
      [5c6cd98b3f3ebcb1f9c7f1c204630408]=help

      # crosshair/cell
      [cross]=crosshair
      [cross_reverse]=crosshair
      [tcross]=crosshair
      [cell]=crosshair
      [plus]=crosshair

      # move
      [fleur]=move
      [all-scroll]=move
      [size_all]=move
      [4498f0e0c1937ffe01fd06f973665830]=move
      [9081237383d90e509aa00f00170e968f]=move

      # no-drop
      [not-allowed]=no-drop
      [forbidden]=no-drop
      [circle]=no-drop
      [crossed_circle]=no-drop
      [03b6e0fcb3499374a867c041f52298f0]=no-drop

      # vertical resize
      [ns-resize]=row-resize
      [sb_v_double_arrow]=row-resize
      [v_double_arrow]=row-resize
      [size_ver]=row-resize
      [00008160000006810000408080010102]=row-resize

      # horizontal resize
      [ew-resize]=col-resize
      [sb_h_double_arrow]=col-resize
      [h_double_arrow]=col-resize
      [size_hor]=col-resize
      [028006030e0e7ebffc7f7070c0600140]=col-resize

      # nwse resize (\)
      [size_fdiag]=nwse-resize
      [fd_double_arrow]=nwse-resize
      [bottom_right_corner]=nwse-resize
      [top_left_corner]=nwse-resize
      [38c5dff7c7b8962045400281044508d2]=nwse-resize
      [c7088f0f3e6c8088236ef8e1e3e70000]=nwse-resize

      # nesw resize (/)
      [size_bdiag]=nesw-resize
      [bd_double_arrow]=nesw-resize
      [bottom_left_corner]=nesw-resize
      [top_right_corner]=nesw-resize
      [50585d75b494802d0151028115016902]=nesw-resize
      [fcf21c00b30f7e3f83fe0dfd12e71cff]=nesw-resize

      # windows "alternate select"
      [center_ptr]=up-arrow

      # grab falls back to neutral arrow
      [grab]=default
      [openhand]=default
      [grabbing]=default
      [closedhand]=default
    )

    for name in "''${!aliases[@]}"; do
      target="''${aliases[$name]}"
      if [ -e "$target" ]; then
        ln -sf "$target" "$name"
      else
        echo "warning: alias '$name' -> missing target '$target', skipping" >&2
      fi
    done

    runHook postInstall
  '';
}
