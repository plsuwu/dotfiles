{
  pkgs ? import <nixpkgs> { },
  ...
}:
let
  inherit (pkgs) lib stdenv;
  pname = "myramors";
  commit = "d5c878cbecaf23b6491a69bc2eea90670c0354d0";
  url = "https://github.com/plsuwu/dotfiles/raw/${commit}/myramors_cursor_linux.tar.gz";

  themeFileContent = ''
    [Icon Theme]
    name=myramors
    inherits="myramors"
  '';
in
stdenv.mkDerivation {
  inherit pname;
  version = "1.1";
  src = fetchTarball {
    inherit url;
    sha256 = "181bsyqy34z4sh1lcmna3kdjjbkq7scc1911dbs3fwwwlag2mhw5";
  };

  buildInputs = with pkgs; [
    win2xcur
  ];

  installPhase = ''
    win_cursors="$PWD/ani"
    base_filename="MyraMors_"
    conversions="$win_cursors/conversions"

    mkdir -p "$conversions"

    declare -a x_cursors
    x_cursors=(
      "default" 
      "help" 
      "progress" 
      "wait" 
      "move" 
      "text"
      "pencil"
      "no-drop"
      "row-resize" 
      "col-resize"
      "nwse-resize"
      "nesw-resize" 
      "crosshair"
      "hand2"
      "pointer"
      "pin"
      "person"
    )

    cat <<< "${themeFileContent}" > 'cursor.theme'
    cat <<< "${themeFileContent}" > 'index.theme'

    for index in "''${!x_cursors[@]}"; do
      num=$((index + 1))

      original="$win_cursors/$base_filename$num.ani"

      mv "$original" "''${x_cursors[$index]}"
      win2xcur "''${x_cursors[$index]}" -o "$conversions/"
    done


    install_dir=$out/share/icons/${pname}/cursors/
    mkdir -p $install_dir
    cp -r $conversions/* $install_dir

    ln -sf "$install_dir/crosshair" "$install_dir/cell"

    cp -t $out/share/icons/${pname}/ cursor.theme index.theme
  '';
}
