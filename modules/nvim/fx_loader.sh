#! /usr/bin/env nix-shell
#! nix-shell -i bash -p patchelf

for bin in ${@}
do
    patchelf \
        --set-interpreter "$(cat ${NIX_CC}/nix-support/dynamic-linkier" \
        "${bin}"
done
