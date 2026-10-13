final: prev: {
  codex = prev.codex.overrideAttrs (
    finalAttrs: prevAttrs:
    let
      v8Version = "150.4.0";
      target = prev.stdenv.hostPlatform.rust.rustcTarget;
      v8Base = "https://github.com/openai/codex/releases/download/rusty-v8-v${v8Version}";

      archiveHashes = {
        x86_64-linux = "a35c75d1f26e6a983885a45b33490a4ebe54f05050568b32b89cfb421b30b583";
        aarch64-linux = "d1517eed405468537029b005d5fe997ec74d5c8d351f916b3a6df20b7d2811ba";
        x86_64-darwin = "e0d9bb64e8b3a034c2930c83972f3f35760211148342fa0407b38250ef330856";
        aarch64-darwin = "00adbb48798848c77550441c68673a5e8529b8e1b73eabcdee232cb39b40f4a1";
      };
      bindingHashes = {
        x86_64-linux = "7727826ae479bdb645e807239fb12d1f8e2e23de7a6cf16f5ee592690d1d8506";
        aarch64-linux = "7727826ae479bdb645e807239fb12d1f8e2e23de7a6cf16f5ee592690d1d8506";
        x86_64-darwin = "ca5adf0cf89c9a70ad460ae73648b2fe89b74aa113b3cb7f757b6a02b758394f";
        aarch64-darwin = "ca5adf0cf89c9a70ad460ae73648b2fe89b74aa113b3cb7f757b6a02b758394f";
      };
      system = prev.stdenv.hostPlatform.system;

      librusty_v8 = prev.fetchurl {
        name = "librusty_v8_ptrcomp_sandbox-${v8Version}.a.gz";
        url = "${v8Base}/librusty_v8_ptrcomp_sandbox_release_${target}.a.gz";
        sha256 = archiveHashes.${system};
      };
      v8Binding = prev.fetchurl {
        name = "src_binding_ptrcomp_sandbox-${v8Version}.rs";
        url = "${v8Base}/src_binding_ptrcomp_sandbox_release_${target}.rs";
        sha256 = bindingHashes.${system};
      };
    in
    {
      version = "0.162.0";
      src = prev.fetchFromGitHub {
        owner = "openai";
        repo = "codex";
        tag = "rust-v${finalAttrs.version}";
        hash = "sha256-YG/9hFOCl4cMYzjaH/3gBid4osxcrvCYQUDDzdbIygo=";
      };

      cargoDeps = prev.rustPlatform.fetchCargoVendor {
        inherit (finalAttrs)
          pname
          version
          src
          sourceRoot
          ;
        hash = "sha256-UTu+ws1DqL375C+1jaVI9HBqDHnTuAQr7/h1rSzsEzg=";
      };

      env = prevAttrs.env // {
        RUSTY_V8_ARCHIVE = librusty_v8;
        RUSTY_V8_SRC_BINDING_PATH = v8Binding;
      };

      postInstall = prevAttrs.postInstall + ''
        pkg=$out/libexec/codex
        mkdir -p $pkg/bin $pkg/codex-path $pkg/codex-resources
        mv $out/bin/codex $out/bin/codex-code-mode-host $pkg/bin/
        ln -s $pkg/bin/codex $out/bin/codex

        makeWrapper ${prev.lib.getExe prev.ripgrep} $pkg/codex-path/rg

        cat > $pkg/codex-package.json <<EOF
        {
          "layoutVersion": 1,
          "version": "${finalAttrs.version}+nix",
          "target": "${prev.stdenv.hostPlatform.rust.rustcTarget}",
          "variant": "codex",
          "entrypoint": "bin/codex"
        }
        EOF
      '';

      postFixup = ''
        wrapProgram $out/libexec/codex/bin/codex --prefix PATH : ${
          prev.lib.makeBinPath (
            [ prev.ripgrep ]
            ++ prev.lib.optionals prev.stdenv.hostPlatform.isLinux [ prev.bubblewrap ]
          )
        }
      '';
    }
  );
}
