final: prev: {
  swayimg = prev.swayimg.overrideAttrs (
    finalAttrs: prevAttrs: {
      version = "v5.5";
      src = prev.fetchFromGitHub {
        owner = "artemsen";
        repo = "swayimg";
        tag = "v${finalAttrs.version}";
        hash = "sha256-PaxVcuEafLdUETSG78lGSaDukPv/2m1TUbfvpBZTT40=";
      };

      buildInputs =
        prevAttrs.buildInputs
        ++ (with final; [
          luajit
          openjpeg
          exiv2
        ]);
    }
  );
}
