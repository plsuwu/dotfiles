final: prev: {
  claude-code = prev.claude-code.override {
    manifest = prev.lib.importJSON ./claude-code-manifest.json;
  };
}
