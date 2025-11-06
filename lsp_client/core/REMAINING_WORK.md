# LSP Client Core - Headless Status

All protocol units now compile headless. The callback interface provides every
service the core expects (environment setup, language server lookup, document
lifecycle, workspace edits, timers). `Null_Callback` keeps the build runnable
without a host UI.

## Still excluded

- `gps-lsp_client-requests-check_syntax.*`
- `gps-lsp_client-requests-*_formatting.*`

These remain GUI-dependent and stay out of `lsp_client_core.gpr` until the TUI
provides replacements.

## Validation commands

```
ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr -p
ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P safety_build.gpr
```

Both succeed (clang warns about macOS deployment versions).
