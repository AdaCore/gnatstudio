# Deleted LSP Client GUI Integration

This directory contains the GUI integration layer of lsp_client, separated during lsp_client_core split (PLAN.md Phase 5).

## Contents (50 files)

GUI integration files that depend on:
- GPS.Kernel (full IDE integration)
- Src_Editor (GTK text editor - DELETED)
- Views (GTK view system - DELETED)
- Browsers (GTK tree browsers - DELETED)
- GTK+ widgets

### Files Archived:

**Module Registration:**
- gps-lsp_module.ads/adb - GPS module initialization

**Editor Integration:**
- gps-lsp_client-editors.ads/adb - Base editor integration
- gps-lsp_client-editors-code_actions.ads/adb - Code actions UI
- gps-lsp_client-editors-code_actions-dialog.ads/adb - Code action dialogs
- gps-lsp_client-editors-folding.ads/adb - Code folding UI
- gps-lsp_client-editors-formatting.ads/adb - Document formatting UI
- gps-lsp_client-editors-highlight.ads/adb - Syntax highlight UI
- gps-lsp_client-editors-navigation.ads/adb - Navigation UI (goto def, etc.)
- gps-lsp_client-editors-semantic_tokens.ads/adb - Semantic token UI
- gps-lsp_client-editors-signature_help.ads/adb - Signature help popup
- gps-lsp_client-editors-tooltips.ads/adb - Hover tooltip UI

**GUI Views:**
- gps-lsp_client-completion.ads/adb - GTK completion popup
- gps-lsp_client-outline.ads/adb - Outline tree view
- gps-lsp_client-references.ads/adb - Find references view
- gps-lsp_client-call_tree.ads/adb - Call hierarchy view
- gps-lsp_client-dependency_browers.ads/adb - Dependency browser

**Refactoring UI:**
- gps-lsp_client-refactoring.ads/adb - Refactoring UI base
- gps-lsp_client-refactoring-rename.ads/adb - Rename UI
- gps-lsp_client-refactoring-name_parameters.ads/adb - Name parameters UI
- gps-lsp_client-edit_workspace.ads/adb - Workspace edit UI

**Search Integration:**
- gps-lsp_client-search.ads/adb - Search integration
- gps-lsp_client-search-entities.ads/adb - Entity search

**Python/Shell Bindings:**
- gps-lsp_client-shell.ads/adb - Python shell bindings

**Background Tasks:**
- gps-lsp_client-tasks.ads/adb - Background task UI

## Why Separated

These files cannot be used in TUI because:
1. Depend on Src_Editor module (deleted)
2. Use GTK widgets for UI (popups, trees, dialogs)
3. Integrate with GPS Views system (deleted)
4. Use GPS.Kernel for full IDE integration

## TUI Replacement

All functionality will be reimplemented in TUI during Phase 5-6:
- Completion: TUI popup using terminal UI
- Hover/tooltips: Ephemeral overlay in terminal
- Outline: Terminal tree view
- References: Terminal list view
- Code actions: TUI menu selection
- Diagnostics: Gutter marks in terminal

## Protocol Layer

The protocol layer (74 files) has been moved to lsp_client/core/ and includes:
- All LSP request/response handling
- Server lifecycle management
- Text document synchronization
- Configuration (ALS, clangd)
- No GUI dependencies

## Recovery

If needed, these files can be restored from git history:
```bash
git log --all --full-history -- lsp_client/deleted_gui/
```

## Status

Created: 2025-11-05
Part of: PLAN.md Phase 5 (LSP Support)
Related: lsp_client/core/lsp_client_core.gpr
