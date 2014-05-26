"""
Defines the default icon theme for GPS.

Most icons in GPS can be edited by changing the contents of this script
(or creating another one similar to this one and loading it instead of
this one).
"""

import GPS
from GPS import Icon


def add_icons():
    Icon("gtk-paste", "Paste", "svg/paste.svg")
    Icon("gtk-copy", "Copy", "svg/copy.svg")
    Icon("gtk-cut", "Cut", "svg/cut.svg")
    Icon("gps-navigate-forward", "Forward", "svg/right.svg")
    Icon("gps-navigate-back", "Back", "svg/left.svg")
    Icon("gtk-new", "New", "svg/newdocument.svg")
    Icon("gtk-open", "Open", "svg/folder.svg",
         alt_local_toolbar="svg/folder-12.svg")
    Icon("gtk-redo", "Redo", "svg/redo.svg")
    Icon("gtk-cdrom", "Save", "svg/save.svg",
         alt_local_toolbar="svg/save-12.svg")
    Icon("gtk-undo", "Undo", "svg/undo.svg"),
    Icon("gtk-preferences", "Preferences", "svg/settings.svg"),
    Icon("gtk-close", "Close", "svg/close.svg"),
    Icon("gps-clear-entry", "Clear Entry", "svg/clear_entry.svg"),
    Icon("gtk-print", "Print", "svg/print.svg"),
    Icon("gtk-clear", "Clean", "svg/clean.svg",
         alt_local_toolbar="svg/clean-12.svg")
    Icon("gtk-media-play", "Play", "svg/run.svg"),
    Icon("gtk-remove", "Remove", "svg/minus.svg"),
    Icon("gtk-add", "Add", "svg/plus.svg"),
    Icon("gps-edit", "Edit", "svg/edit.svg"),
    Icon("gps-environment", "Environment", "svg/environment.svg"),
    Icon("gps-messages", "Messages", "svg/messages.svg"),
    Icon("gps-power", "Power", "svg/power.svg"),
    Icon("gps-error", "Error", "svg/error.svg"),
    Icon("gps-warning", "Warning", "svg/warning.svg"),
    Icon("gps-info", "Info", "svg/info.svg"),
    Icon("gps-project", "Project", "svg/project.svg"),
    Icon("gps-refresh", "Refresh", "svg/refresh.svg"),
    Icon("gps-search", "Search", "svg/search.svg"),
    Icon("gps-zip", "Zip", "svg/zip.svg"),
    Icon("gps-zoom", "Zoom", "svg/zoom.svg"),
    Icon("gps-zoomout", "Zoom out", "svg/zoomout.svg"),
    Icon("gps-debugger-run", "Go", "24px/play_24.png",
         alt_menu="16px/play_16.png", alt_small_toolbar="18px/play_18.png")
    Icon("gps-debugger-step", "Step", "24px/step_24.png",
         alt_menu="16px/step_16.png", alt_small_toolbar="18px/step_18.png")

    Icon("gps-debugger-next", "Next", "24px/next_24.png",
         alt_menu="16px/next_16.png", alt_small_toolbar="18px/next_18.png")

    Icon("gps-debugger-finish", "Finish", "24px/finish_24.png",
         alt_menu="16px/finish_16.png", alt_small_toolbar="18px/finish_18.png")

    Icon("gps-debugger-up", "Up", "24px/stack_up_24.png",
         alt_menu="16px/stack_up_16.png",
         alt_small_toolbar="18px/stack_up_18.png")

    Icon("gps-debugger-down", "Down", "24px/stack_down_24.png",
         alt_menu="16px/stack_down_16.png",
         alt_small_toolbar="18px/stack_down_18.png")

    Icon("gps-debugger-break", "Break", "16px/debug_break.png",
         alt_menu="16px/debug_break.png")

    Icon("gps-debugger-current", "Current", "16px/debug_indicator.png",
         alt_menu="16px/debug_indicator.png")

    Icon("gps-debugger-line-might-have-code", "",
         "9px/line_might_have_code.png",
         alt_menu="9px/line_might_have_code.png")

    Icon("gps-debugger-line-has-code", "", "9px/line_has_code.png",
         alt_menu="9px/line_has_code.png")

    Icon("gps-file", "", "16px/file.png")
    Icon("gps-file-modified", "", "16px/file_pencil.png")

    Icon("gps-configMenu", "", "svg/menu.svg",
         alt_local_toolbar="svg/menu-12.svg")

    Icon("gps-stopTask", "", "svg/close.svg")
    Icon("gps-expandAll", "", "9px/expand_12.png")
    Icon("gps-collapseAll", "", "9px/collapse_12.png")
    Icon("gps-toggle-links", "", "9px/toggle_links_12.png")
    Icon("gps-remove-unselected", "", "9px/remove_unselected_12.png")
    Icon("gps-read-only", "", "svg/lock-12.svg")
    Icon("gps-writable", "", "svg/unlock-12.svg")
    Icon("gps-regexp", "", "16px/regexp.png")
    Icon("gps-negate", "", "16px/negate.png")

    Icon("gps-folder-closed", "", "16px/folder_closed.png")
    Icon("gps-folder-open", "", "16px/folder_open.png")

    Icon("gps-folder-exec-closed", "", "16px/folder_e_closed.png")
    Icon("gps-folder-exec-open", "", "16px/folder_e_open.png")

    Icon("gps-folder-obj-closed", "", "16px/folder_o_closed.png")
    Icon("gps-folder-obj-open", "", "16px/folder_o_open.png")

    Icon("gps-project-open", "", "16px/project_box.png")
    Icon("gps-project-closed", "", "16px/project_box.png")
    Icon("gps-project-root-open", "", "16px/project_root_box.png")
    Icon("gps-project-root-closed", "", "16px/project_root_box.png")
    Icon("gps-project-modified-open", "", "16px/project_box_pencil.png")
    Icon("gps-project-modified-closed", "", "16px/project_box_pencil.png")

    Icon("gps-search-and-menu", "", "svg/search-and-menu.svg")
    Icon("gps-fold-block", "", "svg/gps-fold-block.svg")
    Icon("gps-unfold-block", "", "svg/gps-unfold-block.svg")

    Icon("gps-double-arrow", "", "svg/double_arrow.svg")

    Icon("gps-box", "", "16px/box.png")

    Icon("adacore-logo", "", "other/adacore_logo.png",
         alt_large_toolbar="other/adacore_logo.png")

    Icon("button-guide", "", "other/button_guide.png",
         alt_large_toolbar="other/button_guide.png")

    Icon("button-guide-over", "", "other/button_guide_over.png",
         alt_large_toolbar="other/button_guide_over.png")

    Icon("button-overview", "", "other/button_overview.png",
         alt_large_toolbar="other/button_overview.png")

    Icon("button-overview-over", "", "other/button_overview_over.png",
         alt_large_toolbar="other/button_overview_over.png")

    Icon("button-tutorial", "", "other/button_tutorial.png",
         alt_large_toolbar="other/button_tutorial.png")

    Icon("button-tutorial-over", "", "other/button_tutorial_over.png",
         alt_large_toolbar="other/button_tutorial_over.png")

    Icon("get-started", "", "other/get_started.png",
         alt_large_toolbar="other/get_started.png")

    Icon("welcome-header", "", "other/welcome_header.png",
         alt_large_toolbar="other/welcome_header.png")

    Icon("gps-warning", "", "16px/warning.png",
         alt_menu="16px/warning.png")

    # Entities -->

    Icon("gps-entity-package", "", "svg/square_x.svg")
    Icon("gps-entity-subprogram", "", "svg/circle_x.svg")
    Icon("gps-entity-type", "", "svg/triangle_x.svg")
    Icon("gps-entity-variable", "", "svg/dot_x.svg")
    Icon("gps-entity-generic", "", "svg/diamond_x.svg")
    Icon("gps-entity-package-spec", "", "svg/square_xs.svg")
    Icon("gps-entity-subprogram-spec", "", "svg/circle_xs.svg")
    Icon("gps-entity-type-spec", "", "svg/triangle_xs.svg")
    Icon("gps-entity-variable-spec", "", "svg/dot_xs.svg")
    Icon("gps-entity-generic-spec", "", "svg/diamond_xs.svg")
    Icon("gps-entity-package-private", "", "svg/square.svg")
    Icon("gps-entity-subprogram-private", "", "svg/circle.svg")
    Icon("gps-entity-type-private", "", "svg/triangle.svg")
    Icon("gps-entity-variable-private", "", "svg/dot.svg")
    Icon("gps-entity-generic-private", "", "svg/diamond.svg")
    Icon("gps-entity-package-private-spec", "", "svg/square_s.svg")
    Icon("gps-entity-subprogram-private-spec", "", "svg/circle_s.svg")
    Icon("gps-entity-type-private-spec", "", "svg/triangle_s.svg")
    Icon("gps-entity-variable-private-spec", "", "svg/dot_s.svg")
    Icon("gps-entity-generic-private-spec", "", "svg/diamond_s.svg")
    Icon("gps-entity-package-protected", "", "16px/entities/square_p.png")
    Icon("gps-entity-subprogram-protected", "", "16px/entities/circle_p.png")
    Icon("gps-entity-type-protected", "", "16px/entities/triangle_p.png")
    Icon("gps-entity-variable-protected", "", "16px/entities/dot_p.png")
    Icon("gps-entity-generic-protected", "", "16px/entities/diamond_p.png")
    Icon("gps-entity-package-protected-spec", "",
         "16px/entities/square_ps.png")
    Icon("gps-entity-subprogram-protected-spec", "",
         "16px/entities/circle_ps.png")
    Icon("gps-entity-type-protected-spec", "", "16px/entities/triangle_ps.png")
    Icon("gps-entity-variable-protected-spec", "", "16px/entities/dot_ps.png")
    Icon("gps-entity-generic-protected-spec", "",
         "16px/entities/diamond_ps.png")

    # <!-- Aliases icons -->,

    Icon("gps-alias", "", "svg/diamond_x_blue.svg")

    # <!-- VCS icons -->

    Icon("gps-vcs-added", "", "20px/vcs-added.png")
    Icon("gps-vcs-modified", "", "20px/vcs-modified.png")
    Icon("gps-vcs-needs-merge", "", "20px/vcs-needs-merge.png")
    Icon("gps-vcs-needs-update", "", "20px/vcs-needs-update.png")
    Icon("gps-vcs-not-registered", "", "20px/vcs-not-registered.png")
    Icon("gps-vcs-removed", "", "20px/vcs-removed.png")
    Icon("gps-vcs-unknown", "", "20px/vcs-unknown.png")
    Icon("gps-vcs-up-to-date", "", "20px/vcs-up-to-date.png")
    Icon("gps-vcs-has-conflicts", "", "20px/vcs-has-conflicts.png")

    # <!-- GPS remote icons -->

    Icon("gps-sync-to-local", "", "16px/sync_to_local.png")
    Icon("gps-sync-to-remote", "", "16px/sync_to_remote.png")

    # <!-- GPS window icon -->

    Icon("gps-icon-16", "", "16px/gps_16.png")
    Icon("gps-icon-32", "", "32px/gps_32.png")
    Icon("gps-icon-48", "", "48px/gps_48.png")

    # <!-- Code coverage icons -->

    Icon("gps-gcov-covered", "", "9px/executed_code.png",
         alt_menu="9px/executed_code.png")

    Icon("gps-gcov-non-covered", "", "9px/non_executed_code.png",
         alt_menu="9px/non_executed_code.png")

    # <!--  Builder icons -->

    Icon("gps-clean", "Clean", "svg/clean.svg",
         alt_local_toolbar="svg/clean-12.svg")
    Icon("gps-build-all", "Build all", "svg/buildall.svg")
    Icon("gps-compile", "Build main", "svg/compile.svg")
    Icon("gps-build", "Build main", "svg/build.svg")

    Icon("gps-build-main", "Build main", "svg/buildmain.svg")

    Icon("gps-compute-xref", "Compute Xref", "24px/compute_xref_24.png",
         alt_menu="16px/compute_xref_16.png",
         alt_small_toolbar="18px/compute_xref_18.png")

    Icon("gps-custom-build", "Custom build", "svg/buildcustom.svg")

    Icon("gps-semantic-check", "Semantic check", "svg/gnatcheck.svg")
    Icon("gps-gnatprove-path", "Show Path", "svg/gnatprove-path.svg")

    Icon("gps-metric", "Metrics", "svg/gnatmetric.svg")
    Icon("gps-test", "Test", "svg/gnattest.svg")

    Icon("gps-syntax-check", "Syntax check", "24px/syntax_check_24.png",
         alt_menu="16px/syntax_check_16.png",
         alt_small_toolbar="18px/syntax_check_18.png")

    Icon("gps-build-error", "", "9px/build_error.png",
         alt_menu="9px/build_error.png")

    Icon("gps-build-warning", "", "9px/build_warning.png",
         alt_menu="9px/build_warning.png")

    Icon("gps-build-style", "", "9px/build_style.png",
         alt_menu="9px/build_style.png")

    Icon("gps-build-error-fixable", "", "9px/build_error_fixable.png",
         alt_menu="9px/build_error_fixable.png")

    Icon("gps-build-warning-fixable", "", "9px/build_warning_fixable.png",
         alt_menu="9px/build_warning_fixable.png")

    Icon("gps-build-style-fixable", "", "9px/build_style_fixable.png",
         alt_menu="9px/build_style_fixable.png")

    Icon("gps-wrench", "", "9px/wrench.png",
         alt_menu="9px/wrench.png")

    Icon("gps-wrench-multi", "", "9px/wrench_multi.png",
         alt_menu="9px/wrench_multi.png")


class IconProvider(object):
    def __init__(self):
        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        self.current_theme = None
        self.__on_preferences_changed(None)

    def __on_preferences_changed(self, h):
        del h  # Unused parameter

        t = GPS.Preference("GPS6-Gtk-Theme-Name").get()
        if t != self.current_theme:
            self.current_theme = t
            add_icons()


IconProvider()
