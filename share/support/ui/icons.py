"""
Defines the default icon theme for GPS.

Most icons in GPS can be edited by changing the contents of this script
(or creating another one similar to this one and loading it instead of
this one).
"""



import GPS
import os.path

XML = r"""<?xml version="1.0"?>
<GPS>
  <stock>
    <!-- General-purpose icons -->

    <icon id="gtk-paste"              label="Paste" file="svg/paste.svg" />
    <icon id="gtk-copy"               label="Copy" file="svg/copy.svg" />
    <icon id="gtk-cut"                label="Cut" file="svg/cut.svg" />
    <icon id="gps-navigate-forward"   label="Forward" file="svg/right.svg" />
    <icon id="gps-navigate-back"      label="Back" file="svg/left.svg" />
    <icon id="gtk-new"                label="New" file="svg/newdocument.svg" />
    <icon id="gtk-open"               label="Open" file="svg/folder.svg">
       <alternate   file="svg/folder-12.svg" size="Icon_Size_Local_Toolbar" />
    </icon>
    <icon id="gtk-redo"               label="Redo" file="svg/redo.svg" />
    <icon id="gtk-save"               label="Save" file="svg/save.svg">
       <alternate   file="svg/save-12.svg" size="Icon_Size_Local_Toolbar" />
    </icon>
    <icon id="gtk-undo"               label="Undo" file="svg/undo.svg" />
    <icon id="gtk-preferences"        label="Preferences" file="svg/settings.svg" />
    <icon id="gtk-close"              label="Close" file="svg/close.svg" />
    <icon id="gps-clear-entry"        label="Clear Entry" file="svg/clear_entry.svg" />
    <icon id="gtk-print"              label="Print" file="svg/print.svg" />
    <icon id="gtk-clear"              label="Clean" file="svg/clean.svg">
       <alternate   file="svg/clean-12.svg" size="Icon_Size_Local_Toolbar" />
    </icon>
    <icon id="gtk-media-play"         file="svg/run.svg" />
    <icon id="gtk-remove"             label="Remove" file="svg/minus.svg" />
    <icon id="gtk-add"                label="Add" file="svg/plus.svg" />

    <icon id="gps-edit"               label="Edit" file="svg/edit.svg" />
    <icon id="gps-environment"        label="Environment" file="svg/environment.svg" />
    <icon id="gps-messages"           label="Messages" file="svg/messages.svg" />

    <icon id="gps-power"              label="Power" file="svg/power.svg" />

    <icon id="gps-error"              label="Error" file="svg/error.svg" />
    <icon id="gps-warning"            label="Warning" file="svg/warning.svg" />
    <icon id="gps-info"               label="Info" file="svg/info.svg" />

    <icon id="gps-project"            label="Project" file="svg/project.svg" />
    <icon id="gps-refresh"            label="Refresh" file="svg/refresh.svg" />
    <icon id="gps-search"             label="Search" file="svg/search.svg" />

    <icon id="gps-zip"                label="Zip" file="svg/zip.svg" />
    <icon id="gps-zoom"               label="Zoom" file="svg/zoom.svg" />
    <icon id="gps-zoomout"            label="Zoom out" file="svg/zoomout.svg" />

    <!-- Debugger icons -->

    <icon id="gps-debugger-run"       label="Go"
                                      file="24px/play_24.png" >
       <alternate                     file="16px/play_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/play_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-step"      label="Step"
                                      file="24px/step_24.png" >
       <alternate                     file="16px/step_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/step_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-next"      label="Next"
                                      file="24px/next_24.png" >
       <alternate                     file="16px/next_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/next_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-finish"    label="Finish"
                                      file="24px/finish_24.png" >
       <alternate                     file="16px/finish_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/finish_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-up"        label="Up"
                                      file="24px/stack_up_24.png" >
       <alternate                     file="16px/stack_up_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/stack_up_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-down"      label="Down"
                                      file="24px/stack_down_24.png" >
       <alternate                     file="16px/stack_down_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/stack_down_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>


    <!-- Note that the icon file is specified twice. The first occurrence is
         a general-purpose one (this is the reference image which will be scaled
         at different sizes if needed, the other is the image to use as-is for
         the "menu" size (16x16).
         It's necessary to specify both, because the actual icon is 9x9 pixels,
         and we don't want gtk+ to scale it to 16x16. -->

    <icon id="gps-debugger-break"     file="16px/debug_break.png">
       <alternate                     file="16px/debug_break.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-current"   file="16px/debug_indicator.png">
       <alternate                     file="16px/debug_indicator.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-line-might-have-code" file="9px/line_might_have_code.png">
       <alternate                                file="9px/line_might_have_code.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-line-has-code" file="9px/line_has_code.png">
       <alternate                                file="9px/line_has_code.png" size="Icon_Size_Menu" />
    </icon>

    <!-- General-purpose icons -->

    <icon id="gps-file"              file="16px/file.png" />
    <icon id="gps-file-modified"     file="16px/file_pencil.png" />

    <icon id="gps-configMenu"        file="svg/menu.svg">
       <alternate   file="svg/menu-12.svg" size="Icon_Size_Local_Toolbar" />
    </icon>

    <icon id="gps-stopTask"          file="svg/close.svg"/>
    <icon id="gps-expandAll"         file="9px/expand_12.png"/>
    <icon id="gps-collapseAll"       file="9px/collapse_12.png"/>
    <icon id="gps-toggle-links"      file="9px/toggle_links_12.png"/>
    <icon id="gps-remove-unselected" file="9px/remove_unselected_12.png"/>
    <icon id="gps-read-only"         file="svg/lock-12.svg"/>
    <icon id="gps-writable"          file="svg/unlock-12.svg"/>
    <icon id="gps-regexp"            file="16px/regexp.png"/>
    <icon id="gps-negate"            file="16px/negate.png"/>

    <icon id="gps-folder-closed"     file="16px/folder_closed.png" />
    <icon id="gps-folder-open"       file="16px/folder_open.png" />

    <icon id="gps-folder-exec-closed" file="16px/folder_e_closed.png" />
    <icon id="gps-folder-exec-open"   file="16px/folder_e_open.png" />

    <icon id="gps-folder-obj-closed"  file="16px/folder_o_closed.png" />
    <icon id="gps-folder-obj-open"    file="16px/folder_o_open.png" />

    <icon id="gps-project-open"                file="16px/project_box.png" />
    <icon id="gps-project-closed"              file="16px/project_box.png" />
    <icon id="gps-project-modified-open"       file="16px/project_box_pencil.png" />
    <icon id="gps-project-modified-closed"     file="16px/project_box_pencil.png" />

    <icon id="gps-search-and-menu" file="svg/search-and-menu.svg" />

    <icon id="gps-double-arrow" file="svg/double_arrow.svg"/>

    <icon id="gps-box"                file="16px/box.png" />

    <icon id="adacore-logo"           file="other/adacore_logo.png">
       <alternate                     file="other/adacore_logo.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-guide"           file="other/button_guide.png">
       <alternate                     file="other/button_guide.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-guide-over"      file="other/button_guide_over.png">
       <alternate                     file="other/button_guide_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-overview"        file="other/button_overview.png">
       <alternate                     file="other/button_overview.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-overview-over"   file="other/button_overview_over.png">
       <alternate                     file="other/button_overview_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-tutorial"        file="other/button_tutorial.png">
       <alternate                     file="other/button_tutorial.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-tutorial-over"   file="other/button_tutorial_over.png">
       <alternate                     file="other/button_tutorial_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="get-started"            file="other/get_started.png">
       <alternate                     file="other/get_started.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="welcome-header"         file="other/welcome_header.png">
       <alternate                     file="other/welcome_header.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="gps-warning"            file="16px/warning.png">
       <alternate                     file="16px/warning.png" size="Icon_Size_Menu" />
    </icon>

    <!-- Entities -->

    <icon id="gps-entity-package"         file="svg/square_x.svg" />
    <icon id="gps-entity-subprogram"      file="svg/circle_x.svg" />
    <icon id="gps-entity-type"            file="svg/triangle_x.svg" />
    <icon id="gps-entity-variable"        file="svg/dot_x.svg" />
    <icon id="gps-entity-generic"         file="svg/diamond_x.svg" />

    <icon id="gps-entity-package-spec"         file="svg/square_xs.svg" />
    <icon id="gps-entity-subprogram-spec"      file="svg/circle_xs.svg" />
    <icon id="gps-entity-type-spec"            file="svg/triangle_xs.svg" />
    <icon id="gps-entity-variable-spec"        file="svg/dot_xs.svg" />
    <icon id="gps-entity-generic-spec"         file="svg/diamond_xs.svg" />

    <icon id="gps-entity-package-private"         file="svg/square.svg" />
    <icon id="gps-entity-subprogram-private"      file="svg/circle.svg" />
    <icon id="gps-entity-type-private"            file="svg/triangle.svg" />
    <icon id="gps-entity-variable-private"        file="svg/dot.svg" />
    <icon id="gps-entity-generic-private"         file="svg/diamond.svg" />

    <icon id="gps-entity-package-private-spec"         file="svg/square_s.svg" />
    <icon id="gps-entity-subprogram-private-spec"      file="svg/circle_s.svg" />
    <icon id="gps-entity-type-private-spec"            file="svg/triangle_s.svg" />
    <icon id="gps-entity-variable-private-spec"        file="svg/dot_s.svg" />
    <icon id="gps-entity-generic-private-spec"         file="svg/diamond_s.svg" />

    <icon id="gps-entity-package-protected"         file="16px/entities/square_p.png" />
    <icon id="gps-entity-subprogram-protected"      file="16px/entities/circle_p.png" />
    <icon id="gps-entity-type-protected"            file="16px/entities/triangle_p.png" />
    <icon id="gps-entity-variable-protected"        file="16px/entities/dot_p.png" />
    <icon id="gps-entity-generic-protected"         file="16px/entities/diamond_p.png" />

    <icon id="gps-entity-package-protected-spec"         file="16px/entities/square_ps.png" />
    <icon id="gps-entity-subprogram-protected-spec"      file="16px/entities/circle_ps.png" />
    <icon id="gps-entity-type-protected-spec"            file="16px/entities/triangle_ps.png" />
    <icon id="gps-entity-variable-protected-spec"        file="16px/entities/dot_ps.png" />
    <icon id="gps-entity-generic-protected-spec"         file="16px/entities/diamond_ps.png" />

    <!-- Aliases icons -->

    <icon id="gps-alias"              file="svg/diamond_x_blue.svg" />

    <!-- VCS icons -->

    <icon id="gps-vcs-added"          file="20px/vcs-added.png" />
    <icon id="gps-vcs-modified"       file="20px/vcs-modified.png" />
    <icon id="gps-vcs-needs-merge"    file="20px/vcs-needs-merge.png" />
    <icon id="gps-vcs-needs-update"   file="20px/vcs-needs-update.png" />
    <icon id="gps-vcs-not-registered" file="20px/vcs-not-registered.png" />
    <icon id="gps-vcs-removed"        file="20px/vcs-removed.png" />
    <icon id="gps-vcs-unknown"        file="20px/vcs-unknown.png" />
    <icon id="gps-vcs-up-to-date"     file="20px/vcs-up-to-date.png" />
    <icon id="gps-vcs-has-conflicts"  file="20px/vcs-has-conflicts.png" />

    <!-- GPS remote icons -->

    <icon id="gps-sync-to-local"      file="16px/sync_to_local.png" />
    <icon id="gps-sync-to-remote"     file="16px/sync_to_remote.png" />

    <!-- GPS window icon -->

    <icon id="gps-icon-16"            file="16px/gps_16.png" />
    <icon id="gps-icon-32"            file="32px/gps_32.png" />
    <icon id="gps-icon-48"            file="48px/gps_48.png" />

    <!-- Code coverage icons -->

    <icon id="gps-gcov-covered"       file="9px/executed_code.png">
       <alternate                     file="9px/executed_code.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-gcov-non-covered"   file="9px/non_executed_code.png">
       <alternate                     file="9px/non_executed_code.png" size="Icon_Size_Menu" />
    </icon>

    <!--  Builder icons -->

    <icon id="gps-clean"              label="Clean" file="svg/clean.svg">
       <alternate   file="svg/clean-12.svg" size="Icon_Size_Local_Toolbar" />
    </icon>
    <icon id="gps-build-all"          label="Build all" file="svg/buildall.svg" />
    <icon id="gps-compile"            label="Build main" file="svg/compile.svg" />
    <icon id="gps-build"            label="Build main" file="svg/build.svg" />

    <icon id="gps-build-main"         label="Build main" file="svg/buildmain.svg" />

    <icon id="gps-compute-xref"       label="Compute Xref"
                                      file="24px/compute_xref_24.png" >
       <alternate                     file="16px/compute_xref_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/compute_xref_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-custom-build"       label="Custom build" file="svg/buildcustom.svg" />

    <icon id="gps-semantic-check"       label="Semantic check"  file="svg/gnatcheck.svg" />
    <icon id="gps-gnatprove-path"       label="Show Path" file="svg/gnatprove-path.svg" />

    <icon id="gps-metric"             label="Metrics"  file="svg/gnatmetric.svg" />
    <icon id="gps-test"               label="Test"  file="svg/gnattest.svg" />

    <icon id="gps-syntax-check"       label="Syntax check"
                                      file="24px/syntax_check_24.png" >
       <alternate                     file="16px/syntax_check_16.png" size="Icon_Size_Menu" />
       <alternate                     file="18px/syntax_check_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-build-error"       file="9px/build_error.png">
       <alternate                    file="9px/build_error.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-warning"     file="9px/build_warning.png">
       <alternate                    file="9px/build_warning.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-style"       file="9px/build_style.png">
       <alternate                    file="9px/build_style.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-error-fixable"   file="9px/build_error_fixable.png">
       <alternate                        file="9px/build_error_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-warning-fixable" file="9px/build_warning_fixable.png">
       <alternate                        file="9px/build_warning_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-style-fixable"   file="9px/build_style_fixable.png">
       <alternate                        file="9px/build_style_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-wrench"    file="9px/wrench.png">
       <alternate            file="9px/wrench.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-wrench-multi" file="9px/wrench_multi.png">
       <alternate               file="9px/wrench_multi.png" size="Icon_Size_Menu" />
    </icon>
</stock>
</GPS>
"""


class Icon_Provider(object):

    def __init__(self):
        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        self.current_theme = None
        self.__on_preferences_changed(None)

    def __on_preferences_changed(self, h):
        t = GPS.Preference("GPS6-Gtk-Theme-Name").get()
        if t != self.current_theme:
            self.current_theme = t
            GPS.parse_xml(XML)


Icon_Provider()
