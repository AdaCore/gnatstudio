"""
Defines the default icon theme for GPS.

Most icons in GPS can be edited by changing the contents of this script
(or creating another one similar to this one and loading it instead of
this one).
"""



import GPS
import os.path

icons = os.path.normpath(
    os.path.join(os.path.dirname(__file__), "../../icons"))

XML = r"""<?xml version="1.0"?>
<GPS>
  <stock>
    <!-- General-purpose icons -->

    <icon id="gtk-paste"              label="Paste" file="${icons}/svg/paste.svg" />
    <icon id="gtk-copy"               label="Copy" file="${icons}/svg/copy.svg" />
    <icon id="gtk-cut"                label="Cut" file="${icons}/svg/cut.svg" />
    <icon id="gps-navigate-forward"   label="Forward" file="${icons}/svg/right.svg" />
    <icon id="gps-navigate-back"      label="Back" file="${icons}/svg/left.svg" />
    <icon id="gtk-new"                label="New" file="${icons}/svg/document.svg" />
    <icon id="gtk-open"               label="Open" file="${icons}/svg/folder.svg" />
    <icon id="gtk-redo"               label="Redo" file="${icons}/svg/redo.svg" />
    <icon id="gtk-save"               label="Save" file="${icons}/svg/save.svg" />
    <icon id="gtk-undo"               label="Undo" file="${icons}/svg/undo.svg" />
    <icon id="gtk-preferences"        label="Preferences" file="${icons}/svg/settings.svg" />
    <icon id="gtk-close"              label="Close" file="${icons}/svg/close.svg" />
    <icon id="gtk-print"              label="Print" file="${icons}/svg/print.svg" />
    <icon id="gtk-clear"              label="Clean" file="${icons}/svg/clean.svg" />

    <!-- Debugger icons -->

    <icon id="gps-debugger-run"       label="Go"
                                      file="${icons}/24px/play_24.png" >
       <alternate                     file="${icons}/16px/play_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/play_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-step"      label="Step"
                                      file="${icons}/24px/step_24.png" >
       <alternate                     file="${icons}/16px/step_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/step_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-next"      label="Next"
                                      file="${icons}/24px/next_24.png" >
       <alternate                     file="${icons}/16px/next_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/next_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-finish"    label="Finish"
                                      file="${icons}/24px/finish_24.png" >
       <alternate                     file="${icons}/16px/finish_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/finish_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-up"        label="Up"
                                      file="${icons}/24px/stack_up_24.png" >
       <alternate                     file="${icons}/16px/stack_up_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/stack_up_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-debugger-down"      label="Down"
                                      file="${icons}/24px/stack_down_24.png" >
       <alternate                     file="${icons}/16px/stack_down_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/stack_down_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>


    <!-- Note that the icon file is specified twice. The first occurrence is
         a general-purpose one (this is the reference image which will be scaled
         at different sizes if needed, the other is the image to use as-is for
         the "menu" size (16x16).
         It's necessary to specify both, because the actual icon is 9x9 pixels,
         and we don't want gtk+ to scale it to 16x16. -->

    <icon id="gps-debugger-break"     file="${icons}/16px/debug_break.png">
       <alternate                     file="${icons}/16px/debug_break.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-current"   file="${icons}/16px/debug_indicator.png">
       <alternate                     file="${icons}/16px/debug_indicator.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-line-might-have-code" file="${icons}/9px/line_might_have_code.png">
       <alternate                                file="${icons}/9px/line_might_have_code.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-debugger-line-has-code" file="${icons}/9px/line_has_code.png">
       <alternate                                file="${icons}/9px/line_has_code.png" size="Icon_Size_Menu" />
    </icon>

    <!-- General-purpose icons -->

    <icon id="gps-file"              file="${icons}/16px/file.png" />
    <icon id="gps-file-modified"     file="${icons}/16px/file_pencil.png" />

    <icon id="gps-configMenu"        file="${icons}/svg/menu.svg"/>
    <icon id="gps-stopTask"          file="${icons}/svg/close.svg"/>
    <icon id="gps-expandAll"         file="${icons}/9px/expand_12.png"/>
    <icon id="gps-collapseAll"       file="${icons}/9px/collapse_12.png"/>
    <icon id="gps-clear-entry"       file="${icons}/16px/clear_entry_16.png"/>
    <icon id="gps-toggle-links"      file="${icons}/9px/toggle_links_12.png"/>
    <icon id="gps-remove-unselected" file="${icons}/9px/remove_unselected_12.png"/>
    <icon id="gps-read-only"         file="${icons}/svg/lock.svg"/>
    <icon id="gps-writable"          file="${icons}/svg/unlock.svg"/>
    <icon id="gps-regexp"            file="${icons}/16px/regexp.png"/>
    <icon id="gps-negate"            file="${icons}/16px/negate.png"/>

    <icon id="gps-folder-closed"     file="${icons}/16px/folder_closed.png" />
    <icon id="gps-folder-open"       file="${icons}/16px/folder_open.png" />

    <icon id="gps-folder-exec-closed" file="${icons}/16px/folder_e_closed.png" />
    <icon id="gps-folder-exec-open"   file="${icons}/16px/folder_e_open.png" />

    <icon id="gps-folder-obj-closed"  file="${icons}/16px/folder_o_closed.png" />
    <icon id="gps-folder-obj-open"    file="${icons}/16px/folder_o_open.png" />

    <icon id="gps-project-open"                file="${icons}/16px/project_box.png" />
    <icon id="gps-project-closed"              file="${icons}/16px/project_box.png" />
    <icon id="gps-project-modified-open"       file="${icons}/16px/project_box_pencil.png" />
    <icon id="gps-project-modified-closed"     file="${icons}/16px/project_box_pencil.png" />

    <icon id="gps-double-arrow" file="${icons}/svg/double_arrow.svg"/>

    <icon id="gps-box"                file="${icons}/16px/box.png" />

    <icon id="adacore-logo"           file="${icons}/other/adacore_logo.png">
       <alternate                     file="${icons}/other/adacore_logo.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-guide"           file="${icons}/other/button_guide.png">
       <alternate                     file="${icons}/other/button_guide.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-guide-over"      file="${icons}/other/button_guide_over.png">
       <alternate                     file="${icons}/other/button_guide_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-overview"        file="${icons}/other/button_overview.png">
       <alternate                     file="${icons}/other/button_overview.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-overview-over"   file="${icons}/other/button_overview_over.png">
       <alternate                     file="${icons}/other/button_overview_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-tutorial"        file="${icons}/other/button_tutorial.png">
       <alternate                     file="${icons}/other/button_tutorial.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="button-tutorial-over"   file="${icons}/other/button_tutorial_over.png">
       <alternate                     file="${icons}/other/button_tutorial_over.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="get-started"            file="${icons}/other/get_started.png">
       <alternate                     file="${icons}/other/get_started.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="welcome-header"         file="${icons}/other/welcome_header.png">
       <alternate                     file="${icons}/other/welcome_header.png" size="Icon_Size_Large_Toolbar" />
    </icon>

    <icon id="gps-warning"            file="${icons}/16px/warning.png">
       <alternate                     file="${icons}/16px/warning.png" size="Icon_Size_Menu" />
    </icon>

    <!-- Entities -->

    <icon id="gps-entity-package"         file="${icons}/16px/entities/square_x.png" />
    <icon id="gps-entity-subprogram"      file="${icons}/16px/entities/circle_x.png" />
    <icon id="gps-entity-type"            file="${icons}/16px/entities/triangle_x.png" />
    <icon id="gps-entity-variable"        file="${icons}/16px/entities/dot_x.png" />
    <icon id="gps-entity-generic"         file="${icons}/16px/entities/diamond_x.png" />

    <icon id="gps-entity-package-spec"         file="${icons}/16px/entities/square_xs.png" />
    <icon id="gps-entity-subprogram-spec"      file="${icons}/16px/entities/circle_xs.png" />
    <icon id="gps-entity-type-spec"            file="${icons}/16px/entities/triangle_xs.png" />
    <icon id="gps-entity-variable-spec"        file="${icons}/16px/entities/dot_xs.png" />
    <icon id="gps-entity-generic-spec"         file="${icons}/16px/entities/diamond_xs.png" />

    <icon id="gps-entity-package-private"         file="${icons}/16px/entities/square.png" />
    <icon id="gps-entity-subprogram-private"      file="${icons}/16px/entities/circle.png" />
    <icon id="gps-entity-type-private"            file="${icons}/16px/entities/triangle.png" />
    <icon id="gps-entity-variable-private"        file="${icons}/16px/entities/dot.png" />
    <icon id="gps-entity-generic-private"         file="${icons}/16px/entities/diamond.png" />

    <icon id="gps-entity-package-private-spec"         file="${icons}/16px/entities/square_s.png" />
    <icon id="gps-entity-subprogram-private-spec"      file="${icons}/16px/entities/circle_s.png" />
    <icon id="gps-entity-type-private-spec"            file="${icons}/16px/entities/triangle_s.png" />
    <icon id="gps-entity-variable-private-spec"        file="${icons}/16px/entities/dot_s.png" />
    <icon id="gps-entity-generic-private-spec"         file="${icons}/16px/entities/diamond_s.png" />

    <icon id="gps-entity-package-protected"         file="${icons}/16px/entities/square_p.png" />
    <icon id="gps-entity-subprogram-protected"      file="${icons}/16px/entities/circle_p.png" />
    <icon id="gps-entity-type-protected"            file="${icons}/16px/entities/triangle_p.png" />
    <icon id="gps-entity-variable-protected"        file="${icons}/16px/entities/dot_p.png" />
    <icon id="gps-entity-generic-protected"         file="${icons}/16px/entities/diamond_p.png" />

    <icon id="gps-entity-package-protected-spec"         file="${icons}/16px/entities/square_ps.png" />
    <icon id="gps-entity-subprogram-protected-spec"      file="${icons}/16px/entities/circle_ps.png" />
    <icon id="gps-entity-type-protected-spec"            file="${icons}/16px/entities/triangle_ps.png" />
    <icon id="gps-entity-variable-protected-spec"        file="${icons}/16px/entities/dot_ps.png" />
    <icon id="gps-entity-generic-protected-spec"         file="${icons}/16px/entities/diamond_ps.png" />

    <!-- Aliases icons -->

    <icon id="gps-alias"              file="${icons}/16px/entities/diamond_x_blue.png" />

    <!-- VCS icons -->

    <icon id="gps-vcs-added"          file="${icons}/20px/vcs-added.png" />
    <icon id="gps-vcs-modified"       file="${icons}/20px/vcs-modified.png" />
    <icon id="gps-vcs-needs-merge"    file="${icons}/20px/vcs-needs-merge.png" />
    <icon id="gps-vcs-needs-update"   file="${icons}/20px/vcs-needs-update.png" />
    <icon id="gps-vcs-not-registered" file="${icons}/20px/vcs-not-registered.png" />
    <icon id="gps-vcs-removed"        file="${icons}/20px/vcs-removed.png" />
    <icon id="gps-vcs-unknown"        file="${icons}/20px/vcs-unknown.png" />
    <icon id="gps-vcs-up-to-date"     file="${icons}/20px/vcs-up-to-date.png" />
    <icon id="gps-vcs-has-conflicts"  file="${icons}/20px/vcs-has-conflicts.png" />

    <!-- GPS remote icons -->

    <icon id="gps-sync-to-local"      file="${icons}/16px/sync_to_local.png" />
    <icon id="gps-sync-to-remote"     file="${icons}/16px/sync_to_remote.png" />

    <!-- GPS window icon -->

    <icon id="gps-icon-16"            file="${icons}/16px/gps_16.png" />
    <icon id="gps-icon-32"            file="${icons}/32px/gps_32.png" />
    <icon id="gps-icon-48"            file="${icons}/48px/gps_48.png" />

    <!-- Code coverage icons -->

    <icon id="gps-gcov-covered"       file="${icons}/9px/executed_code.png">
       <alternate                     file="${icons}/9px/executed_code.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-gcov-non-covered"   file="${icons}/9px/non_executed_code.png">
       <alternate                     file="${icons}/9px/non_executed_code.png" size="Icon_Size_Menu" />
    </icon>

    <!--  Builder icons -->

    <icon id="gps-clean"              label="Clean" file="${icons}/svg/clean.svg" />
    <icon id="gps-build-all"          label="Build all" file="${icons}/svg/buildall.svg" />
    <icon id="gps-compile"            label="Build main" file="${icons}/svg/build.svg" />
    <icon id="gps-build-main"         label="Build main" file="${icons}/svg/buildmain.svg" />

    <icon id="gps-compute-xref"       label="Compute Xref"
                                      file="${icons}/24px/compute_xref_24.png" >
       <alternate                     file="${icons}/16px/compute_xref_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/compute_xref_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-custom-build"       label="Custom build"
                                      file="${icons}/24px/custom_build_24.png" >
       <alternate                     file="${icons}/16px/custom_build_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/custom_build_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-semantic-check"       label="Semantic check"
                                      file="${icons}/24px/semantic_check_24.png" >
       <alternate                     file="${icons}/16px/semantic_check_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/semantic_check_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-syntax-check"       label="Syntax check"
                                      file="${icons}/24px/syntax_check_24.png" >
       <alternate                     file="${icons}/16px/syntax_check_16.png" size="Icon_Size_Menu" />
       <alternate                     file="${icons}/18px/syntax_check_18.png" size="Icon_Size_Small_Toolbar" />
    </icon>

    <icon id="gps-build-error"       file="${icons}/9px/build_error.png">
       <alternate                    file="${icons}/9px/build_error.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-warning"     file="${icons}/9px/build_warning.png">
       <alternate                    file="${icons}/9px/build_warning.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-style"       file="${icons}/9px/build_style.png">
       <alternate                    file="${icons}/9px/build_style.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-error-fixable"   file="${icons}/9px/build_error_fixable.png">
       <alternate                        file="${icons}/9px/build_error_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-warning-fixable" file="${icons}/9px/build_warning_fixable.png">
       <alternate                        file="${icons}/9px/build_warning_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-build-style-fixable"   file="${icons}/9px/build_style_fixable.png">
       <alternate                        file="${icons}/9px/build_style_fixable.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-wrench"    file="${icons}/9px/wrench.png">
       <alternate            file="${icons}/9px/wrench.png" size="Icon_Size_Menu" />
    </icon>

    <icon id="gps-wrench-multi" file="${icons}/9px/wrench_multi.png">
       <alternate               file="${icons}/9px/wrench_multi.png" size="Icon_Size_Menu" />
    </icon>
</stock>
</GPS>
"""

XML = XML.replace("${icons}", icons)

GPS.parse_xml(XML)
