# Copyright (c) 2000, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
proc paf_rc_symbol_browser {top menu_frame tool_frame} {
    destroy ${tool_frame}.make

    button ${tool_frame}.xterm -text xterm -command " exec xterm\
      -T {Source-Navigator} & "
    balloon_bind_info ${tool_frame}.xterm "XTERM"

    pack ${tool_frame}.xterm -side left

    set m ${menu_frame}.cust

    menubutton ${m} -menu ${m}.c -text {Extras} -underline 0

    pack ${m} -side left

    menu ${m}.c

    ${m}.c add command -label Functions -command " custom_show_syms Functions\
      " -underline 0

    ${m}.c add command -label Methods -command " custom_show_syms Methods "\
      -underline 0
}

# This function shows either the known functions or methods.
# If the window already exists, the contents of the listbox
# will be first deleted.

proc custom_show_syms {title} {

    switch -- ${title} {
        "Methods" {
                set scope "md"
            }
        default {
                set scope "fu"
            }
    }

    set w .custom_win_${scope}

    if {![winfo exists ${w}]} {
        toplevel ${w}

        wm title ${w} "[sn_read_option project-name]  ${title}"

        frame ${w}.l

        scrollbar ${w}.l.x -orient horizontal -relief sunken -command\
          " ${w}.l.l xview "

        scrollbar ${w}.l.y -relief sunken -command " ${w}.l.l yview "

        listbox ${w}.l.l -xscrollcommand "${w}.l.x set"\
          -yscrollcommand "${w}.l.y set" -width 40 -height 20

        bind ${w}.l.l <Double-1> {
				custom_edit_file [%W get [%W nearest %y]]
			}

        pack ${w}.l.y -side right -fill y
        pack ${w}.l.x -side bottom -fill x
        pack ${w}.l.l -fill both -expand y

        pack ${w}.l -fill both -expand y
    } else {
        ${w}.l.l delete 0 end
    }
    # Query the database only if that exists!
    if {[info commands paf_db_${scope}] != ""} {
        eval ${w}.l.l insert end [paf_db_${scope} seq -data]
    }
}

# The user has double clicked on a item, we have to start
# the editor.

proc custom_edit_file {sel} {
    set len [llength ${sel}]

    set pos [lindex ${sel} [expr ${len} - 2]]
    set filename [lindex ${sel} [expr ${len} - 1]]

    # Start the editor!
    tkeEditFile {} ${filename} ${pos}
}

