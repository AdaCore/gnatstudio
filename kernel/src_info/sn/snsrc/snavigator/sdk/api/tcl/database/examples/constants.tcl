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
# the next line restarts using wish \
exec hyper "$0" "$@"

###############################################################################
#
# Copyright (C) 1997 Cygnus Solutions, Inc.
#
# Description:
# This Tcl tool identifies global variables in projects which are only
# accessed as read-only objects.  These variables are therefore possible
# candidates for becoming constants.
#
###############################################################################

# Build a list of all the global variables in the project that are never
# modified and then output the list to stdout.

proc globals {path projname} {
    set db_functions [dbopen nav_func ${path}/${projname}.to RDONLY 0644 btree]
    foreach entry [${db_functions} seq] {
        set subentry [lindex ${entry} 0]
        set class [lindex ${subentry} 3]
        set name [lindex ${subentry} 4]
        set type [lindex ${subentry} 5]
        set access [lindex ${subentry} 6]

        if {[string compare ${class} "\#"] == 0 && [string compare ${type}\
          "gv"] == 0} {
            if {[string compare ${access} "r"] != 0} {
                set vars(${name}) 0
            } else {
                if {![info exists vars(${name})]} {
                    set vars(${name}) 1
                }
            }
        }
    }

    foreach variable [array names vars] {
        if {$vars(${variable})} {
            puts "${variable} could possibly become constant"
        }
    }
}

wm withdraw .

if {${argc} != 2} {
    puts "Usage: ${argv0} projectdir project"
    exit
}

set projdir [lindex ${argv} 0]/.snprj
set projname [lindex ${argv} 1]

globals ${projdir} ${projname}

exit

