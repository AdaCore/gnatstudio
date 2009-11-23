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
# clobber.tcl
#
# Copyright (C) 1997 Cygnus Solutions, Inc.
#
# Description:
# This Tcl tool is used to show all the points of a program that modifies
# a global variable.
#
###############################################################################

# Take a class and method name and turn it into a fully-qualified C++ name.

proc qualify {class method} {
    if {[string compare "${class}" "\#"] == 0} {
        return ${method}
    }
    # C++ style naming
    return "${class}::${method}"
}

# Take a fully-qualified C++ name and break it into its component parts.

proc disqualify {name} {
    if {[regexp ".*::.*" ${name}] == 0} {
        return ${name}
    }
    set tokens [split ${name} ":"]
    return [concat [lindex ${tokens} 0] [lindex ${tokens} 2]]
}

# Take a simple abbreviation stored in the project database and turn it into
# a plain English description.

proc objtype {abbrev} {
    switch ${abbrev} {
        gv {
                return "global variable"
            }
        mi {
                return "method implementation"
            }
        fu {
                return "function"
            }
        iv {
                return "instance variable"
            }
        default {
                return "unknown object"
            }
    }
}

# Extract all the refers-to references from the project database.

proc torefs {path projname} {
    set db_functions [dbopen nav_func ${path}/${projname}.to RDONLY 0644 btree]
    return [${db_functions} seq]
}

if {${argc} != 3} {
    puts "Usage: ${argv0} projectdir project variable-name"
    exit
}

set projdir [lindex ${argv} 0]/.snprj
set var [lindex ${argv} 2]

wm withdraw .

foreach entry [torefs ${projdir} [lindex ${argv} 1]] {
    set subentry [lindex ${entry} 0]

    if {[llength [disqualify ${var}]] > 1} {
        set classname [lindex [disqualify ${var}] 0]
        set methodname [lindex [disqualify ${var}] 1]
    } else {
        set classname "\#"
        set methodname [disqualify ${var}]
    }

    if {"[lindex ${subentry} 3]" == "${classname}" && "[lindex ${subentry} 4]"\
      == "${methodname}" && "[lindex ${subentry} 6]" == "w"} {
        puts -nonewline "[objtype [lindex ${subentry} 2]] "
        puts -nonewline "[qualify [lindex ${subentry} 0] [lindex ${subentry}\
          1]] "
        puts -nonewline "modifies [objtype [lindex ${subentry} 5]] ${var} in "
        puts "[lindex ${subentry} 8]:[lindex ${subentry} 7]"
    }
}

exit

