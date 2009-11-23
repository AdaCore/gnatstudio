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
# This Tcl tool is used to determine where unused variables exist in a C or
# C++ project.
#
###############################################################################

# Take a class and method name and turn it into a fully-qualified C++ name.

proc qualify {class method} {
    if {"${class}" == "\#"} {
        return ${method}
    }
    # C++ style naming
    return "${class}::${method}"
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

wm withdraw .

if {${argc} != 2} {
    puts "Usage: ${argv0} projectdir project"
    exit
}

set projdir [lindex ${argv} 0]/.snprj

foreach entry [torefs ${projdir} [lindex ${argv} 1]] {
    set subentry [lindex ${entry} 0]
    if {[lindex ${subentry} 6] == "u"} {
        puts -nonewline "[objtype [lindex ${subentry} 2]] "
        puts -nonewline "[qualify [lindex ${subentry} 0] [lindex ${subentry}\
          1]] "
        puts -nonewline "does not use [objtype [lindex ${subentry} 5]] "
        puts -nonewline "[qualify [lindex ${subentry} 3] [lindex ${subentry}\
          4]] "
        puts "in [lindex ${subentry} 8]:[lindex ${subentry} 7]"
    }
}

exit

