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
# This Tcl tool very simply reports on redundant #includes in C/C++ programs.
#
# Usage: multicludes.tcl projectdir project ?-transitive?
#
###############################################################################

proc includetable {path projname} {
    set db_functions [dbopen nav_func ${path}/${projname}.iu RDONLY 0644 btree]
    return [${db_functions} seq]
}

proc subincludes {key file} {
    global includes
    if {[info exists includes(${file})]} {
        foreach header [split $includes(${file})] {
            if {[info exists includes(${header})]} {
                subincludes ${key} ${header}
            } else {
                # leaf header file that includes nothing more
                if {[string first "${header}\t" $includes(${key})] == 0 ||\
                  [string first "\t${header}\t" $includes(${key})] >= 0} {
                    puts "Warning: ${key} unnecessarily includes ${header}"
                }
            }
        }
    }
}

proc transitives {} {
    global includes
    foreach key [array names includes] {
        foreach file [split $includes(${key})] {
            subincludes ${key} ${file}
        }
    }
}

if {${argc} < 2} {
    puts "Usage: ${argv0} projectdir project ?-transitive?"
    exit
}

set table [includetable [lindex ${argv} 0]/.snprj [lindex ${argv} 1]]

wm withdraw .

foreach entry ${table} {
    set filename [lindex [lindex ${entry} 0] 2]
    set header [lindex [lindex ${entry} 0] 0]

    if {![info exists includes(${filename})]} {
        set includes(${filename}) ${header}\t
        continue
    }

    if {[string first "${header}\t" $includes(${filename})] < 0 ||\
      [string first "\t${header}\t" $includes(${filename})] < 0} {
        append includes(${filename}) "${header}\t"
    } else {
        if {![info exists dupe_reported_for(${filename})]} {
            set dupe_reported_for(${filename}) yes
            puts stderr "warning: ${filename} multiply includes ${header}"
        }
    }
}

if {${argc} > 2 && [string compare [lindex ${argv} 2] "-transitive"] == 0} {
    transitives
}

exit

