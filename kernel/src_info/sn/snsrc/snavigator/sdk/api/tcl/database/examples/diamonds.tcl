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
# This Tcl tool can be used to locate multiple inheritence diamonds in C++
# class hierarchies.
#
# Scott Myers refers to such diamonds as poor C++ programming practice.  A
# diamond is defined to be a graph relationship in which a base class derives
# two subclasses which are then multiply inherited by a further class.
#
###############################################################################

# Extract all the class names from the project database.

proc inhertable {path projname} {
    set db_functions [dbopen nav_func ${path}/${projname}.in RDONLY 0644 btree]
    return [${db_functions} seq -data]
}

# Locate diamonds in the inheritence graph and report them to stdout.

proc find_diamonds {} {
    global inheritence

    foreach baseclass [array names inheritence] {
        set numsubs [llength $inheritence(${baseclass})]
        set subclasses $inheritence(${baseclass})

        for {set i 0} {${i} < ${numsubs}} {incr i} {
            for {set j [expr ${i} + 1]} {${j} < ${numsubs}} {incr j} {

                set sub1 [lindex ${subclasses} ${i}]
                set sub2 [lindex ${subclasses} ${j}]

                # if sub1 and sub2 have a common subclass, then we have a
                # multiple inheritence diamond between baseclass, sub1, sub2
                # and derivedmost.

                if {![info exists inheritence(${sub1})] || ![info exists\
                  inheritence(${sub2})]} {
                    continue
                }

                foreach derivedmost $inheritence(${sub1}) {
                    foreach candidate $inheritence(${sub2}) {
                        if {[string compare ${derivedmost} ${candidate}] == 0} {
                            puts "Multiple inheritence diamond found between:\n"
                            puts "\t\t${baseclass}"
                            puts "${sub1}\t\t\t${sub2}"
                            puts "\t\t${derivedmost}\n"
                        }
                    }
                }
            }
        }
    }
}

wm withdraw .

if {${argc} != 2} {
    puts "Usage: ${argv0} projectdir project"
    exit
}

set projdir [lindex ${argv} 0]/.snprj

foreach entry [inhertable ${projdir} [lindex ${argv} 1]] {
    set derived [lindex ${entry} 0]
    set base [lindex ${entry} 1]
    lappend inheritence(${base}) ${derived}
}

find_diamonds

exit




