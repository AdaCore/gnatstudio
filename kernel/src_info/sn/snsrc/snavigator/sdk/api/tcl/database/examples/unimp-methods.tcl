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
# This Tcl tool locates class method definitions that are surplus to a project
# (ie. for which there is no method implementation).  This tool is not always
# accurate in the sense that it may report inline methods.
#
###############################################################################

wm withdraw .

if {${argc} != 2} {
    puts "Usage: ${argv0} projectdir project"
    exit
}

set projdir [lindex ${argv} 0]/.snprj
set projname [lindex ${argv} 1]

proc mangle {returntype class name args} {
    set mangled "${returntype}.${class}.${name}"
    foreach arg ${args} {
        append mangled ".${arg}"
    }
    return ${mangled}
}

proc methods {path projname table} {
    set db_functions [dbopen nav_func ${path}/${projname}.${table} RDONLY 0644\
      btree]
    set table {}

    foreach entry [${db_functions} seq] {
        set key [lindex ${entry} 0]
        set data [lindex ${entry} 1]

        set class [lindex ${key} 0]
        set name [lindex ${key} 1]
        set rettype [lindex ${data} 2]
        set args [lindex ${data} 3]

        lappend table [list [mangle ${class} ${name} ${rettype} ${args}]]
    }
    return ${table}
}

proc build_md {path projname} {
    global md_table

    foreach method [methods ${path} ${projname} md] {
        if {![info exists md_table(${method})]} {
            set md_table(${method}) 1
        }
    }
}

proc build_mi {path projname} {
    global mi_table

    foreach method [methods ${path} ${projname} mi] {
        if {![info exists mi_table(${method})]} {
            set mi_table(${method}) 1
        }
    }
}

proc build_tables {projdir projname} {
    foreach table {md mi} {
        build_${table} ${projdir} ${projname}
    }
}

proc expand {method} {
    set components [split ${method} "."]
    set class [lindex ${components} 0]
    set name [lindex ${components} 1]
    set rettype [lindex ${components} 2]
    set args [lrange ${components} 3 end]

    set result "${rettype} "

    if {[string compare ${class} "\\#"] != 0} {
        append result "${class}::"
    }
    append result "${name}("
    foreach arg ${args} {
        append result ${arg}
    }
    append result ")"

    return ${result}
}

build_tables ${projdir} ${projname}

foreach method [array names md_table] {
    if {![info exists mi_table(${method})]} {
        puts "method may not be implemented: [expand ${method}]"
    }
}

foreach method [array names mi_table] {
    if {![info exists md_table(${method})]} {
        puts "method may not be implemented: [expand ${method}]"
    }
}

exit

