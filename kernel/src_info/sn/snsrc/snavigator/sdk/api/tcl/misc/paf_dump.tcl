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

set custom_sn_line_sep "\ngo"
# Sybase
set custom_sn_line_sep "\\g"
# Ingres
set custom_sn_line_sep ";"
# Oracle, SQLDB, ADABAS, MUMPS, MINISQL

proc paf_rc_symbol_browser {top menu exp} {
    menubutton ${menu}.sql -text SQL -menu ${menu}.sql.m -underline 0

    menu ${menu}.sql.m

    ${menu}.sql.m add command -label "Dump" -underline 0 -command\
      " custom_paf_get_id "

    ${menu}.sql.m add command -label "Start SN" -underline 0 -command " exec\
      /usr/local/bin/sn_start & "

    pack ${menu}.sql -side left -fill x
}

# This procedure asks for a project id.

proc custom_paf_get_id {} {
    if {[winfo exists .custom]} {
        return
    }
    toplevel .custom
    wm title .custom "Project: [sn_read_option project-name]"
    wm transient .custom

    frame .custom.pid
    label .custom.pid.l -text "Enter project id for $[sn_read_option\
      project-name]:"
    entry .custom.pid.e -relief sunken
    button .custom.pid.b -text "OK" -width 6 -command {
            set tmp_id [string trim [.custom.pid.e get]]
            set tmp_nm [sn_read_option project-name].tmp

            destroy .custom

            custom_paf_dump_symbols_to_sqlfile ${tmp_id} ${tmp_nm}
        }
    .custom.pid.e insert 0 1
    .custom.pid.e select from 0
    .custom.pid.e select to end
    bind .custom.pid.e <Return> {.custom.pid.b invoke}

    focus .custom.pid.e
    pack .custom.pid.l -side left
    pack .custom.pid.e -side left -fill y -padx 4
    pack .custom.pid.b -side left -fill y -padx 4
    pack .custom.pid -side top -pady 2
}

# This procedures dumps the symbol information into the
# $HOME/.sn/queries/paf_dump.sql batch file that can be
# executed through the Query->Execute Batch File menu entry.

proc custom_paf_dump_symbols_to_sqlfile {{id 1} {pname "PAF"}} {
    global tkbClassList tkbClassInherit tkbMbFuncDefnList
    global tkbMbVarDefnList tkbFuncDefnList tkbVarDefnList
    global tkbConsDefnList tkbEnumDefnList tkbProjectFiles
    global custom_sn_line_sep sn_options

    set term ${custom_sn_line_sep}

    catch {mkdir -p ~/.sn/queries}
    set file_name ~/.sn/queries/paf_dump.sql
    if {[catch {set fd [open ${file_name} "w"]}]} {
        return
    }
    fconfigure ${fd} -encoding $sn_options(def,system-encoding) -blocking 0

    #	Create the tables
    puts ${fd} "create table paf_projects (pid int not null, name char(100)\
      not null,host_name char(50) not null, dump_date char(50) not null)${term}"

    puts ${fd} "create table paf_files (pid int not null, file_id int not\
      null, name char(100) not null)${term}"

    puts ${fd} "create table paf_classes (pid int not null, class_id int not\
      null,name char(100) not null, file_id int not null,line_num int not\
      null)${term}"

    puts ${fd} "create table paf_inherits (pid int not null, class_id int not\
      null,mother_id int not null)${term}"

    puts ${fd} "create table paf_methods (pid int not null, name char(100) not\
      null,class_id int not null, file_id int not null,line_num int not\
      null)${term}"

    puts ${fd} "create table paf_cvars (pid int not null, name char(100) not\
      null,class_id int not null, file_id int not null,line_num int not\
      null)${term}"

    puts ${fd} "create table paf_procs (pid int not null, id int not null,name\
      char(100) not null, file_id int not null,line_num int not null)${term}"

    puts ${fd} "create table paf_typedefs (pid int not null, id int not\
      null,name char(100) not null, file_id int not null,line_num int not\
      null)${term}"

    puts ${fd} "create table paf_defines (pid int not null, id int not\
      null,name char(100) not null, file_id int not null,line_num int not\
      null)${term}"

    puts ${fd} "create table paf_enums (pid int not null, id int not null,name\
      char(100) not null, file_id int not null,line_num int not null)${term}"

    puts ${fd} "create table paf_cross (pid int not null, caller_id int not\
      null, calle_id int, calle_name char(100) not null, file_id int not\
      null,line_num int not null)${term}"

    puts ${fd} "commit${term}"

    # Delete the old entries of the project.
    puts ${fd} "delete from paf_projects where pid=${id}${term}"
    puts ${fd} "delete from paf_files where pid=${id}${term}"
    puts ${fd} "delete from paf_classes where pid=${id}${term}"
    puts ${fd} "delete from paf_inherits where pid=${id}${term}"
    puts ${fd} "delete from paf_procs where pid=${id}${term}"
    puts ${fd} "delete from paf_methods where pid=${id}${term}"
    puts ${fd} "delete from paf_cvars where pid=${id}${term}"
    puts ${fd} "delete from paf_typedefs where pid=${id}${term}"
    puts ${fd} "delete from paf_defines where pid=${id}${term}"
    puts ${fd} "delete from paf_enums where pid=${id}${term}"
    puts ${fd} "delete from paf_cross where pid=${id}${term}"

    puts ${fd} "commit${term}"

    puts ${fd} "insert into paf_projects (pid,name,host_name,dump_date) values\
      (${id},'${pname}','[info hostname]','[exec date]')${term}"

    set files [lsort ${tkbProjectFiles}]
    if {[catch {set classes [lsort [array names tkbClassList]]}]} {
        set classes ""
    }

    set cou 0
    foreach c ${files} {
        puts ${fd} "insert into paf_files (pid,file_id,name) values\
          (${id},${cou},'${c}')${term}"
        incr cou
        if {${cou} % 100 == 0} {
            puts ${fd} "commit${term}"
        }
    }

    puts ${fd} "commit${term}"

    set cou 0
    foreach c ${classes} {
        set pars [split ${c} ":"]
        set lin [lindex [split $tkbClassList(${c}) ":"] 1]
        set fid [lsearch -exact ${files} [lindex ${pars} 2]]
        set nm [lindex ${pars} 0]
        puts ${fd} "insert into paf_classes (pid,class_id,name,\
          file_id,line_num) values (${id},${cou}, '${nm}',${fid},${lin})${term}"
        # Inheritance
        if {[catch {set inh $tkbClassInherit(${nm})}] == 0} {
            foreach i ${inh} {
                set cid [lsearch -glob ${classes} "${i}::*"]
                puts ${fd} "insert into paf_inherits (pid,class_id,mother_id)\
                  values (${id},${cou},${cid})${term}"
            }
        }
        incr cou
        if {${cou} % 100 == 0} {
            puts ${fd} "commit${term}"
        }
    }
    # Methods
    custom_paf_dump_class_vars tkbMbFuncDefnList ${fd} ${id} paf_methods\
      ${classes} ${files} ${term}
    # Class variables
    custom_paf_dump_class_vars tkbMbVarDefnList ${fd} ${id} paf_cvars\
      ${classes} ${files} ${term}
    # Procedures
    custom_paf_dump_vars tkbFuncDefnList ${fd} ${id} paf_procs ${files} ${term}
    # Typedefs
    custom_paf_dump_vars tkbVarDefnList ${fd} ${id} paf_typedefs ${files}\
      ${term}
    # Defines
    custom_paf_dump_vars tkbConsDefnList ${fd} ${id} paf_defines ${files}\
      ${term}
    # Enums
    custom_paf_dump_vars tkbEnumDefnList ${fd} ${id} paf_enums ${files} ${term}

    custom_paf_dump_cross ${fd} ${id} "paf_cross" ${term}

    puts ${fd} "commit${term}"

    close ${fd}

    SQLError "The \"${file_name}\" SQL batch file is created with the symbol\
      table information. \nTo execute it you have to start SN and connect to a\
      database (Open)\nand with the \"Query->Execute Batch File\" menu the\
      batch file can be
		loaded into the database." "SQL Bacth File"
}

proc custom_paf_dump_vars {var fd id tbl files term} {
    upvar ${var} v
    if {[catch {set cont [lsort [array_names v]]}]} {
        return
    }
    puts ${fd} "commit${term}"

    set cou 0
    foreach c ${cont} {
        set pars [split ${c} ":"]
        set nm [lindex ${pars} 0]
        set pos [split $v(${c}) ":"]
        set lin [lindex ${pos} 1]
        set fid [lsearch -exact ${files} [lindex ${pos} 0]]

        puts ${fd} "insert into ${tbl} (pid,id,name, file_id,line_num) values\
          (${id},${cou},'${nm}', ${fid},${lin})${term}"
        incr cou
        if {${cou} % 100 == 0} {
            puts ${fd} "commit${term}"
        }
    }
}

proc custom_paf_dump_class_vars {var fd id tbl classes files term} {
    upvar ${var} v
    if {[catch {set cont [lsort [array_names v]]}]} {
        return
    }
    puts ${fd} "commit${term}"

    set cou 0
    foreach c ${cont} {
        set pars [split ${c} ":"]
        set nm [lindex ${pars} 0]
        set cnm [lindex [split [lindex ${pars} 2] "#"] 0]
        set cid [lsearch -glob ${classes} "${cnm}::*"]
        set pos [split $v(${c}) ":"]
        set lin [lindex ${pos} 1]
        set fid [lsearch -exact ${files} [lindex ${pos} 0]]

        puts ${fd} "insert into ${tbl} (pid,name,class_id, file_id,line_num)\
          values (${id},'${nm}',${cid}, ${fid},${lin})${term}"
        incr cou
        if {${cou} % 100 == 0} {
            puts ${fd} "commit${term}"
        }
    }
}

proc custom_paf_dump_cross {fd id tbl term} {
    global tkbCrossRef tkbFuncDefnList tkbProjectFiles

    if {[catch {set cross [lsort [array_names tkbCrossRef]]}]} {
        return
    }

    if {[catch {set func [lsort [array_names tkbFuncDefnList]]}]} {
        return
    }

    set files [lsort ${tkbProjectFiles}]
    puts ${fd} "commit${term}"

    set last_file ""
    set cou 0
    set file_id -1
    foreach c ${cross} {
        set caller_id -2
        foreach calls $tkbCrossRef(${c}) {
            set calle [lindex ${calls} 0]
            set pars [split [lindex ${calls} 1] ":"]
            set file [lindex ${pars} 0]
            if {[string compare ${file} ${last_file}] != 0} {
                set file_id [lsearch -exact ${files} ${file}]
                set last_file ${file}
            }
            if {${caller_id} == -2} {
                set caller_id [lsearch -exact ${func} "${c}::${file}"]
            }
            set line [lindex ${pars} 1]

            set cid [lsearch -exact ${func} "${calle}::${file}"]
            if {${cid} == -1} {
                set cid [lsearch -glob ${func} "${calle}::*"]
            }
            puts ${fd} "insert into ${tbl} (pid,caller_id,calle_id,\
              calle_name,file_id,line_num) values\
              (${id},${caller_id},${cid},'${calle}',${file_id},${line})${term}"
            incr cou
            if {${cou} % 100 == 0} {
                puts ${fd} "commit${term}"
            }
        }
    }
}

