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
#!/bin/sh
# REPLACE $HOME/snavigator/bin/hyper \
# WITH THE Source-Navigator INSTALLATION DIRECTORY !!! \
exec $HOME/snavigator/bin/hyper "$0" "$@"

# file: example3.tcl
# Usage example3.tcl file1 file2 ....
# This program tells Source-Navigator to reload files
# specified in the command line. The target project name
# must contain the string "c++_demo".
# The c++_demo.proj called project is optionally 
# generated during the PathFinder+ installation. Upon it is
# generated you can start Source-Navigator with the
# "c++_demo.proj" project as below:
#
# cd /install_dir
# cd demos/c++_demo
# snavigator c++_demo.proj

wm withdraw .

foreach intp [winfo interps] {
    if {![string match "*navigato*" ${intp}]} {
        continue
    }
    set msg ""
    set pars ""
    set cmd {set t_m_p [sn_read_option project-name]}
    catch {set pars [send ${intp} ${cmd}]} msg
    if {${msg} != ""} {
        puts stdout ${msg}
    }
    if {[string match "*c++_demo*" ${pars}]} {
        send ${intp} paf_parse_uptodate [list ${argv}]
    }
}
exit

