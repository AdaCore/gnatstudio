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
    set info ${top}.status.msg

    #	destroy $tool_frame.make

    button ${tool_frame}.netscape -text Netscape -command " exec netscape & "

    bind ${tool_frame}.netscape <Motion> "${info} config -text \"Start\
      Netscape browser\""
    bind ${tool_frame}.netscape <Leave> "${info} config -text \"\""

    pack ${tool_frame}.netscape -side left
}

proc paf_edit_rc {top menu_frame tool_frame text} {
    set info ${top}.msg.help

    #	destroy $tool_frame.make;	# Delete a button!


    # Create a new button to start netscape
    button ${tool_frame}.netscape -text Netscape -command " exec netscape\
      \[tkeGetFileName ${text}\] & "

    bind ${tool_frame}.netscape <Motion> "${info} config -text \"Start\
      Netscape browser\""
    bind ${tool_frame}.netscape <Leave> "${info} config -text \"\""

    pack ${tool_frame}.netscape -side left

    # Create a new button to start appletviewer
    button ${tool_frame}.av -text ApV -command " exec appletviewer\
      \[tkeGetFileName ${text}\] & "

    bind ${tool_frame}.av <Motion> "${info} config -text \"Start Appletviewer\
      (HTML only)\""
    bind ${tool_frame}.av <Leave> "${info} config -text \"\""

    pack ${tool_frame}.av -side left


    # Create a new button to start the Java Interpreter
    button ${tool_frame}.java -text Java -command " custom_java ${text} "

    bind ${tool_frame}.java <Motion> "${info} config -text \"Start Java\
      Interpreter\""
    bind ${tool_frame}.java <Leave> "${info} config -text \"\""

    pack ${tool_frame}.java -side left

    # Change the command assigned to a button!
    set file [tkeGetFileName ${text}]

    if {[string match {*.jav*} ${file}]} {
        bind ${tool_frame}.compile <Motion> "${info} config -text \"Java\
          Compile\""
        ${tool_frame}.compile config -command " custom_compile ${text} "
    }
}

proc custom_compile {w} {
    global PafEditMakeCmdStr

    set save_PafEditMakeCmdStr ${PafEditMakeCmdStr}
    # Save the command!

    set file [tkeGetFileName ${w}]

    if {[string match {*.jav*} ${file}]} {
        set PafEditMakeCmdStr "javac %F"
    }

    PafMakeProject ${w}

    set PafEditMakeCmdStr ${save_PafEditMakeCmdStr}
    # Restore the command!
}

proc custom_java {w} {
    set file [tkeGetFileName ${w}]
    set dir [file dirname ${file}]
    set bn [file tail ${file}]
    set bn [file rootname ${bn}]
    exec xterm -T "Java ${bn}" -e paf_java ${dir} ${bn} &
}

