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

proc paf_rc {} {
    # Create a port that everybody can connect to!
    set port [CreateAccessHandler AcceptApplication]
    puts stdout "Listening to port: ${port}"
}

proc ReadFromApplication {fd} {
    if {[gets ${fd} cmd] == -1} {
        puts stdout "Application exited"
        close ${fd}
        # Close the pipe!
    }
    puts stdout executing:${cmd}

# FIXME: THIS IS SO BAD, WHY DO WE EVEN HAVE THIS CODE IN OUR TREE ??????
    eval ${cmd}
    # Execute the received tcl command!
}

# Accept the conection request!
proc AcceptApplication {fd ip port} {
    #	puts stdout fd:$fd,ip:$ip,port:$port;flush stdout

    fconfigure ${fd} -buffering line -blocking 0
    fileevent ${fd} readable "ReadFromApplication ${fd}"
}

# This function tries to find a free TCP/IP port. When that
# is found a socket will be generated to get the port number
# and the socket will be closed.
# With the new port number, a "server" socket will be created
# thus clients can connet the this socket.
proc CreateAccessHandler {accept_script} {
    # Try to connet to the "time" service-port to get a free port number!
    if {[catch {set fd [socket [info hostname -full] 37]}]} {
        set port 12600
    } else {
        set port [lindex [fconfigure ${fd} -sockname] 2]
        close ${fd}
    }

    for {set max [expr ${port} + 50]} {${port} < ${max}} {incr port} {
        if {![catch {set fd [socket -server ${accept_script} ${port}]}]} {
            return ${port}
        }
    }

    return -1
}

