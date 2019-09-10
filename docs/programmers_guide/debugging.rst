*********
Debugging
*********

X11 server
==========

If you are developing on a linux system, it is recommended that you
reconfigure your X11 server with the following setup (see the file
:file:`/etc/X11/XF86Config-4`)::

  Section "ServerFlags"
          Option "AllowDeactivateGrabs" "true"   # Ctrl+Alt+Keypad *
          Option "AllowClosedownGrabs"  "true"   # Ctrl+Alt+Keypad /
  EndSection
  

The two key bindings described above are used to release any grab that
a GUI application might have. This is especially useful when debugging
through `gdb`: it might happen that the breakpoint happens while
such a grab is in place, and would therefore prevent any input (mouse
or keyboard) to any application in your X11 session, in particular the
debugger.

gtk+ library
============

It is also recommended that you recompile your own gtk+ library (on
systems where this is easily doable such as Unix systems), with the
following configure command::

     ./configure --with-debug=yes
  

In addition to providing the usual debugging information in the
debugger, this also activates several environment variables which
might be used to monitor the actions in gtk+ and its associated
libraries.

These variables are the following::

  export GTK_DEBUG=misc:plugsocket:text:tree:updates:keybindings;
  export GDK_DEBUG=updates:nograbs:events:dnd:misc:xim:colormap:gdkrb:gc:pixmap:image:input:cursor;
  export GOBJECT_DEBUG=objects:signals;
  

Some of the values for these variables can be omitted. The exact
semantic (or even the exact list) of such variables depends on your
version of gtk+, and you should therefore consult its documentation.

debugger
========

When debugging with `gdb`, it is recommended that you always
specify the flag `--sync` to gnatstudio. This forces any gtk+
application, and in particular GNAT Studio, to process X11 events
synchronously, and therefore makes it easier to debug possible
problems.

If your application is printing some gtk+ warnings on the console, you
should do the following in the debugger::

    (gdb) set args --sync
    (gdb) begin
    (gdb) break g_log
    (gdb) cont
  

This will stop the application as soon as the gtk+ warning is printed.

