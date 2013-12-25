.. _Using_GPS_for_Remote_Development:

********************************
Using GPS for Remote Development
********************************

.. index:: remote
.. index:: network
.. index:: client/server

It is common for programmers in a networked environment to use a desktop
computer that's not itself suitable for their development tasks. For
example, each developer may have a desktop PC running Windows or GNU/Linux
as their main entry to a company network and do all their development work
using project resources on shared networked servers. These remote servers
may be running an operating system different from the one on their desktop
machine.

One common way of operating in such an environment is to access the server
through a remote windowing system such as X-Windows. GPS works in such a
context but it's not necessarily the most efficient configuration because
running GPS remotely on a shared server increases the workload of the
server as well as traffic on the network. When the network is slow, user
interactions can become uncomfortably sluggish. This is unfortunate because
the desktop used to access the network is often a powerful PC that remains
idle most of the time. To address this situation, GPS offers the option of
running natively on the desktop, with compilation, execution, and/or
debugging activities performed transparently on one or more remote servers.

.. _Requirements:

Requirements
============

In order to compile, run, or debug on a host remote from GPS, your
configuration must meet the following conditions:

* 
  .. index:: password

  Have a remote connection to the host using 'rsh', 'ssh' or 'telnet'.  GPS
  can handle passwords for such connections.

* Have either a Network Filesystem (i.e. NFS, SMB or equivalent) sharing
  the project files between the host and the target or have `rsync`
  installed on both client and server.  (`rsync` can be found at
  `http://www.samba.org/rsync/ <http://www.samba.org/rsync/>`_ for Unix,
  and is part of Cygwin under Windows: `http://www.cygwin.com
  <http://www.cygwin.com>`_.

* Either subprojects must be 'withed' by the main project using relative
  paths or the same absolute paths must exist on both the desktop and the
  server.

The full remote development setup is performed in two steps:

* Setup the remote servers configuration.
* Setup a remote project.

.. _Setup_the_remote_servers:

Setup the remote servers
========================

.. _The_remote_configuration_dialog:

The remote configuration dialog
-------------------------------

Open the remote configuration dialog using the menu `Tools->Views->Remote`
to configure remote servers. You can also set a predefined configuration
when installing GPS by using XML files. :ref:`Defining_a_remote_server`,
and :ref:`Defining_a_remote_path_translation`, for more information.


.. index:: screen shot
.. image:: remote-view.jpg

Once you've opened the Remote View, click on `Settings` to open the servers
configuration dialog.

.. index:: screen shot
.. image:: servers-configuration.jpg

This dialog consists of two parts:

* The left part of the dialog contains the list of configured servers, each
  identified by a nickname. Three buttons allow you to create, reinitialize
  or delete a server.

* The right part of the dialog contains the selected server's configuration.

First, create a new server by clicking on the `Add Server` button on the
bottom left of the dialog. Enter a unique nickname identifying the server
(this is not necessarily the network name of this server).  This server is
automatically selected and the right part of the dialog shows its
configuration, which is mostly initially empty.

.. _Connection_settings:

Connection settings
-------------------

For each server, you first need to fill in the section that describes how
GPS should connect to that server.  All mandatory fields are identified by
an asterisk:

* The "Network Name" is the name used to connect to the server via your
  network.  It can be either an IP address, a host name on your local
  network, or a fully qualified name.

* The "Remote Access Tool" is a drop-down list specifying the tool used to
  connect to the server.  Support for the following tools is built in to
  GPS: ssh, rsh, telnet and plink (Windows tool) in ssh, rsh or telnet
  mode.  :ref:`Defining_a_remote_connection_tool` if you need to add a
  different tool.  If a tool is not in your path (for example, because it's
  noit installed), it won't appear in the tools list. Some tools
  incompatible with GPS aren't displayed either, such as the Microsoft
  telnet client.

* The "Shell" tells GPS what shell runs on the remote server. The following
  Unix shells are supported by GPS: sh, bash, csh and tcsh. Windows' shell
  is also supported (cmd.exe). :ref:`Limitations`, for Cygwin's shell usage
  on Windows: it;s preferable to use :file:`cmd.exe` as a remote shell on
  Windows servers.

Other fields may need to be specified but are not mandatory. Most are
accessible through the advanced configuration pane.

* The Remote Sync Tool is used to synchronize remote and local filesystems,
  if these are not shared filesystems. Only `rsync` is supported by GPS.

* The Extra Init Commands lists initialization commands that GPS will send
  to the server when GPS connects to your remote machine, the chosen shell
  is launched, and your default initialization files are read (i.e.
  .bashrc file for the bash shell).  GPS sends these extra commands,
  allowing you, for example, to specify a compilation toolchain.

* (In Advanced configuration pane) The User Name specifies the name used to
  connect to the server.  The default is to use your current login name.

* (In Advanced configuration pane) The Timeout value is used to determine
  if a connection to a remote host is dead. All elementary operations
  performed on the remote host (i.e., those operations that normally
  complete almost immediately) use this timeout value. By default, this
  value is set to 10 seconds. If you have a very slow network connection or
  a very overloaded server, set this to a higher value.

* (In Advanced configuration pane) The Maximum Number of Connections is the
  maximum number of simultaneous connections GPS is allowed to make to this
  server. If you want to compile, debug, and execute at the same time on
  the machine, GPS needs more that one connection to do this. The default
  is 3.

* (In Advanced configuration pane) Depending on the kind of server and the
  remote access tool used, commands sent to the server may require a
  specific line terminator, typically either the LF character or CR/LF
  characters. Usually GPS can automatically detect which is needed (the
  'auto' mode), you can force the choice to CR/LF (cr/lf handling set to
  'on') or LF (cr/lf handling set to 'off').

* (In Advanced configuration pane) The Debug Console allows you to easily
  debug a remote connection. If checked, it opens a console displaying all
  exchanges between GPS and the selected server.

.. _Paths_settings:

Paths settings
--------------

The final section of the configuration defines the path translations
between your local host and the remote server.

The remote paths definition allow GPS to translate your locally loaded
project (the project that resides in your local filesystem) to paths used
on the remote server. This section also tells GPS how to keep those paths
synchronized between the local machine and the remote server.

All your project's dependencies must reside in a path defined here.  You
retrieve those paths by using `gnat list -v -Pyour_project`. In particular,
the path to the GNAT run-time (adainclude` directory) needs to be mapped so
that code completion and source navigation work properly on run-time
entities.

To add a new path, click on the `+` button and enter the corresponding
local and remote paths.

You can easily select the desired paths by clicking on the icon next to the
path's entry. Remote browsing is allowed only when the connection
configuration is set (:ref:`Connection_settings`.) Clicking on `Apply` will
apply your connection configuration and allow you to browse the remote host
to select the remote paths.

Five types of path synchronization can be set for each defined path:

* `Never`: no synchronization is required from GPS because the paths
  are shared using an OS mechanism like NFS.
* `Manually`: synchronization is needed, but is only performed
  manually using the remote view buttons.
* `Always`: Relevant to source and object paths of your project.
  They're kept synchronised by GPS before and after every remote action (such
  as performing a build or run).
* `Once to local`/`Once to remote`: Relevant to project's
  dependencies. They're synchronized once when a remote project is
  loaded or when a local project is set remote. They can still be
  manually synchronized using the Remote View (:ref:`The_remote_view`.)

The way those paths need to be configured depends on your network architecture.

* If your project is on a filesystem shared between your host and the
  remote host (using NFS or SMB filestems, for example), only the roots of
  those filesystems need to be specified, using each server's native paths
  (on Windows, the paths are specified using the
  "X:\\my\\mounted\\directory\\" synax and on Unix, using the using the
  "/mnt/path/" syntax).

* If the project's files are synchronized using `rsync`, defining a too
  generic path translation leads to very slow synchronization. In that case
  define the paths as specifically as possible in order to speed up the
  synchronization process.

.. _Setup_a_remote_project:

Setup a remote project
======================

.. index:: remote project

.. _Remote_operations:

Remote operations
-----------------

GPS defines four different categories of remote operation: Build
operations, Debug operations, Execution operations and Tools
operations. All compiler-related operations are performed on the
Build_Server. The Tools_Server is somewhat special and will be explained
later. The debugger is run on the Debug_Server and the project's resulting
programs are run on the Execution_Server. The GPS_Server (the local
machine) is used for all other operations.

The Tools_Server handles all compiler related operations that don't depend
on a specific compiler version. It's used in dual compilation mode, for
example, to determine whether the action can be safely run using a very
recent compiler toolchain (the Tools_Server), or whether a specific, older
baseline compiler version must be used.

If the remote mode is activated and the dual compilation mode is not, all
Tools_Server operations are executed on the Build_Server. Otherwise, if the
dual compilation mode is activated, the Tools_Server operations are always
executed on the local machine.

.. _The_remote_view:

The remote view
---------------

Use the Remote view (`Tools->Views->Remote`) to assign servers to
categories of operations for the currently loaded project.  You can assign
a different server to each operation category if the Servers Assignment tab
is fully expanded. Alternatively, you can assign all categories to a single
server in one step if the Servers Assignment tab is collapsed.

.. index:: screen shot
.. image:: remote-view-full.jpg

When a server is selected for a particular category, the change is not
immediately effective, indicated by the server's name appearing in
red. This allows you to check the configuration before applying it, by
pressing the `Check` button. This tests for a correct remote connection. It
also verifies that the project path exists on the build server and has an
equivalent on the local machine.

Clicking on the `Apply` button performs the following actions:

* Read the default project paths on the Build_Server and translate them
  into local paths.
* Synchronize from the build server those paths marked as Sync `Always`
  or `Once to local`.
* Load the translated local project.
* Assign the Build, Execution and Debug servers.

If one of the above operations fails, the errors are reported in the
`Messages` view and the previous project settings are retained.  Once a
remote server is assigned, the remote configuration is automatically loaded
each time the project is loaded.

The two buttons on the right of each server can be used to manually perform
a synchronization from the server to your local machine (left button) or
from your local machine to the server (right button).

.. _Loading_a_remote_project:

Loading a remote project
------------------------

If the project you want to use is already on a remote server, you can
directly load it on your local GPS by using the `Project->Open From Host`
menu and selecting the server's nickname. This shows you its file tree.
Navigate to your project and select it. The project is loaded as described
above with all remote operations categories assigned to the selected server
by default.

You can reload your project from local files on your machine. The remote
configuration is automatically reapplied.

.. _Limitations:

Limitations
===========

The GPS remote mode imposes some limitations:

* Execution: you can't use an external terminal to remotely execute your
  application. The `Use external terminal` checkbox of the run dialog hasno
  effect if the program is run remotely.

* Debugging: you can't use a separate execution window. The `Use separate
  execution window` option is ignored for remote debugging sessions.

* Cygwin on remote host: the GNAT compilation toolchain doesn't understand
  Cygwin's mounted directories. In order to use GPS with a remote Windows
  server using Cygwin's `bash`, you must use directories that are the same
  on Windows and Cygwin (absolute paths). For example, a project using
  "C:\\my_project" is accepted if Cygwin's path is :file`/my_project`, but 
  not if :file:`/cygdrive/c/my_project` is specified.

  Even if you use Cygwin's `sshd` on such a server, you can still access it
  using :file:`cmd.exe` (:ref:`Connection_settings`.)
