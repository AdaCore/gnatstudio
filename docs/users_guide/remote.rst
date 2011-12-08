.. _Using_GPS_for_Remote_Development:

********************************
Using GPS for Remote Development
********************************

.. index:: remote
.. index:: network
.. index:: client/server

In a network environment, it is common for programmers to use a desktop
computer that is not directly suitable for their development tasks. For
example, each developer may have a desktop PC running Windows or GNU/Linux as
their main entrypoint to the company network. They may do all their actual
development work using project resources shared on networked servers. These
remote servers may also be running an operating system that is different from
the one on their desktop machine.

A typical way of operating in such an environment is to access the server
through a remote windowing system such as X-Window. GPS does indeed work in
such a context but it is not necessarily the most efficient organization.
Running GPS remotely on a shared server will increase the workload of the
server as well as the traffic on the network. When the network is slow or
saturated, user interactions can become uncomfortably sluggish. This is
unfortunate because the desktop used to access the network is often a powerful
PC that remains idle most of the time. To address this situation, GPS offers
the option to run natively on the desktop, with compilation, run and/or debug
activities performed transparently on one or more remote servers.

.. _Requirements:

Requirements
============

In order to compile, run or debug on a host remote from GPS, three conditions
must be met:

* 
  .. index:: password

  Have a remote connection to the host using 'rsh', 'ssh' or 'telnet'. Note that
  GPS can now handle passwords for such connections.

* Have either a Network Filesystem (i.e. NFS, SMB or equivalent) sharing the
  project files between the host and the target, or have rsync installed on
  both client and server. Note that rsync can be found at
  `http://www.samba.org/rsync/ <http://www.samba.org/rsync/>`_ for unix, and
  comes as part of cygwin under Windows: `http://www.cygwin.com
  <http://www.cygwin.com>`_.

* Subprojects must be 'withed' by the main project using relative paths, or the
  same absolute paths must exist on the machines involved.

The full remote development setup is performed in two broad steps:

* Setup the remote servers configuration.
* Setup a remote project.

.. _Setup_the_remote_servers:

Setup the remote servers
========================

.. _The_remote_configuration_dialog:

The remote configuration dialog
-------------------------------

In order to configure remote servers, you need to open the remote configuration
dialog. A predefined configuration can also be set when installing GPS, using
xml files. :ref:`Defining_a_remote_server`, and
:ref:`Defining_a_remote_path_translation`, for more information.

The remote configuration dialog is opened via the remote view. You can open it
using the menu `Tools->Views->Remote`.

.. index:: screen shot
.. image:: remote-view.jpg

Once the Remote View is opened, click on `Settings` to open the servers
configuration dialog.

.. index:: screen shot
.. image:: servers-configuration.jpg

This dialog is composed of two parts:

* The left part of the dialog contains the list of configured servers,
  identified by their nickname. Three buttons allow you to create, reinitialize
  or delete a server.

* The right part of the dialog contains the selected server's configuration.

You need first to create a new server. For this, click on the button `Add
Server` on the bottom left part of the dialog. Enter a nickname identifying the
server you want to connect to (this is not necessarily the network name of this
server). Note that this nickname identifies the server and therefore must be
unique. This new server is then automatically selected, and the right part of
the dialog shows its configuration, which is empty for the most part.

.. _Connection_settings:

Connection settings
-------------------

The first configuration part that needs to be filled concerns the way we will
connect to this server:

You have to enter first all mandatory fields, identified by an asterisk:

* The network name is the name used to connect to this server via your network.
  It can be either an IP address, a host name of your local network, or a fully
  qualified network name.

* The remote access tool is the tool used to connect to this server. You select
  it using the drop down list. The following tools are supported natively by
  GPS: ssh, rsh, telnet and plink (Windows tool) in ssh, rsh or telnet mode.
  :ref:`Defining_a_remote_connection_tool`, if you need to add a specific tool.
  Note also that if one of those tools is not installed (e.g. is not in your
  path), then it won't appear in the tools list. Some tools incompatible with
  GPS will not be displayed either, such as the Microsoft telnet client.

* The shell tells GPS what shell runs on the remote server. The following unix
  shells are supported by GPS: sh, bash, csh and tcsh. Windows' shell is also
  supported (cmd.exe). :ref:`Limitations`, for cygwin's shell usage on windows:
  it is preferable to use cmd.exe as a remote shell on Windows servers.

Other fields might need to be taken into consideration, but they are not
mandatory. They are, for the most part, accessible through the advanced
configuration pane.

* The remote sync tool is used to synchronize remote and local filesystems, if
  these are not shared filesystems. For now, only rsync is supported.

* The Extra Init Commands field represents initialization commands sent to the
  server upon connection: when GPS connects to your remote machine, the chosen
  shell is launched, and your default initialization files are read (i.e.
  .bashrc file for the bash shell). Then GPS sends these extra init commands,
  allowing you for example to specify a compilation toolchain.

* (In Advanced configuration pane) The user name specifies the name used to
  connect to the server. If unspecified, the remote access tool will typically
  use your current login name. If not, and a user name is requested, GPS will
  prompt you for a user name.

* (In Advanced configuration pane) The timeout value is used to determine if a
  connection to a remote host is dead. All elementary operations performed on
  the remote host (i.e., operations that normally complete almost immediately)
  will use this timeout value. By default, this value is set to 10s. If you
  have a very slow network connection or a very overloaded server, set this
  timeout to a higher value.

* (In Advanced configuration pane) The maximum number of connections determines
  the maximum number of simultaneous connections GPS is allowed to have to this
  server. In fact, if you want to compile, debug and execute at the same time
  on the machine, GPS will need more that one connection to do this. The
  default value is 3.

* (In Advanced configuration pane) Depending on the kind of server and the
  remote access tool used, commands sent to the server may require a specific
  line terminator, i.e., either the LF character or CR/LF characters. Usually
  GPS can automatically detect what is needed (the 'auto' mode), but the choice
  can be forced to CR/LF (cr/lf handling set to 'on') or LF (cr/lf handling set
  to 'off').

* (In Advanced configuration pane) The Debug console allows you to easily
  debug a remote connection. If checked, it will open a console
  reporting all exchanges between GPS and the selected server.

.. _Paths_settings:

Paths settings
--------------

The last configuration part defines the path translations between
your local host and the remote server.

The remote paths definition will allow GPS to translate your locally loaded
project (the project that resides in your local filesystem) to paths used on
the remote server. This part also tells GPS how to keep those paths
synchronized between the local machine and the remote server.

All your project's dependencies must then reside in a path that is defined
here. Note that you can retrieve those paths by using `gnat list -v
-Pyour_project`. In particular, the path to the GNAT run-time (`adainclude`
directory) needs to be mapped so that code completion and source navigation
work properly on run-time entities.

To add a new path, click on the `+` button, and enter the corresponding
local and remote paths.

You can easily select the desired paths by clicking on the icon next to the
path's entry. Remote browsing is allowed only when the connection configuration
is set (:ref:`Connection_settings`.) Clicking on `Apply` will apply your
connection configuration and allow you to browse the remote host to select the
remote paths.

Five kinds of path synchronization can be set for each defined path:

* `Never`: no synchronization is required from GPS, the paths
  are shared using an OS mechanism like NFS.
* `Manually`: synchronization is needed, but will only be performed
  manually using the remote view buttons.
* `Always`: Relevant to source and object paths of your project.
  They are kept synchronised by GPS before and after every remote action (such
  as performing a build or run).
* `Once to local`/`Once to remote`: Relevant to project's
  dependencies. They are synchronized once when a remote project is
  loaded or when a local project is set remote. They can still be
  manually synchronized using the Remote View (:ref:`The_remote_view`.)

The way those paths need to be configured depends on your network architecture.

* If your project is on a filesystem that is shared between your host and the
  remote host (using NFS of SMB filestems, for example), then only the roots of
  those filesystems need to be specified, using each server's native paths (on
  Windows, the paths will be expressed using X:\\my\\mounted\\directory\\ while
  on unix, the paths will be expressed using /mnt/path/).

* If the project's files are synchronized using rsync, defining a too generic
  path translation will lead to very slow synchronization. In that case you
  should define the paths as specifically as possible, in order to speed up the
  synchronization process.

.. _Setup_a_remote_project:

Setup a remote project
======================

.. index:: remote project

.. _Remote_operations:

Remote operations
-----------------

GPS defines four different remote operation categories: Build operations, Debug
operations, Execution operations and Tools operations. All compiler related
operations are performed on the Build_Server. The Tools server is somewhat
special and will be explained later. The debugger is run on the Debug_Server,
and the project's resulting programs are run on the Execution_Server. The
GPS_Server (the local machine) is used for all other operations.

The Tools server is defined to handle all compiler related operations that do
not depend on a specific compiler version. It is used in dual compilation mode,
for example, to determine whether the action can be safely run using a very
recent compiler toolchain (this is the tools server), or whether a specific
older baseline compiler version must be used.

In case the remote mode is activated, and the dual compilation mode is not, all
Tools server operations are executed on the build server. Otherwise, if the
dual compilation mode is activated, then the tools server operations are always
executed on the local machine.

.. _The_remote_view:

The remote view
---------------

The Remote view (`Tools->Views->Remote`) allows you to assign servers to
operation categories for the currently loaded project.  You may assign each
operation category a distinct server if the Servers assignment tab is fully
expanded. Alternatively, you may assign all categories to a single server in
one step if the Servers assignment tab is collapsed.

.. index:: screen shot
.. image:: remote-view-full.jpg

When a server is selected for a particular category, the change is not
immediately effective. To indicate that fact, the server's name will appear in
red. This approach allows you to check the configuration before applying it, by
pressing the `Check` button. This action will test for correct remote hosts
connection. It will also verify that the project path exists on the build
server and that it has an equivalence on the local machine.

Clicking on the `Apply` button will perform the following actions:

* Read the default project paths on the Build machine and translate them
  into local paths.
* Synchronize from the build server those paths marked as Sync `Always` or `Once to local`.
* Load the translated local project.
* Assign the Build, Execution and Debug servers.

If one of the above operations fails, corresponding errors are reported in the
`Messages` view and the previous project settings are retained.

Once a remote server is assigned, this remote configuration will be
automatically loaded each time the project is loaded.

The two buttons on the right of each server can be used to manually perform a
synchronization from the remote host to your local machine (left button) or
from your local machine to the remote host (right button).

.. _Loading_a_remote_project:

Loading a remote project
------------------------

If the project you want to work with is already on a distant server, you can
directly load it on your local GPS.

To do this, use the `Project->Open From Host` menu.  Then select the server's
nickname. This will show you its file tree.  Navigate to your project and
select it. The project will be loaded as described above, with all remote
operations categories assigned to the selected server by default.

You can reload your project using the local files on your machine. The remote
configuration will then be automatically reapplied.

.. _Limitations:

Limitations
===========

The GPS remote mode imposes a few limitations:

* Execution: you cannot use an external terminal to remotely execute your
  application. The `Use external terminal` checkbox of the run dialog will have
  no effect if the program is run remotely.

* Debugging: you cannot use a separate execution window. The `Use separate
  execution window` option is ignored for remote debugging sessions.

* Cygwin on remote host: the GNAT compilation toolchain does not understand
  cygwin's mounted directories. In order to use GPS with a remote Windows
  server using cygwin's bash, you need to use directories that are the same on
  Windows and cygwin (absolute paths). For example, a project having a
  C:\\my_project will be accepted if cygwin's path is /my_project, but will not
  be accepted if /cygdrive/c/my_project is used.

  Note that even if you use cygwin's sshd on such a server, you can still
  access it using cmd.exe (:ref:`Connection_settings`.)

