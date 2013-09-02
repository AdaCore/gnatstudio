***************
The GPS modules
***************

GPS is organized around the concept of modules. The only part of GPS that
is mandatory is its kernel (:ref:`The_GPS_Kernel`), all the other tools,
menus and features are provided in optional modules.

Although currently all modules have to be loaded at startup, some proof of
concept for dynamically loadable module was implemented, and will most likely
be part of a future version of GPS.

Every new feature you implement will be part of one or more modules. We will
go through the details of creating new modules all along this manual, starting
from a simple Hello World module to more advanced features like providing
new shell or python commands.

Generally speaking, a module provides a limited set of features, and adds
new GUI features in the GPS interface, like menus, toolbar buttons, contextual
menu entries, new windows,... As much as possible, a menu shouldn't directly
depend on any other module, only on the GPS kernel itself.

See the file :file:`gps-kernel-modules.ads` for more information on modules.

