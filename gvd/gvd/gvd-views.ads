------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  Various instanciations of GVD.Generic_Views

with GVD.Generic_View;
with GVD.Process;          use GVD.Process;
with GVD.Scripts;          use GVD.Scripts;
with GVD_Module;           use GVD_Module;
with Gtk.Box;              use Gtk.Box;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Interactive_Consoles; use Interactive_Consoles;

package GVD.Views is

   package Scrolled_Views is new GVD.Generic_View
     (Base_Type                     => Gtk_Scrolled_Window_Record,
      Base_Type_Access              => Gtk_Scrolled_Window,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger);

   package Console_Views is new GVD.Generic_View
     (Base_Type                     => Interactive_Console_Record,
      Base_Type_Access              => Interactive_Console,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger);

   package Boxed_Views is new GVD.Generic_View
     (Base_Type                     => Gtk_Box_Record,
      Base_Type_Access              => Gtk_Box,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger);

end GVD.Views;
