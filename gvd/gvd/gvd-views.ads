-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2005-2008, AdaCore          --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Various instanciations of GVD.Generic_Views

with GVD.Generic_View;
with GVD.Process;          use GVD.Process;
with GVD.Scripts;          use GVD.Scripts;
with GVD_Module;           use GVD_Module;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Interactive_Consoles; use Interactive_Consoles;

package GVD.Views is

   package Scrolled_Views is new GVD.Generic_View
     (Base_Type                     => Gtk_Scrolled_Window_Record,
      Base_Type_Access              => Gtk_Scrolled_Window,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger,
      Debugger_Process_Stopped_Hook => Debugger_Process_Stopped_Hook,
      Debugger_Context_Changed_Hook => Debugger_Context_Changed_Hook,
      Debugger_Terminated_Hook      => Debugger_Terminated_Hook);

   package Console_Views is new GVD.Generic_View
     (Base_Type                     => Interactive_Console_Record,
      Base_Type_Access              => Interactive_Console,
      Visual_Debugger_Record        => GVD.Process.Visual_Debugger_Record,
      Visual_Debugger               => GVD.Process.Visual_Debugger,
      Debugger_Process_Stopped_Hook => Debugger_Process_Stopped_Hook,
      Debugger_Context_Changed_Hook => Debugger_Context_Changed_Hook,
      Debugger_Terminated_Hook      => Debugger_Terminated_Hook);

end GVD.Views;
