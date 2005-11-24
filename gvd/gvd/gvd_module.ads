-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2005                      --
--                              AdaCore                              --
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

--  This package defines the debugger module (called GVD).

with Glib.Object;
with Gtk.Window;
with GPS.Kernel.Modules;
with Ada.Unchecked_Deallocation;

package GVD_Module is

   Debugger_Module_ID : GPS.Kernel.Modules.Module_ID;

   function Get_Module return GPS.Kernel.Modules.Module_ID;
   --  Return the debugger module

   type Debugger_List_Node;
   type Debugger_List_Link is access Debugger_List_Node;

   type Debugger_List_Node is record
      Debugger : Glib.Object.GObject;
      --  The real type is a Visual_Debugger

      Next     : Debugger_List_Link;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Debugger_List_Node, Debugger_List_Link);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String);
   --  Initialize the debugger if needed.

   function Get_Debugger_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Debugger_List_Link;
   --  Return to the current list of active debuggers

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Glib.Object.GObject;
   --  Return the current visual debugger

   procedure Set_First_Debugger
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : Debugger_List_Link);
   --  Set the first debugger returned by Get_Debugger_List

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : Glib.Object.GObject);
   --  Set the current active visual debugger

   function Get_Breakpoints_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Window.Gtk_Window;
   --  Return the breakpoint editor associated with Kernel, if any

end GVD_Module;
