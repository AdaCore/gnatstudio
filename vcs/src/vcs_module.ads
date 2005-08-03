-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines the GPS module for communication with VCS.

with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Gtk.Widget;
with Gtkada.MDI;          use Gtkada.MDI;

with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.Modules;  use GPS.Kernel.Modules;
with VCS_View_Pkg;        use VCS_View_Pkg;
with VCS_Activities_View; use VCS_Activities_View;

package VCS_Module is

   VCS_Module_Name : constant String := "VCS_Interface";

   type VCS_Module_ID_Record is new Module_ID_Record with record
      VCS_List         : Argument_List_Access;
      --  The list of all VCS systems recognized by the kernel

      Explorer         : VCS_View_Access;
      --  The VCS Explorer

      Explorer_Child   : MDI_Child;
      --  The child containing the VCS Explorer

      Activities       : VCS_Activities_View_Access;
      --  The VCS Activities explorer

      Activities_Child : MDI_Child;
   end record;

   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   VCS_Module_ID : VCS_Module_ID_Access;

   procedure Destroy (Module : in out VCS_Module_ID_Record);
   function Default_Context_Factory
     (Module : access VCS_Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  See inherited documentation

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   function Get_VCS_List (Module : Module_ID) return GNAT.OS_Lib.Argument_List;
   --  Return the list of recognized VCS systems.
   --  You mustn't free the returned array.

   procedure Register_VCS (Module : Module_ID; VCS_Identifier : String);
   --  Add a VCS identifier to the list of recognized VCS systems.
   --  ??? This is temporary, until the VCS module can directly add a page in
   --  the wizard or the project properties editor.

   function Get_Explorer
     (Kernel      : Kernel_Handle;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_View_Access;
   --  Return the VCS Explorer. If Show is True, place it in the MDI and show
   --  it.

   procedure Hide_VCS_Explorer;
   --  Call this subprogram when the VCS Explorer is about to be removed

   function Explorer_Is_Open return Boolean;
   --  Return whether the Explorer is open

   function Get_Activities_Explorer
     (Kernel      : Kernel_Handle;
      Raise_Child : Boolean := True;
      Show        : Boolean := False) return VCS_Activities_View_Access;
   --  Return the VCS Explorer. If Show is True, place it in the MDI and show
   --  it.

   procedure Hide_VCS_Activities_Explorer;
   --  Call this subprogram when the VCS Explorer is about to be removed

   function Activities_Explorer_Is_Open return Boolean;
   --  Return whether the Explorer is open

end VCS_Module;
