-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package defines the module for code fixing.

with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Menu;               use Gtk.Menu;

with Glide_Kernel;           use Glide_Kernel;

with Codefix;                use Codefix;
with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;

package Codefix_Module is

   type Codefix_Module_ID_Record is new Glide_Kernel.Module_ID_Record with
   record
      Current_Text : Ptr_Text_Navigator;
      Corrector    : Ptr_Correction_Manager;
      Errors_Found : Ptr_Errors_Interface;
      Kernel       : Kernel_Handle;
   end record;

   type Codefix_Module_ID_Access is access all Codefix_Module_ID_Record'Class;

   Codefix_Module_ID   : Codefix_Module_ID_Access;
   Codefix_Module_Name : constant String := "Code_Fixing";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Codefix_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Fix_Command : Ptr_Command;
      Error       : Error_Id;
   end record;

   type Codefix_Menu_Item is access all Codefix_Menu_Item_Record;

   procedure Gtk_New (This : out Codefix_Menu_Item; Label : String := "");

   procedure Initialize
     (Menu_Item : access Codefix_Menu_Item_Record;
      Label     : String);

   function Create_Submenu (Error : Error_Id) return Gtk_Menu;

   Compilation_Category : constant String := "Builder Results";

end Codefix_Module;
