-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2008, AdaCore             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines the module for code fixing.

with Gtk.Menu;                use Gtk.Menu;
with GPS.Kernel;              use GPS.Kernel;
with GNAT.Strings;            use GNAT.Strings;
with Codefix;                 use Codefix;
with Codefix.Errors_Manager;  use Codefix.Errors_Manager;
with Codefix.Text_Manager;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Codefix.Formal_Errors;    use Codefix.Formal_Errors;

package Codefix_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Codefix_Session_Record is record
      Category     : GNAT.Strings.String_Access;
      Corrector    : Ptr_Correction_Manager;
      Current_Text : Codefix.Text_Manager.Ptr_Text_Navigator;
      Timestamp    : Integer := 0;
   end record;
   type Codefix_Session is access all Codefix_Session_Record;

   procedure Create_Submenu
     (Kernel       : access Kernel_Handle_Record'Class;
      Menu         : access Gtk.Menu.Gtk_Menu_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id);
   --  Add to menu all the possible fixes for Error

   procedure Create_Pixmap_And_Category
     (Kernel       : access Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id);
   --  Add to the location box a pixmap that will fixes the error.

   procedure Remove_Pixmap
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id);
   --  Remove from the location box the pixmap of the error.

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the codefix-related preferences

   type Codefix_Remove_Policy is
     (Always_Remove, Always_Comment, Propose_Both_Choices);
   for Codefix_Remove_Policy'Size use Glib.Gint'Size;
   pragma Convention (C, Codefix_Remove_Policy);

   package Codefix_Remove_Policy_Properties is new Generic_Enumeration_Property
     ("Codefix_Remove_Policy", Codefix_Remove_Policy);

   Remove_Policy : Param_Spec_Enum;

   function Policy_To_Operations
     (Policy : Codefix_Remove_Policy) return Useless_Entity_Operations;

end Codefix_Module;
