------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This package defines the module for code fixing.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Gtk.Menu;               use Gtk.Menu;
with GPS.Kernel;             use GPS.Kernel;
with Codefix;                use Codefix;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Text_Manager;
with Default_Preferences.Enums;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;

package Codefix_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Codefix_Session_Record is record
      Category     : Unbounded_String;
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

   package Codefix_Remove_Policy_Preferences is new
     Default_Preferences.Enums.Generics (Codefix_Remove_Policy);

   Remove_Policy : Codefix_Remove_Policy_Preferences.Preference;

end Codefix_Module;
