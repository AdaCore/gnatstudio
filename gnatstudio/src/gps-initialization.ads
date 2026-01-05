------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2024-2026, AdaCore                     --
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

with Interfaces.C.Strings;
with Glib;
with Glib.Object;          use Glib.Object;
with Glib.Error;
with Gtkada.Bindings;      use Gtkada.Bindings;
with System;

package GPS.Initialization is

   package ICS renames Interfaces.C.Strings;
   use type ICS.chars_ptr;

   function On_Switch
      (Option_Name : ICS.chars_ptr;
       Value       : ICS.chars_ptr;
       Data        : System.Address;  --  ignored
       Error       : access Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, On_Switch);
   --  General callback for switch handling from GApplication

   function On_File_Switch
     (Option_Name : ICS.chars_ptr;
      Value       : ICS.chars_ptr;
      Data        : System.Address;
      Error       : access Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, On_File_Switch);
   --  General callback for file opening handling from GApplication

   function Local_Command_Line
      (Self        : System.Address;
       Arguments   : access chars_ptr_array_access;
       Exit_Status : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Local_Command_Line);
   --  override gtk+ builtin virtual method for an application.
   --  This makes sure that we can do our own handling of --help

   procedure Application_Class_Init (Self : GObject_Class);
   pragma Convention (C, Application_Class_Init);

   procedure Report_Error (Message : String);
   --  Report an error to the user. This is meant to handle any error that
   --  occurs before the log file is created or the main window appears.
   --  Under Linux/UNIX, this is done on the standard output; on Windows,
   --  we log the error in the TMP dir.

end GPS.Initialization;
