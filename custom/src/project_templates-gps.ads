------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

--  This package handles interaction between the project templates
--  engine and GPS.

with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with GPS.Kernel; use GPS.Kernel;

package Project_Templates.GPS is

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class);
   --  Register the module

   function Display_Project_Templates_Assistant
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean;
   --  Display the "project from template" assistant.
   --
   --  Parent is set to be the transient window of the displayed project
   --  templates assistant.
   --
   --  Return True if the new project has been correctly loaded by GPS and
   --  False if the assistant was cancelled by the user.

end Project_Templates.GPS;
