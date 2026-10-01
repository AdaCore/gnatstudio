------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

--  This package provides generic Help facilities.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Default_Preferences;   use Default_Preferences;
with GPS.Kernel;
with Gtk.Link_Button;

package Help_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   type Help_Link is private;

   function Create_Help_Button
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class; Link : String)
      return Default_Preferences.External_Link;

private
   type Help_Link is new External_Link_Interface with record
      Kernel : GPS.Kernel.Kernel_Handle;
      Link   : Unbounded_String;
   end record;

   overriding function Create_Link
     (Link : not null access Help_Link)
      return Gtk.Link_Button.Gtk_Link_Button;
end Help_Module;
