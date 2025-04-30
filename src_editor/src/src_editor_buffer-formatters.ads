------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with GPS.Editors;   use GPS.Editors;
with GPS.Kernel;    use GPS.Kernel;
with Gtk.Text_Iter; use Gtk.Text_Iter;

package Src_Editor_Buffer.Formatters is

   type Known_Provider is (LSP, Construct, Disabled);
   --  List of Known Providers, it will be used to create the preferences

   procedure Add_Provider
     (Name : Known_Provider; Provider : Editor_Formatting_Provider_Access);
   --  Activate the provider by adding its implementation

   procedure Delete_Provider (Name : Known_Provider);
   --  Deactivate the provider

   procedure Range_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False);
   --  Execute provider selected in the preferences to do range formatting

   procedure On_Type_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False);
   --  Execute provider selected in the preferences to do formatting on enter

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the preferences

end Src_Editor_Buffer.Formatters;
