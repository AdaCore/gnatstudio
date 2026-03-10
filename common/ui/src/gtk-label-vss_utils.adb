------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

with VSS.Strings.Conversions;

package body Gtk.Label.VSS_Utils is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Label : out Gtk_Label;
      Text  : VSS.Strings.Virtual_String) is
   begin
      Gtk.Label.Gtk_New
        (Label, VSS.Strings.Conversions.To_UTF_8_String (Text));
   end Gtk_New;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Label : not null access Gtk_Label_Record'Class;
       Text  : VSS.Strings.Virtual_String) is
   begin
      Gtk.Label.Set_Markup
        (Label, VSS.Strings.Conversions.To_UTF_8_String (Text));
   end Set_Markup;

end Gtk.Label.VSS_Utils;
