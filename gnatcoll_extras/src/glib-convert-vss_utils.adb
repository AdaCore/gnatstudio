------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with VSS.Strings.Conversions;

package body Glib.Convert.VSS_Utils is

   -----------------
   -- Escape_Text --
   -----------------

   function Escape_Text
     (S : Ada.Strings.UTF_Encoding.UTF_8_String)
      return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Glib.Convert.Escape_Text (S));
   end Escape_Text;

   -----------------
   -- Escape_Text --
   -----------------

   function Escape_Text
     (S : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Glib.Convert.Escape_Text
             (VSS.Strings.Conversions.To_UTF_8_String (S)));
   end Escape_Text;

   -----------------
   -- Escape_Text --
   -----------------

   function Escape_Text
     (S : Wide_Wide_String) return VSS.Strings.Virtual_String is
   begin
      return Escape_Text (VSS.Strings.To_Virtual_String (S));
   end Escape_Text;

end Glib.Convert.VSS_Utils;
