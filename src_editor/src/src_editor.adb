-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Strings.Fixed;
with Gtkada.Intl; use Gtkada.Intl;

package body Src_Editor is

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Editor", Msg);
   end "-";

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
   begin
      return Image (Gint (N));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (N : Gint) return String is
   begin
      return Ada.Strings.Fixed.Trim (Gint'Image (N), Ada.Strings.Left);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (N : Natural; Length : Positive) return String is
   begin
      return Image (Gint (N), Length);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (N : Gint; Length : Positive) return String is
      Pad         : constant Character := ' ';
      Small_Image : constant String := Image (N);

   begin
      if Small_Image'Length >= Length then
         return Small_Image;
      else
         declare
            Padded_Image : String (1 .. Length);
         begin
            for Index in 1 .. Length - Small_Image'Length loop
               Padded_Image (Index) := Pad;
            end loop;

            Padded_Image
              (Length - Small_Image'Length + 1 ..  Length) :=
              Small_Image;

            return Padded_Image;
         end;
      end if;
   end Image;

   -------------------------
   -- Number_Of_Digits_In --
   -------------------------

   function Number_Of_Digits_In (N : Natural) return Natural is
   begin
      case N is
         when 0 .. 9 =>
            return 1;
         when 10 .. 99 =>
            return 2;
         when 100 .. 999 =>
            return 3;
         when 1_000 .. 9_999 =>
            return 4;
         when 10_000 .. 99_999 =>
            return 5;
         when others =>
            return Image (Gint (N))'Length;
      end case;
   end Number_Of_Digits_In;

end Src_Editor;
