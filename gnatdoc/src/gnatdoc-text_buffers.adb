------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

package body GNATdoc.Text_Buffers is

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String) is
   begin
      if Self.New_Line then
         Self.Text.Append (Item);
         Self.New_Line := False;

      else
         Self.Text.Replace_Element
           (Self.Text.Last, Self.Text.Last_Element & Item);
      end if;
   end Append;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String) is
   begin
      if Self.New_Line then
         Self.Text.Append (Item);

      else
         Self.Text.Replace_Element
           (Self.Text.Last, Self.Text.Last_Element & Item);
         Self.New_Line := True;
      end if;
   end Append_Line;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String_Vectors.Vector) is
   begin
      if Item.Is_Empty then
         return;

      elsif Self.New_Line then
         Self.Text.Append (Item);

      else
         Self.Text.Replace_Element
           (Self.Text.Last,
            Self.Text.Last_Element & Item.First_Element);

         for Index in Item.First_Index + 1 .. Item.Last_Index loop
            Self.Text.Append (Item.Element (Index));
         end loop;

         Self.New_Line := True;
      end if;
   end Append_Line;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Text_Buffer'Class) is
   begin
      Self.Text.Clear;
      Self.New_Line := True;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Text_Buffer'Class) return Boolean is
   begin
      return Self.Text.Is_Empty;
   end Is_Empty;

   ----------
   -- Text --
   ----------

   function Text
     (Self : Text_Buffer'Class) return Unbounded_String_Vectors.Vector is
   begin
      return Self.Text;
   end Text;

   --------------------
   -- To_Text_Buffer --
   --------------------

   function To_Text_Buffer
     (Item : Unbounded_String_Vectors.Vector) return Text_Buffer is
   begin
      return (Item, True);
   end To_Text_Buffer;

end GNATdoc.Text_Buffers;
