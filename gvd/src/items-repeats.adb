------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with GNAT.IO;         use GNAT.IO;
with Glib;            use Glib;
with Language;        use Language;

package body Items.Repeats is

   ---------------------
   -- New_Repeat_Type --
   ---------------------

   function New_Repeat_Type return Generic_Type_Access is
   begin
      return new Repeat_Type'
        (Generic_Type with
         Value            => null,
         Repeat_Str_Width => 0,
         Repeat_Num       => 0);
   end New_Repeat_Type;

   --------------------
   -- Get_Repeat_Num --
   --------------------

   function Get_Repeat_Num (Item : access Repeat_Type) return Integer is
   begin
      return Item.Repeat_Num;
   end Get_Repeat_Num;

   --------------------
   -- Set_Repeat_Num --
   --------------------

   procedure Set_Repeat_Num
     (Item : in out Repeat_Type;
      Num  : Integer) is
   begin
      Item.Repeat_Num := Num;
   end Set_Repeat_Num;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Repeat_Type) return Generic_Type_Access is
   begin
      return Item.Value;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Repeat_Type;
      Value : Generic_Type_Access) is
   begin
      Item.Valid := True;
      Item.Value := Value;
   end Set_Value;

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Repeat_Type; Indent : Natural := 0) is
   begin
      Put ("{<" & Value.Repeat_Num'Img & " times> : ");

      if Value.Value /= null then
         Print (Value.Value.all, Indent + 3);
         Put ("}");
      else
         Put ("<null>}");
      end if;
   end Print;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Repeat_Type; Only_Value : Boolean := False) is
   begin
      if Item.Value /= null then
         --  Keep the structure of the item that is repeated, if required.
         Free (Item.Value, Only_Value);
      end if;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Repeat_Type;
      Clone : in out Generic_Type_Access) is
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      --  duplicate the type of the repeated item.
      --  The value itself is in fact not duplicated, since the leafs of the
      --  type tree is a simple_type (or one of its children), that does not
      --  clone the value.
      Repeat_Type_Access (Clone).Value := Items.Clone (Item.Value.all);
   end Clone_Dispatching;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self : not null access Repeat_Type;
      Name : String;
      View : not null access Debugger_Data_View_Record'Class;
      Lang : Language.Language_Access;
      Mode : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Str : constant String :=
        "<repeat" & Integer'Image (Self.Repeat_Num) & "> ";
      Rect : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
   begin
      Rect.Add_Child (Gtk_New_Text (Styles.Text_Font, Str));
      Rect.Add_Child (Self.Value.Build_Display (Name, View, Lang, Mode));
      return Rect;
   end Build_Display;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      if Parent.Value = Generic_Type_Access (Current) then
         Free (Parent.Value, Only_Value => False);
         Parent.Value := Generic_Type_Access (Replace_With);
         return Generic_Type_Access (Replace_With);
      end if;

      return null;
   end Replace;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Repeat_Type) return Generic_Iterator'Class is
      Iter : Repeat_Iterator;
   begin
      Iter.Item := Repeat_Type_Access (Item);
      Iter.At_End := False;
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Repeat_Iterator) is
   begin
      Iter.At_End := True;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Repeat_Iterator) return Boolean is
   begin
      return Iter.At_End;
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Repeat_Iterator) return Generic_Type_Access is
   begin
      return Iter.Item.Value;
   end Data;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Repeat_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Repeat_Type'Class
        and then Structurally_Equivalent
        (Item1.Value, Repeat_Type_Access (Item2).Value);
   end Structurally_Equivalent;

end Items.Repeats;
