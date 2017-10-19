------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Glib;                   use Glib;
with Gtkada.Canvas_View;     use Gtkada.Canvas_View;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with Browsers;               use Browsers;
with GVD.Canvas;             use GVD.Canvas;

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

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access Repeat_Type) return String is
   begin
      return
        (if Self.Value = null then "" else Self.Value.Get_Simple_Value)
        & " <" & Image (Self.Repeat_Num, Min_Width => 0) & " times>";
   end Get_Simple_Value;

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
      View : not null access GVD.Canvas.Debugger_Data_View_Record'Class;
      Lang : Language.Language_Access;
      Mode : GVD.Canvas.Display_Mode) return GVD.Canvas.Component_Item
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

   -------------------
   -- Get_Type_Name --
   -------------------

   overriding function Get_Type_Name
     (Self    : access Repeat_Type;
      Lang    : Language.Language_Access) return String is
   begin
      --  So that we display  "(record) <repeat 11 times>", and not
      --  "() <repeat 11 times>".  The latter requires one extra level of
      --  expansion in the variables view.
      if Self.Value = null then
         return "";
      else
         return Self.Value.Get_Type_Name (Lang);
      end if;
   end Get_Type_Name;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Repeat_Type) return Generic_Iterator'Class is
   begin
      --  No child ? Return an iterator that does nothing
      if Item.Value = null then
         return Create_Empty_Iterator;
      else
         --  Else iterate directly the components of the repeated type, so that
         --  we have one less level in the Variables view
         return Item.Value.Start;
      end if;
   end Start;

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
