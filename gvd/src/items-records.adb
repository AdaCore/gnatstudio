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

with Glib;            use Glib;
with Language;        use Language;

package body Items.Records is

   type Record_Iterator is new Generic_Iterator with record
      Item    : Record_Type_Access;
      Field   : Natural;
      Variant : Natural;
   end record;
   overriding procedure Next (Iter : in out Record_Iterator);
   overriding function At_End (Iter : Record_Iterator) return Boolean;
   overriding function Data
     (Iter : Record_Iterator) return Generic_Type_Access;
   overriding function Field_Name
     (Iter : Record_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type
     (Num_Fields : Natural) return Generic_Type_Access
   is
      R : constant Generic_Type_Access := new Record_Type (Num_Fields);
   begin
      --  A null record is always valid.
      if Num_Fields = 0 then
         R.Valid := True;
      end if;
      return R;
   end New_Record_Type;

   ----------------
   -- Num_Fields --
   ----------------

   function Num_Fields (Item : Record_Type) return Natural is
   begin
      return Item.Num_Fields;
   end Num_Fields;

   --------------------
   -- Set_Field_Name --
   --------------------

   procedure Set_Field_Name
     (Item          : in out Record_Type;
      Index         : Positive;
      Name          : String;
      Variant_Parts : Natural := 0) is
   begin
      if Item.Fields (Index).Value /= null then
         Free (Item.Fields (Index).Value, Only_Value => False);
      end if;

      if Item.Fields (Index).Variant_Part /= null then
         Free (Item.Fields (Index).Variant_Part);
      end if;

      if Item.Fields (Index).Name /= Null_Unbounded_String then
         Item.Fields (Index).Name := Null_Unbounded_String;
      end if;

      if Variant_Parts = 0 then
         Item.Fields (Index) := Record_Field'
           (Name         => To_Unbounded_String (Name),
            Value        => null,
            Variant_Part => null);

      else
         Item.Fields (Index) := Record_Field'
           (Name         => To_Unbounded_String (Name),
            Value        => null,
            Variant_Part => new Record_Type_Array (1 .. Variant_Parts));
      end if;
   end Set_Field_Name;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name
     (Item  : Record_Type;
      Index : Positive) return String is
   begin
      return To_String (Item.Fields (Index).Name);
   end Get_Field_Name;

   -----------------------
   -- Set_Variant_Field --
   -----------------------

   procedure Set_Variant_Field
     (Item          : in out Record_Type;
      Index         : Positive;
      Variant_Index : Positive;
      Value         : access Record_Type'Class) is
   begin
      if Item.Fields (Index).Variant_Part /= null then
         if Item.Fields (Index).Variant_Part (Variant_Index) /= null then
            Free (Item.Fields (Index).Variant_Part (Variant_Index),
                  Only_Value => False);
         end if;

         Item.Fields (Index).Variant_Part (Variant_Index) :=
           Record_Type_Access (Value);

         --  If there is at least one field, the record is valid.
         Item.Valid := True;
      end if;
   end Set_Variant_Field;

   -----------------------
   -- Get_Variant_Parts --
   -----------------------

   function Get_Variant_Parts
     (Item  : Record_Type;
      Field : Positive) return Natural is
   begin
      if Item.Fields (Field).Variant_Part = null then
         return 0;
      else
         return Item.Fields (Field).Variant_Part'Length;
      end if;
   end Get_Variant_Parts;

   -----------------------
   -- Find_Variant_Part --
   -----------------------

   function Find_Variant_Part
     (Item     : Record_Type;
      Field    : Positive;
      Contains : String) return Generic_Type_Access is
   begin
      for J in Item.Fields (Field).Variant_Part'Range loop
         Item.Fields (Field).Variant_Part (J).Valid := False;
      end loop;

      for J in Item.Fields (Field).Variant_Part'Range loop
         if (Contains'Length = 0
             and then Item.Fields (Field).Variant_Part (J).Fields'Length = 0)
           or else
             (Item.Fields (Field).Variant_Part (J).Fields'Length /= 0
              and then
                Item.Fields (Field).Variant_Part (J).Fields (1).Name =
                Contains)
         then
            Item.Fields (Field).Variant_Part (J).Valid := True;
            return Generic_Type_Access (Item.Fields (Field).Variant_Part (J));
         end if;
      end loop;

      return null;
   end Find_Variant_Part;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : String) is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name = Field then
            if Item.Fields (J).Value /= null then
               Free (Item.Fields (J).Value, Only_Value => False);
            end if;

            Item.Fields (J).Value := Generic_Type_Access (Value);
            Item.Fields (J).Value.Valid := True;
         end if;
      end loop;

      --  If there is at least one field, the record is valid.
      Item.Valid := True;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : Positive) is
   begin
      if Item.Fields (Field).Value /= null then
         Free (Item.Fields (Field).Value, Only_Value => False);
      end if;

      Item.Fields (Field).Value := Generic_Type_Access (Value);
      Item.Fields (Field).Value.Valid := True;
      --  If there is at least one field, the record is valid.
      Item.Valid := True;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Item  : Record_Type;
      Field : String) return Generic_Type_Access is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name = Field then
            return Item.Fields (J).Value;
         end if;
      end loop;

      return null;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Item  : Record_Type;
      Field : Positive) return Generic_Type_Access is
   begin
      return Item.Fields (Field).Value;
   end Get_Value;

   --------------------
   -- New_Union_Type --
   --------------------

   function New_Union_Type
     (Num_Fields : Positive) return Generic_Type_Access is
   begin
      return new Union_Type (Num_Fields);
   end New_Union_Type;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self    : not null access Record_Type) return String is
   begin
      if Self.Fields'Length = 0 then
         return "null record";
      else
         return "";  --  Value given in the fields
      end if;
   end Get_Simple_Value;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Record_Type;
      Only_Value : Boolean := False) is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Value /= null then
            Free (Item.Fields (J).Value, Only_Value);

            if Item.Fields (J).Variant_Part /= null then
               for V in Item.Fields (J).Variant_Part'Range loop
                  Free (Item.Fields (J).Variant_Part (V), Only_Value);
               end loop;
            end if;

            if not Only_Value then
               Item.Fields (J).Name := Null_Unbounded_String;
               Free (Item.Fields (J).Variant_Part);
            end if;
         end if;
      end loop;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Record_Type;
      Clone : in out Generic_Type_Access)
   is
      R : Record_Type_Access;
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);
      R := Record_Type_Access (Clone);

      for J in Item.Fields'Range loop
         R.Fields (J).Name := Item.Fields (J).Name;
         --  Duplicate the type structure, but not the value itself.

         if Item.Fields (J).Value = null then
            R.Fields (J).Value := null;
         else
            R.Fields (J).Value := Items.Clone (Item.Fields (J).Value.all);
         end if;

         if Item.Fields (J).Variant_Part /= null then
            R.Fields (J).Variant_Part :=
              new Record_Type_Array'(Item.Fields (J).Variant_Part.all);
            for V in Item.Fields (J).Variant_Part'Range loop
               R.Fields (J).Variant_Part (V) := Record_Type_Access
                 (Items.Clone (Item.Fields (J).Variant_Part (V).all));
            end loop;
         end if;
      end loop;
   end Clone_Dispatching;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self   : not null access Record_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
      R : Collapsible_Item;
   begin
      if not Self.Valid then
         Rect.Add_Child
           (Gtk_New_Image
              (Styles.Invisible,
               "gps-unknown-item-symbolic",
               Allow_Rescale => False, Width => 16.0, Height => 16.0));

      --  A null record ?
      elsif Self.Num_Fields = 0 then
         null;

      elsif not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);

      else
         Rect.Set_Style (Styles.Nested);

         if Show_Type (Mode)
           and then Self.Type_Name /= Null_Unbounded_String
         then
            Rect.Add_Child
              (Gtk_New_Text (Styles.Text_Font, Self.Get_Type_Name (Lang)),
               Margin => Margin);
         end if;

         for F in Self.Fields'Range loop
            --  not a variant part ?

            if Self.Fields (F).Value /= null then
               R := new Collapsible_Item_Record;
               R.For_Component := Self.Fields (F).Value;
               R.Initialize_Rect (Styles.Invisible);
               R.Set_Child_Layout (Horizontal_Stack);
               Rect.Add_Child (R, Margin => Margin);
               R.Add_Child
                 (Gtk_New_Text
                    (Styles.Text_Font,
                     To_String (Self.Fields (F).Name) & " => "));
               R.Add_Child
                 (Self.Fields (F).Value.Build_Display
                  (Record_Field_Name (Lang, Name,
                       To_String (Self.Fields (F).Name)),
                       View, Lang, Mode));
            end if;

            --  a variant part ?

            if Self.Fields (F).Variant_Part /= null then
               for V in Self.Fields (F).Variant_Part'Range loop
                  if Self.Fields (F).Variant_Part (V).Valid then
                     R := new Collapsible_Item_Record;
                     R.For_Component := Self.Fields (F).Variant_Part (V);
                     R.Initialize_Rect (Styles.Invisible);
                     R.Set_Child_Layout (Horizontal_Stack);
                     Rect.Add_Child (R, Margin => Margin);
                     R.Add_Child
                       (Gtk_New_Text
                          (Styles.Text_Font,
                           To_String (Self.Fields (F).Name) & " => "));
                     R.Add_Child
                       (Self.Fields (F).Variant_Part (V).Build_Display
                        (Name, View, Lang, Mode));
                  end if;
               end loop;
            end if;
         end loop;
      end if;

      return Rect;
   end Build_Display;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Record_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      for F in Parent.Fields'Range loop
         if Parent.Fields (F).Value = Generic_Type_Access (Current) then
            Free (Parent.Fields (F).Value, Only_Value => False);
            Parent.Fields (F).Value := Generic_Type_Access (Replace_With);
            return Generic_Type_Access (Replace_With);
         end if;

         if Parent.Fields (F).Variant_Part /= null then
            for V in Parent.Fields (F).Variant_Part'Range loop
               if Generic_Type_Access (Parent.Fields (F).Variant_Part (V)) =
                 Generic_Type_Access (Current)
               then
                  Free (Parent.Fields (F).Variant_Part (V),
                        Only_Value => False);
                  Parent.Fields (F).Variant_Part (V) :=
                    Record_Type_Access (Replace_With);
                  return Generic_Type_Access (Replace_With);
               end if;
            end loop;
         end if;
      end loop;

      return null;
   end Replace;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Item : access Record_Type) return Generic_Iterator'Class
   is
      Iter : Record_Iterator;
   begin
      Iter.Item := Record_Type_Access (Item);
      Iter.Field := Item.Fields'First;
      Iter.Variant := Natural'Last;

      if Iter.Field <= Iter.Item.Fields'Last
        and then Iter.Item.Fields (Iter.Field).Variant_Part /= null
      then
         Iter.Variant := Iter.Item.Fields (Iter.Field).Variant_Part'First;
      end if;

      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Record_Iterator) is
      Var : Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;

   begin
      --  If the current field has a variant part, we need to iterate
      if Var /= null then
         Iter.Variant := Iter.Variant + 1;
         if Iter.Variant <= Var'Last then
            return;
         end if;
      end if;

      Iter.Field := Iter.Field + 1;
      if Iter.Field <= Iter.Item.Fields'Last then
         Var := Iter.Item.Fields (Iter.Field).Variant_Part;
         if Var /= null then
            Iter.Variant := Var'First;
         end if;
      end if;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Record_Iterator) return Boolean is
   begin
      return Iter.Field > Iter.Item.Fields'Last;
   end At_End;

   ----------
   -- Data --
   ----------

   overriding function Data
     (Iter : Record_Iterator) return Generic_Type_Access
   is
      Var : constant Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;
   begin
      if Var /= null then
         return Generic_Type_Access (Var (Iter.Variant));
      else
         return Iter.Item.Fields (Iter.Field).Value;
      end if;
   end Data;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Iter : Record_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String
   is
      Var : constant Record_Type_Array_Access :=
        Iter.Item.Fields (Iter.Field).Variant_Part;
   begin
      if Var /= null then
         return "<variant part>";
      else
         return Lang.Record_Field_Name
           (Base, To_String (Iter.Item.Fields (Iter.Field).Name));
      end if;
   end Field_Name;

   -----------------
   -- Draw_Border --
   -----------------

   procedure Draw_Border
     (Item : access Record_Type;
      Draw : Boolean := True) is
   begin
      if Draw then
         Item.Border_Spacing := Border_Spacing;
      else
         Item.Border_Spacing := 0;
      end if;
   end Draw_Border;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Record_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      if not (Item2.all in Record_Type'Class)
        or else Item1.Num_Fields /= Record_Type_Access (Item2).Num_Fields
      then
         return False;
      end if;

      for F in Item1.Fields'Range loop
         if (Item1.Fields (F).Variant_Part /= null
             and then Record_Type_Access
                        (Item2).Fields (F).Variant_Part = null)
           or else
           (Item1.Fields (F).Variant_Part = null
            and then Record_Type_Access
                       (Item2).Fields (F).Variant_Part /= null)
         then
            return False;
         end if;

         --  Protect against the null Name, which happens for variant parts
         if Item1.Fields (F).Value /= null
           and then Record_Type_Access (Item2).Fields (F).Value /= null
           and then not Structurally_Equivalent
           (Item1.Fields (F).Value,
            Record_Type_Access (Item2).Fields (F).Value)
         then
            return False;
         end if;

         if Item1.Fields (F).Variant_Part /= null then
            if Item1.Fields (F).Variant_Part'Length /=
              Record_Type_Access (Item2).Fields (F).Variant_Part'Length
            then
               return False;
            end if;

            for V in Item1.Fields (F).Variant_Part'Range loop
               if not Structurally_Equivalent
                 (Item1.Fields (F).Variant_Part (V),
                  Record_Type_Access (Item2).Fields (F).Variant_Part (V))
               then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Structurally_Equivalent;

end Items.Records;
