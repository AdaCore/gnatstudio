-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.IO;  use GNAT.IO;
with Ada.Tags; use Ada.Tags;

package body Generic_Values is

   ---------------------
   -- New_Simple_Type --
   ---------------------

   function New_Simple_Type return Generic_Type_Access is
   begin
      return new Simple_Type;
   end New_Simple_Type;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item : Simple_Type) return String_Access is
   begin
      return Item.Value;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item : in out Simple_Type; Value : String) is
   begin
      if Item.Value /= null then
         Free (Item.Value);
      end if;
      Item.Value := new String'(Value);
   end Set_Value;

   --------------------
   -- New_Range_Type --
   --------------------

   function New_Range_Type (Min, Max : Long_Integer)
                           return Generic_Type_Access
   is
   begin
      return new Range_Type'(Value => null,
                             Min   => Min,
                             Max   => Max);
   end New_Range_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access is
   begin
      return new Mod_Type'(Value  => null,
                           Modulo => Modulo);
   end New_Mod_Type;

   ---------------------
   -- New_Access_Type --
   ---------------------

   function New_Access_Type return Generic_Type_Access is
   begin
      return new Access_Type;
   end New_Access_Type;

   -------------------
   -- New_Enum_Type --
   -------------------

   function New_Enum_Type return Generic_Type_Access is
   begin
      return new Enum_Type;
   end New_Enum_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type (Num_Dimensions : Positive)
                           return Generic_Type_Access
   is
   begin
      return new Array_Type (Num_Dimensions => Num_Dimensions);
   end New_Array_Type;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions (Item : in out Array_Type;
                             Dim  : Positive;
                             Size : Dimension)
   is
   begin
      Item.Dimensions (Dim) := Size;
   end Set_Dimensions;

   --------------------
   -- Num_Dimensions --
   --------------------

   function Num_Dimensions (Item : Array_Type) return Positive is
   begin
      return Item.Num_Dimensions;
   end Num_Dimensions;

   --------------------
   -- Get_Dimensions --
   --------------------

   function Get_Dimensions (Item : Array_Type;
                            Dim  : Positive)
                           return Dimension
   is
   begin
      return Item.Dimensions (Dim);
   end Get_Dimensions;

   -------------------
   -- Set_Item_Type --
   -------------------

   procedure Set_Item_Type
     (Item     : in out Array_Type;
      The_Type : access Generic_Type'Class)
   is
   begin
      Item.Item_Type := Generic_Type_Access (The_Type);
   end Set_Item_Type;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type (Item : Array_Type)
                          return Generic_Type_Access
   is
   begin
      return Item.Item_Type;
   end Get_Item_Type;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Array_Type;
                        Elem_Value : access Generic_Type'Class;
                        Elem_Index : Long_Integer;
                        Repeat_Num : Positive := 1)
   is
      Tmp : Array_Item_Array_Access;
   begin
      if Item.Values = null  then
         Item.Values := new Array_Item_Array (1 .. 100);
         Item.Last_Value := 1;
      else
         Item.Last_Value := Item.Last_Value + 1;
      end if;

      if Item.Last_Value > Item.Values'Last then
         Tmp := Item.Values;
         Item.Values := new Array_Item_Array (1 .. 2 * Item.Values'Last);
         Item.Values (1 .. Tmp'Last) := Tmp.all;
         Free (Tmp);
      end if;

      if Item.Values (Item.Last_Value).Value /= null then
         Free (Item.Values (Item.Last_Value).Value);
      end if;
      if Repeat_Num = 1 then
         Item.Values (Item.Last_Value) :=
           Array_Item'(Index => Elem_Index,
                       Value => Generic_Type_Access (Elem_Value));
      else
         Item.Values (Item.Last_Value) :=
           Array_Item'(Index => Elem_Index,
                       Value => new Repeat_Type'
                         (Repeat_Num => Repeat_Num,
                          Value      => Generic_Type_Access (Elem_Value)));
      end if;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item       : Array_Type;
                       Elem_Index : Long_Integer)
                      return Generic_Type_Access
   is
   begin
      for J in Item.Values'Range loop
         if Item.Values (J).Index = Elem_Index then
            return Item.Values (J).Value;

         elsif Item.Values (J).Value'Tag = Repeat_Type'Tag
           and then Item.Values (J).Index < Elem_Index
           and then Item.Values (J).Index
           + Long_Integer (Repeat_Type_Access
                           (Item.Values (J).Value).Repeat_Num) > Elem_Index
         then
            return Repeat_Type_Access (Item.Values (J).Value).Value;
         end if;
      end loop;
      return null;
   end Get_Value;

   -------------------
   -- Shrink_Values --
   -------------------

   procedure Shrink_Values (Item : in out Array_Type) is
      Tmp : Array_Item_Array_Access;
   begin
      Tmp := Item.Values;
      Item.Values := new Array_Item_Array (1 .. Item.Last_Value);
      Item.Values.all := Tmp (1 .. Item.Last_Value);
      Free (Tmp);
   end Shrink_Values;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type (Num_Fields : Natural)
                            return Generic_Type_Access
   is
   begin
      return new Record_Type (Num_Fields);
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

   procedure Set_Field_Name (Item          : in out Record_Type;
                             Index         : Positive;
                             Name          : String;
                             Variant_Parts : Natural := 0)
   is
   begin
      if Item.Fields (Index).Value /= null then
         Free (Item.Fields (Index).Value);
      end if;
      if Item.Fields (Index).Variant_Part /= null then
         Free (Item.Fields (Index).Variant_Part);
      end if;

      if Variant_Parts = 0 then
         Item.Fields (Index) := Record_Field'
           (Name          => new String'(Name),
            Value        => null,
            Variant_Part => null);
      else
         Item.Fields (Index) := Record_Field'
           (Name         => new String'(Name),
            Value        => null,
            Variant_Part => new Record_Type_Array (1 .. Variant_Parts));
      end if;
   end Set_Field_Name;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name (Item  : in Record_Type;
                            Index : Positive)
                           return String_Access
   is
   begin
      return Item.Fields (Index).Name;
   end Get_Field_Name;

   -----------------------
   -- Set_Variant_Field --
   -----------------------

   procedure Set_Variant_Field (Item          : in out Record_Type;
                                Index         : Positive;
                                Variant_Index : Positive;
                                Value         : access Record_Type'Class)
   is
   begin
      if Item.Fields (Index).Variant_Part /= null then
         Item.Fields (Index).Variant_Part (Variant_Index) :=
           Record_Type_Access (Value);
      end if;
   end Set_Variant_Field;

   -----------------------
   -- Get_Variant_Parts --
   -----------------------

   function Get_Variant_Parts (Item  : Record_Type;
                               Field : Positive)
                              return Natural
   is
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

   function Find_Variant_Part (Item     : Record_Type;
                               Field    : Positive;
                               Contains : String)
                              return Generic_Type_Access
   is
   begin
      for J in Item.Fields (Field).Variant_Part'Range loop
         if Item.Fields (Field).Variant_Part (J).Fields (1).Name.all = Contains
         then
            return Generic_Type_Access (Item.Fields (Field).Variant_Part (J));
         end if;
      end loop;
      return null;
   end Find_Variant_Part;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : String)
   is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name.all = Field then
            Free (Item.Fields (J).Value);
            Item.Fields (J).Value := Generic_Type_Access (Value);
         end if;
      end loop;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : Positive)
   is
   begin
      Free (Item.Fields (Field).Value);
      Item.Fields (Field).Value := Generic_Type_Access (Value);
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item  : Record_Type;
                       Field : String)
                      return Generic_Type_Access
   is
   begin
      for J in Item.Fields'Range loop
         if Item.Fields (J).Name.all = Field then
            return Item.Fields (J).Value;
         end if;
      end loop;
      return null;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Item  : Record_Type;
                       Field : Positive)
                      return Generic_Type_Access
   is
   begin
      return Item.Fields (Field).Value;
   end Get_Value;

   --------------------
   -- New_Union_Type --
   --------------------

   function New_Union_Type (Num_Fields : Positive)
                           return Generic_Type_Access
   is
   begin
      return new Union_Type (Num_Fields);
   end New_Union_Type;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Simple_Type) is
   begin
      if Value.Value = null then
         Put ("{Simple: <null>}");
      else
         Put ("{Simple: " & Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Range_Type) is
   begin
      Put ("{Range" & Value.Min'Img & " .." & Value.Max'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Mod_Type) is
   begin
      Put ("{Modulo " & Value.Modulo'Img & " = ");
      if Value.Value /= null then
         Put (Value.Value.all);
      end if;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Access_Type) is
   begin
      Put ("{Access ");
      if Value.Value = null then
         Put ("<null>)");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Array_Type) is
   begin
      Put ("{Array (");
      for J in 1 .. Value.Num_Dimensions loop
         Put (Value.Dimensions (J).First'Img & " .. "
              & Value.Dimensions (J).Last'Img);
         if J /= Value.Num_Dimensions then
            Put (", ");
         end if;
      end loop;

      Put (") of ");
      Print (Value.Item_Type.all);

      Put ("= (");
      if Value.Values /= null then
         for J in 1 .. Value.Last_Value loop
            Put (Value.Values (J).Index'Img & " => ");
            Print (Value.Values (J).Value.all);
            if J /= Value.Values'Last then
               Put (", ");
            end if;
         end loop;
      end if;
      Put (")}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Repeat_Type) is
   begin
      Put ("{<" & Value.Repeat_Num'Img & " times> : ");
      if Value.Value /= null then
         Print (Value.Value.all);
         Put ("}");
      else
         Put ("<null>}");
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Record_Type) is
   begin
      Put ("{Record: ");
      if Value.Fields'Length = 0 then
         Put ("null record");
      end if;
      for J in Value.Fields'Range loop
         if Value.Fields (J).Variant_Part /= null then
            Put ("<variant_part on "
                 & Value.Fields (J).Name.all
                 & "> => ");
            for P in Value.Fields (J).Variant_Part'Range loop
               Put ("{");
               Print (Value.Fields (J).Variant_Part (P).all);
               Put ("}");
            end loop;

         else
            Put (Value.Fields (J).Name.all & " => ");
            if Value.Fields (J).Value /= null then
               Print (Value.Fields (J).Value.all);
            end if;
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
         end if;
      end loop;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Union_Type) is
   begin
      Put ("{Union: ");
      for J in Value.Fields'Range loop
         Put (Value.Fields (J).Name.all & " => ");
         if Value.Fields (J).Value /= null then
            Print (Value.Fields (J).Value.all);
         end if;
         if J /= Value.Fields'Last then
            Put (", ");
         end if;
      end loop;
      Put ("}");
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Enum_Type) is
   begin
      Put ("{Enumeration = ");
      if Value.Value = null then
         Put ("<Unknown>}");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Simple_Type) is
   begin
      Free (Value.Value);
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Array_Type) is
   begin
      if Value.Values /= null then
         for J in 1 .. Value.Last_Value loop
            Clear_Value (Value.Values (J).Value.all);
         end loop;
         Free (Value.Values);
      end if;
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Repeat_Type) is
   begin
      if Value.Value /= null then
         Clear_Value (Value.Value.all);
      end if;
   end Clear_Value;

   -----------------
   -- Clear_Value --
   -----------------

   procedure Clear_Value (Value : in out Record_Type) is
   begin
      for J in Value.Fields'Range loop
         if Value.Fields (J).Value /= null then
            Clear_Value (Value.Fields (J).Value.all);
            if Value.Fields (J).Variant_Part /= null then
               for V in Value.Fields (J).Variant_Part'Range loop
                  Clear_Value (Value.Fields (J).Variant_Part (V).all);
               end loop;
            end if;
         end if;
      end loop;
   end Clear_Value;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Simple_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Simple_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Range_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Range_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Mod_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Mod_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Access_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Access_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Enum_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Enum_Type'(Value);
   begin
      Simple_Type_Access (R).Value := null;
      return R;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Array_Type)
                  return Generic_Type_Access
   is
      R : Array_Type_Access := new Array_Type'(Value);
   begin
      R.Values := null;
      R.Item_Type := Clone (Value.Item_Type.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Repeat_Type)
                  return Generic_Type_Access
   is
      R : Repeat_Type_Access := new Repeat_Type'(Value);
   begin
      R.Value := Clone (Value.Value.all);
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Record_Type)
                  return Generic_Type_Access
   is
      R : Record_Type_Access := new Record_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(R.Fields (J).Name.all);
         R.Fields (J).Value := Clone (R.Fields (J).Value.all);
         if R.Fields (J).Variant_Part /= null then
            R.Fields (J).Variant_Part
              := new Record_Type_Array'(R.Fields (J).Variant_Part.all);
            for V in R.Fields (J).Variant_Part'Range loop
               R.Fields (J).Variant_Part (V) := Record_Type_Access
                 (Clone (R.Fields (J).Variant_Part (V).all));
            end loop;
         end if;
      end loop;
      return Generic_Type_Access (R);
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Union_Type)
                  return Generic_Type_Access
   is
      R : Union_Type_Access := new Union_Type'(Value);
   begin
      for J in R.Fields'Range loop
         R.Fields (J).Name := new String'(R.Fields (J).Name.all);
         R.Fields (J).Value := Clone (R.Fields (J).Value.all);
      end loop;
      return Generic_Type_Access (R);
   end Clone;

end Generic_Values;
