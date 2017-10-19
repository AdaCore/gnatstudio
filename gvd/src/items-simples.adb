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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Gtkada.Canvas_View;      use Gtkada.Canvas_View;
with Gtkada.Style;            use Gtkada.Style;

with Browsers;                use Browsers;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GVD.Canvas;              use GVD.Canvas;
with Language;                use Language;
with String_Utils;            use String_Utils;

package body Items.Simples is

   Unknown_Value_Str : constant String := "<unknown>";
   --  The string representation of a null value for the user.

   function Quote_Non_Printable_Characters (Str : String) return String;
   --  Protect non-printable characters in the string

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

   function Get_Value (Item : Simple_Type) return Unbounded_String is
   begin
      return Item.Value;
   end Get_Value;

   ------------------------------------
   -- Quote_Non_Printable_Characters --
   ------------------------------------

   function Quote_Non_Printable_Characters (Str : String) return String is
      Output : String (1 .. Str'Length * 4);
      Index  : Integer := Output'First;
   begin
      for S in Str'Range loop
         if not Is_Graphic (Str (S)) then
            declare
               Img : constant String :=
                 Integer'Image (Character'Pos (Str (S)));
            begin
               Output (Index) := '[';
               Output (Index + 1) := Img (Img'Last - 1);
               Output (Index + 2) := Img (Img'Last);
               Output (Index + 3) := ']';
               Index := Index + 4;
            end;
         else
            Output (Index) := Str (S);
            Index := Index + 1;
         end if;
      end loop;
      return Output (Output'First .. Index - 1);
   end Quote_Non_Printable_Characters;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item : in out Simple_Type; Value : String) is
      Q : constant String := Quote_Non_Printable_Characters (Value);
   begin
      Item.Has_Changed := Item.Value = Null_Unbounded_String
        or else To_String (Item.Value) /= Q;
      Item.Value := To_Unbounded_String (Q);
      Item.Valid := True;
   end Set_Value;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access Simple_Type) return String is
   begin
      if Self.Value = Null_Unbounded_String then
         return Unknown_Value_Str;
      else
         return To_String (Self.Value);
      end if;
   end Get_Simple_Value;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Simple_Type; Only_Value : Boolean := False) is
   begin
      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Simple_Type;
      Clone : in out Generic_Type_Access) is
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      if Item.Value /= Null_Unbounded_String then
         Simple_Type_Access (Clone).Value := Item.Value;
      end if;
   end Clone_Dispatching;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self   : not null access Simple_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
      T      : Text_Item;
      S      : constant Drawing_Style :=
        (if Self.Has_Changed then View.Modified else Styles.Text_Font);
   begin
      if not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);
      else
         if Show_Type (Mode)
           and then Self.Type_Name /= Null_Unbounded_String
         then
            T := Gtk_New_Text (S, Self.Get_Type_Name (Lang));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;

         if Show_Value (Mode)
           and then Self.Value /= Null_Unbounded_String
         then
            T := Gtk_New_Text (S, To_String (Self.Value));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;
      end if;

      return Rect;
   end Build_Display;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self   : not null access Access_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item
   is
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant Component_Item := new Xref_Item_Record;
      T      : Text_Item;
      S      : constant Drawing_Style :=
        (if Self.Has_Changed then View.Modified else Styles.Hyper_Link);
   begin
      Rect.Initialize_Component_Item (Styles, Self, Name);

      if not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);
      else
         if Show_Type (Mode)
           and then Self.Type_Name /= Null_Unbounded_String
         then
            T := Gtk_New_Text (S, Self.Get_Type_Name (Lang));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;

         if Self.Value /= Null_Unbounded_String
           and then Show_Value (Mode)
         then
            T := Gtk_New_Text (S, To_String (Self.Value));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;
      end if;

      return Rect;
   end Build_Display;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive (Item : access Simple_Type) is
   begin
      Item.Has_Changed := False;
   end Reset_Recursive;

   --------------------
   -- New_Range_Type --
   --------------------

   function New_Range_Type
     (Min, Max : Long_Integer) return Generic_Type_Access is
   begin
      return new Range_Type'(Simple_Type with Min => Min, Max => Max);
   end New_Range_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access is
   begin
      return new Mod_Type'(Simple_Type with Modulo => Modulo);
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

   -----------------------
   -- New_Debugger_Type --
   -----------------------

   function New_Debugger_Type
     (Cmd         : String;
      Split_Lines : Boolean := False) return Generic_Type_Access
   is
      Item : Debugger_Output_Type_Access;
   begin
      Item := new Debugger_Output_Type;
      Item.Refresh_Cmd := To_Unbounded_String (Cmd);
      Item.Split_Lines := Split_Lines;
      return Generic_Type_Access (Item);
   end New_Debugger_Type;

   ---------------------
   -- Refresh_Command --
   ---------------------

   function Refresh_Command (Item : Debugger_Output_Type) return String is
   begin
      return To_String (Item.Refresh_Cmd);
   end Refresh_Command;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access Debugger_Output_Type) return String
   is
      Value : Unbounded_String;
   begin
      if Self.Value.Is_Empty then
         return Unknown_Value_Str;
      elsif Self.Split_Lines then
         return "";   --  value is split into components
      else
         for L of Self.Value loop
            Append (Value, To_String (L.Value) & ASCII.LF);
         end loop;
         return To_String (Value);
      end if;
   end Get_Simple_Value;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Debugger_Output_Type;
      Clone : in out Generic_Type_Access) is
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);
      Debugger_Output_Type_Access (Clone).Refresh_Cmd := Item.Refresh_Cmd;
   end Clone_Dispatching;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item       : access Debugger_Output_Type;
      Only_Value : Boolean := False) is
   begin
      Item.Value.Clear;

      if not Only_Value then
         Item.Refresh_Cmd := Null_Unbounded_String;
      end if;

      if not Item.As_Record.Is_Empty then
         Free (Item.As_Record);
      end if;

      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -------------------
   -- Build_Display --
   -------------------

   overriding function Build_Display
     (Self : not null access Debugger_Output_Type;
      Name : String;
      View : not null access Debugger_Data_View_Record'Class;
      Lang : Language.Language_Access;
      Mode : Display_Mode) return Component_Item
   is
      pragma Unreferenced (Lang, Mode);
      Styles : constant access Browser_Styles := View.Get_View.Get_Styles;
      Rect   : constant Component_Item :=
        New_Component_Item (Styles, Self, Name);
   begin
      if not Self.Visible then
         Rect.Add_Child (View.Item_Hidden);
      else
         for L of Self.Value loop
            Rect.Add_Child
              (Gtk_New_Text
                 ((if L.Modified then View.Modified else Styles.Text_Font),
                  To_String (L.Value)));
         end loop;
      end if;

      return Rect;
   end Build_Display;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item : in out Debugger_Output_Type; Value : String)
   is
      S     : constant String := Do_Tab_Expansion (Value, 8);
      Old   : constant Line_Vector.Vector := Item.Value;
      Lines : String_List_Access := Split (S, ASCII.LF);
   begin
      --  Compute which lines have changed since the last update.

      Item.Value := Line_Vector.To_Vector (Lines'Length);
      for L in 1 .. Positive (Item.Value.Length) loop
         if Old.Is_Empty then
            Item.Value.Replace_Element
              (L,
               (Modified => True,
                Value    => To_Unbounded_String (Lines (L).all)));
         else
            Item.Value.Replace_Element
              (L,
               (Modified => L > Natural (Old.Length)
                or else Lines (L).all /= To_String (Old (L).Value),
                Value    => To_Unbounded_String (Lines (L).all)));
         end if;
      end loop;

      if Item.Split_Lines then
         Free (Item.As_Record);
         Item.As_Record := Type_Vector.To_Vector (Item.Value.Length);

         for L in 1 .. Integer (Item.Value.Length) loop
            Item.As_Record (L).Name := Null_Unbounded_String;
            Item.As_Record (L).Typ := new Simple_Type'
              (Base_Simple_Type with
               Value       => Item.Value (L).Value,
               Has_Changed => Item.Value (L).Modified);
         end loop;
      end if;

      Free (Lines);
      Item.Valid := True;
   end Set_Value;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive (Item : access Debugger_Output_Type) is

      procedure Mark_Unmodified (X : in out Line_Value);
      --  Mark X as unmodified

      procedure Mark_Unmodified (X : in out Line_Value) is
      begin
         X.Modified := False;
      end Mark_Unmodified;

   begin
      if not Item.Value.Is_Empty then
         for L in 1 .. Positive (Item.Value.Length) loop
            Item.Value.Update_Element (L, Mark_Unmodified'Access);
         end loop;
      end if;
   end Reset_Recursive;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Simple_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Simple_Type'Class
        and then Item1.Type_Name = Item2.Type_Name;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Range_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Range_Type'Class
        and then Item1.Min = Range_Type_Access (Item2).Min
        and then Item1.Max = Range_Type_Access (Item2).Max;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Mod_Type; Item2 : access Generic_Type'Class)
      return Boolean is
   begin
      return Item2.all in Mod_Type'Class
        and then Item1.Modulo = Mod_Type_Access (Item2).Modulo;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Access_Type; Item2 : access Generic_Type'Class)
      return Boolean
   is
      pragma Unreferenced (Item1);
   begin
      return Item2.all in Access_Type'Class;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Enum_Type; Item2 : access Generic_Type'Class)
      return Boolean
   is
      pragma Unreferenced (Item1);
   begin
      return Item2.all in Enum_Type'Class;
   end Structurally_Equivalent;

   -----------------------------
   -- Structurally_Equivalent --
   -----------------------------

   overriding function Structurally_Equivalent
     (Item1 : access Debugger_Output_Type; Item2 : access Generic_Type'Class)
      return Boolean
   is
      pragma Unreferenced (Item1, Item2);
   begin
      --  Never any aliasing
      return False;
   end Structurally_Equivalent;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self   : not null access Debugger_Output_Type)
      return Generic_Iterator'Class is
   begin
      if Self.As_Record.Is_Empty then
         return Create_Empty_Iterator;
      else
         return Start (Self.As_Record);
      end if;
   end Start;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self : not null access Simple_Type) return Boolean is
   begin
      return Self.Has_Changed;
   end Is_Changed;

end Items.Simples;
