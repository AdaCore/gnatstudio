------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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
with Browsers;                use Browsers;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GVD.Canvas;              use GVD.Canvas;
with Language;                use Language;
with String_Utils;            use String_Utils;

package body Items.Simples is
   use type GNAT.Strings.String_Access;

   Unknown_Value_Str : constant String := "<unknown>";
   --  The string representation of a null value for the user.

   function Quote_Non_Printable_Characters (Str : String) return String;
   --  Protect non-printable characters in the string

   type Xref_Item_Record is
     new Component_Item_Record and Clickable_Item with record
        null;
     end record;
   overriding procedure On_Click
     (Self    : not null access Xref_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

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

   function Get_Value (Item : Simple_Type) return GNAT.Strings.String_Access is
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
      Item.Has_Changed := Item.Value = null or else Item.Value.all /= Q;
      GNAT.Strings.Free (Item.Value);
      Item.Value := new String'(Q);
      Item.Valid := True;
   end Set_Value;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access Simple_Type) return String is
   begin
      if Self.Value = null then
         return Unknown_Value_Str;
      else
         return Self.Value.all;
      end if;
   end Get_Simple_Value;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item : access Simple_Type; Only_Value : Boolean := False) is
   begin
      GNAT.Strings.Free (Item.Value);
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

      if Item.Value /= null then
         Simple_Type_Access (Clone).Value := new String'(Item.Value.all);
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
           and then Self.Type_Name /= null
         then
            T := Gtk_New_Text (S, Self.Get_Type_Name (Lang));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;

         if Show_Value (Mode) and then Self.Value /= null then
            T := Gtk_New_Text (S, Self.Value.all);
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;
      end if;

      return Rect;
   end Build_Display;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Xref_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (View, Details);
   begin
      Dereference_Item (Self);
   end On_Click;

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
           and then Self.Type_Name /= null
         then
            T := Gtk_New_Text (S, Self.Get_Type_Name (Lang));
            T.Set_Height_Range (Min => (Unit_Pixels, 10.0));
            Rect.Add_Child (T);
         end if;

         if Self.Value /= null and then Show_Value (Mode) then
            T := Gtk_New_Text (S, Self.Value.all);
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
      Item.Refresh_Cmd := new String'(Cmd);
      Item.Split_Lines := Split_Lines;
      return Generic_Type_Access (Item);
   end New_Debugger_Type;

   ---------------------
   -- Refresh_Command --
   ---------------------

   function Refresh_Command (Item : Debugger_Output_Type) return String is
   begin
      return Item.Refresh_Cmd.all;
   end Refresh_Command;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access Debugger_Output_Type) return String
   is
      Value : Unbounded_String;
   begin
      if Self.Value = null then
         return Unknown_Value_Str;
      elsif Self.Split_Lines then
         return "";   --  value is split into components
      else
         for L of Self.Value.all loop
            Append (Value, L.Value.all & ASCII.LF);
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
      Debugger_Output_Type_Access (Clone).Refresh_Cmd :=
        new String'(Item.Refresh_Cmd.all);
   end Clone_Dispatching;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Item       : access Debugger_Output_Type;
      Only_Value : Boolean := False) is
   begin
      Free (Item.Value);

      if not Only_Value then
         GNAT.Strings.Free (Item.Refresh_Cmd);
      end if;

      if Item.As_Record /= null then
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
         for L of Self.Value.all loop
            Rect.Add_Child
              (Gtk_New_Text
                 ((if L.Modified then View.Modified else Styles.Text_Font),
                  L.Value.all));
         end loop;
      end if;

      return Rect;
   end Build_Display;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Line_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Line_Array, Line_Array_Access);
   begin
      if Self /= null then
         for L in Self'Range loop
            Free (Self (L).Value);
         end loop;
         Unchecked_Free (Self);
      end if;
   end Free;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Item : in out Debugger_Output_Type; Value : String)
   is
      S         : constant String := Do_Tab_Expansion (Value, 8);
      Old       : Line_Array_Access := Item.Value;
      Index_Old : Positive;
      Lines     : String_List_Access := Split (S, ASCII.LF);
   begin
      --  Compute which lines have changed since the last update.

      Item.Value := new Line_Array (Lines'Range);
      for L in Item.Value'Range loop
         if Old = null then
            Item.Value (L) :=
              (Modified => True,
               Value    => new String'(Lines (L).all));
         else
            Index_Old := L - Item.Value'First + Old'First;
            Item.Value (L) :=
              (Modified =>
                 Index_Old > Old'Last
                 or else Lines (L).all /= Old (Index_Old).Value.all,
               Value    => new String'(Lines (L).all));
         end if;
      end loop;

      if Item.Split_Lines then
         Free (Item.As_Record);
         Item.As_Record := new Type_Array (Item.Value'Range);

         for L in Item.Value'Range loop
            Item.As_Record (L).Name := new String'("");
            Item.As_Record (L).Typ := new Simple_Type'
              (Base_Simple_Type with
               Value       => new String'(Item.Value (L).Value.all),
               Has_Changed => Item.Value (L).Modified);
         end loop;
      end if;

      Free (Lines);
      Free (Old);

      Item.Valid := True;
   end Set_Value;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive (Item : access Debugger_Output_Type) is
   begin
      if Item.Value /= null then
         for L of Item.Value.all loop
            L.Modified := False;
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
        and then Item1.Type_Name.all = Item2.Type_Name.all;
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
      if Self.As_Record = null then
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
