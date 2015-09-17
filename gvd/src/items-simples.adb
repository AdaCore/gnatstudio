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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Browsers;                use Browsers;
with GNAT.IO;                 use GNAT.IO;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GVD.Canvas;              use GVD.Canvas;
with Language;                use Language;
with String_Utils;            use String_Utils;

package body Items.Simples is
   use type GNAT.Strings.String_Access;

   Line_Highlighted     : constant Character := '@';
   Line_Not_Highlighted : constant Character := ' ';
   --  Special characters inserted at the beginning of each line for the
   --  value of Debugger_Output_Type, that indicate whether the following line
   --  should be displayed in red or not.

   Null_Value_Str : constant String := "<null>";
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

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Simple_Type; Indent : Natural := 0) is
      pragma Unreferenced (Indent);
   begin
      if Value.Value = null then
         Put ("{Simple: " & Null_Value_Str & "}");
      else
         Put ("{Simple: " & Value.Value.all & "}");
      end if;
   end Print;

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

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Enum_Type; Indent : Natural := 0) is
      pragma Unreferenced (Indent);
   begin
      Put ("{Enumeration = ");

      if Value.Value = null then
         Put ("<Unknown>}");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

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

   -----------
   -- Print --
   -----------

   overriding procedure Print (Value : Range_Type; Indent : Natural := 0) is
      pragma Unreferenced (Indent);
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

   overriding procedure Print (Value : Mod_Type; Indent : Natural := 0) is
      pragma Unreferenced (Indent);
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

   overriding procedure Print (Value : Access_Type; Indent : Natural := 0) is
      pragma Unreferenced (Indent);
   begin
      Put ("{Access ");

      if Value.Value = null then
         Put (Null_Value_Str & ")");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------------------
   -- New_Debugger_Type --
   -----------------------

   function New_Debugger_Type (Cmd : String) return Generic_Type_Access is
      Item : Debugger_Output_Type_Access;
   begin
      Item := new Debugger_Output_Type;
      Item.Refresh_Cmd := new String'(Cmd);
      return Generic_Type_Access (Item);
   end New_Debugger_Type;

   ---------------------
   -- Refresh_Command --
   ---------------------

   function Refresh_Command (Item : Debugger_Output_Type) return String is
   begin
      return Item.Refresh_Cmd.all;
   end Refresh_Command;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (Value : Debugger_Output_Type; Indent : Natural := 0)
   is
      pragma Unreferenced (Indent);
   begin
      if Value.Value = null then
         Put ("{Debugger_Type: " & Null_Value_Str & "}");
      else
         Put
           ("{Debugger_Type: " & Join ("" & ASCII.LF, Value.Value.all) & "}");
      end if;
   end Print;

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
      GNAT.Strings.Free (Item.Value);

      if not Only_Value then
         GNAT.Strings.Free (Item.Refresh_Cmd);
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
         for L in Self.Value'Range loop
            declare
               V : constant String := Self.Value (L).all;
            begin
               Rect.Add_Child
                 (Gtk_New_Text
                    ((if V (V'First) = Line_Highlighted
                     then View.Modified else Styles.Text_Font),
                     V (V'First + 1 .. V'Last)));
            end;
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
      S         : constant String := Do_Tab_Expansion (Value, 8);
      Old       : GNAT.Strings.String_List_Access := Item.Value;
      Index_Old : Positive;
      Lines     : String_List_Access := Split (S, ASCII.LF);
   begin
      Item.Value := new String_List (Lines'Range);

      for L in Item.Value'Range loop
         if Old = null then
            Item.Value (L) := new String'(Line_Highlighted & Lines (L).all);
         else
            Index_Old := L - Item.Value'First + Old'First;
            if Index_Old > Old'Last then
               Item.Value (L) := new String'(Line_Highlighted & Lines (L).all);
            else
               declare
                  OV : constant String := Old (Index_Old).all;
               begin
                  if  Lines (L).all /= OV (OV'First + 1 .. OV'Last) then
                     Item.Value (L) := new String'
                       (Line_Highlighted & Lines (L).all);
                  else
                     Item.Value (L) := new String'
                       (Line_Not_Highlighted & Lines (L).all);
                  end if;
               end;
            end if;
         end if;
      end loop;

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
         for L in Item.Value'Range loop
            Item.Value (L) (Item.Value (L)'First) := Line_Not_Highlighted;
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

end Items.Simples;
