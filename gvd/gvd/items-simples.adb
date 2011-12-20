------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Glib;            use Glib;
with Cairo;           use Cairo;

with Pango.Cairo;     use Pango.Cairo;
with Pango.Layout;    use Pango.Layout;

with Gdk.Cairo;       use Gdk.Cairo;
with Gdk.Color;       use Gdk.Color;

with Gtkada.Style;    use Gtkada.Style;

with Language;        use Language;

with String_Utils;    use String_Utils;

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

   procedure Paint_Simple
     (Item    : in out Simple_Type'Class;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      Color   : Gdk_Color;
      X, Y    : Gint := 0);
   --  Paint a simple type or one of its children

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
   begin
      if Item.Value /= null then
         if Item.Value.all /= Value then
            Item.Has_Changed := True;
         end if;

         GNAT.Strings.Free (Item.Value);
      end if;

      Item.Value := new String'(Quote_Non_Printable_Characters (Value));
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

   ------------------
   -- Paint_Simple --
   ------------------

   procedure Paint_Simple
     (Item    : in out Simple_Type'Class;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      Color   : Gdk_Color;
      X, Y    : Gint := 0)
   is
      Text_Color : Gdk_Color := Color;
      Y2         : Gint := Y;
      W, H       : Gint;

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y2;

      if not Item.Valid or else Item.Value = null then
         Draw_Pixbuf (Cr, Context.Unknown_Pixmap, X + Border_Spacing, Y2);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Cr, Context.Selection_Color,
            Filled => True,
            X      => X,
            Y      => Y2,
            Width  => Item.Width,
            Height => Item.Height);
      end if;

      if Item.Has_Changed then
         Text_Color := Context.Modified_Color;
      end if;

      if Show_Type (Mode)
        and then Item.Type_Name /= null
      then
         Set_Text (Context.Type_Layout, Get_Type_Name (Item'Access, Lang));
         Set_Source_Color (Cr, Text_Color);
         Move_To (Cr, Gdouble (X), Gdouble (Y2));
         Pango.Cairo.Show_Layout (Cr, Context.Type_Layout);
         Get_Pixel_Size (Context.Type_Layout, W, H);
         Y2 := Y2 + H;
      end if;

      if Show_Value (Mode) then
         Set_Text (Context.Text_Layout, Item.Value.all);
         Set_Source_Color (Cr, Text_Color);
         Move_To (Cr, Gdouble (X), Gdouble (Y2));
         Pango.Cairo.Show_Layout (Cr, Context.Text_Layout);
      end if;
   end Paint_Simple;

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Simple_Type;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Gint := 0)
   is
   begin
      Paint_Simple (Item, Context, Cr, Lang, Mode, Context.Foreground, X, Y);
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Item           : in out Simple_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      pragma Unreferenced (Hide_Big_Items);

      Unknown_Height, Unknown_Width, W, H  : Glib.Gint;
   begin
      Unknown_Width := Get_Width (Context.Unknown_Pixmap);
      Unknown_Height := Get_Height (Context.Unknown_Pixmap);

      Item.Width := Unknown_Width;
      Item.Height := 0;

      if Show_Type (Mode)
        and then Item.Type_Name /= null
      then
         Set_Text (Context.Type_Layout, Get_Type_Name (Item'Access, Lang));
         Get_Pixel_Size (Context.Type_Layout, W, H);
         Item.Width  := Gint'Max (W, Item.Width);
         Item.Height := H;
      end if;

      if Show_Value (Mode) then
         if Item.Value = null then
            Set_Text (Context.Text_Layout, Null_Value_Str);
         else
            Set_Text (Context.Text_Layout, Item.Value.all);
         end if;

         Get_Pixel_Size (Context.Text_Layout, W, H);
         Item.Width := Gint'Max (W, Item.Width);
         Item.Height := Item.Height + H;
      end if;

      if not Item.Valid then
         Item.Height := Unknown_Height;
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   overriding function Get_Component_Name
     (Item : access Simple_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String
   is
      pragma Unreferenced (Item, Lang, X, Y);
   begin
      return Name;
   end Get_Component_Name;

   overriding function Get_Component_Name
     (Item : access Simple_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String
   is
      pragma Unreferenced (Item, Lang, Comp);
   begin
      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Item : access Simple_Type;
      X, Y : Glib.Gint) return Generic_Type_Access
   is
      pragma Unreferenced (X, Y);
   begin
      return Generic_Type_Access (Item);
   end Get_Component;

   -------------
   -- Replace --
   -------------

   overriding function Replace
     (Parent       : access Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access
   is
      pragma Unreferenced (Parent, Current, Replace_With);
   begin
      return null;
   end Replace;

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

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Access_Type;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Glib.Gint := 0) is
   begin
      Paint_Simple (Item, Context, Cr, Lang, Mode, Context.Xref_Color, X, Y);
   end Paint;

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
         Put ("{Debugger_Type: " & Value.Value.all & "}");
      end if;
   end Print;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   overriding procedure Clone_Dispatching
     (Item  : Debugger_Output_Type;
      Clone : in out Generic_Type_Access) is
   begin
      Clone_Dispatching (Simple_Type (Item), Clone);
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
      if not Only_Value then
         GNAT.Strings.Free (Item.Refresh_Cmd);
      end if;

      Free (Simple_Type (Item.all)'Access, Only_Value);
   end Free;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Item           : in out Debugger_Output_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False)
   is
      pragma Unreferenced (Hide_Big_Items, Lang, Mode);
   begin
      if Item.Valid and then Item.Value /= null then
         Set_Text (Context.Text_Layout, Item.Value.all);
         Get_Pixel_Size (Context.Text_Layout, Item.Width, Item.Height);
      else
         Item.Width := Get_Width (Context.Unknown_Pixmap);
         Item.Height := Get_Height (Context.Unknown_Pixmap);
      end if;
   end Size_Request;

   -----------
   -- Paint --
   -----------

   overriding procedure Paint
     (Item    : in out Debugger_Output_Type;
      Context : Drawing_Context;
      Cr      : Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Gint := 0)
   is
      pragma Unreferenced (Lang, Mode);
      Text_Color : Gdk_Color;
      Line       : Gint := Y;
      Line_Start : Positive;
      W, H       : Gint;

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid or else Item.Value = null then
         Draw_Pixbuf (Cr, Context.Unknown_Pixmap, X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Cr, Context.Selection_Color,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
      end if;

      Line_Start := Item.Value'First;

      for J in Item.Value'Range loop
         if Item.Value (J) = ASCII.LF then
            if Item.Value (Line_Start) = Line_Highlighted then
               Text_Color := Context.Modified_Color;
            else
               Text_Color := Context.Foreground;
            end if;

            Set_Text
              (Context.Text_Layout, Item.Value (Line_Start + 1 .. J - 1));
            Set_Source_Color (Cr, Text_Color);
            Move_To (Cr, Gdouble (X), Gdouble (Line));
            Show_Layout (Cr, Context.Text_Layout);
            Get_Pixel_Size (Context.Text_Layout, W, H);
            Line := Line + H;
            Line_Start := J + 1;
         end if;
      end loop;

      if Item.Value (Line_Start) = Line_Highlighted then
         Text_Color := Context.Modified_Color;
      else
         Text_Color := Context.Foreground;
      end if;

      Set_Text
        (Context.Text_Layout, Item.Value (Line_Start + 1 .. Item.Value'Last));
      Set_Source_Color (Cr, Text_Color);
      Move_To (Cr, Gdouble (X), Gdouble (Line));
      Show_Layout (Cr, Context.Text_Layout);
   end Paint;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Item : in out Debugger_Output_Type; Value : String)
   is
      S              : constant String := Do_Tab_Expansion (Value, 8);
      V              : GNAT.Strings.String_Access := Item.Value;

      Index_New      : Positive := S'First;
      Line_Start_New : Positive;
      Index_Old      : Positive;
      Line_Start_Old : Positive;
      Index_Item     : Positive := 1;

      Num_Lines      : Natural := 1;

   begin
      --  Count the number of lines

      for J in S'Range loop
         if S (J) = ASCII.LF then
            Num_Lines := Num_Lines + 1;
         end if;
      end loop;

      --  Allocate memory for the value

      Item.Value := new String (1 .. S'Length + Num_Lines);

      --  Compare the lines

      --  Find the current line in the old value

      if V /= null then
         Index_Old := V'First;
      end if;

      while Index_New <= S'Last loop
         if V /= null then
            Line_Start_Old := Index_Old;
            Skip_To_Char (V.all, Index_Old, ASCII.LF);
         end if;

         --  Find the current line in the new value

         Line_Start_New := Index_New;
         Skip_To_Char (S, Index_New, ASCII.LF);

         --  Compare the lines

         if V /= null and then
           S (Line_Start_New .. Index_New - 1) /=
           V (Line_Start_Old + 1 .. Index_Old - 1)
         then
            Item.Value (Index_Item) := Line_Highlighted;
         else
            Item.Value (Index_Item) := Line_Not_Highlighted;
         end if;

         Item.Value
           (Index_Item + 1 .. Index_Item + Index_New - Line_Start_New) :=
           S (Line_Start_New .. Index_New - 1);

         --  Skip one more for a possible ASCII.LF
         Index_Item := Index_Item + Index_New - Line_Start_New + 2;

         if Index_New < S'Last then
            Item.Value (Index_Item - 1) := ASCII.LF;
         end if;

         Index_New := Index_New + 1;

         if V /= null then
            Index_Old := Index_Old + 1;
         end if;
      end loop;

      if V /= null then
         GNAT.Strings.Free (V);
      end if;

      Item.Valid := True;
   end Set_Value;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   overriding procedure Reset_Recursive (Item : access Debugger_Output_Type) is
   begin
      Item.Has_Changed := False;
      if Item.Value /= null then
         Item.Value (Item.Value'First) := Line_Not_Highlighted;
         for J in Item.Value'First .. Item.Value'Last - 1 loop
            if Item.Value (J) = ASCII.LF then
               Item.Value (J + 1) := Line_Not_Highlighted;
            end if;
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
