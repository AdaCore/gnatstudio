-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.IO;         use GNAT.IO;

with Glib;            use Glib;
with Gdk.Font;        use Gdk.Font;
with Gdk.Drawable;    use Gdk.Drawable;

--  ??? Needed with GtkAda 1.2 but not GtkAda 2.0
pragma Warnings (Off);
with Gdk.Window;      use Gdk.Window;
with Gdk.Types;       use Gdk.Types;
pragma Warnings (On);
with Gdk.GC;          use Gdk.GC;
with Language;        use Language;

with Basic_Types;     use Basic_Types;
with String_Utils;    use String_Utils;
with GVD.Preferences; use GVD.Preferences;

package body Items.Simples is

   Line_Highlighted     : constant Character := '@';
   Line_Not_Highlighted : constant Character := ' ';
   --  Special characters inserted at the beginning of each line for the
   --  value of Debugger_Output_Type, that indicate whether the following line
   --  should be displayed in red or not.

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
         if Item.Value.all /= Value then
            Item.Has_Changed := True;
         end if;
         Free (Item.Value);
      end if;

      Item.Value := new String'(Value);
      Item.Valid := True;
   end Set_Value;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Simple_Type; Indent : Natural := 0) is
   begin
      if Value.Value = null then
         Put ("{Simple: <null>}");
      else
         Put ("{Simple: " & Value.Value.all & "}");
      end if;
   end Print;

   ----------
   -- Free --
   ----------

   procedure Free
     (Item : access Simple_Type; Only_Value : Boolean := False) is
   begin
      Free (Item.Value);
      Free (Generic_Type (Item.all)'Access, Only_Value);
   end Free;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   procedure Clone_Dispatching
     (Item  : Simple_Type;
      Clone : out Generic_Type_Access) is
   begin
      Clone_Dispatching (Generic_Type (Item), Clone);

      if Item.Value /= null then
         Simple_Type_Access (Clone).Value := new String'(Item.Value.all);
      end if;
   end Clone_Dispatching;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (Item    : in out Simple_Type;
      Context : Drawing_Context;
      X, Y    : Gint := 0)
   is
      Text_GC : Gdk_GC := Context.GC;
      Y2      : Gint := Y;

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y2;

      if not Item.Valid or else Item.Value = null then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Border_Spacing, Y2);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Context.Pixmap,
            Context.Selection_GC,
            Filled => True,
            X      => X,
            Y      => Y2,
            Width  => Item.Width,
            Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      if Item.Has_Changed then
         Text_GC := Context.Modified_GC;
      end if;

      if Show_Type (Context.Mode)
        and then Item.Type_Name /= null
        and then Context.Type_Font /= null
      then
         Draw_Text
           (Context.Pixmap,
            Font => Context.Type_Font,
            GC   => Text_GC,
            X    => X,
            Y    => Y2 + Get_Ascent (Context.Type_Font),
            Text => Get_Type_Name (Item'Access, Context));
         Y2 := Y2 + Get_Ascent (Context.Type_Font) +
           Get_Descent (Context.Type_Font);
      end if;

      if Show_Value (Context.Mode)
        and then Context.Font /= null
      then
         Draw_Text
           (Context.Pixmap,
            Font => Context.Font,
            GC   => Text_GC,
            X    => X,
            Y    => Y2 + Get_Ascent (Context.Font),
            Text => Item.Value.all);
      end if;

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Simple_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False)
   is
      Unknown_Height : Glib.Gint;
      Unknown_Width  : Glib.Gint;
   begin
      Get_Size (Context.Unknown_Pixmap, Unknown_Width, Unknown_Height);

      Item.Width := Unknown_Width;
      Item.Height := 0;

      if Item.Valid
        and then Item.Value /= null
        and then Show_Value (Context.Mode)
      then
         Item.Width  := GVD_Text_Width (Context.Font, Item.Value.all);
         Item.Height := GVD_Font_Height (Context.Font);
      end if;

      if Item.Valid
        and then Item.Type_Name /= null
        and then Show_Type (Context.Mode)
      then
         Item.Width := Gint'Max
           (Item.Width,
            GVD_Text_Width (Context.Type_Font,
                        Get_Type_Name (Item'Access, Context)));
         Item.Height := Item.Height + GVD_Font_Height (Context.Type_Font);
      end if;

      if not Item.Valid then
         Item.Height := Unknown_Height;
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name
     (Item : access Simple_Type;
      Lang : access Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String is
   begin
      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component
     (Item : access Simple_Type;
      X, Y : Glib.Gint) return Generic_Type_Access is
   begin
      return Generic_Type_Access (Item);
   end Get_Component;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access is
   begin
      return null;
   end Replace;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Item : access Simple_Type) is
   begin
      Item.Has_Changed := False;
   end Reset_Recursive;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Enum_Type; Indent : Natural := 0) is
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
      return new Range_Type' (Simple_Type with Min => Min, Max => Max);
   end New_Range_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access is
   begin
      return new Mod_Type' (Simple_Type with Modulo => Modulo);
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

   procedure Print (Value : Range_Type; Indent : Natural := 0) is
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

   procedure Print (Value : Mod_Type; Indent : Natural := 0) is
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

   procedure Print (Value : Access_Type; Indent : Natural := 0) is
   begin
      Put ("{Access ");

      if Value.Value = null then
         Put ("<null>)");
      else
         Put (Value.Value.all & "}");
      end if;
   end Print;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (Item    : in out Access_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0)
   is
      Text_GC : Gdk_GC := Context.Xref_GC;
      Y2      : Gint := Y;

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y2;

      if not Item.Valid then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Border_Spacing, Y2);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Context.Pixmap,
            Context.GC,
            Filled => True,
            X      => X,
            Y      => Y2,
            Width  => Item.Width,
            Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      if Item.Has_Changed then
         Text_GC := Context.Modified_GC;
      end if;

      if Item.Type_Name /= null
        and then Show_Type (Context.Mode)
        and then Context.Type_Font /= null
      then
         Draw_Text
           (Context.Pixmap,
            Font => Context.Type_Font,
            GC   => Text_GC,
            X    => X,
            Y    => Y2 + Get_Ascent (Context.Type_Font),
            Text => Get_Type_Name (Item'Access, Context));
         Y2 := Y2 + Get_Ascent (Context.Type_Font) +
           Get_Descent (Context.Type_Font);
      end if;

      if Item.Value /= null
        and then Show_Value (Context.Mode)
        and then Context.Font /= null
      then
         Draw_Text
           (Context.Pixmap,
            Font => Context.Font,
            GC   => Text_GC,
            X    => X,
            Y    => Y2 + Get_Ascent (Context.Font),
            Text => Item.Value.all);
      end if;

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

   -----------------------
   -- New_Debugger_Type --
   -----------------------

   function New_Debugger_Type (Cmd : String) return Generic_Type_Access is
      Item : Debugger_Output_Type_Access;
   begin
      Item := new Debugger_Output_Type;
      Item.Refresh_Cmd := new String' (Cmd);
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

   procedure Print (Value : Debugger_Output_Type; Indent : Natural := 0) is
   begin
      if Value.Value = null then
         Put ("{Debugger_Type: <null>}");
      else
         Put ("{Debugger_Type: " & Value.Value.all & "}");
      end if;
   end Print;

   -----------------------
   -- Clone_Dispatching --
   -----------------------

   procedure Clone_Dispatching
     (Item  : Debugger_Output_Type;
      Clone : out Generic_Type_Access) is
   begin
      Clone_Dispatching (Simple_Type (Item), Clone);
      Debugger_Output_Type_Access (Clone).Refresh_Cmd :=
        new String'(Item.Refresh_Cmd.all);
   end Clone_Dispatching;

   ----------
   -- Free --
   ----------

   procedure Free
     (Item       : access Debugger_Output_Type;
      Only_Value : Boolean := False) is
   begin
      if not Only_Value then
         Free (Item.Refresh_Cmd);
      end if;

      Free (Simple_Type (Item.all)'Access, Only_Value);
   end Free;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Item           : in out Debugger_Output_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False)
   is
      Num_Lines  : Gint := 1;
      Width      : Gint := 0;
      Line_Start : Positive;

   begin
      if Item.Valid and then Item.Value /= null then
         Line_Start := Item.Value'First;

         for J in Item.Value'Range loop
            if Item.Value (J) = ASCII.LF then
               Num_Lines := Num_Lines + 1;
               Width := Gint'Max
                 (Width,
                  GVD_Text_Width (Context.Command_Font,
                                  Item.Value (Line_Start + 1 .. J - 1)));
               Line_Start := J + 1;
            end if;
         end loop;

         Item.Width := Gint'Max
           (Width,
            GVD_Text_Width (Context.Command_Font,
                            Item.Value (Line_Start + 1 .. Item.Value'Last)));
         Item.Height := GVD_Font_Height (Context.Command_Font) * Num_Lines;

      else
         Get_Size (Context.Unknown_Pixmap, Item.Width, Item.Height);
      end if;
   end Size_Request;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (Item    : in out Debugger_Output_Type;
      Context : Drawing_Context;
      X, Y    : Gint := 0)
   is
      Text_GC    : Gdk_GC;
      Line       : Gint := Y;
      Line_Start : Positive;

      use Gdk;

   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid or else Item.Value = null then
         Display_Pixmap
           (Context.Pixmap, Context.GC, Context.Unknown_Pixmap,
            Context.Unknown_Mask, X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle
           (Context.Pixmap,
            Context.GC,
            Filled => True,
            X      => X,
            Y      => Y,
            Width  => Item.Width,
            Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      Line_Start := Item.Value'First;

      if Context.Command_Font /= null then
         for J in Item.Value'Range loop
            if Item.Value (J) = ASCII.LF then
               if Item.Value (Line_Start) = Line_Highlighted then
                  Text_GC := Context.Modified_GC;
               else
                  Text_GC := Context.GC;
               end if;

               Draw_Text
                 (Context.Pixmap,
                  Font => Context.Command_Font,
                  GC   => Text_GC,
                  X    => X,
                  Y    => Line + Get_Ascent (Context.Command_Font),
                  Text => Item.Value (Line_Start + 1 .. J - 1));

               Line := Line + Get_Ascent (Context.Command_Font)
                 + Get_Descent (Context.Command_Font);
               Line_Start := J + 1;
            end if;
         end loop;
      end if;

      if Item.Value (Line_Start) = Line_Highlighted then
         Text_GC := Context.Modified_GC;
      else
         Text_GC := Context.GC;
      end if;

      Draw_Text
        (Context.Pixmap,
         Font => Context.Command_Font,
         GC   => Text_GC,
         X    => X,
         Y    => Line + Get_Ascent (Context.Command_Font),
         Text => Item.Value (Line_Start + 1 .. Item.Value'Last));

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Item : in out Debugger_Output_Type; Value : String) is
      S              : constant String :=
        Do_Tab_Expansion (Value, Integer (Get_Tab_Size));
      V              : String_Access := Item.Value;

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
         Index_Old := Index_Old + 1;
      end loop;

      if V /= null then
         Free (V);
      end if;

      Item.Valid := True;
   end Set_Value;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Item : access Debugger_Output_Type) is
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

end Items.Simples;
