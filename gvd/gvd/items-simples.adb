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

with Glib;         use Glib;
with Gdk.Font;     use Gdk.Font;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.GC;       use Gdk.GC;
with Gdk.Types;    use Gdk.Types;
with Language;     use Language;

with Odd.Types;    use Odd.Types;

package body Items.Simples is

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

   procedure Free (Item : access Simple_Type;
                   Only_Value : Boolean := False)
   is
      I : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      if Item.Value /= null then
         Free (Item.Value);
      end if;
      if not Only_Value then
         Free_Internal (I);
      end if;
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Value : Simple_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Simple_Type'(Value);
   begin
      if Value.Value /= null then
         Simple_Type_Access (R).Value := new String'(Value.Value.all);
      end if;
      return R;
   end Clone;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Simple_Type;
                    Context : Drawing_Context;
                    X, Y    : Gint := 0)
   is
      Text_GC : Gdk_GC := Context.GC;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid or else Item.Value = null then
         Display_Pixmap (Context.Pixmap, Context.GC, Unknown_Pixmap,
                         Unknown_Mask, X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle (Context.Pixmap,
                         Context.GC,
                         Filled => True,
                         X      => X,
                         Y      => Y,
                         Width  => Item.Width,
                         Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      if Item.Has_Changed then
         Text_GC := Context.Modified_GC;
      end if;

      Draw_Text (Context.Pixmap,
                 Font => Context.Font,
                 GC   => Text_GC,
                 X    => X,
                 Y    => Y + Get_Ascent (Context.Font),
                 Text => Item.Value.all);

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
   begin
      if Item.Valid and then Item.Value /= null then
         Item.Width  := Text_Width (Context.Font, Item.Value.all);
         Item.Height := Get_Ascent (Context.Font) + Get_Descent (Context.Font);
      else
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
      end if;
   end Size_Request;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   function Get_Component_Name (Item : access Simple_Type;
                                Lang : access Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String
   is
   begin
      return Name;
   end Get_Component_Name;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (Item : access Simple_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access
   is
   begin
      return Generic_Type_Access (Item);
   end Get_Component;

   -------------
   -- Replace --
   -------------

   function Replace
     (Parent       : access Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access
   is
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
   -- Clone --
   -----------

   function Clone (Value : Access_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Access_Type'(Value);
   begin
      if Value.Value /= null then
         Simple_Type_Access (R).Value := new String'(Value.Value.all);
      end if;
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
      if Value.Value /= null then
         Simple_Type_Access (R).Value := new String'(Value.Value.all);
      end if;
      return R;
   end Clone;

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

   function New_Range_Type (Min, Max : Long_Integer)
                           return Generic_Type_Access
   is
   begin
      return new Range_Type'(Value    => null,
                             Visible  => True,
                             Selected => False,
                             Min      => Min,
                             Max      => Max,
                             Width    => 0,
                             Height   => 0,
                             Valid    => False,
                             Has_Changed => False,
                             X        => -1,
                             Y        => -1);
   end New_Range_Type;

   ------------------
   -- New_Mod_Type --
   ------------------

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access is
   begin
      return new Mod_Type'(Value    => null,
                           Modulo   => Modulo,
                           Visible  => True,
                           Selected => False,
                           Width    => 0,
                           Height   => 0,
                           Valid    => False,
                           Has_Changed => False,
                           X        => -1,
                           Y        => -1);
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
   -- Clone --
   -----------

   function Clone (Value : Range_Type)
                  return Generic_Type_Access
   is
      R : Generic_Type_Access := new Range_Type'(Value);
   begin
      if Value.Value /= null then
         Simple_Type_Access (R).Value := new String'(Value.Value.all);
      end if;
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
      if Value.Value /= null then
         Simple_Type_Access (R).Value := new String'(Value.Value.all);
      end if;
      return R;
   end Clone;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Access_Type;
                    Context : Drawing_Context;
                    X, Y    : Glib.Gint := 0)
   is
      Text_GC : Gdk_GC := Context.Xref_GC;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid then
         Display_Pixmap (Context.Pixmap, Context.GC, Unknown_Pixmap,
                         Unknown_Mask, X + Border_Spacing, Y);
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

      if Item.Has_Changed then
         Text_GC := Context.Modified_GC;
      end if;

      Draw_Text (Context.Pixmap,
                 Font => Context.Font,
                 GC   => Text_GC,
                 X    => X,
                 Y    => Y + Get_Ascent (Context.Font),
                 Text => Item.Value.all);

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

   -----------
   -- Clone --
   -----------

   function Clone (Value : Debugger_Output_Type) return Generic_Type_Access is
      R : Debugger_Output_Type_Access := new Debugger_Output_Type'(Value);
   begin
      if Value.Value /= null then
         R.Value := new String'(Value.Value.all);
      end if;
      R.Refresh_Cmd := new String'(Value.Refresh_Cmd.all);
      return Generic_Type_Access (R);
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free
     (Item       : access Debugger_Output_Type;
      Only_Value : Boolean := False)
   is
      A_Type : Generic_Type_Access := Generic_Type_Access (Item);
   begin
      Free (Item.Refresh_Cmd);

      if Item.Value /= null then
         Free (Item.Value);
      end if;

      if not Only_Value then
         Free_Internal (A_Type);
      end if;
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
                  Text_Width (Context.Font, Item.Value (Line_Start .. J - 1)));
               Line_Start := J + 1;
            end if;
         end loop;

         Item.Width := Gint'Max
           (Width,
            Text_Width (Context.Font,
                        Item.Value (Line_Start .. Item.Value'Last)));
         Item.Height :=
           (Get_Ascent (Context.Font) + Get_Descent (Context.Font))
           * Num_Lines;
      else
         Item.Width := Unknown_Width;
         Item.Height := Unknown_Height;
      end if;
   end Size_Request;

   -----------
   -- Paint --
   -----------

   procedure Paint (Item    : in out Debugger_Output_Type;
                    Context : Drawing_Context;
                    X, Y    : Gint := 0)
   is
      Text_GC : Gdk_GC := Context.GC;
      Line    : Gint := Y;
      Line_Start : Positive;
   begin
      Item.X := X;
      Item.Y := Y;

      if not Item.Valid or else Item.Value = null then
         Display_Pixmap (Context.Pixmap, Context.GC, Unknown_Pixmap,
                         Unknown_Mask, X + Border_Spacing, Y);
         return;
      end if;

      if Item.Selected then
         Draw_Rectangle (Context.Pixmap,
                         Context.GC,
                         Filled => True,
                         X      => X,
                         Y      => Y,
                         Width  => Item.Width,
                         Height => Item.Height);
         Set_Function (Context.GC, Copy_Invert);
      end if;

      if Item.Has_Changed then
         Text_GC := Context.Modified_GC;
      end if;

      Line_Start := Item.Value'First;
      for J in Item.Value'Range loop
         if Item.Value (J) = ASCII.LF then
            Draw_Text
              (Context.Pixmap,
               Font => Context.Font,
               GC   => Text_GC,
               X    => X,
               Y    => Line + Get_Ascent (Context.Font),
               Text => Item.Value (Line_Start .. J - 1));
            Line :=
              Line + Get_Ascent (Context.Font) + Get_Descent (Context.Font);
            Line_Start := J + 1;
         end if;
      end loop;

      Draw_Text
        (Context.Pixmap,
         Font => Context.Font,
         GC   => Text_GC,
         X    => X,
         Y    => Line + Get_Ascent (Context.Font),
         Text => Item.Value (Line_Start .. Item.Value'Last));

      if Item.Selected then
         Set_Function (Context.GC, Copy);
      end if;
   end Paint;

end Items.Simples;
