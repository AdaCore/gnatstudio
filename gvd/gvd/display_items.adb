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

with Glib;             use Glib;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.GC;           use Gdk.GC;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtk.Widget;       use Gtk.Widget;
with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Generic_Values;   use Generic_Values;
with Odd.Process;      use Odd.Process;
with Process_Proxies;  use Process_Proxies;
with Debugger;         use Debugger;
with Language;         use Language;
with Gdk.Types;        use Gdk.Types;
with Gdk.Event;        use Gdk.Event;

with Ada.Text_IO;      use Ada.Text_IO;

package body Display_Items is

   Spacing : constant Gint := 2;
   --  Space on each sides of the title.

   Refresh_Button_Size : constant Gint := 8;
   --  Size of the auto-refresh button on the right of the title bar.

   Xref_Color : constant String := "blue";
   --  Color to use for the items that are clickable.

   Title_Color : constant String := "grey";
   --  Color to use for the background of the title.

   White_GC   : Gdk.GC.Gdk_GC;
   Grey_GC    : Gdk.GC.Gdk_GC;
   Black_GC   : Gdk.GC.Gdk_GC;
   Xref_Gc    : Gdk.GC.Gdk_GC;
   Font       : Gdk.Font.Gdk_Font;
   Title_Font : Gdk.Font.Gdk_Font;
   Refresh_Button_Gc : Gdk.GC.Gdk_GC;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item          : out Display_Item;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Debugger      : Debugger_Process_Tab;
      Auto_Refresh  : Boolean := True)
   is
   begin
      Item := new Display_Item_Record;
      Display_Items.Initialize
        (Item, Win, Variable_Name, Debugger, Auto_Refresh);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Debugger      : Debugger_Process_Tab;
      Auto_Refresh  : Boolean := True)
   is
      use type Gdk.GC.Gdk_GC;
      Alloc_Width  : Gint;
      Alloc_Height : Gint;
      Height : Gint;
      Color  : Gdk_Color;

   begin
      Set_Internal_Command (Get_Process (Debugger.Debugger), True);

      begin
         Item.Entity := Parse_Type (Debugger.Debugger, Variable_Name);
         if Item.Entity = null then
            Item.Entity := New_Simple_Type;
            Set_Value (Simple_Type (Item.Entity.all), "<???>");
         else
            Parse_Value (Debugger.Debugger, Variable_Name, Item.Entity);
         end if;

      exception
         when Language.Unexpected_Type =>
            Item.Entity := New_Simple_Type;
            Set_Value (Simple_Type (Item.Entity.all), "<parse_error>");
      end;

      Set_Internal_Command (Get_Process (Debugger.Debugger), False);

      Item.Name         := new String'(Variable_Name);
      Item.Auto_Refresh := Auto_Refresh;
      Item.Debugger     := Debugger;

      if White_GC = null then
         Gdk_New (White_GC, Win);
         Set_Foreground (White_GC, White (Get_Default_Colormap));

         Color := Parse (Xref_Color);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Gdk_New (Xref_Gc, Win);
         Set_Foreground (Xref_GC, Color);

         Color := Parse (Title_Color);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Gdk_New (Grey_GC, Win);
         Set_Foreground (Grey_GC, Color);

         Gdk_New (Black_GC, Win);
         Set_Foreground (Black_GC, Black (Gtk.Widget.Get_Default_Colormap));

         Gdk_New (Refresh_Button_Gc, Win);

         Font := Get_Gdkfont ("Helvetica", 8);
         if Font = Null_Font then
            null;  --  ??  Should use a default font
         end if;

         Title_Font := Get_Gdkfont ("Helvetica-Bold", 8);
         if Title_Font = Null_Font then
            null;  --  ??  Should use a default font
         end if;

      end if;

      Size_Request (Item.Entity.all, Font);
      Propagate_Width (Item.Entity.all, Get_Width (Item.Entity.all));
      Alloc_Width := Get_Width (Item.Entity.all);
      Alloc_Height := Get_Height (Item.Entity.all);
      Height := Gint'Max (String_Height (Title_Font, Item.Name.all) + 6,
                          Refresh_Button_Size + 2 * Spacing);

      --  +2 so that there are blank lines on top and below the value.
      Alloc_Height := Height + (Alloc_Height + 2) + Spacing;

      Alloc_Width :=
        Gint'Max (Alloc_Width,
                  String_Width (Font, Item.Name.all) + Refresh_Button_Size)
        + Spacing * 2;
      if Alloc_Width < 40 then
         Alloc_Width := 40;
      end if;

      Gtkada.Canvas.Initialize (Item, Win, Alloc_Width, Alloc_Height);

      Draw_Rectangle (Pixmap (Item),
                      GC     => White_GC,
                      Filled => True,
                      X      => 0,
                      Y      => Height,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - Height - 1);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Grey_GC,
                      Filled => True,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Height);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Black_GC,
                      Filled => False,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - 1);

      Draw_Line (Pixmap (Item),
                 GC     => Black_GC,
                 X1     => 0,
                 Y1     => Height,
                 X2     => Alloc_Width - 1,
                 Y2     => Height);

      Draw_Text (Pixmap (Item),
                 Font   => Title_Font,
                 GC     => Black_GC,
                 X      => Spacing,
                 Y      => Height - 4,
                 Text   => Item.Name.all);

      Set_Auto_Refresh (Item, Win, Auto_Refresh);

      if Item.Entity /= null then

         --  Y is Height + 2 so that there is a blank line between the title
         --  and the value.
         Paint (Item.Entity.all, Black_GC, Xref_Gc, Font,
                Pixmap (Item),
                X => 2,
                Y => Height + 2);
      end if;
   end Initialize;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Event  : Gdk.Event.Gdk_Event_Button)
   is
      New_Item : Display_Item;
      Button_X : Gint :=
        Gint (Display_Items.Get_Coord (Item).Width) - Refresh_Button_Size -
          Spacing + 1;
      --  ??? GNAT is apparently not finding Get_Coord if we don't use the
      --  dotted notation.

   begin

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_X (Event)) >= Button_X
        and then Gint (Get_X (Event)) <= Button_X + Refresh_Button_Size - 1
        and then Gint (Get_Y (Event)) >= Spacing + 1
        and then Gint (Get_Y (Event)) <= Spacing + Refresh_Button_Size
      then
         Set_Auto_Refresh (Item, Get_Window (Item.Debugger.Data_Canvas),
                           not Item.Auto_Refresh);

      --  Dereferencing access types.

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Item.Entity.all in Access_Type'Class
      then
         Gtk_New
           (New_Item,
            Get_Window (Item.Debugger.Data_Canvas),
            Variable_Name =>
              Dereference
                (Get_Language (Item.Debugger.Debugger), Item.Name.all),
            Debugger      => Item.Debugger,
            Auto_Refresh  => Item.Auto_Refresh);
         Add_Link
           (Item.Debugger.Data_Canvas,
            Src   => Item,
            Dest  => New_Item,
            Arrow => End_Arrow,
            Descr =>
              Dereference (Get_Language (Item.Debugger.Debugger), "()"));
         Put (Item.Debugger.Data_Canvas, New_Item);
      end if;
   end On_Button_Click;

   ----------------------
   -- Set_Auto_Refresh --
   ----------------------

   procedure Set_Auto_Refresh (Item         : access Display_Item_Record;
                               Win          : Gdk.Window.Gdk_Window;
                               Auto_Refresh : Boolean)
   is
      Color : Gdk_Color;
      Width : Gint := Gint (Get_Coord (Item).Width);
   begin
      Item.Auto_Refresh := Auto_Refresh;

      Draw_Rectangle (Pixmap (Item),
                      GC     => Black_GC,
                      Filled => False,
                      X      => Width - Refresh_Button_Size - Spacing,
                      Y      => Spacing,
                      Width  => Refresh_Button_Size - 1,
                      Height => Refresh_Button_Size - 1);

      if Item.Auto_Refresh then
         Color := Parse ("green");
      else
         Color := Parse ("red");
      end if;
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      Set_Foreground (Refresh_Button_Gc, Color);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Refresh_Button_Gc,
                      Filled => True,
                      X      => Width - Refresh_Button_Size -
                                Spacing + 1,
                      Y      => Spacing + 1,
                      Width  => Refresh_Button_Size - 2,
                      Height => Refresh_Button_Size - 2);
   end Set_Auto_Refresh;

end Display_Items;
