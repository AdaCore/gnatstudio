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
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Bitmap;       use Gdk.Bitmap;
with Gdk.Window;       use Gdk.Window;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Generic_Values;   use Generic_Values;
with Odd.Process;      use Odd.Process;
with Process_Proxies;  use Process_Proxies;
with Debugger;         use Debugger;
with Language;         use Language;
with Gdk.Types;        use Gdk.Types;
with Gdk.Event;        use Gdk.Event;
with Odd.Pixmaps;      use Odd.Pixmaps;

with Ada.Text_IO;      use Ada.Text_IO;

package body Display_Items is

   --  The items are drawn with the following spacings:
   --
   --   _______________________
   --  |<->TITLE<->| |<->| |<->|
   --   -----------------------
   --  |                       |
   --  |                       |
   --   -----------------------
   --
   --  <->: Spacing
   --  Each of the two Buttons has a width and height of Buttons_Size.


   Spacing : constant Gint := 2;
   --  Space on each sides of the title.

   Buttons_Size : constant Gint := 15;
   --  Size of the buttons in the title bar of the items

   Border_Spacing : constant Gint := 2;
   --  Space left on each side of the value

   Xref_Color : constant String := "blue";
   --  Color to use for the items that are clickable.

   Title_Color : constant String := "grey";
   --  Color to use for the background of the title.

   Look_3d : constant Boolean := True;
   --  Should the items have a 3d look ?

   Title_Font_Name : constant String := "Helvetica-Bold";
   --  Font used for the name of the item.

   Title_Font_Size : constant Gint := 10;
   --  Size of the font used for the name of the item.

   Value_Font_Name : constant String := "Helvetica";
   --  Font used to display the value of the item.

   Value_Font_Size : constant Gint := 10;
   --  Size of the font used to display the value of the item.

   Num_Buttons : constant := 2;
   --  Number of buttons in the title bar.
   --  This is not user-configurable.

   Detect_Aliases : constant Boolean := True;
   --  If True, do not create new items when a matching item is already
   --  present in the canvas.

   Hide_Big_Items : constant Boolean := True;
   --  If True, items higher than a given limit will start in a hidden state.

   --  ??? Should get rid of these global variables.
   --  This could be done in a global initialization file, for all the
   --  graphic contexts we use in Odd.
   White_GC   : Gdk.GC.Gdk_GC;
   Grey_GC    : Gdk.GC.Gdk_GC;
   Black_GC   : Gdk.GC.Gdk_GC;
   Xref_Gc    : Gdk.GC.Gdk_GC;
   Font       : Gdk.Font.Gdk_Font;
   Title_Font : Gdk.Font.Gdk_Font;
   Refresh_Button_Gc : Gdk.GC.Gdk_GC;

   Close_Pixmap  : Gdk_Pixmap;
   Close_Mask    : Gdk_Bitmap;
   Locked_Pixmap : Gdk_Pixmap;
   Locked_Mask   : Gdk_Bitmap;

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Auto_Refresh  : Boolean := True);
   --  Item.Entity must have been parsed already.

   procedure Update_Display (Item : access Display_Item_Record'Class);
   --  Recompute the size of an item, and redraw its contents.
   --  It also warns the canvas that the item has changed

   procedure Dereference_Item (Item : access Display_Item_Record;
                               X    : Gint;
                               Y    : Gint);
   --  Create a new item (or reference an existing one) that dereference
   --  the field pointed to by (X, Y) in Item.
   --  (X, Y) are relative to the top-left corner of item.

   function Search_Item (Canvas : access Interactive_Canvas_Record'Class;
                         Id     : String)
                        return Display_Item;
   --  Search for an item whose Id is Id in the canvas.

   procedure Create_Link (Canvas : access Interactive_Canvas_Record'Class;
                          From, To : access Display_Item_Record'Class;
                          Name : String);
   --  Add a new link between two items.
   --  The link is not created if there is already a similar one.

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
      Entity : Generic_Type_Access;
   begin
      Set_Internal_Command (Get_Process (Debugger.Debugger), True);

      declare
         Id : String := Get_Uniq_Id (Debugger.Debugger, Variable_Name);
      begin

         --  Do not create a new item if the Id is the same, and Detect_Aliases
         --  is True.
         Item := null;

         if Search_Item (Debugger.Data_Canvas, Id) /= null then
            Set_Internal_Command (Get_Process (Debugger.Debugger), False);
            return;
         end if;

         --  Parse the type and value of the variable. If we have an error at
         --  this level, this means that the variable is unknown, and we don't
         --  create an item in that case.

         begin
            Entity := Parse_Type (Debugger.Debugger, Variable_Name);
            if Entity = null then
               Set_Internal_Command (Get_Process (Debugger.Debugger), False);
               return;
            else
               Parse_Value (Debugger.Debugger, Variable_Name, Entity);
            end if;

            Item := new Display_Item_Record;
            Item.Entity := Entity;

            --  If we got an exception while parsing the value, we create the
            --  item, but indicate there was a parse error. Hopefully, this
            --  should not happen, but at least the user knows why the variable
            --  is not displayed correctly.
         exception
            when Language.Unexpected_Type =>
               Item := new Display_Item_Record;
               Item.Entity := New_Simple_Type;
               Set_Value (Simple_Type (Item.Entity.all), "<parse_error>");
         end;

         if Id /= "" then
            Item.Id       := new String'(Id);
         end if;
         Item.Debugger := Debugger;
         Set_Internal_Command (Get_Process (Debugger.Debugger), False);
         Display_Items.Initialize (Item, Win, Variable_Name, Auto_Refresh);
      end;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Auto_Refresh  : Boolean := True)
   is
      use type Gdk.GC.Gdk_GC;
      Color  : Gdk_Color;
      Box_Pixmap    : Gdk_Pixmap;
      Box_Mask      : Gdk_Bitmap;

   begin
      Item.Name         := new String'(Variable_Name);
      Item.Auto_Refresh := Auto_Refresh;

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

         Font := Get_Gdkfont (Value_Font_Name, Value_Font_Size);
         Title_Font := Get_Gdkfont (Title_Font_Name, Title_Font_Size);

         Create_From_Xpm_D
           (Close_Pixmap, Win, Close_Mask, Null_Color, cancel_xpm);
         Create_From_Xpm_D
           (Locked_Pixmap, Win, Locked_Mask, Null_Color, lock_xpm);
         Create_From_Xpm_D (Box_Pixmap, Win, Box_Mask, Null_Color, box_xpm);

         Set_Hidden_Pixmap (Box_Pixmap, Box_Mask);
      end if;

      --  Compute the size, hidding if necessary the big components. However,
      --  we never want the top level item to be hidden, so we force it to
      --  visible (and possibly recalculate the size).
      Size_Request (Item.Entity.all, Font, Hide_Big_Items => Hide_Big_Items);
      if not Get_Visibility (Item.Entity.all) then
         Set_Visibility (Item.Entity.all, True);
         Size_Request (Item.Entity.all, Font);
      end if;

      Update_Display (Item);
      Gtkada.Canvas.Item_Resized (Item.Debugger.Data_Canvas, Item);
   end Initialize;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Item : access Display_Item_Record'Class) is
      Alloc_Width  : Gint;
      Alloc_Height : Gint;
      Title_Height, Title_Width : Gint;
   begin
      --  Compute the required size for the value itself.

      Alloc_Width := Get_Width (Item.Entity.all) + 2 * Border_Spacing;
      Alloc_Height := Get_Height (Item.Entity.all) + 2 * Border_Spacing;

      --  Compute the width and height of the title bar

      Title_Width := (2 + Num_Buttons) * Spacing
        + String_Width (Font, Item.Name.all)
        + Num_Buttons * Buttons_Size;
      Title_Height := Gint'Max
        (Get_Ascent (Title_Font) + Get_Descent (Title_Font), Buttons_Size)
        + 2 * Spacing;
      Item.Title_Height := Title_Height;

      --  Finally, we can find the total size for the display item.

      Alloc_Width := Gint'Max (Alloc_Width, Title_Width);
      Alloc_Height := Title_Height + Alloc_Height;
      if Alloc_Width < 40 then
         Alloc_Width := 40;
      end if;

      Propagate_Width (Item.Entity.all, Alloc_Width - 2 * Border_Spacing);

      --  3D Look ? If yes, keep some space for the shadow.
      if Look_3d then
         Gtkada.Canvas.Initialize
           (Item,
            Get_Window (Item.Debugger.Data_Canvas),
            Alloc_Width + 1, Alloc_Height + 1);
      else
         Gtkada.Canvas.Initialize
           (Item, Get_Window (Item.Debugger.Data_Canvas),
            Alloc_Width, Alloc_Height);
      end if;


      Draw_Rectangle (Pixmap (Item),
                      GC     => White_GC,
                      Filled => True,
                      X      => 0,
                      Y      => Title_Height,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - Title_Height - 1);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Grey_GC,
                      Filled => True,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Title_Height);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Black_GC,
                      Filled => False,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - 1);
      if Look_3d then
         Draw_Line (Pixmap (Item),
                    GC   => Black_GC,
                    X1   => Alloc_Width - 1,
                    Y1   => 2,
                    X2   => Alloc_Width - 1,
                    Y2   => Alloc_Height - 1);
         Draw_Line (Pixmap (Item),
                    GC   => Black_GC,
                    X1   => 1,
                    Y1   => Alloc_Height - 1,
                    X2   => Alloc_Width - 1,
                    Y2   => Alloc_Height - 1);
      end if;

      Draw_Line (Pixmap (Item),
                 GC     => Black_GC,
                 X1     => 0,
                 Y1     => Title_Height,
                 X2     => Alloc_Width - 1,
                 Y2     => Title_Height);

      Draw_Text (Pixmap (Item),
                 Font   => Title_Font,
                 GC     => Black_GC,
                 X      => Spacing,
                 Y      => Spacing + Get_Ascent (Title_Font),
                 Text   => Item.Name.all);

      --  First button

      Set_Auto_Refresh (Item, Get_Window (Item.Debugger.Data_Canvas),
                        Item.Auto_Refresh);

      --  Second button

      Set_Clip_Mask (Black_Gc, Close_Mask);
      Set_Clip_Origin (Black_Gc,
                       Alloc_Width - Buttons_Size - Spacing,
                       Spacing);
      Draw_Pixmap (Pixmap (Item),
                   GC     => Black_Gc,
                   Src    => Close_Pixmap,
                   Xsrc   => 0,
                   Ysrc   => 0,
                   Xdest  => Alloc_Width - Buttons_Size - Spacing,
                   Ydest  => Spacing);
      Set_Clip_Mask (Black_Gc, Null_Pixmap);
      Set_Clip_Origin (Black_Gc, 0, 0);


      if Item.Entity /= null then
         Paint (Item.Entity.all, Black_GC, Xref_Gc, Font,
                Pixmap (Item),
                X => Border_Spacing,
                Y => Title_Height + Border_Spacing);
      end if;
   end Update_Display;

   -----------------
   -- Search_Item --
   -----------------

   function Search_Item (Canvas : access Interactive_Canvas_Record'Class;
                         Id     : String)
                        return Display_Item
   is
      Alias_Item : Display_Item := null;

      procedure Alias_Found (Canvas : access Interactive_Canvas_Record'Class;
                             Item   : access Canvas_Item_Record'Class);
      --  Set New_Item to a non-null value if an alias was found for

      -----------------
      -- Alias_Found --
      -----------------

      procedure Alias_Found (Canvas : access Interactive_Canvas_Record'Class;
                             Item   : access Canvas_Item_Record'Class)
      is
      begin
         if Display_Item (Item).Id /= null
           and then Alias_Item = null
           and then Display_Item (Item).Id.all = Id
         then
            Alias_Item := Display_Item (Item);
         end if;
      end Alias_Found;

   begin
      if Detect_Aliases then
         For_Each_Item (Canvas, Alias_Found'Unrestricted_Access);
      end if;
      return Alias_Item;
   end Search_Item;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link (Canvas : access Interactive_Canvas_Record'Class;
                          From, To : access Display_Item_Record'Class;
                          Name : String)
   is
   begin
      if not Has_Link (Canvas, From, To, Name) then
         Add_Link (Canvas, From, To, End_Arrow, Name);
      end if;
   end Create_Link;

   ----------------------
   -- Dereference_Item --
   ----------------------

   procedure Dereference_Item (Item : access Display_Item_Record;
                               X    : Gint;
                               Y    : Gint)
   is
      Name : constant String := Get_Component_Name
        (Item.Entity,
         Get_Language (Item.Debugger.Debugger),
         Item.Name.all,
         X, Y);
      Component : constant Generic_Type_Access := Get_Component
        (Item.Entity, X, Y);
      New_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger), Name);
      Link_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger),
         Get_Component_Name (Item.Entity,
                             Get_Language (Item.Debugger.Debugger),
                             "@", X, Y));
      New_Item : Display_Item;

   begin
      --  Do we have an existing item that matches this ?
      New_Item := Search_Item (Item.Debugger.Data_Canvas,
                               Get_Value (Access_Type (Component.all)).all);

      if New_Item = null then
         Gtk_New (New_Item,
                  Get_Window (Item.Debugger.Data_Canvas),
                  Variable_Name => New_Name,
                  Debugger      => Item.Debugger,
                  Auto_Refresh  => Item.Auto_Refresh);
         if New_Item /= null then
            Create_Link (Item.Debugger.Data_Canvas, Item, New_Item, Link_Name);
            Put (Item.Debugger.Data_Canvas, New_Item);
         end if;
      else
         Create_Link (Item.Debugger.Data_Canvas, Item, New_Item, Link_Name);
         --  Force a redraw.
         Item_Resized (Item.Debugger.Data_Canvas, Item);
      end if;
   end Dereference_Item;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Event  : Gdk.Event.Gdk_Event_Button)
   is
      Buttons_Start : Gint :=
        Gint (Get_Coord (Item).Width) - Num_Buttons * Buttons_Size
        - Num_Buttons * Spacing + 1;
   begin

      --  Click in a button ?

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
        and then Gint (Get_Y (Event)) > Spacing
        and then Gint (Get_Y (Event)) <= Spacing + Buttons_Size
      then
         if Is_On_Top (Item.Debugger.Data_Canvas, Item) then
            Lower_Item (Item.Debugger.Data_Canvas, Item);
         else
            Raise_Item (Item.Debugger.Data_Canvas, Item);
         end if;

         for B in 0 .. Num_Buttons - 1 loop
            if Gint (Get_X (Event)) >= Buttons_Start
              and then Gint (Get_X (Event))
              <= Buttons_Start + Buttons_Size + Spacing
            then
               case B is
                  when 0 =>
                     Set_Auto_Refresh
                       (Item, Get_Window (Item.Debugger.Data_Canvas),
                        not Item.Auto_Refresh);
                     Item_Updated (Item.Debugger.Data_Canvas, Item);

                  when 1 =>
                     Free (Item);

               end case;
               return;
            end if;
            Buttons_Start := Buttons_Start + Buttons_Size + Spacing;
         end loop;
      end if;

      --  Dereferencing access types.

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_Y (Event)) >= Item.Title_Height + Border_Spacing
        and then Get_Component
        (Item.Entity,
         Gint (Get_X (Event)),
         Gint (Get_Y (Event)) - Item.Title_Height - Border_Spacing).all
        in Access_Type'Class
      then
         Dereference_Item
           (Item,
            Gint (Get_X (Event)),
            Gint (Get_Y (Event)) - Item.Title_Height - Border_Spacing);


      --  Hiding a component

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_Y (Event)) >= Item.Title_Height + Border_Spacing
      then
         declare
            Component : Generic_Type_Access := Get_Component
              (Item.Entity, Gint (Get_X (Event)),
               Gint (Get_Y (Event)) - Item.Title_Height - Border_Spacing);
         begin
            Set_Visibility (Component.all, not Get_Visibility (Component.all));
            Size_Request (Item.Entity.all, Font);
            Update_Display (Item);
            Gtkada.Canvas.Item_Resized (Item.Debugger.Data_Canvas, Item);
         end;

      --  Selecting a component

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
        and then Gint (Get_Y (Event)) > Spacing + Buttons_Size
      then
         declare
            Component : Generic_Type_Access := Get_Component
              (Item.Entity, Gint (Get_X (Event)),
               Gint (Get_Y (Event)) - Item.Title_Height - Border_Spacing);

            Value : Boolean := not Get_Selected (Component);
            --  Have to memorize it first, since its state might be changed
            --  when we unselect the current one.
         begin
            if Item.Debugger.Selected_Item /= null then
               Set_Selected
                 (Display_Item (Item.Debugger.Selected_Item).Entity, False);
               if Item.Debugger.Selected_Item /= Canvas_Item (Item)  then

                  --  Size hasn't change, so no need to recalculate it.
                  Update_Display (Display_Item (Item.Debugger.Selected_Item));
                  Gtkada.Canvas.Item_Updated
                    (Item.Debugger.Data_Canvas, Item.Debugger.Selected_Item);
               end if;
            end if;

            Set_Selected (Component, Value);
            Update_Display (Item);
            Gtkada.Canvas.Item_Updated (Item.Debugger.Data_Canvas, Item);
            if Get_Selected (Component) then
               Item.Debugger.Selected_Item := Canvas_Item (Item);
            else
               Item.Debugger.Selected_Item := null;
            end if;
         end;
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

      if Item.Auto_Refresh then
         Draw_Rectangle (Pixmap (Item),
                         GC     => Black_GC,
                         Filled => False,
                         X      => Width - 2 * Buttons_Size - 2 * Spacing,
                         Y      => Spacing,
                         Width  => Buttons_Size - 1,
                         Height => Buttons_Size - 1);
         Color := Parse ("green");
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Set_Foreground (Refresh_Button_Gc, Color);

         Draw_Rectangle (Pixmap (Item),
                         GC     => Refresh_Button_Gc,
                         Filled => True,
                         X      => Width - 2 * Buttons_Size - 2 * Spacing + 1,
                         Y      => Spacing + 1,
                         Width  => Buttons_Size - 2,
                         Height => Buttons_Size - 2);
      else
         Draw_Rectangle (Pixmap (Item),
                         GC     => Grey_Gc,
                         Filled => True,
                         X      => Width - 2 * Buttons_Size - 2 * Spacing,
                         Y      => Spacing,
                         Width  => Buttons_Size,
                         Height => Buttons_Size);
         Set_Clip_Mask (Black_Gc, Locked_Mask);
         Set_Clip_Origin (Black_Gc,
                          Width - 2 * Buttons_Size - 2 * Spacing,
                          Spacing);
         Draw_Pixmap (Pixmap (Item),
                      GC     => Black_Gc,
                      Src    => Locked_Pixmap,
                      Xsrc   => 0,
                      Ysrc   => 0,
                      Xdest  => Width - 2 * Buttons_Size - 2 * Spacing,
                      Ydest  => Spacing);
         Set_Clip_Mask (Black_Gc, Null_Pixmap);
         Set_Clip_Origin (Black_Gc, 0, 0);
      end if;
   end Set_Auto_Refresh;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Display_Item_Record) is
   begin
      Free (Item.Name);
      Free (Item.Entity);
      Free (Item.Id);
      Remove (Item.Debugger.Data_Canvas, Item);
      --  Warning: the memory has been freed after Remove.
   end Free;

end Display_Items;
