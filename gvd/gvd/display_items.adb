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
with Odd.Canvas;       use Odd.Canvas;
with Gtk.Widget;       use Gtk.Widget;
with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Bitmap;       use Gdk.Bitmap;
with Gdk.Window;       use Gdk.Window;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Odd.Process;      use Odd.Process;
with Process_Proxies;  use Process_Proxies;
with Debugger;         use Debugger;
with Language;         use Language;
with Gdk.Types;        use Gdk.Types;
with Gdk.Event;        use Gdk.Event;
with Odd.Pixmaps;      use Odd.Pixmaps;
with Gtk.Menu;         use Gtk.Menu;
with Odd.Menus;        use Odd.Menus;
with Odd.Types;        use Odd.Types;
with Gtkada.Canvas;    use Gtkada.Canvas;

with Items;            use Items;
with Items.Simples;    use Items.Simples;

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

   Buttons_Size : constant Gint := 16;
   --  Size of the buttons in the title bar of the items

   Border_Spacing : constant Gint := 2;
   --  Space left on each side of the value

   Xref_Color : constant String := "blue";
   --  Color to use for the items that are clickable.

   Title_Color : constant String := "grey";
   --  Color to use for the background of the title.

   Change_Color : constant String := "red";
   --  Color used to highlight fields that have changed since the last update.

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

   Hide_Big_Items : constant Boolean := True;
   --  If True, items higher than a given limit will start in a hidden state.

   --  ??? Should get rid of these global variables.
   --  This could be done in a global initialization file, for all the
   --  graphic contexts we use in Odd.

   White_GC   : Gdk.GC.Gdk_GC;
   Grey_GC    : Gdk.GC.Gdk_GC;
   Black_GC   : Gdk.GC.Gdk_GC;
   Xref_GC    : Gdk.GC.Gdk_GC;
   Change_GC  : Gdk.GC.Gdk_GC;
   Font       : Gdk.Font.Gdk_Font;
   Title_Font : Gdk.Font.Gdk_Font;
   Refresh_Button_GC : Gdk.GC.Gdk_GC;

   Trash_Pixmap        : Gdk_Pixmap;
   Trash_Mask          : Gdk_Bitmap;
   Close_Pixmap        : Gdk_Pixmap;
   Close_Mask          : Gdk_Bitmap;
   Locked_Pixmap       : Gdk_Pixmap;
   Locked_Mask         : Gdk_Bitmap;
   Auto_Display_Pixmap : Gdk_Pixmap;
   Auto_Display_Mask   : Gdk_Bitmap;


   --  Aliases detection
   --  ==================
   --
   --  This package provides a complete aliases detection, ie when some items
   --  are found at the same location in memory. Each item has a uniq id,
   --  which most often is an address in memory, but can also be different for
   --  instance on the Java Virtual Machine.
   --  Every time a new item is inserted in the canvas, either as a result of
   --  a "graph print" or "graph display" command, or when the user clicks on
   --  an access type to dereference it, this package will test that there is
   --  not already an item on the canvas with the same Id.
   --  In every case, the new item is created (with possibly a link to it if it
   --  was a dereference). However, if there was already an item with the same
   --  id then the new item is set to be hidden (ie will not be displayed,
   --  nor any link to or from it).
   --  The links to and from an alias (hidden item) are automatically
   --  duplicated to reference the visible item, so that they are correctly
   --  visible and moved on the canvas. These temporary links have a special
   --  attribute Alias_Link set.
   --
   --  Just before the next update of the canvas, all these temporary links are
   --  removed, all aliases are cancelled and all items are made visible. Then
   --  we recompute the list of aliases before redrawing the canvas. It is
   --  worth nothing that when we have an hidden item, we do not waste time
   --  reparsing its value.
   --
   --  Note also that for simplicity we do not create chains of aliases, ie
   --  an item is an alias to a second, which in turn in an alias to a third.
   --  Instead, both the first and the second will refer the same third. It is
   --  thus much easier to deal with aliases.


   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Auto_Refresh  : Boolean := True);
   --  Item.Entity must have been parsed already.

   procedure Update_Display (Item : access Display_Item_Record'Class);
   --  Recompute the size of an item, and redraw its contents.
   --  It also warns the canvas that the item has changed.

   procedure Update_Component
     (Item : access Display_Item_Record'Class;
      Component : Generic_Type_Access := null);
   --  Update a specific component of a complex item.
   --  The item must have been displayed at least once before the last time
   --  its visibility state changed.
   --  If Component is null, the whole item is redraw, otherwise only the
   --  specific Component is updated.

   procedure Select_Item
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access);
   --  Select a specific Component in Item, after unselecting the current
   --  selection.
   --  If Component is null, no new selection is made, but the current one is
   --  released.

   procedure Dereference_Item
     (Item : access Display_Item_Record;
      X    : Gint;
      Y    : Gint);
   --  Create a new item (or reference an existing one) that dereference
   --  the field pointed to by (X, Y) in Item.
   --  (X, Y) are relative to the top-left corner of item.

   function Search_Item
     (Canvas : access Odd_Canvas_Record'Class;
      Id     : String;
      Name   : String := "")
     return Display_Item;
   --  Search for an item whose Id is Id in the canvas.
   --  If Name is not "", then the item returned must also have the same name
   --  as Name.

   procedure Create_Link
     (Canvas     : access Interactive_Canvas_Record'Class;
      From, To   : access Canvas_Item_Record'Class;
      Name       : String;
      Arrow      : Arrow_Type := End_Arrow;
      Alias_Link : Boolean := False);
   --  Add a new link between two items.
   --  The link is not created if there is already a similar one.

   function Recompute_Address
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean;
   --  Recompute the address of the item, and make it an alias of other
   --  items if required.

   procedure Duplicate_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class);
   --  Create a temporary version of all the links to and from Item, and
   --  insert the copies in the canvas. Any reference to Item is replaced
   --  by a reference to Item.Is_Alias_Of (so that the temporary links
   --  point to the alias)

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item           : out Display_Item;
      Win            : Gdk.Window.Gdk_Window;
      Variable_Name  : String;
      Debugger       : access Debugger_Process_Tab_Record'Class;
      Auto_Refresh   : Boolean := True;
      Default_Entity : Items.Generic_Type_Access := null)
   is
      Entity : Generic_Type_Access;
      Value_Found : Boolean := False;
   begin
      Push_Internal_Command_Status (Get_Process (Debugger.Debugger), True);

      if Default_Entity = null then

         declare
            Id : String := Get_Uniq_Id (Debugger.Debugger, Variable_Name);
         begin

            --  Parse the type and value of the variable. If we have an error
            --  at this level, this means that the variable is unknown, and we
            --  don't create an item in that case.

            begin
               Entity := Parse_Type (Debugger.Debugger, Variable_Name);

               if Entity = null then
                  Pop_Internal_Command_Status
                    (Get_Process (Debugger.Debugger));
                  return;
               else
                  Parse_Value
                    (Debugger.Debugger, Variable_Name, Entity, Value_Found);
               end if;

               Item := new Display_Item_Record;
               Item.Entity := Entity;
               Item.Num := Get_Next_Item_Num (Debugger.Data_Canvas);
               Set_Valid (Item.Entity, Value_Found);

               --  If we got an exception while parsing the value, we do not
               --  create the item, since otherwise the user would be able to
               --  try to update the value for instance, and the type would
               --  have nothing to do with what the variable really is.  ???
               --  Should display an error message somewhere.
            exception
               when Language.Unexpected_Type | Constraint_Error =>
                  Pop_Internal_Command_Status
                    (Get_Process (Debugger.Debugger));
                  return;
            end;

            if Id /= "" then
               Item.Id := new String'(Id);
            end if;
         end;

      --  Default_Entity /= null
      else
         Item := new Display_Item_Record;
         Item.Entity := Default_Entity;
         Item.Num := Get_Next_Item_Num (Debugger.Data_Canvas);
         Set_Valid (Item.Entity, True);
      end if;

      Item.Debugger := Debugger_Process_Tab (Debugger);
      Pop_Internal_Command_Status (Get_Process (Debugger.Debugger));
      Display_Items.Initialize (Item, Win, Variable_Name, Auto_Refresh);
   end Gtk_New;

   ---------------------
   -- Gtk_New_And_Put --
   ---------------------

   procedure Gtk_New_And_Put
     (Item           : out Display_Item;
      Win            : Gdk.Window.Gdk_Window;
      Variable_Name  : String;
      Debugger       : access Debugger_Process_Tab_Record'Class;
      Auto_Refresh   : Boolean := True;
      Link_From      : access Display_Item_Record'Class;
      Link_Name      : String := "";
      Default_Entity : Items.Generic_Type_Access := null)
   is
      Id : String := Get_Uniq_Id (Debugger.Debugger, Variable_Name);
      Alias_Item : Display_Item;
   begin

      --  Is the item already on the canvas ? If yes, do not display it
      --  again
      Alias_Item := Search_Item (Debugger.Data_Canvas, Id, Variable_Name);

      if Alias_Item = null then
         Gtk_New (Item,
                  Get_Window (Debugger.Data_Canvas),
                  Variable_Name => Variable_Name,
                  Debugger      => Debugger,
                  Auto_Refresh  => Auto_Refresh);

         if Item /= null then
            Create_Link (Debugger.Data_Canvas, Link_From, Item, Link_Name);

            --  Do we have an existing item that matches this ? (same address as
            --  in the current access type itself)

            Alias_Item := Search_Item (Debugger.Data_Canvas, Id);
            Put (Debugger.Data_Canvas, Item);

            if Alias_Item /= null then
               --  Hide the item so as not to interfer with another one.
               Set_Visibility (Item, False);

               Item.Is_Alias_Of := Alias_Item;
               Duplicate_Links (Debugger.Data_Canvas, Item);
            end if;
         end if;

      --  There was already such an item
      else
         Create_Link (Debugger.Data_Canvas, Link_From, Alias_Item, Link_Name);
         Refresh_Canvas (Debugger.Data_Canvas);
      end if;
   end Gtk_New_And_Put;

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
      Item.Is_Dereference := False;

      if not Is_Visible (Item) then
         return;
      end if;

      if White_GC = null then
         Gdk_New (White_GC, Win);
         Set_Foreground (White_GC, White (Get_Default_Colormap));

         Color := Parse (Xref_Color);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Gdk_New (Xref_GC, Win);
         Set_Foreground (Xref_GC, Color);

         Color := Parse (Title_Color);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Gdk_New (Grey_GC, Win);
         Set_Foreground (Grey_GC, Color);

         Color := Parse (Change_Color);
         Alloc (Gtk.Widget.Get_Default_Colormap, Color);
         Gdk_New (Change_GC, Win);
         Set_Foreground (Change_GC, Color);

         Gdk_New (Black_GC, Win);
         Set_Foreground (Black_GC, Black (Gtk.Widget.Get_Default_Colormap));

         Gdk_New (Refresh_Button_GC, Win);

         Font := Get_Gdkfont (Value_Font_Name, Value_Font_Size);
         Title_Font := Get_Gdkfont (Title_Font_Name, Title_Font_Size);

         Create_From_Xpm_D
           (Close_Pixmap, Win, Close_Mask, Null_Color, cancel_xpm);
         Create_From_Xpm_D
           (Locked_Pixmap, Win, Locked_Mask, Null_Color, lock_xpm);
         Create_From_Xpm_D (Box_Pixmap, Win, Box_Mask, Null_Color, box_xpm);
         Create_From_Xpm_D
           (Trash_Pixmap, Win, Trash_Mask, Null_Color, trash_xpm);
         Create_From_Xpm_D
           (Auto_Display_Pixmap,
            Win, Auto_Display_Mask, Null_Color, display_small_xpm);

         Set_Hidden_Pixmap (Box_Pixmap, Box_Mask);
         Set_Unknown_Pixmap (Trash_Pixmap, Trash_Mask);
      end if;

      --  Compute the size, hidding if necessary the big components. However,
      --  we never want the top level item to be hidden, so we force it to
      --  visible (and possibly recalculate the size).

      Size_Request
        (Item.Entity.all,
         Drawing_Context'(Pixmap      => Pixmap (Item),
                          GC          => Black_GC,
                          Xref_GC     => Xref_GC,
                          Modified_GC => Change_GC,
                          Font        => Font),
         Hide_Big_Items => Hide_Big_Items);
      if not Get_Visibility (Item.Entity.all) then
         Set_Visibility (Item.Entity.all, True);
         Size_Request
           (Item.Entity.all,
            Drawing_Context'(Pixmap      => Pixmap (Item),
                             GC          => Black_GC,
                             Xref_GC     => Xref_GC,
                             Modified_GC => Change_GC,
                             Font        => Font));
      end if;

      Update_Display (Item);
      Item_Resized (Item.Debugger.Data_Canvas, Item);
   end Initialize;

   ---------------
   -- Find_Item --
   ---------------

   function Find_Item
     (Canvas : access Odd.Canvas.Odd_Canvas_Record'Class;
      Num    : Integer)
     return Display_Item
   is
      Found : Display_Item := null;

      function Search_By_Num
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class)
        return Boolean;
      --  Search for the item whose number is Num.

      -------------------
      -- Search_By_Num --
      -------------------

      function Search_By_Num
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class)
        return Boolean
      is
      begin
         if Display_Item (Item).Num = Num then
            Found := Display_Item (Item);
            return False;
         end if;
         return True;
      end Search_By_Num;

   begin
      For_Each_Item (Canvas, Search_By_Num'Unrestricted_Access);
      return Found;
   end Find_Item;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Item : access Display_Item_Record'Class) is
      Alloc_Width  : Gint;
      Alloc_Height : Gint;
      Title_Height, Title_Width : Gint;
      Num_Width    : Gint;
   begin
      --  Compute the required size for the value itself.

      Alloc_Width := Get_Width (Item.Entity.all) + 2 * Border_Spacing;
      Alloc_Height := Get_Height (Item.Entity.all) + 2 * Border_Spacing;

      --  Compute the width and height of the title bar

      Num_Width := String_Width (Font, Integer'Image (Item.Num) & ": ");
      Title_Width := (5 + Num_Buttons) * Spacing
        + String_Width (Title_Font, Item.Name.all)
        + Num_Buttons * Buttons_Size + Num_Width;
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


      Draw_Rectangle
        (Pixmap (Item),
         GC     => White_GC,
         Filled => True,
         X      => 0,
         Y      => Title_Height,
         Width  => Alloc_Width - 1,
         Height => Alloc_Height - Title_Height - 1);

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Grey_GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => Alloc_Width - 1,
         Height => Title_Height);

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Black_GC,
         Filled => False,
         X      => 0,
         Y      => 0,
         Width  => Alloc_Width - 1,
         Height => Alloc_Height - 1);

      if Look_3d then
         Draw_Line
           (Pixmap (Item),
            GC   => Black_GC,
            X1   => Alloc_Width - 1,
            Y1   => 2,
            X2   => Alloc_Width - 1,
            Y2   => Alloc_Height - 1);
         Draw_Line
           (Pixmap (Item),
            GC   => Black_GC,
            X1   => 1,
            Y1   => Alloc_Height - 1,
            X2   => Alloc_Width - 1,
            Y2   => Alloc_Height - 1);
      end if;

      Draw_Line
        (Pixmap (Item),
         GC     => Black_GC,
         X1     => 0,
         Y1     => Title_Height,
         X2     => Alloc_Width - 1,
         Y2     => Title_Height);

      Draw_Text
        (Pixmap (Item),
         Font   => Font,
         GC     => Black_GC,
         X      => Spacing,
         Y      => Spacing + Get_Ascent (Title_Font),
         Text   => Integer'Image (Item.Num) & ":");

      Draw_Text
        (Pixmap (Item),
         Font   => Title_Font,
         GC     => Black_GC,
         X      => Spacing + Num_Width,
         Y      => Spacing + Get_Ascent (Title_Font),
         Text   => Item.Name.all);

      --  First button

      Set_Auto_Refresh
        (Item, Get_Window (Item.Debugger.Data_Canvas), Item.Auto_Refresh);

      --  Second button

      Set_Clip_Mask (Black_GC, Close_Mask);
      Set_Clip_Origin
        (Black_GC, Alloc_Width - Buttons_Size - Spacing, Spacing);
      Draw_Pixmap
        (Pixmap (Item),
         GC     => Black_GC,
         Src    => Close_Pixmap,
         Xsrc   => 0,
         Ysrc   => 0,
         Xdest  => Alloc_Width - Buttons_Size - Spacing,
         Ydest  => Spacing);
      Set_Clip_Mask (Black_GC, Null_Pixmap);
      Set_Clip_Origin (Black_GC, 0, 0);

      if Item.Entity /= null then
         Paint
           (Item.Entity.all,
            Drawing_Context'(Pixmap      => Pixmap (Item),
                             GC          => Black_GC,
                             Xref_GC     => Xref_GC,
                             Modified_GC => Change_GC,
                             Font        => Font),
            X => Border_Spacing,
            Y => Title_Height + Border_Spacing);
      end if;
   end Update_Display;

   ----------------------
   -- Update_Component --
   ----------------------

   procedure Update_Component
     (Item : access Display_Item_Record'Class;
      Component : Generic_Type_Access := null) is
   begin
      if not Get_Selected (Component) then
         Draw_Rectangle
           (Pixmap (Item),
            GC     => White_GC,
            Filled => True,
            X      => Get_X (Component.all),
            Y      => Get_Y (Component.all),
            Width  => Get_Width (Component.all),
            Height => Get_Height (Component.all));
      end if;

      Paint
        (Component.all,
         Drawing_Context'(Pixmap      => Pixmap (Item),
                          GC          => Black_GC,
                          Xref_GC     => Xref_GC,
                          Modified_GC => Change_GC,
                          Font        => Font),
         X => Get_X (Component.all),
         Y => Get_Y (Component.all));
   end Update_Component;

   -----------------
   -- Search_Item --
   -----------------

   function Search_Item
     (Canvas    : access Odd_Canvas_Record'Class;
      Id        : String;
      Name      : String := "")
     return Display_Item
   is
      Alias_Item : Display_Item := null;

      function Alias_Found
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Set New_Item to a non-null value if an alias was found for
      --  Return False when we need to stop traversing the list of children.

      -----------------
      -- Alias_Found --
      -----------------

      function Alias_Found
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         pragma Warnings (Off, Canvas);
      begin
         --  Do not detect aliases with what are already aliases, so as to
         --  avoid chains of aliases.
         if Display_Item (Item).Id /= null
           and then Display_Item (Item).Auto_Refresh
           and then Display_Item (Item).Id.all = Id
           and then
           ((Name = "" and then Display_Item (Item).Is_Alias_Of = null)
            or else (Display_Item (Item).Name /= null
                     and then Display_Item (Item).Name.all = Name))
         then
            Alias_Item := Display_Item (Item);
            return False;
         end if;

         return True;
      end Alias_Found;

   begin
      --  Always search if we have a special name to look for, so as to avoid
      --  creating the same item multiple times
      if Name = "" or else Get_Detect_Aliases (Canvas) then
         For_Each_Item (Canvas, Alias_Found'Unrestricted_Access);
      end if;
      return Alias_Item;
   end Search_Item;

   ----------------------------
   -- Update_On_Auto_Refresh --
   ----------------------------

   function Update_On_Auto_Refresh
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
   begin
      --  Only update when the item is not an alias of something else (and thus
      --  is hidden).
      if Display_Item (Item).Auto_Refresh
        and then Display_Item (Item).Is_Alias_Of = null
      then
         Update (Odd_Canvas (Canvas), Display_Item (Item));
      end if;

      return True;
   end Update_On_Auto_Refresh;

   ------------
   -- Update --
   ------------

   procedure Update
     (Canvas : access Odd_Canvas_Record'Class;
      Item   : access Display_Item_Record'Class)
   is
      Value_Found : Boolean;
      Was_Visible : Boolean := Get_Visibility (Item.Entity.all);
   begin
      --  Parse the value

      Push_Internal_Command_Status
        (Get_Process (Item.Debugger.Debugger), True);

      if Item.Entity.all in Debugger_Output_Type'Class then
         Set_Value
           (Debugger_Output_Type (Item.Entity.all),
            Send (Item.Debugger.Debugger,
                  Refresh_Command (Debugger_Output_Type (Item.Entity.all))));

      elsif Item.Name /= null then
         Parse_Value (Item.Debugger.Debugger, Item.Name.all,
                      Item.Entity, Value_Found);
         Set_Valid (Item.Entity, Value_Found);

      end if;

      --  Update graphically.
      --  Note that we should not change the visibility status of item
      --  and its children.

      Size_Request
        (Item.Entity.all,
         Drawing_Context'(Pixmap      => Pixmap (Item),
                          GC          => Black_GC,
                          Xref_GC     => Xref_GC,
                          Modified_GC => Change_GC,
                          Font        => Font),
         Hide_Big_Items => Hide_Big_Items);

      --  Make sure we don't hide the item, unless it was already hidden.
      --  It could have been hidden automatically if it is now too high.

      if not Get_Visibility (Item.Entity.all)
        and then Was_Visible
      then
         Set_Visibility (Item.Entity.all, True);
         Size_Request
           (Item.Entity.all,
            Drawing_Context'(Pixmap      => Pixmap (Item),
                             GC          => Black_GC,
                             Xref_GC     => Xref_GC,
                             Modified_GC => Change_GC,
                             Font        => Font));
      end if;

      Update_Display (Item);
      Item_Resized (Canvas, Item);

      Pop_Internal_Command_Status (Get_Process (Item.Debugger.Debugger));

      --  If we got an exception while parsing the value, we register the new
      --  value as being incorrect.
   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Set_Valid (Item.Entity, False);
         Pop_Internal_Command_Status (Get_Process (Item.Debugger.Debugger));
   end Update;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link
     (Canvas     : access Interactive_Canvas_Record'Class;
      From, To   : access Canvas_Item_Record'Class;
      Name       : String;
      Arrow      : Arrow_Type := End_Arrow;
      Alias_Link : Boolean := False)
   is
      L : Odd_Link;
   begin
      if not Has_Link (Canvas, From, To, Name) then
         L := new Odd_Link_Record;
         Configure (L, From, To, Arrow, Name);
         L.Alias_Link := Alias_Link;
         Add_Link (Canvas, L);
      end if;
   end Create_Link;

   ----------------------
   -- Dereference_Item --
   ----------------------

   procedure Dereference_Item
     (Item : access Display_Item_Record;
      X    : Gint;
      Y    : Gint)
   is
      use String_History;
      Name : constant String := Get_Component_Name
        (Item.Entity,
         Get_Language (Item.Debugger.Debugger),
         Item.Name.all,
         X, Y);
      Link_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger),
         Get_Component_Name
           (Item.Entity,
            Get_Language (Item.Debugger.Debugger), "@", X, Y));
      New_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger), Name);
      Cmd : String := "graph display " & New_Name & " dependent on "
        & Integer'Image (Item.Num) & " link_name " & Link_Name;
--        New_Item : Display_Item;
   begin
      Text_Output_Handler (Item.Debugger, Cmd & ASCII.LF, Is_Command => True);
      Process_User_Command (Item.Debugger, Cmd);

--        --  Do not call Process_User_Command, so as to be sure we use the
--        --  correct item.
--        Append (Item.Debugger.Command_History, Cmd);

--        Gtk_New_And_Put
--          (New_Item, Get_Window (Item.Debugger.Data_Canvas),
--           Variable_Name => New_Name,
--           Debugger      => Item.Debugger,
--           Auto_Refresh  => True,
--           Link_From     => Display_Item (Item),
--           Link_Name     => Link_Name);
--        if New_Item /= null then
--           New_Item.Is_Dereference := True;
--        end if;
--        Display_Prompt (Item.Debugger.Debugger);
   end Dereference_Item;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access) is
   begin
      --  Unselect the current selection

      if Item.Debugger.Selected_Item /= null
        and then Item.Debugger.Selected_Component /= Component
      then
         Set_Selected (Item.Debugger.Selected_Component, False);
         Update_Component (Display_Item (Item.Debugger.Selected_Item),
                           Item.Debugger.Selected_Component);
         if Item.Debugger.Selected_Item /= Canvas_Item (Item)
           or else Component = null
         then
            Item_Updated
              (Item.Debugger.Data_Canvas, Item.Debugger.Selected_Item);
         end if;
      end if;

      --  Select the new one

      if Component /= null then
         Set_Selected (Component, not Get_Selected (Component));
         Update_Component (Item, Component);
         Item_Updated (Item.Debugger.Data_Canvas, Item);
         if Get_Selected (Component) then
            Item.Debugger.Selected_Item := Canvas_Item (Item);
            Item.Debugger.Selected_Component := Component;
         else
            Item.Debugger.Selected_Item := null;
         end if;
      else
         Item.Debugger.Selected_Item := null;
      end if;
   end Select_Item;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item   : access Display_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button)
   is
      Buttons_Start : Gint :=
        Gint (Get_Coord (Item).Width) - Num_Buttons * Buttons_Size -
          Num_Buttons * Spacing + 1;
      Component : Generic_Type_Access;

   begin
      --  Click on a button ?

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
              and then Gint (Get_X (Event)) <=
                Buttons_Start + Buttons_Size + Spacing
            then
               case B is
                  when 0 =>
                     Set_Auto_Refresh
                       (Item, Get_Window (Item.Debugger.Data_Canvas),
                        not Item.Auto_Refresh);

                     --  If we moved back to the auto-refresh state, force an
                     --  update of the value.
                     if Item.Auto_Refresh then
                        Update (Item.Debugger.Data_Canvas, Item);
                     else
                        --  Redisplay the item, so that no field is displayed
                        --  in red anymore.
                        Reset_Recursive (Item);
                        Update_Display (Item);
                        Item_Updated (Item.Debugger.Data_Canvas, Item);
                     end if;

                  when 1 =>
                     Free (Item);

               end case;
               return;
            end if;
            Buttons_Start := Buttons_Start + Buttons_Size + Spacing;
         end loop;
         return;
      end if;

      --  Get the selected component

      Component := Get_Component
        (Item.Entity, Gint (Get_X (Event)),
         Gint (Get_Y (Event)) - Item.Title_Height - Border_Spacing);

      --  Contextual menus ?

      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Popup (Item_Contextual_Menu (Item.Debugger.Data_Canvas,
                                      Item,
                                      Component),
                Button            => Get_Button (Event),
                Activate_Time     => Get_Time (Event));

      --  Dereferencing access types.

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_Y (Event)) >= Item.Title_Height + Border_Spacing
        and then Component.all in Access_Type'Class
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
         Set_Visibility (Component.all, not Get_Visibility (Component.all));
         Size_Request
           (Item.Entity.all,
            Drawing_Context'(Pixmap      => Pixmap (Item),
                             GC          => Black_GC,
                             Xref_GC     => Xref_GC,
                             Modified_GC => Change_GC,
                             Font        => Font));
         Update_Display (Item);
         Item_Resized (Item.Debugger.Data_Canvas, Item);

      --  Selecting a component

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
        and then Gint (Get_Y (Event)) > Spacing + Buttons_Size
      then
         Select_Item (Item, Component);
      end if;
   end On_Button_Click;

   ----------------------
   -- Set_Auto_Refresh --
   ----------------------

   procedure Set_Auto_Refresh
     (Item         : access Display_Item_Record;
      Win          : Gdk.Window.Gdk_Window;
      Auto_Refresh : Boolean)
   is
      Width : Gint := Gint (Get_Coord (Item).Width);
   begin
      Item.Auto_Refresh := Auto_Refresh;

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Grey_GC,
         Filled => True,
         X      => Width - 2 * Buttons_Size - 2 * Spacing,
         Y      => Spacing,
         Width  => Buttons_Size,
         Height => Buttons_Size);
      Set_Clip_Origin
        (Black_GC,
         Width - 2 * Buttons_Size - 2 * Spacing,
         Spacing);

      if Item.Auto_Refresh then
         Set_Clip_Mask (Black_GC, Auto_Display_Mask);
         Draw_Pixmap
           (Pixmap (Item),
            GC     => Black_GC,
            Src    => Auto_Display_Pixmap,
            Xsrc   => 0,
            Ysrc   => 0,
            Xdest  => Width - 2 * Buttons_Size - 2 * Spacing,
            Ydest  => Spacing);

      else
         Set_Clip_Mask (Black_GC, Locked_Mask);
         Draw_Pixmap
           (Pixmap (Item),
            GC     => Black_GC,
            Src    => Locked_Pixmap,
            Xsrc   => 0,
            Ysrc   => 0,
            Xdest  => Width - 2 * Buttons_Size - 2 * Spacing,
            Ydest  => Spacing);
      end if;

      Set_Clip_Mask (Black_GC, Null_Pixmap);
      Set_Clip_Origin (Black_GC, 0, 0);
   end Set_Auto_Refresh;

   ----------
   -- Free --
   ----------

   procedure Free (Item : access Display_Item_Record) is
   begin
      if Item.Debugger.Selected_Item = Canvas_Item (Item) then
         Item.Debugger.Selected_Item := null;
      end if;

      Free (Item.Entity);
      Free (Item.Name);
      Free (Item.Id);
      Remove (Item.Debugger.Data_Canvas, Item);
      --  Warning: the memory has been freed after Remove.
   end Free;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (Canvas : access Odd_Canvas_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
   is
      --  This is slightly complicated since we need to get a valid item
      --  to undo the selection.

      function Unselect
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Unselect the currently selected canvas item.

      function Unselect
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         pragma Warnings (Off, Canvas);
      begin
         Select_Item (Display_Item (Item), null);
         return False;
      end Unselect;

   begin
      if Get_Button (Event) = 1 then
         For_Each_Item (Canvas, Unselect'Unrestricted_Access);

      elsif Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Popup
           (Contextual_Background_Menu (Canvas),
            Button            => Get_Button (Event),
            Activate_Time     => Get_Time (Event));
      end if;
   end On_Background_Click;

   -----------------------
   -- Recompute_Address --
   -----------------------

   function Recompute_Address
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      It : Display_Item := Display_Item (Item);

      function Has_Alias
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  True if Item is a possible alias of It.
      --  We only check the items before It in the list of items

      ---------------
      -- Has_Alias --
      ---------------

      function Has_Alias
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         It2 : Display_Item := Display_Item (Item);
      begin
         --  Stop iterating when we saw It, since we only want to check the
         --  items before it.
         if It2 = It then
            return False;
         end if;

         --  Do we have an alias ?
         --  Do not detect aliases with items that are aliases themselves,
         --  so as to avoid chains of aliases
         if It2.Id /= null
           and then It2.Is_Alias_Of = null
           and then It.Id /= null
           and then It2.Id.all = It.Id.all
         then
            --  Keep only one of them:
            --   - if none are the result of a dereference, keep them both
            --   - if only one is the result of a dereference, keep the other
            --   - otherwise keep the one with the shortest name

            if It2.Is_Dereference and then not It.Is_Dereference then
               It2.Is_Alias_Of := It;
               return False;

            elsif It.Is_Dereference and then not It2.Is_Dereference then
               It.Is_Alias_Of := It2;
               return False;

            elsif It.Is_Dereference and then It2.Is_Dereference then
               if It.Name /= null
                 and then It2.Name /= null
                 and then It2.Name'Length < It.Name'Length
               then
                  It.Is_Alias_Of := It2;
               else
                  It2.Is_Alias_Of := It;
               end if;
               return False;

            --  They both were explicitly displayed by the user. Print a
            --  special link to reflect that fact.
            else
               Create_Link
                 (Canvas, It, It2, "  =  ", Both_Arrow, Alias_Link => True);
            end if;
         end if;

         return True;
      end Has_Alias;

   begin
      --  If this is not an item associated with a variable, ignore it.
      if It.Name = null then
         return True;
      end if;

      --  Else recompute the id for the item
      Free (It.Id);
      declare
         Id : String := Get_Uniq_Id (It.Debugger.Debugger, It.Name.all);
      begin
         if Id /= "" then
            It.Id := new String'(Id);
         end if;
      end;

      --  Search if there is an alias related to this new item
      For_Each_Item (Canvas, Has_Alias'Unrestricted_Access);

      --  Keep processing the next items.
      return True;
   end Recompute_Address;

   ---------------------
   -- Duplicate_Links --
   ---------------------

   procedure Duplicate_Links
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class)
   is
      function Duplicate
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class)
        return Boolean;
      --  Duplicate Link if it is bound to Item and is not a temporary link

      ---------------
      -- Duplicate --
      ---------------

      function Duplicate
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class)
        return Boolean
      is
         Src, Dest : Canvas_Item;
         Replace : Boolean;
      begin
         if not Odd_Link (Link).Alias_Link then

            Src := Get_Src (Link);
            Dest := Get_Dest (Link);
            Replace := False;

            if Get_Src (Link) = Canvas_Item (Item) then
               Src := Canvas_Item (Display_Item (Item).Is_Alias_Of);
               Replace := True;
            end if;

            if Get_Dest (Link) = Canvas_Item (Item) then
               Dest := Canvas_Item (Display_Item (Item).Is_Alias_Of);
               Replace := True;
            end if;

            if Replace then
               Create_Link
                 (Canvas, Src, Dest, Get_Descr (Link), Alias_Link => True);
            end if;
         end if;
         return True;
      end Duplicate;

   begin
      if Display_Item (Item).Is_Alias_Of /= null then
         For_Each_Link (Canvas, Duplicate'Unrestricted_Access);
      end if;
   end Duplicate_Links;

   -------------------------------
   -- On_Canvas_Process_Stopped --
   -------------------------------

   procedure On_Canvas_Process_Stopped
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Canvas : Odd_Canvas := Debugger_Process_Tab (Object).Data_Canvas;
   begin
      Recompute_All_Aliases (Canvas);
   end On_Canvas_Process_Stopped;

   ---------------------------
   -- Recompute_All_Aliases --
   ---------------------------

   procedure Recompute_All_Aliases
     (Canvas : access Odd.Canvas.Odd_Canvas_Record'Class)
   is
      function Recompute_Sizes
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Recomput the sizes and positions for all the items that are aliases
      --  of other items.

      function Remove_Aliases
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Remove all the aliases currently set up

      function Remove_Temporary
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Remove all temporary links from the canvas

      --------------------
      -- Remove_Aliases --
      --------------------

      function Remove_Aliases
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         It : Display_Item := Display_Item (Item);
      begin
         It.Was_Alias := It.Is_Alias_Of /= null;
         It.Is_Alias_Of := null;
         Set_Visibility (It, True);
         return True;
      end Remove_Aliases;

      ----------------------
      -- Remove_Temporary --
      ----------------------

      function Remove_Temporary
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
      begin
         if Odd_Link (Link).Alias_Link then
            Remove_Link (Canvas, Link);
            Destroy (Link);
         end if;
         return True;
      end Remove_Temporary;

      ---------------------
      -- Recompute_Sizes --
      ---------------------

      function Recompute_Sizes
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         It : Display_Item := Display_Item (Item);
      begin
         if It.Is_Alias_Of /= null then
            Set_Visibility (It, False);

            --  Duplicate the links if required
            Duplicate_Links (Canvas, Item);

         --  If we broke the alias, move the item back to some new coordinates
         elsif It.Was_Alias then
            Move_To (Canvas, It);
         end if;
         return True;
      end Recompute_Sizes;

   begin
      --  Remove all the temporary links and aliases
      For_Each_Link (Canvas, Remove_Temporary'Unrestricted_Access);
      For_Each_Item (Canvas, Remove_Aliases'Unrestricted_Access);

      --  First: Recompile all the addresses, and detect the aliases.
      if Get_Detect_Aliases (Canvas) then
         For_Each_Item (Canvas, Recompute_Address'Access);
      end if;

      --  Then re-parse the value of each item and display them again.
      For_Each_Item (Canvas, Update_On_Auto_Refresh'Access);

      --  Now that everything has been redimensionned, we can finish to
      --  manipulate the aliases
      For_Each_Item (Canvas, Recompute_Sizes'Unrestricted_Access);
   end Recompute_All_Aliases;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Item : access Display_Item_Record'Class) is
   begin
      Reset_Recursive (Item.Entity);
   end Reset_Recursive;

   -----------------
   -- Is_Alias_Of --
   -----------------

   function Is_Alias_Of (Item : access Display_Item_Record)
                        return Display_Item
   is
   begin
      return Item.Is_Alias_Of;
   end Is_Alias_Of;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : access Display_Item_Record) return String is
   begin
      if Item.Name = null then
         return "";
      end if;
      return Item.Name.all;
   end Get_Name;

end Display_Items;
