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

with Glib;             use Glib;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.GC;           use Gdk.GC;
with Gtk.Widget;       use Gtk.Widget;
with Gdk.Font;         use Gdk.Font;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Window;       use Gdk.Window;
pragma Warnings (Off);
with Gdk.Types;        use Gdk.Types;
pragma Warnings (On);
with Gdk.Event;        use Gdk.Event;
with Gtk.Menu;         use Gtk.Menu;
with Gtkada.Canvas;    use Gtkada.Canvas;

with Debugger;         use Debugger;
with Language;         use Language;
with Items;            use Items;
with Items.Simples;    use Items.Simples;

with Odd_Intl;         use Odd_Intl;
with GVD.Canvas;       use GVD.Canvas;
with GVD.Preferences;  use GVD.Preferences;
with GVD.Process;      use GVD.Process;
with GVD.Types;
with Basic_Types;      use Basic_Types;
with GVD.Trace;        use GVD.Trace;

package body Display_Items is

   ---------------------
   -- Local Constants --
   ---------------------

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

   Num_Buttons : constant := 2;
   --  Number of buttons in the title bar.
   --  This is not user-configurable.

   Item_Name_In_Link : constant String := "@";
   --  Shortcut used to represent the dereferenced item when generating a new
   --  item.

   Attach_Links_To_Components : constant Boolean := False;
   --  If True, then the links are attached to the middle of the actual
   --  component that was dereferenced, not to the middle of the item.

   Typed_Aliases : constant Boolean := True;
   --  If True, then two items are aliases only if they have the same address
   --  *and* they are structurally equivalent. If False, only the addresses
   --  are checked.

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
   --
   --  To improve the support for strings in Ada, an extra rule is added:
   --  X.all can not be an alias of X. It is always considered to be a
   --  different object. This is needed, otherwise it is mostly impossible to
   --  properly display a String parameter correctly.

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Variable_Name : String;
      Auto_Refresh  : Boolean := True);
   --  Item.Entity must have been parsed already.

   procedure Update_Display (Item : access Display_Item_Record'Class);
   --  Redraw the contents of item.
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
     (Item            : access Display_Item_Record;
      Deref_Component : Generic_Type_Access;
      X               : Gint;
      Y               : Gint);
   --  Create a new item (or reference an existing one) that dereference
   --  the field pointed to by (X, Y) in Item.
   --  (X, Y) are relative to the top-left corner of item.
   --  Deref_Component is the component on which the user has clicked and
   --  that is being derefenced.

   function Search_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Id     : String;
      Name   : String) return Display_Item;
   --  Search for an item whose Id is Id in the canvas.

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

   procedure Compute_Link_Pos (Link : access GVD_Link_Record'Class);
   --  Compute the attachment of the link in its source item.
   --  The position is based on the Source_Component field of the link.

   procedure Change_Visibility
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access);
   --  Change the visibility status of a specific component in the item

   function Create_Drawing_Context
     (Item : access Display_Item_Record'Class)
     return Drawing_Context;
   --  Return a graphic context that can be used to display the contents of
   --  the item

   function Is_Alias_Of
     (Item : access Display_Item_Record'Class;
      Id   : String;
      Name : String;
      Deref_Name : String) return Boolean;
   --  Return True if Item is an alias of the entity with name Name and
   --  whose Id is Id.
   --  Deref_Name should be the dereferenced version of Name

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item           : out Display_Item;
      Variable_Name  : String;
      Debugger       : access Debugger_Process_Tab_Record'Class;
      Auto_Refresh   : Boolean := True;
      Default_Entity : Items.Generic_Type_Access := null;
      Link_From      : Display_Item := null;
      Link_Name      : String := "")
   is
      Entity      : Generic_Type_Access := Default_Entity;
      Value_Found : Boolean := False;
      Alias_Item  : Display_Item;
      Id : String_Access;

   begin
      --  We need the information on the type, so that we detect aliases only
      --  for structurally equivalent types. If we have an error at this level,
      --  this means that the variable is unknown, and we don't create an item
      --  in that case.

      if Default_Entity = null then
         begin
            Entity := Parse_Type (Debugger.Debugger, Variable_Name);
         exception
            when Language.Unexpected_Type | Constraint_Error =>
               Output_Error
                 (Debugger.Window,
                  (-"Could not parse type for ") & Variable_Name);
               return;
         end;

         if Entity = null then
            Output_Error
              (Debugger.Window,
               (-"Could not get the type of ") & Variable_Name);
            return;
         end if;
      end if;

      --  If the an auto-updated similar item is on the canvas, we simply show
      --  and select it.

      if Variable_Name /= "" then
         Id := new String' (Get_Uniq_Id (Debugger.Debugger, Variable_Name));
      end if;

      if Auto_Refresh then
         --  The following call always returns null if the preference for
         --  alias detection is unset.

         Alias_Item :=
           Search_Item (Debugger.Data_Canvas, Id.all, Variable_Name);

         if Alias_Item /= null
           and then
           (not Typed_Aliases
            or else Structurally_Equivalent (Alias_Item.Entity, Entity))
         then
            Select_Item (Alias_Item, Alias_Item.Entity);
            Show_Item (Debugger.Data_Canvas, Alias_Item);

            if Default_Entity = null then
               Free (Entity);
            end if;

            Free (Id);

            if Link_From /= null then
               Create_Link
                 (Debugger.Data_Canvas, Link_From, Alias_Item, Link_Name);
               Refresh_Canvas (Debugger.Data_Canvas);
            end if;

            return;
         end if;
      end if;

      --  We now know there is no alias, so we can spend time parsing the value
      --  if necessary.


      if Default_Entity = null then
         begin
            Parse_Value
              (Debugger.Debugger, Variable_Name, Entity, Value_Found);

            if not Value_Found then
               Output_Error
                 (Debugger.Window,
                  (-"Could not get the value of ") & Variable_Name);
            end if;

         exception
            when Language.Unexpected_Type | Constraint_Error =>
               Output_Error
                 (Debugger.Window,
                  (-"Could not parse the value for ") & Variable_Name);
               return;
         end;

      else
         Value_Found := True;
      end if;

      --  Create the item

      Item := new Display_Item_Record;
      Item.Entity := Entity;
      Item.Is_A_Variable := (Default_Entity = null);
      Item.Num := Get_Next_Item_Num (GVD_Canvas (Debugger.Data_Canvas));
      Item.Debugger := Debugger_Process_Tab (Debugger);
      Set_Valid (Item.Entity, Value_Found);
      Item.Id := Id;

      Display_Items.Initialize (Item, Variable_Name, Auto_Refresh);

      if Link_From /= null then
         Item.Is_Dereference := True;
         Create_Link (Debugger.Data_Canvas, Link_From, Item, Link_Name);
         Put (Debugger.Data_Canvas, Item);
      end if;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Variable_Name : String;
      Auto_Refresh  : Boolean := True)
   is
      use type Gdk.GC.Gdk_GC;
   begin
      Item.Name          := new String'(Variable_Name);
      Item.Auto_Refresh  := Auto_Refresh;
      Item.Is_Dereference := False;

      if not Is_Visible (Item) then
         return;
      end if;

      --  Compute the size, hidding if necessary the big components. However,
      --  we never want the top level item to be hidden, so we force it to
      --  visible (and possibly recalculate the size).

      Size_Request
        (Item.Entity.all,
         Create_Drawing_Context (Item),
         Hide_Big_Items => Get_Pref (Hide_Big_Items));

      if not Get_Visibility (Item.Entity.all) then
         Set_Visibility (Item.Entity, True);
         Size_Request (Item.Entity.all, Create_Drawing_Context (Item));
      end if;

      Constraint_Size (Item.Entity.all);

      Update_Display (Item);
      Refresh_Canvas (Item.Debugger.Data_Canvas);
   end Initialize;

   -----------------
   -- Is_Alias_Of --
   -----------------

   function Is_Alias_Of
     (Item : access Display_Item_Record'Class;
      Id   : String;
      Name : String;
      Deref_Name : String) return Boolean is
   begin
      --  Do not detect aliases that are already aliases, so as to
      --  avoid chains of aliases.
      --  Note also that X.all can not be an alias of X, so as to properly
      --  display string parameters in Ada (they appear otherwise as access
      --  types, which, once dereferenced, would point to themselves).

      return Item.Id /= null
        and then Item.Auto_Refresh
        and then Item.Id.all = Id
        and then Item.Name.all /= Deref_Name
        and then Name /= Dereference_Name
        (Get_Language (Item.Debugger.Debugger), Item.Name.all);
   end Is_Alias_Of;

   ---------------
   -- Find_Item --
   ---------------

   function Find_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Num    : Integer) return Display_Item
   is
      Found : Display_Item := null;

      function Search_By_Num
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Search for the item whose number is Num.

      -------------------
      -- Search_By_Num --
      -------------------

      function Search_By_Num
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
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

      Context : constant Box_Drawing_Context :=
        Get_Box_Context (GVD_Canvas (Item.Debugger.Data_Canvas));
      Zoom_Spacing : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Spacing);
      Zoom_Buttons : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Buttons_Size);
      Zoom_Border : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Border_Spacing);
      W, H : Gint;

      use Gdk;

   begin
      --  Compute the required size for the value itself.

      Alloc_Width  := Get_Width  (Item.Entity.all) + 2 * Zoom_Border;
      Alloc_Height := Get_Height (Item.Entity.all) + 2 * Zoom_Border;

      --  Compute the width and height of the title bar

      Num_Width := GVD_Text_Width
        (Context.Title_Font, Integer'Image (Item.Num) & ": ");
      Title_Width := (5 + Num_Buttons) * Zoom_Spacing
        + GVD_Text_Width (Context.Title_Font, Item.Name.all)
        + Num_Buttons * Zoom_Buttons + Num_Width;
      Title_Height := Gint'Max
        (GVD_Font_Height (Context.Title_Font), Zoom_Buttons)
        + 2 * Zoom_Spacing;
      Item.Title_Height := Title_Height;

      --  Finally, we can find the total size for the display item.

      Alloc_Width := Gint'Max (Alloc_Width, Title_Width);
      Alloc_Width := Gint'Max
        (Alloc_Width, To_Canvas_Coordinates (Item.Debugger.Data_Canvas, 40));
      Alloc_Height := Title_Height + Alloc_Height;

      Propagate_Width (Item.Entity.all, Alloc_Width - 2 * Zoom_Border);

      --  3D Look ? If yes, keep some space for the shadow.

      if Get_Pref (Look_3d) then
         Set_Screen_Size_And_Pixmap
           (Item,
            Get_Window (Item.Debugger.Data_Canvas),
            Alloc_Width + 1, Alloc_Height + 1);
      else
         Set_Screen_Size_And_Pixmap
           (Item, Get_Window (Item.Debugger.Data_Canvas),
            Alloc_Width, Alloc_Height);
      end if;

      if Item.Auto_Refresh then
         Draw_Rectangle
           (Pixmap (Item),
            GC     => Context.Thaw_Bg_GC,
            Filled => True,
            X      => 0,
            Y      => Title_Height,
            Width  => Alloc_Width,
            Height => Alloc_Height - Title_Height);

      else
         Draw_Rectangle
           (Pixmap (Item),
            GC     => Context.Freeze_Bg_GC,
            Filled => True,
            X      => 0,
            Y      => Title_Height,
            Width  => Alloc_Width,
            Height => Alloc_Height - Title_Height);
      end if;

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Context.Grey_GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => Alloc_Width - 1,
         Height => Title_Height);

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Context.Black_GC,
         Filled => False,
         X      => 0,
         Y      => 0,
         Width  => Alloc_Width - 1,
         Height => Alloc_Height - 1);

      if Get_Pref (Look_3d) then
         Draw_Line
           (Pixmap (Item),
            GC   => Context.Black_GC,
            X1   => Alloc_Width,
            Y1   => 2,
            X2   => Alloc_Width,
            Y2   => Alloc_Height);
         Draw_Line
           (Pixmap (Item),
            GC   => Context.Black_GC,
            X1   => 1,
            Y1   => Alloc_Height,
            X2   => Alloc_Width,
            Y2   => Alloc_Height);
      end if;

      Draw_Line
        (Pixmap (Item),
         GC     => Context.Black_GC,
         X1     => 0,
         Y1     => Title_Height,
         X2     => Alloc_Width - 1,
         Y2     => Title_Height);

      if Context.Title_Font /= null then
         Draw_Text
           (Pixmap (Item),
            Font   => Context.Title_Font,
            GC     => Context.Black_GC,
            X      => Zoom_Spacing,
            Y      => Zoom_Spacing + Get_Ascent (Context.Title_Font),
            Text   => Integer'Image (Item.Num) & ":");

         Draw_Text
           (Pixmap (Item),
            Font   => Context.Title_Font,
            GC     => Context.Black_GC,
            X      => Zoom_Spacing + Num_Width,
            Y      => Zoom_Spacing + Get_Ascent (Context.Title_Font),
            Text   => Item.Name.all);
      end if;

      --  First button

      Set_Auto_Refresh
        (Item, Get_Window (Item.Debugger.Data_Canvas), Item.Auto_Refresh);

      --  Second button

      Set_Clip_Mask (Context.Black_GC, Context.Close_Mask);
      Set_Clip_Origin
        (Context.Black_GC,
         Alloc_Width - Zoom_Buttons - Zoom_Spacing,
         Zoom_Spacing);
      Get_Size (Context.Close_Pixmap, W, H);
      Draw_Pixmap
        (Pixmap (Item),
         GC     => Context.Black_GC,
         Src    => Context.Close_Pixmap,
         Xsrc   => 0,
         Ysrc   => 0,
         Xdest  => Alloc_Width - Zoom_Buttons - Zoom_Spacing,
         Ydest  => Zoom_Spacing,
         Width  => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, W),
         Height => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, H));
      Set_Clip_Mask (Context.Black_GC, Null_Pixmap);
      Set_Clip_Origin (Context.Black_GC, 0, 0);

      if Item.Entity /= null then
         Paint
           (Item.Entity.all,
            Create_Drawing_Context (Item),
            X => Zoom_Border,
            Y => Title_Height + Zoom_Border);
      end if;
   end Update_Display;

   ----------------------
   -- Update_Component --
   ----------------------

   procedure Update_Component
     (Item : access Display_Item_Record'Class;
      Component : Generic_Type_Access := null)
   is
      Context : constant Box_Drawing_Context :=
        Get_Box_Context (GVD_Canvas (Item.Debugger.Data_Canvas));
   begin
      if not Get_Selected (Component) then
         if Item.Auto_Refresh then
            Draw_Rectangle
              (Pixmap (Item),
               GC     => Context.Thaw_Bg_GC,
               Filled => True,
               X      => Get_X (Component.all),
               Y      => Get_Y (Component.all),
               Width  => Get_Width (Component.all),
               Height => Get_Height (Component.all));

         else
            Draw_Rectangle
              (Pixmap (Item),
               GC     => Context.Freeze_Bg_GC,
               Filled => True,
               X      => Get_X (Component.all),
               Y      => Get_Y (Component.all),
               Width  => Get_Width (Component.all),
               Height => Get_Height (Component.all));
         end if;
      end if;

      Paint
        (Component.all,
         Create_Drawing_Context (Item),
         X => Get_X (Component.all),
         Y => Get_Y (Component.all));
   end Update_Component;

   -----------------
   -- Search_Item --
   -----------------

   function Search_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Id     : String;
      Name   : String) return Display_Item
   is
      Alias_Item : Display_Item := null;
      Deref_Name : constant String := Dereference_Name
        (Get_Language
         (Debugger_Process_Tab (Get_Process (GVD_Canvas (Canvas))).Debugger),
         Name);

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
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Is_Alias_Of (Display_Item (Item), Id, Name, Deref_Name) then
            if Display_Item (Item).Is_Alias_Of /= null then
               Alias_Item := Display_Item (Item).Is_Alias_Of;
            else
               Alias_Item := Display_Item (Item);
            end if;
            return False;
         end if;

         return True;
      end Alias_Found;

   begin
      --  Always search if we have a special name to look for, so as to avoid
      --  creating the same item multiple times
      if Name = "" or else Get_Detect_Aliases (GVD_Canvas (Canvas)) then
         For_Each_Item (Canvas, Alias_Found'Unrestricted_Access);
      end if;

      return Alias_Item;
   end Search_Item;

   ----------------------------
   -- Update_On_Auto_Refresh --
   ----------------------------

   function Update_On_Auto_Refresh
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean is
   begin
      --  Only update when the item is not an alias of something else (and thus
      --  is hidden).

      if Display_Item (Item).Auto_Refresh
        and then Display_Item (Item).Is_Alias_Of = null
      then
         Update (GVD_Canvas (Canvas), Display_Item (Item));
      end if;

      return True;
   end Update_On_Auto_Refresh;

   ------------
   -- Update --
   ------------

   procedure Update
     (Canvas           : access Interactive_Canvas_Record'Class;
      Item             : access Display_Item_Record'Class;
      Redisplay_Canvas : Boolean := False)
   is
      Value_Found : Boolean;
      Was_Visible : Boolean := Get_Visibility (Item.Entity.all);

   begin
      --  Parse the value

      if Item.Entity.all in Debugger_Output_Type'Class then
         Set_Value
           (Debugger_Output_Type (Item.Entity.all),
            Send (Item.Debugger.Debugger,
                  Refresh_Command (Debugger_Output_Type (Item.Entity.all)),
                  Mode => GVD.Types.Internal));

      elsif Item.Name /= null then
         Parse_Value (Item.Debugger.Debugger, Item.Name.all,
                      Item.Entity, Value_Found);
         Set_Valid (Item.Entity, Value_Found);
      end if;

      Update_Resize_Display
        (Item, Was_Visible, Get_Pref (Hide_Big_Items),
         Redisplay_Canvas => Redisplay_Canvas);

      --  If we got an exception while parsing the value, we register the new
      --  value as being incorrect.
   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Set_Valid (Item.Entity, False);
   end Update;

   ---------------------------
   -- Update_Resize_Display --
   ---------------------------

   procedure Update_Resize_Display
     (Item             : access Display_Item_Record'Class;
      Was_Visible      : Boolean := False;
      Hide_Big         : Boolean := False;
      Redisplay_Canvas : Boolean := True) is
   begin
      --  Update graphically.
      --  Note that we should not change the visibility status of item
      --  and its children.

      Size_Request
        (Item.Entity.all,
         Create_Drawing_Context (Item),
         Hide_Big_Items => not Was_Visible and then Hide_Big);

      Constraint_Size (Item.Entity.all);

      Update_Display (Item);

      if Redisplay_Canvas then
         Refresh_Canvas (Item.Debugger.Data_Canvas);
      end if;
   end Update_Resize_Display;

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
      L : GVD_Link;
   begin
      if not Has_Link (Canvas, From, To, Name) then
         L := new GVD_Link_Record;
         L.Alias_Link := Alias_Link;
         Add_Link (Canvas, L, From, To, Arrow, Name);
      end if;
   end Create_Link;

   ----------------------
   -- Compute_Link_Pos --
   ----------------------

   procedure Compute_Link_Pos (Link : access GVD_Link_Record'Class) is
      X : constant Gint := Get_X (Link.Source_Component.all) +
        Get_Width (Link.Source_Component.all) / 2;
      Y : constant Gint := Get_Y (Link.Source_Component.all) +
        Get_Height (Link.Source_Component.all) / 2;
      Xpos, Ypos : Gfloat;
      F : Boolean;
      Visible : Boolean;

   begin
      Component_Is_Visible
        (Display_Item (Get_Src (Link)).Entity, Link.Source_Component,
         Visible, F);

      if Visible and F then
         Xpos := Gfloat (X)
           / Gfloat (Get_Coord (Canvas_Item (Get_Src (Link))).Width);
         Ypos := Gfloat (Y)
           / Gfloat (Get_Coord (Canvas_Item (Get_Src (Link))).Height);
         Set_Src_Pos (Link, Xpos, Ypos);

      else
         Set_Src_Pos (Link, 0.5, 0.5);
      end if;
   end Compute_Link_Pos;

   ----------------------
   -- Dereference_Item --
   ----------------------

   procedure Dereference_Item
     (Item            : access Display_Item_Record;
      Deref_Component : Generic_Type_Access;
      X               : Gint;
      Y               : Gint)
   is
      Name : constant String := Get_Component_Name
        (Item.Entity,
         Get_Language (Item.Debugger.Debugger),
         Item.Name.all,
         X, Y);
      Link_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger),
         Get_Component_Name
           (Item.Entity,
            Get_Language (Item.Debugger.Debugger), Item_Name_In_Link, X, Y));
      New_Name : constant String := Dereference_Name
        (Get_Language (Item.Debugger.Debugger), Name);

      function Set_Link_Pos
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Set the attachment position of the newly created link.
      --  The link is attached to the middle of the component that was
      --  dereferenced.

      ------------------
      -- Set_Link_Pos --
      ------------------

      function Set_Link_Pos
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean is
      begin
         GVD_Link (Link).Source_Component := Deref_Component;
         Compute_Link_Pos (GVD_Link (Link));
         --  Only the first one
         return False;
      end Set_Link_Pos;

   begin
      --  The newly created item should have the same auto-refresh state as
      --  the one we are dereferencing

      if Item.Auto_Refresh then
         Process_User_Command
           (Item.Debugger,
            "graph display " & New_Name & " dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link_Name,
            Output_Command => True);
      else
         Process_User_Command
           (Item.Debugger,
            "graph print " & New_Name & " dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link_Name,
            Output_Command => True);
      end if;

      if Attach_Links_To_Components then
         For_Each_Link
           (Item.Debugger.Data_Canvas, Set_Link_Pos'Unrestricted_Access);
      end if;
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
         Update_Component
           (Display_Item (Item.Debugger.Selected_Item),
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
      Zoom_Spacing : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Spacing);
      Zoom_Buttons : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Buttons_Size);
      Zoom_Buttons_Size : constant Gint :=
        Gint'Min (Buttons_Size, Zoom_Buttons);
      Zoom_Border  : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Border_Spacing);

      Buttons_Start : Gint :=
        Gint (Get_Coord (Item).Width) - Num_Buttons * Zoom_Buttons -
          Num_Buttons * Zoom_Spacing + 1;
      Component : Generic_Type_Access;

   begin
      --  Click on a button ?

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
        and then Gint (Get_Y (Event)) > Zoom_Spacing
        and then Gint (Get_Y (Event)) <= Zoom_Spacing + Zoom_Buttons_Size
      then
         for B in 0 .. Num_Buttons - 1 loop
            if Gint (Get_X (Event)) >= Buttons_Start
              and then Gint (Get_X (Event)) <=
                Buttons_Start + Zoom_Buttons_Size
            then
               case B is
                  when 0 =>
                     if Item.Auto_Refresh then
                        Process_User_Command
                          (Item.Debugger,
                           "graph disable display" & Integer'Image (Item.Num),
                           Output_Command => True);

                     else
                        Process_User_Command
                          (Item.Debugger,
                           "graph enable display" & Integer'Image (Item.Num),
                           Output_Command => True);
                     end if;

                  when 1 =>
                     Process_User_Command
                       (Item.Debugger,
                        "graph undisplay" & Integer'Image (Item.Num),
                        Output_Command => True);

               end case;

               return;
            end if;

            Buttons_Start := Buttons_Start + Zoom_Buttons + Zoom_Spacing;
         end loop;

         return;
      end if;

      --  Raise or lower the item

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
        and then Gint (Get_Y (Event)) <= Item.Title_Height
      then
         if Is_On_Top (Item.Debugger.Data_Canvas, Item) then
            Lower_Item (Item.Debugger.Data_Canvas, Item);
         else
            Raise_Item (Item.Debugger.Data_Canvas, Item);
         end if;
      end if;

      --  Get the selected component

      Component := Get_Component
        (Item.Entity, Gint (Get_X (Event)),
         Gint (Get_Y (Event)) - Item.Title_Height - Zoom_Border);

      --  Contextual menus ?

      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Popup
           (Item_Contextual_Menu
             (GVD_Canvas (Item.Debugger.Data_Canvas),
              Item,
              Component,
              Get_Component_Name
                (Item.Entity,
                 Get_Language (Item.Debugger.Debugger),
                 Item.Name.all,
                 Gint (Get_X (Event)),
                 Gint (Get_Y (Event)) - Item.Title_Height - Zoom_Border)),
            Button            => Get_Button (Event),
            Activate_Time     => Get_Time (Event));

      --  Dereferencing access types.

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_Y (Event)) >= Item.Title_Height + Zoom_Border
        and then Component.all in Access_Type'Class
      then
         Dereference_Item
           (Item,
            Component,
            Gint (Get_X (Event)),
            Gint (Get_Y (Event)) - Item.Title_Height - Zoom_Border);

      --  Hiding a component

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Gint (Get_Y (Event)) >= Item.Title_Height + Zoom_Border
      then
         Change_Visibility (Item, Component);

      --  Selecting a component

      elsif Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
        and then Gint (Get_Y (Event)) > Item.Title_Height
      then
         Select_Item (Item, Component);
      end if;
   end On_Button_Click;

   -----------------------
   -- Change_Visibility --
   -----------------------

   procedure Change_Visibility
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access)
   is
      function Reattach_All_Links
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Recompute the position of all the links attached to Item.

      ------------------------
      -- Reattach_All_Links --
      ------------------------

      function Reattach_All_Links
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean is
      begin
         if Canvas_Item (Get_Src (Link)) = Canvas_Item (Item)
           and then GVD_Link (Link).Source_Component /= null
         then
            Compute_Link_Pos (GVD_Link (Link));
         end if;

         return True;
      end Reattach_All_Links;

   begin
      Set_Visibility (Component, not Get_Visibility (Component.all));
      --  Need to recompute the positions of the components first
      Update_Resize_Display
        (Item,
         Was_Visible      => False,
         Hide_Big         => False,
         Redisplay_Canvas => False);

      --  If needed, recompute the position of the links
      if Attach_Links_To_Components then
         For_Each_Link
           (Item.Debugger.Data_Canvas,
            Reattach_All_Links'Unrestricted_Access);
      end if;

      --  Redraw the canvas
      Refresh_Canvas (Item.Debugger.Data_Canvas);
   end Change_Visibility;

   ----------------------
   -- Set_Auto_Refresh --
   ----------------------

   procedure Set_Auto_Refresh
     (Item         : access Display_Item_Record;
      Win          : Gdk.Window.Gdk_Window;
      Auto_Refresh : Boolean;
      Update_Value : Boolean := False)
   is
      Zoom_Spacing : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Spacing);
      Zoom_Buttons : constant Gint :=
        To_Canvas_Coordinates (Item.Debugger.Data_Canvas, Buttons_Size);

      Width : Gint := Gint (Get_Coord (Item).Width);
      Context : constant Box_Drawing_Context :=
        Get_Box_Context (GVD_Canvas (Item.Debugger.Data_Canvas));
      W, H : Gint;
   begin
      Item.Auto_Refresh := Auto_Refresh;

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Context.Grey_GC,
         Filled => True,
         X      => Width - 2 * Zoom_Buttons - 2 * Zoom_Spacing,
         Y      => Zoom_Spacing,
         Width  => Zoom_Buttons,
         Height => Zoom_Buttons);
      Set_Clip_Origin
        (Context.Black_GC,
         Width - 2 * Zoom_Buttons - 2 * Zoom_Spacing, Zoom_Spacing);

      if Item.Auto_Refresh then
         Set_Clip_Mask (Context.Black_GC, Context.Auto_Display_Mask);
         Get_Size (Context.Auto_Display_Pixmap, W, H);
         Draw_Pixmap
           (Pixmap (Item),
            GC     => Context.Black_GC,
            Src    => Context.Auto_Display_Pixmap,
            Xsrc   => 0,
            Ysrc   => 0,
            Xdest  => Width - 2 * Zoom_Buttons - 2 * Zoom_Spacing,
            Ydest  => Zoom_Spacing,
            Width  => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, W),
            Height => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, H));

      else
         Set_Clip_Mask (Context.Black_GC, Context.Locked_Mask);
         Get_Size (Context.Locked_Pixmap, W, H);
         Draw_Pixmap
           (Pixmap (Item),
            GC     => Context.Black_GC,
            Src    => Context.Locked_Pixmap,
            Xsrc   => 0,
            Ysrc   => 0,
            Xdest  => Width - 2 * Zoom_Buttons - 2 * Zoom_Spacing,
            Ydest  => Zoom_Spacing,
            Width  => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, W),
            Height => To_Canvas_Coordinates (Item.Debugger.Data_Canvas, H));
      end if;

      Set_Clip_Mask (Context.Black_GC, Null_Pixmap);
      Set_Clip_Origin (Context.Black_GC, 0, 0);

      if Update_Value then
         --  If we moved back to the auto-refresh state, force an
         --  update of the value.

         if Item.Auto_Refresh then
            Update (Item.Debugger.Data_Canvas, Item);
            Refresh_Canvas (Item.Debugger.Data_Canvas);
         else
            --  Redisplay the item, so that no field is displayed
            --  in red anymore.
            Reset_Recursive (Item);
            Update_Display (Item);
            Item_Updated (Item.Debugger.Data_Canvas, Item);
         end if;
      end if;
   end Set_Auto_Refresh;

   ----------------------
   -- Get_Auto_Refresh --
   ----------------------

   function Get_Auto_Refresh
     (Item : access Display_Item_Record) return Boolean is
   begin
      return Item.Auto_Refresh;
   end Get_Auto_Refresh;

   ----------
   -- Free --
   ----------

   procedure Free
     (Item : access Display_Item_Record;
      Remove_Aliases : Boolean := True)
   is
      function Free_Alias
        (Canvas : access Interactive_Canvas_Record'Class;
         It     : access Canvas_Item_Record'Class) return Boolean;
      --  If It is an alias of Item, and it wasn't displayed explicitly
      --  by the user, then remove it from the canvas as well.
      --  Also remove It if it is currently hidden (alias detection), and was
      --  linked to Item. The user probably expects it to be killed as well

      ----------------
      -- Free_Alias --
      ----------------

      function Free_Alias
        (Canvas : access Interactive_Canvas_Record'Class;
         It     : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Display_Item (It).Is_Alias_Of = Display_Item (Item)
           and then Display_Item (It).Is_Dereference
         then
            Free (Display_Item (It), Remove_Aliases => False);

         --  If It is hidden, and was linked to Item
         elsif Display_Item (It).Is_Alias_Of /= null
           and then Has_Link (Canvas, Item, It)
         then
            Free (Display_Item (It), Remove_Aliases => False);
         end if;

         return True;
      end Free_Alias;

      Canvas : constant GVD_Canvas := GVD_Canvas (Item.Debugger.Data_Canvas);

   begin
      if Item.Debugger.Selected_Item = Canvas_Item (Item) then
         Item.Debugger.Selected_Item := null;
      end if;

      --  Should recompute aliases (delete all the items that we aliased
      --  to this one, since the user was probably expecting them not to be
      --  visible any more).

      if Remove_Aliases then
         For_Each_Item (Canvas, Free_Alias'Unrestricted_Access);
      end if;

      Free (Item.Entity);
      Free (Item.Name);
      Free (Item.Id);
      Remove (Canvas, Item);
      --  Warning: the memory has been freed after Remove.

      if Remove_Aliases then
         Recompute_All_Aliases (Canvas, Recompute_Values => False);
      end if;
   end Free;

   -------------------------
   -- On_Background_Click --
   -------------------------

   procedure On_Background_Click
     (The_Canvas : access Gtk_Widget_Record'Class;
      Event      : Gdk.Event.Gdk_Event)
   is
      Canvas : constant GVD_Canvas := GVD_Canvas (The_Canvas);

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
      It_Deref_Name : constant String := Dereference_Name
        (Get_Language (It.Debugger.Debugger), It.Name.all);

      function Clean_Alias_Chain
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Avoid alias chains in the canvas (ie every Item was an alias of It,
      --  it becomes an alias of It.Is_Alias_Of instead).
      --  This way, all the Is_Alias_Of fields always reference real visible
      --  items on the canvas.

      function Has_Alias
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  True if Item is a possible alias of It.
      --  We only check the items before It in the list of items

      -----------------------
      -- Clean_Alias_Chain --
      -----------------------

      function Clean_Alias_Chain
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Display_Item (Item).Is_Alias_Of = It then
            Display_Item (Item).Is_Alias_Of := It.Is_Alias_Of;
         end if;

         return True;
      end Clean_Alias_Chain;

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

         --  Frozen items can not be part of an alias detection

         if not It2.Auto_Refresh then
            return False;
         end if;

         if It.Id /= null
           and then It2.Is_Alias_Of = null
           and then Is_Alias_Of (It2, It.Id.all, It.Name.all, It_Deref_Name)
           and then (not Typed_Aliases
                     or else Structurally_Equivalent (It2.Entity, It.Entity))
         then
            --  Keep only one of them:
            --   - if none are the result of a dereference, keep them both
            --   - if only one is the result of a dereference, keep the other
            --   - otherwise keep the one with the shortest name

            if It2.Is_Dereference and then not It.Is_Dereference then
               It2.Is_Alias_Of := It;
               It := It2;
               For_Each_Item (Canvas, Clean_Alias_Chain'Unrestricted_Access);
               return False;

            elsif It.Is_Dereference and then not It2.Is_Dereference then
               It.Is_Alias_Of := It2;
               For_Each_Item (Canvas, Clean_Alias_Chain'Unrestricted_Access);
               return False;

            elsif It.Is_Dereference and then It2.Is_Dereference then
               if It.Name /= null
                 and then It2.Name /= null
                 and then It2.Name'Length < It.Name'Length
               then
                  It.Is_Alias_Of := It2;
                  For_Each_Item
                    (Canvas, Clean_Alias_Chain'Unrestricted_Access);
               else
                  It2.Is_Alias_Of := It;
                  It := It2;
                  For_Each_Item
                    (Canvas, Clean_Alias_Chain'Unrestricted_Access);
               end if;
               return False;

            --  They both were explicitly displayed by the user. Print a
            --  special link to reflect that fact.

            else
               Create_Link
                 (Canvas, It, It2, "<=>", Both_Arrow, Alias_Link => True);
            end if;
         end if;

         return True;
      end Has_Alias;

   begin
      --  If this is not an item associated with a variable, ignore it.
      if It.Name = null then
         return True;
      end if;

      --  Only detect aliases if we have an auto_refresh item
      if not It.Auto_Refresh
        or else not Is_A_Variable (It)
      then
         return True;
      end if;

      --  Else recompute the id for the item
      Free (It.Id);
      declare
         Id : String := Get_Uniq_Id (It.Debugger.Debugger, It.Name.all);
      begin
         if Id /= "" then
            It.Id := new String' (Id);
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
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Duplicate Link if it is bound to Item and is not a temporary link

      ---------------
      -- Duplicate --
      ---------------

      function Duplicate
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
         Src, Dest : Canvas_Item;
         Replace   : Boolean;

      begin
         if not GVD_Link (Link).Alias_Link then
            Src := Canvas_Item (Get_Src (Link));
            Dest := Canvas_Item (Get_Dest (Link));
            Replace := False;

            if Canvas_Item (Get_Src (Link)) = Canvas_Item (Item) then
               Src := Canvas_Item (Display_Item (Item).Is_Alias_Of);
               Replace := True;
            end if;

            if Canvas_Item (Get_Dest (Link)) = Canvas_Item (Item) then
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
      Canvas : constant GVD_Canvas :=
        GVD_Canvas (Debugger_Process_Tab (Object).Data_Canvas);
   begin
      Recompute_All_Aliases (Canvas);
      Refresh_Canvas (Canvas);
   end On_Canvas_Process_Stopped;

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
      Link   : access Canvas_Link_Record'Class) return Boolean is
   begin
      if GVD_Link (Link).Alias_Link then
         Remove_Link (Canvas, Link);
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

   ---------------------------
   -- Recompute_All_Aliases --
   ---------------------------

   procedure Recompute_All_Aliases
     (Canvas           : access Interactive_Canvas_Record'Class;
      Recompute_Values : Boolean := True) is
   begin
      --  Remove all the temporary links and aliases
      For_Each_Link (Canvas, Remove_Temporary'Access);
      For_Each_Item (Canvas, Remove_Aliases'Access);

      --  First: Recompile all the addresses, and detect the aliases.
      if Get_Detect_Aliases (GVD_Canvas (Canvas)) then
         For_Each_Item (Canvas, Recompute_Address'Access);
      end if;

      --  Then re-parse the value of each item and display them again.
      if Recompute_Values then
         For_Each_Item (Canvas, Update_On_Auto_Refresh'Access);
      end if;

      --  Now that everything has been redimensionned, we can finish to
      --  manipulate the aliases
      For_Each_Item (Canvas, Recompute_Sizes'Access);
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

   function Is_Alias_Of
     (Item : access Display_Item_Record) return Display_Item is
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

   -------------
   -- Get_Num --
   -------------

   function Get_Num (Item : access Display_Item_Record) return Integer is
   begin
      return Item.Num;
   end Get_Num;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (Item : access Display_Item_Record'Class) return Debugger_Process_Tab is
   begin
      return Item.Debugger;
   end Get_Debugger;

   -------------------
   -- Is_A_Variable --
   -------------------

   function Is_A_Variable
     (Item : access Display_Item_Record'Class) return Boolean is
   begin
      return Item.Is_A_Variable;
   end Is_A_Variable;

   ----------------------
   -- Set_Display_Mode --
   ----------------------

   procedure Set_Display_Mode
     (Item : access Display_Item_Record'Class;
      Mode : Items.Display_Mode) is
   begin
      Item.Mode := Mode;

      --  Hide_Big is set to False, since we don't want to change the
      --  visibility state of the item.
      Update_Resize_Display
        (Item, Get_Visibility (Item.Entity.all), Hide_Big => False);
   end Set_Display_Mode;

   ----------------------
   -- Get_Display_Mode --
   ----------------------

   function Get_Display_Mode
     (Item : access Display_Item_Record) return Items.Display_Mode is
   begin
      return Item.Mode;
   end Get_Display_Mode;

   ----------------------------
   -- Create_Drawing_Context --
   ----------------------------

   function Create_Drawing_Context
     (Item : access Display_Item_Record'Class) return Drawing_Context is
   begin
      return Create_Drawing_Context
        (Canvas => Item.Debugger.Data_Canvas,
         Pixmap => Pixmap (Item),
         Mode   => Item.Mode,
         Lang   => Get_Language (Item.Debugger.Debugger));
   end Create_Drawing_Context;

   ----------------------------
   -- Create_Drawing_Context --
   ----------------------------

   function Create_Drawing_Context
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mode   : Items.Display_Mode := Value;
      Lang   : Language.Language_Access := null) return Drawing_Context
   is
      D : Drawing_Context := Get_Item_Context (GVD_Canvas (Canvas));
   begin
      D.Pixmap := Pixmap;
      D.Mode := Mode;
      D.Lang := Lang;
      return D;
   end Create_Drawing_Context;

   ------------------------------------
   -- Create_Tooltip_Drawing_Context --
   ------------------------------------

   function Create_Tooltip_Drawing_Context
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap) return Drawing_Context
   is
      D : Drawing_Context := Get_Tooltip_Context (GVD_Canvas (Canvas));
   begin
      D.Pixmap := Pixmap;
      return D;
   end Create_Tooltip_Drawing_Context;

end Display_Items;
