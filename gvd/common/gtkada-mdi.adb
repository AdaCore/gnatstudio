-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;             use Glib;
with Gdk;              use Gdk;
with Gdk.Color;        use Gdk.Color;
with Gdk.Cursor;       use Gdk.Cursor;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Main;         use Gdk.Main;
with Gdk.Pixmap;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Region;       use Gdk.Region;
with Gdk.Types;        use Gdk.Types;
with Gdk.Window;       use Gdk.Window;
with Gtk.Accel_Label;  use Gtk.Accel_Label;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Event_Box;    use Gtk.Event_Box;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Gtk.Layout;       use Gtk.Layout;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Object;       use Gtk.Object;
with Gtk.Pixmap;       use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with GNAT.OS_Lib;      use GNAT.OS_Lib;

package body Gtkada.MDI is

   Title_Bar_Focus_Color : constant String := "#000088";
   --  <preference> Color to use for the title bar of the child that has
   --  the focus

   Title_Bar_Color : constant String := "#AAAAAA";
   --  <preference> Color to use for the title bar of children that do not
   --  have the focus.

   Title_Bar_Height : constant Gint := 15;
   --  <preference> Height of the title bar for the children

   Border_Thickness : constant Gint := 4;
   --  <preference> Thickness of the windows in the MDI

   Title_Font_Name : constant String := "Helvetica";
   --  <preference> Name of the font to use in the title bar

   Title_Font_Height : constant Gint := 10;
   --  <preference> Height of the font to use in the title bar

   Icons_Width : constant Gint := 100;
   --  <preference> Width to use for icons

   Opaque_Resize : constant Boolean := False;
   --  <preference> True if the contents of windows should be displayed while
   --  resizing widgets.

   Opaque_Move : constant Boolean := False;
   --  <preference> True if the contents of windows should be displayed while
   --  they are moved.

   Do_Grabs : constant Boolean := True;
   --  Should be set to False when debugging, so that pointer grabs are not
   --  done.

   Min_Width : constant Gint := 40;
   Min_Height : constant Gint := 2 * Border_Thickness + Title_Bar_Height;
   --  Minimal size for all windows

   Corner_Size : constant Gint := Border_Thickness * 2;
   --  Extra tolerance when the user selects a corner for resizing (if the
   --  pointer is within Corner_Size in both coordinates, then we are clicking
   --  on the corner)

   Close_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 3 1"),
      New_String ("      c None"),
      New_String (".     c #C0C0C0"),
      New_String ("+     c #000000"),
      New_String ("............."),
      New_String ("............."),
      New_String ("...++....++.."),
      New_String ("....++..++..."),
      New_String (".....++++...."),
      New_String ("......++....."),
      New_String (".....++++...."),
      New_String ("....++..++..."),
      New_String ("...++....++.."),
      New_String ("............."),
      New_String ("............."));

   Iconify_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 2 1"),
      New_String (". c #000000"),
      New_String ("# c #c0c0c0"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("##.......####"),
      New_String ("##.......####"),
      New_String ("#############"),
      New_String ("#############"));

   Maximize_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 3 1"),
      New_String ("       c None"),
      New_String (".      c #C0C0C0"),
      New_String ("+      c #000000"),
      New_String ("............."),
      New_String ("....+++++++.."),
      New_String ("....+++++++.."),
      New_String ("....+.....+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+.....+++.."),
      New_String ("..+.....+...."),
      New_String ("..+.....+...."),
      New_String ("..+++++++...."),
      New_String ("............."));

   use Widget_List;

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  Test whether an item was selected.

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, refresh the canvas.

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  The pointer has left the mouse.

   function Side
     (Child : access MDI_Child_Record'Class; X, Y : Gint)
      return Gdk_Cursor_Type;
   --  Return the cursor to use depending on the coordinates (X, Y) inside
   --  child.

   procedure Layout_Child
     (Child  : access MDI_Child_Record'Class;
      Region : Gdk.Region.Gdk_Region := null);
   --  Compute the coordinates for Child.
   --  If Region is null, loop through the list of all children, and try
   --  to position the child in an area where it doesn't overlap any child.
   --  If Region is not null, we use this to check that the child doesn't
   --  overlap any widget.

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward a delete_event from the toplevel window to the child.

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class);
   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class);
   --  Called when either the child itself, or the widget we initially put
   --  in it, are destroyed. Remove the child from the MDI properly.

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the MDI is destroyed.

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class);
   --  Called when the Menu_Item associated with a Child is destroyed.

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class);
   --  Called when the Menu associated with a MDI is destroyed.

   procedure Size_Allocate_MDI (MDI : access Gtk_Widget_Record'Class);
   --  MDI was resized, need to resize the docks as well.

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Child (Child : access Gtk_Widget_Record'Class);
   --  Iconify a child, or maximize all children (these act as toggles, for
   --  the title bar of all children).

   function Docked_Side
     (Child : access MDI_Child_Record'Class) return Dock_Side;
   --  Return the side in which a child is currently docked.

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class);
   --  Called when the current page in Docked_Child has changed.
   --  This is used to refresh the notebook so that is reflects the selected
   --  widget.

   procedure Close_Child (Child : access Gtk_Widget_Record'Class);
   --  A child should be destroyed.
   --  As opposed to a direct call to Destroy, this also checks whether the
   --  child can be closed at that time, through a call to delete_event.

   procedure Move_Dock_Notebook
     (MDI : access MDI_Window_Record'Class; Last : Dock_Side);
   --  Reposition the docks on each side of MDI, given their contents.
   --  Last indicates the last dock that was resized or added. This gives it
   --  priority over the others.

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle);
   procedure Draw_Child
     (Child : access Gtk_Widget_Record'Class; Params : Gtk_Args);
   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Draw the child (and the title bar)

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is realized.

   procedure Activate_Child (Child : access MDI_Child_Record'Class);
   --  Make Child the active widget, and raise it at the top.

   function Constrain_X
     (MDI : access MDI_Window_Record'Class; X : Gint; Min : Gint := Min_Width)
      return Gint;
   function Constrain_Y
     (MDI : access MDI_Window_Record'Class; Y : Gint; Min : Gint := Min_Height)
      return Gint;
   --  Constrain the possible values of coordinates for an item in MDI.

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class);
   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child.

   procedure Put_In_Notebook
     (MDI : access MDI_Window_Record'Class;
      Side : Dock_Side;
      Child : access MDI_Child_Record'Class);
   --  Remove Child from MDI, and put it under control of a dock box, on the
   --  specific Side.

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side);
   --  Remove Child from the notebook it belongs to.
   --  Child will be destroyed, unless you Ref' it first.
   --  The notebook is destroyed if Child was the last item.

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class);
   --  Add an entry to the MDI menu that provides easy activation of Child

   procedure Cascade_Cb    (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_H_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_V_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Dock_Cb       (MDI   : access Gtk_Widget_Record'Class);
   procedure Float_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Close_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Focus_Cb      (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Cb   (MDI   : access Gtk_Widget_Record'Class);
   procedure Unmaximize_Cb (MDI   : access Gtk_Widget_Record'Class);
   --  Callbacks for the menu

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (MDI : out MDI_Window) is
   begin
      MDI := new MDI_Window_Record;
      Initialize (MDI);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (MDI : access MDI_Window_Record'Class) is
   begin
      Gtk.Layout.Initialize (MDI, Null_Adjustment, Null_Adjustment);
      Widget_Callback.Connect
        (MDI, "realize",
         Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Connect
        (MDI, "destroy",
         Widget_Callback.To_Marshaller (Destroy_MDI'Access));
      Widget_Callback.Connect
        (MDI, "size_allocate",
         Widget_Callback.To_Marshaller (Size_Allocate_MDI'Access));
   end Initialize;

   ------------------
   -- Layout_Child --
   ------------------

   procedure Layout_Child
     (Child  : access MDI_Child_Record'Class;
      Region : Gdk.Region.Gdk_Region := null)
   is
      use Widget_List;

      R         : Gdk.Region.Gdk_Region;
      List, Tmp : Widget_List.Glist;
      X, Y      : Gint;
      Overlap   : Gdk_Overlap_Type;
      C         : MDI_Child;

   begin
      --  If necessary, create the region with all the existing children

      if Gdk.C_Proxy (Region) = null then
         Gdk_New (R);
         List := Children (Child.MDI);
         Tmp := First (List);

         while Tmp /= Null_List loop
            C := MDI_Child (Get_Data (Tmp));
            if MDI_Child (Child) /= C and then C.State /= Docked then
               Union_With_Rect (R, R,
                                (C.X, C.Y,
                                 GRectangle_Length (C.Uniconified_Width),
                                 GRectangle_Length (C.Uniconified_Height)));
            end if;

            Tmp := Next (Tmp);
         end loop;

         Free (List);
      else
         R := Region;
      end if;

      Y := 10;
      Child.X := 11;
      Child.Y := 11;

      if Child.Uniconified_Width /= 0
        and then Child.Uniconified_Height /= 0
      then
         while Child.X = 11 and then Y < Child.MDI.MDI_Height loop
            X := 10;

            while X < Child.MDI.MDI_Width loop
               Overlap := Rect_In
                 (R, (X, Y, GRectangle_Length (Child.Uniconified_Width),
                      GRectangle_Length (Child.Uniconified_Height)));

               --  Perfect, it doesn't overlap anything

               if Overlap = Overlap_Rectangle_Out then
                  Child.X := X;
                  Child.Y := Y;
                  exit;

                  --  Else, this is a possible candidate
               elsif Overlap = Overlap_Rectangle_Part
                 and then Child.X = 11
               then
                  Child.X := X;
                  Child.Y := Y;
               end if;

               X := X + 10;
            end loop;

            Y := Y + 10;
         end loop;
      end if;

      Move (Child.MDI, Child, Child.X, Child.Y);

      if Gdk.C_Proxy (Region) = null then
         Destroy (R);
      end if;
   end Layout_Child;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      use Widget_List;

      M         : MDI_Window := MDI_Window (MDI);
      Color     : Gdk_Color;
      List      : Widget_List.Glist := Children (M);
      Tmp       : Widget_List.Glist := First (List);
      Child_Req : Gtk_Requisition;
      Child     : MDI_Child;
      Region    : Gdk.Region.Gdk_Region := Null_Region;

   begin
      Gdk_New (M.GC, Get_Window (MDI));
      Color := Parse (Title_Bar_Color);
      Alloc (Get_Default_Colormap, Color);
      Set_Foreground (M.GC, Color);
      Set_Exposures (M.GC, False);

      Gdk_New (M.Focus_GC, Get_Window (MDI));
      Color := Parse (Title_Bar_Focus_Color);
      Alloc (Get_Default_Colormap, Color);
      Set_Foreground (M.Focus_GC, Color);
      Set_Exposures (M.Focus_GC, False);

      Gdk_New (M.Resize_GC, Get_Window (MDI));
      Set_Function (M.Resize_GC, Invert);
      Set_Exposures (M.Resize_GC, False);
      Set_Subwindow (M.Resize_GC, Include_Inferiors);

      M.Title_GC := Get_White_GC (Get_Style (MDI));

      --  Initialize the size of all children.
      --  This couldn't be done earlier since the child would have requested
      --  a size of 0x0

      Gdk_New (Region);

      while Tmp /= Null_List loop
         Child := MDI_Child (Get_Data (Tmp));

         if Child.State /= Docked and then Child.State /= Embedded then
            Size_Request (Child, Child_Req);
            Child.Uniconified_Width := Child_Req.Width;
            Child.Uniconified_Height := Child_Req.Height;
            Layout_Child (Child, Region);
            Union_With_Rect
              (Region, Region,
               (Child.X, Child.Y,
                GRectangle_Length (Child.Uniconified_Width),
                GRectangle_Length (Child.Uniconified_Height)));
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (List);
      Destroy (Region);
   end Realize_MDI;

   -----------------------
   -- Size_Allocate_MDI --
   -----------------------

   procedure Size_Allocate_MDI (MDI : access Gtk_Widget_Record'Class) is
      M : MDI_Window := MDI_Window (MDI);
   begin
      if M.MDI_Width /= Gint (Get_Allocation_Width (M))
        or else M.MDI_Height /= Gint (Get_Allocation_Height (M))
      then
         M.MDI_Width := Gint (Get_Allocation_Width (M));
         M.MDI_Height := Gint (Get_Allocation_Height (M));
         Move_Dock_Notebook (M, None);
      end if;
   end Size_Allocate_MDI;

   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      use Widget_List;

      Tmp : Widget_List.Glist := First (MDI_Window (MDI).Invisible_Items);
      N   : Widget_List.Glist;

   begin
      while Tmp /= Null_List loop
         --  Get the next field first, since Destroy will actually destroy Tmp
         N := Next (Tmp);
         Destroy (Get_Data (Tmp));
         Tmp := N;
      end loop;

      Free (MDI_Window (MDI).Invisible_Items);

      if MDI_Window (MDI).Menu /= null then
         Destroy (MDI_Window (MDI).Menu);
      end if;
   end Destroy_MDI;

   -------------------
   -- Iconify_Child --
   -------------------

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class) is
      C    : MDI_Child := MDI_Child (Child);
      Note : Gtk_Notebook;

   begin
      if C.State = Docked then
         Note := Gtk_Notebook (C.Initial);
         C := Find_MDI_Child
           (C.MDI, Get_Nth_Page (Note, Get_Current_Page (Note)));
      end if;

      Minimize_Child (C, not (C.State = Iconified));
   end Iconify_Child;

   --------------------
   -- Maximize_Child --
   --------------------

   procedure Maximize_Child (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      Maximize_Children (C.MDI, C.MDI.Docks (None) = null);
   end Maximize_Child;

   -----------------
   -- Docked_Side --
   -----------------

   function Docked_Side
     (Child : access MDI_Child_Record'Class) return Dock_Side is
   begin
      pragma Assert (Child.State = Embedded);

      --  Most likely candidate is:
      if Child.MDI.Docks (Child.Dock) /= null
        and then Page_Num (Gtk_Notebook (Child.MDI.Docks (Child.Dock).Initial),
                           Child.Initial_Child) /= -1
      then
         return Child.Dock;

      --  Else, all items are maximed in MDI.Docks (None)
      else
         return None;
      end if;
   end Docked_Side;

   -----------------
   -- Close_Child --
   -----------------

   procedure Close_Child (Child : access Gtk_Widget_Record'Class) is
      C          : MDI_Child := MDI_Child (Child);
      Event      : Gdk_Event;
      Old_Parent : Gtk_Widget;
      Result     : Boolean;
      Note       : Gtk_Notebook;

   begin
      --  If a dock still exists, it has at least one child, and this is what
      --  the user actually wants to remove. The Dock is automatically closed
      --  when its last item is removed.

      if C.State = Docked then
         Note := Gtk_Notebook (C.Initial);
         C := Find_MDI_Child
           (C.MDI, Get_Nth_Page (Note, Get_Current_Page (Note)));
      end if;

      Allocate (Event, Delete, Get_Window (C.MDI));

      --  For a top-level window, we must rebuild the initial widget
      --  temporarily, so that the application can do all the test it wants.
      --  However, we need to restore the initial state before calling
      --  Dock_Child and Float_Child below

      if C.Initial.all in Gtk_Window_Record'Class then
         Old_Parent := Get_Parent (C.Initial_Child);
         Reparent (C.Initial_Child, Gtk_Window (C.Initial));
         Result := Return_Callback.Emit_By_Name
           (C.Initial, "delete_event", Event);
         Reparent (C.Initial_Child, Old_Parent);

      else
         Result := Return_Callback.Emit_By_Name
           (C.Initial, "delete_event", Event);
      end if;

      if not Result then
         Destroy (C);
      end if;

      Free (Event);
   end Close_Child;

   -------------------
   -- Destroy_Child --
   -------------------

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      --  We know at that stage that Child has already been unparent-ed
      pragma Assert (Get_Parent (Child) = null);

      --  Can't destroy non-empty dock items
      if C.State = Docked
        and then Get_Nth_Page (Gtk_Notebook (C.Initial), 0) /= null
      then
         return;
      end if;

      if C.Menu_Item /= null then
         Destroy (C.Menu_Item);
      end if;

      if C = C.MDI.Focus_Child then
         C.MDI.Focus_Child := null;
      end if;

      if C.State = Floating or else C.State = Embedded then
         Widget_List.Remove (C.MDI.Invisible_Items, Gtk_Widget (C));
         Unref (C);

         if C.State = Floating then
            --  Initial_Child could be null if we are destroying a floating
            --  child explicitly (by closing its X11 window)

            if C.Initial_Child /= null
              and then Get_Parent (C.Initial_Child) /= null
            then
               Destroy (Get_Parent (C.Initial_Child));
            end if;
         else
            Ref (C.Initial_Child);
            Remove_From_Notebook (C, Docked_Side (C));
            Unref (C.Initial_Child);
         end if;
      end if;

      --  Destroy the toplevel Child is associated with
      if C.Initial /= C.Initial_Child then
         Destroy (C.Initial);
      end if;

      C.Initial := null;
      Free (C.Title);
   end Destroy_Child;

   ---------------------------
   -- Destroy_Initial_Child --
   ---------------------------

   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class) is
   begin
      --  The initial child has already been unparented
      pragma Assert (Get_Parent (MDI_Child (Child).Initial_Child) = null);

      if MDI_Child (Child).Initial = MDI_Child (Child).Initial_Child then
         MDI_Child (Child).Initial := null;
      end if;

      MDI_Child (Child).Initial_Child := null;

      --  If the initial_child wasn't explicitly destroyed in Destroy_Child

      if not Destroyed_Is_Set (Child) then
         Destroy (Child);
      end if;
   end Destroy_Initial_Child;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle)
   is
      Bt   : Gint := Border_Thickness;
      --  ??? Should be a field in Child
      MDI  : MDI_Window := Child.MDI;
      F    : Gdk.Font.Gdk_Font :=
        Get_Gdkfont (Title_Font_Name, Title_Font_Height);
      GC   : Gdk.Gdk_GC := MDI.GC;
      Note : Gtk_Notebook;

   begin
      if MDI_Child (Child) = MDI.Docks (None) then
         Bt := 0;
      end if;

      --  Call this function so that for a dock item is highlighted if the
      --  current page is linked to the focus child.

      if Child.State = Docked
        and then MDI.Focus_Child /= null
        and then MDI.Focus_Child.State = Embedded
      then
         Note := Gtk_Notebook (Child.Initial);
      end if;

      if MDI.Focus_Child = MDI_Child (Child)
        or else
        (Note /= null
         and then Get_Nth_Page (Note, Get_Current_Page (Note)) =
           Gtk_Widget (MDI.Focus_Child.Initial_Child))
      then
         GC := MDI.Focus_GC;
      end if;

      Draw_Rectangle
        (Get_Window (Child),
         GC,
         True,
         Bt,
         Bt,
         Gint (Get_Allocation_Width (Child)) - 2 * Bt,
         Title_Bar_Height);

      Draw_Text
        (Get_Window (Child),
         F,
         MDI.Title_GC,
         Bt + 3,
         Bt +
         (Title_Bar_Height + Get_Ascent (F) - Get_Descent (F)) / 2,
         Child.Title.all);

      --  For notebooks, we want the tabs area the same color as MDI
      if Child.State = Docked then
         Draw_Rectangle
           (Get_Window (Child),
            Get_Background_GC (Get_Style (MDI), State_Normal),
            True,
            Bt,
            Bt + Title_Bar_Height,
            Gint (Get_Allocation_Width (Child)) - 2 * Bt,
            Gint (Get_Allocation_Height (Child)) - 2 * Bt - Title_Bar_Height);
      end if;

      if MDI_Child (Child) /= MDI.Docks (None) then
         Draw_Shadow
           (Get_Style (Child),
            Get_Window (Child),
            State_Normal,
            Shadow_Out,
            1, 1,
            Gint (Get_Allocation_Width (Child)) - 1,
            Gint (Get_Allocation_Height (Child)) - 1);
      end if;
   end Draw_Child;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access Gtk_Widget_Record'Class; Params : Gtk_Args) is
   begin
      Draw_Child (MDI_Child (Child), Full_Area);
      Gtk.Handlers.Emit_Stop_By_Name (Child, "draw");
   end Draw_Child;

   ----------------
   -- Draw_Child --
   ----------------

   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean is
   begin
      Draw_Child (MDI_Child (Child), Get_Area (Event));
      Gtk.Handlers.Emit_Stop_By_Name (Child, "expose_event");
      return True;
   end Draw_Child;

   -----------------
   -- Constrain_X --
   -----------------

   function Constrain_X
     (MDI : access MDI_Window_Record'Class; X : Gint; Min : Gint := Min_Width)
      return Gint
   is
      Xmin : Gint := 0;
      Xmax : Gint := MDI.MDI_Width - Min;
   begin
      if MDI.Docks (Left) /= null then
         Xmin := MDI.Docks (Left).Uniconified_Width;
      end if;
      if MDI.Docks (Right) /= null then
         Xmax := Xmax - MDI.Docks (Right).Uniconified_Width;
      end if;

      return Gint'Max (Xmin, Gint'Min (X, Xmax));
   end Constrain_X;

   -----------------
   -- Constrain_Y --
   -----------------

   function Constrain_Y
     (MDI : access MDI_Window_Record'Class; Y : Gint; Min : Gint := Min_Height)
      return Gint
   is
      Ymin : Gint := 0;
      Ymax : Gint := MDI.MDI_Height - Min;
   begin
      if MDI.Docks (Top) /= null then
         Ymin := MDI.Docks (Top).Uniconified_Height;
      end if;

      if MDI.Docks (Bottom) /= null then
         Ymax := Ymax - MDI.Docks (Bottom).Uniconified_Height;
      end if;

      return Gint'Max (Ymin, Gint'Min (Y, Ymax));
   end Constrain_Y;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C      : MDI_Child := MDI_Child (Child);
      MDI    : MDI_Window := C.MDI;
      Cursor : Gdk.Cursor.Gdk_Cursor;
      Tmp    : Boolean;
      Curs   : Gdk_Cursor_Type;

   begin
      --  It sometimes happens that widgets let events pass through (for
      --  instance scrollbars do that), and thus wouldn't be useful anymore
      --  if we do a grab.

      if Get_Window (Child) /= Get_Window (Event)
        or else Get_Event_Type (Event) /= Button_Press
      then
         return False;
      end if;

      Activate_Child (C);
      MDI.X_Root := Gint (Get_X_Root (Event));
      MDI.Y_Root := Gint (Get_Y_Root (Event));
      MDI.Initial_Width := Gint (Get_Allocation_Width (Child));
      MDI.Initial_Height := Gint (Get_Allocation_Height (Child));
      MDI.Current_W := MDI.Initial_Width;
      MDI.Current_H := MDI.Initial_Height;
      MDI.Current_X := C.X;
      MDI.Current_Y := C.Y;

      Curs := Side (C, Gint (Get_X (Event)), Gint (Get_Y (Event)));
      MDI.Current_Cursor := Curs;

      if Curs = Left_Ptr then
         Curs := Fleur;
      end if;

      --  Iconified windows can only be moved, not resized.
      --  Docked items can never be moved

      if C.State /= Embedded then
         if (Curs = Fleur and then C.State /= Docked)
           or else (Curs /= Fleur and then C.State /= Iconified)
         then
            if Do_Grabs then
               Gdk_New (Cursor, Curs);
               Tmp := Pointer_Grab
                 (Get_Window (C),
                  False,
                  Button_Press_Mask or Button_Motion_Mask
                  or Button_Release_Mask,
                  Cursor => Cursor,
                  Time => 0);
               Destroy (Cursor);
            end if;

            MDI.Selected_Child := C;

            if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
              or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
            then
               Draw_Rectangle
                 (Get_Bin_Window (MDI),
                  MDI.Resize_GC,
                  Filled => False,
                  X => MDI.Current_X,
                  Y => MDI.Current_Y,
                  Width => MDI.Current_W,
                  Height => MDI.Current_H);
            end if;
         end if;
      end if;

      return True;
   end Button_Pressed;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      MDI : MDI_Window := MDI_Child (Child).MDI;
   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      if MDI.Selected_Child /= null then
         if Do_Grabs then
            Pointer_Ungrab (Time => 0);
         end if;

         if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
           or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
         then
            Draw_Rectangle
              (Get_Bin_Window (MDI),
               MDI.Resize_GC,
               Filled => False,
               X => MDI.Current_X,
               Y => MDI.Current_Y,
               Width => MDI.Current_W,
               Height => MDI.Current_H);

            if MDI.Current_Cursor /= Left_Ptr then
               Set_USize (MDI.Selected_Child, MDI.Current_W, MDI.Current_H);
               if MDI.Selected_Child.State /= Docked then
                  MDI.Selected_Child.Uniconified_Width := MDI.Current_W;
                  MDI.Selected_Child.Uniconified_Height := MDI.Current_H;
               end if;
            end if;

            Move (MDI, MDI.Selected_Child, MDI.Current_X, MDI.Current_Y);
         end if;

         --  If we moved a dock notebook, we have to resize all of them.

         if MDI.Selected_Child.State = Docked then
            Move_Dock_Notebook (MDI, MDI.Selected_Child.Dock);
         end if;

         MDI.Selected_Child.X := MDI.Current_X;
         MDI.Selected_Child.Y := MDI.Current_Y;
         MDI.Selected_Child   := null;
      end if;

      return True;
   end Button_Release;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C       : MDI_Child := MDI_Child (Child);
      MDI     : MDI_Window := C.MDI;
      Delta_X : Gint;
      Delta_Y : Gint;
      Cursor  : Gdk_Cursor;
      Curs    : Gdk_Cursor_Type;
      W, H    : Gint;

   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      --  A button_motion event ?

      if (Get_State (Event) and Button1_Mask) /= 0 then
         if MDI.Selected_Child /= null then
            C := MDI.Selected_Child;

            if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
              or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
            then
               Draw_Rectangle
                 (Get_Bin_Window (MDI),
                  MDI.Resize_GC,
                  Filled => False,
                  X => MDI.Current_X,
                  Y => MDI.Current_Y,
                  Width => MDI.Current_W,
                  Height => MDI.Current_H);
            end if;

            Delta_X := Gint (Get_X_Root (Event)) - MDI.X_Root;
            Delta_Y := Gint (Get_Y_Root (Event)) - MDI.Y_Root;
            W := MDI.Initial_Width;
            H := MDI.Initial_Height;

            MDI.Current_X := C.X;
            MDI.Current_Y := C.Y;

            case MDI.Current_Cursor is
               when Left_Ptr =>
                  MDI.Current_X := Constrain_X (MDI, Delta_X + MDI.Current_X);
                  MDI.Current_Y := Constrain_Y (MDI, Delta_Y + MDI.Current_Y);

               when Left_Side =>
                  W := Gint'Max (Min_Width, W - Delta_X);
                  MDI.Current_X := MDI.Current_X + MDI.Initial_Width - W;

               when Right_Side =>
                  W := Gint'Max (Min_Width, W + Delta_X);

               when Top_Side =>
                  H := Gint'Max (Min_Height, H - Delta_Y);
                  MDI.Current_Y := MDI.Current_Y + MDI.Initial_Height - H;

               when Bottom_Side =>
                  H := Gint'Max (Min_Height, H + Delta_Y);

               when Top_Left_Corner =>
                  W := Gint'Max (Min_Width, W - Delta_X);
                  H := Gint'Max (Min_Height, H - Delta_Y);
                  MDI.Current_X := MDI.Current_X + MDI.Initial_Width - W;
                  MDI.Current_Y := MDI.Current_Y + MDI.Initial_Height - H;

               when Top_Right_Corner =>
                  W := Gint'Max (Min_Width, W + Delta_X);
                  H := Gint'Max (Min_Height, H - Delta_Y);
                  MDI.Current_Y := MDI.Current_Y + MDI.Initial_Height - H;

               when Bottom_Left_Corner =>
                  W := Gint'Max (Min_Width, W - Delta_X);
                  H := Gint'Max (Min_Height, H + Delta_Y);
                  MDI.Current_X := MDI.Current_X + MDI.Initial_Width - W;

               when Bottom_Right_Corner =>
                  W := Gint'Max (Min_Width, W + Delta_X);
                  H := Gint'Max (Min_Height, H + Delta_Y);

               when others => null;
            end case;

            if (MDI.Current_Cursor = Left_Ptr and then Opaque_Move)
              or else (MDI.Current_Cursor /= Left_Ptr and then Opaque_Resize)
            then
               Move (MDI, C, MDI.Current_X, MDI.Current_Y);
            end if;

            if MDI.Current_Cursor /= Left_Ptr
              and then Opaque_Resize
              and then (W /= Gint (Get_Allocation_Width (C))
                        or else H /= Gint (Get_Allocation_Height (C)))
            then
               if C.State /= Docked then
                  C.Uniconified_Width := W;
                  C.Uniconified_Height := H;
               end if;
               Set_USize (C, W, H);
            end if;

            if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
              or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
            then
               MDI.Current_W := W;
               MDI.Current_H := H;
               Draw_Rectangle
                 (Get_Bin_Window (MDI),
                  MDI.Resize_GC,
                  Filled => False,
                  X => MDI.Current_X,
                  Y => MDI.Current_Y,
                  Width => MDI.Current_W,
                  Height => MDI.Current_H);
            end if;
         end if;

      --  A motion_event ? change the cursor if needed

      elsif C.State = Normal or else C.State = Docked then
         Delta_X := Gint (Get_X (Event));
         Delta_Y := Gint (Get_Y (Event));
         Curs := Side (C, Delta_X, Delta_Y);

         if Curs /= MDI.Current_Cursor then
            MDI.Current_Cursor := Curs;
            Gdk_New (Cursor, MDI.Current_Cursor);
            Set_Cursor (Get_Window (Child), Cursor);
            Destroy (Cursor);
         end if;
      end if;

      return True;
   end Button_Motion;

   -----------------
   -- Leave_Child --
   -----------------

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      MDI    : MDI_Window := MDI_Child (Child).MDI;
      Cursor : Gdk_Cursor;

   begin
      if Get_State (Event) = 0
        and then MDI.Current_Cursor /= Left_Ptr
      then
         MDI.Current_Cursor := Left_Ptr;
         Gdk_New (Cursor, MDI.Current_Cursor);
         Set_Cursor (Get_Window (Child), Cursor);
         Destroy (Cursor);
      end if;

      return False;
   end Leave_Child;

   ----------
   -- Side --
   ----------

   function Side
     (Child : access MDI_Child_Record'Class; X, Y  : Gint)
      return Gdk_Cursor_Type
   is
      X_Side, Y_Side : Gint;
   begin
      --  The central notebook (for maximized items) can not be resized nor
      --  moved

      if MDI_Child (Child) = Child.MDI.Docks (None) then
         return Left_Ptr;
      end if;

      if X <= Border_Thickness then
         X_Side := -2;
      elsif X <= Corner_Size then
         X_Side := -1;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Border_Thickness then
         X_Side := 2;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Corner_Size then
         X_Side := 1;
      else
         X_Side := 0;
      end if;

      if Y <= Border_Thickness then
         Y_Side := -2;
      elsif Y <= Corner_Size then
         Y_Side := -1;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Border_Thickness then
         Y_Side := 2;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Corner_Size then
         Y_Side := 1;
      else
         Y_Side := 0;
      end if;

      if X_Side <= -1 and then Y_Side <= -1 then
         if Child.State /= Docked then
            return Top_Left_Corner;
         end if;

      elsif X_Side <= -1 and then Y_Side >= 1 then
         if Child.State /= Docked then
            return Bottom_Left_Corner;
         end if;

      elsif X_Side >= 1 and then Y_Side <= -1 then
         if Child.State /= Docked then
            return Top_Right_Corner;
         end if;

      elsif X_Side >= 1 and then Y_Side >= 1 then
         if Child.State /= Docked then
            return Bottom_Right_Corner;
         end if;

      elsif X_Side = -2 and then Y_Side in -1 .. 1 then
         if Child.State /= Docked or else Child.Dock = Right then
            return Left_Side;
         end if;

      elsif X_Side = 2 and then Y_Side in -1 .. 1 then
         if Child.State /= Docked or else Child.Dock = Left then
            return Right_Side;
         end if;

      elsif Y_Side = -2 and then X_Side in -1 .. 1 then
         if Child.State /= Docked or else Child.Dock = Bottom then
            return Top_Side;
         end if;

      elsif Y_Side = 2 and then X_Side in -1 .. 1 then
         if Child.State /= Docked or else Child.Dock = Top then
            return Bottom_Side;
         end if;
      end if;

      return Left_Ptr;
   end Side;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child   : out MDI_Child;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Is_Dock : Boolean := False;
      Side    : Dock_Side := None) is
   begin
      Child := new MDI_Child_Record;
      Initialize (Child, Widget, Is_Dock, Side);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child   : access MDI_Child_Record;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Is_Dock : Boolean := False;
      Side    : Dock_Side := None)
   is
      Button    : Gtk_Button;
      Box, Box2 : Gtk_Box;
      Pix       : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;

   begin
      Gtk.Event_Box.Initialize (Child);
      Child.Initial := Gtk_Widget (Widget);

      if Is_Dock then
         Child.State := Docked;
         Child.Dock := Side;
      else
         Child.State := Normal;
      end if;

      Add_Events
        (Child, Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Pointer_Motion_Mask);
      Return_Callback.Connect
        (Child, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Child, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Child, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Widget_Callback.Connect (Child, "draw", Draw_Child'Access);
      Widget_Callback.Connect
        (Child, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Child'Access));
      Return_Callback.Connect
        (Child, "leave_notify_event",
         Return_Callback.To_Marshaller (Leave_Child'Access));
      Return_Callback.Connect
        (Child, "expose_event",
         Return_Callback.To_Marshaller (Draw_Child'Access));

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Child, Box);

      --  Buttons in the title bar

      Gtk_New_Hbox (Box2, Homogeneous => False);
      Pack_Start (Box, Box2, Expand => False, Fill => False);

      if Is_Dock and then Side = None then
         Set_Border_Width (Box, 0);
      else
         Set_Border_Width (Box, Border_Thickness);
      end if;

      Set_USize (Box2, -1, Title_Bar_Height);

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Close_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Button);
      Add (Button, Pixmap);
      Pack_End (Box2, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Close_Child'Access), Child);

      if not Is_Dock or else Side = None then
         Gdk.Pixmap.Create_From_Xpm_D
           (Pix, null, Get_Default_Colormap, Mask, Null_Color, Maximize_Xpm);
         Gtk_New (Pixmap, Pix, Mask);
         Gtk_New (Child.Maximize_Button);
         Add (Child.Maximize_Button, Pixmap);
         Pack_End
           (Box2, Child.Maximize_Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Child.Maximize_Button, "clicked",
            Widget_Callback.To_Marshaller (Maximize_Child'Access), Child);
      end if;

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Iconify_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Button);
      Add (Button, Pixmap);
      Pack_End (Box2, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Iconify_Child'Access), Child);

      --  The child widget

      if Widget.all in Gtk_Window_Record'Class then
         pragma Assert (Get_Child (Gtk_Window (Widget)) /= null);
         Child.Initial_Child := Get_Child (Gtk_Window (Widget));
         Reparent (Child.Initial_Child, Box);

      else
         Child.Initial_Child := Gtk_Widget (Widget);
         Pack_Start
           (Box, Widget, Expand => True, Fill => True, Padding => 0);
      end if;

      Widget_Callback.Object_Connect
        (Child.Initial_Child, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Initial_Child'Access), Child);
   end Initialize;

   ---------
   -- Put --
   ---------

   function Put
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
      return MDI_Child
   is
      C : MDI_Child;
   begin
      if Child.all in MDI_Child_Record'Class then
         C := MDI_Child (Child);
      else
         Gtk_New (C, Child);
      end if;

      C.MDI := MDI_Window (MDI);
      C.X := 10;
      C.Y := 10;

      if Child.all in Gtk_Window_Record'Class then
         C.Title := new String' (Get_Title (Gtk_Window (Child)));
      else
         C.Title := new String' (" ");
      end if;

      Gtk.Layout.Put (Gtk_Layout_Record (MDI.all)'Access, C, C.X, C.Y);

      if MDI.Menu /= null then
         Create_Menu_Entry (C);
      end if;

      --  If MDI is not realized, then we don't need to do anything now,
      --  this will be done automatically in Realize_MDI

      if C.State /= Docked
        and then Realized_Is_Set (MDI)
      then
         --  If all items are maximized, add Child to the notebook
         if MDI.Docks (None) /= null then
            Put_In_Notebook (MDI, None, C);
         else
            Layout_Child (C);
         end if;

         Activate_Child (C);
      end if;

      return C;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      C : MDI_Child;
   begin
      C := Put (MDI, Child);
   end Put;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Child : access MDI_Child_Record; Title : String) is
      Note  : Gtk_Notebook;
      Label : Gtk_Accel_Label;
      Side  : Dock_Side;
   begin
      Free (Child.Title);
      Child.Title := new String' (Title);

      if Child.Initial.all in Gtk_Window_Record'Class then
         Set_Title (Gtk_Window (Child.Initial), Title);
      end if;

      if Child.State = Embedded then
         Side := Docked_Side (Child);
         Note := Gtk_Notebook (Child.MDI.Docks (Side).Initial);
         Set_Tab_Label_Text (Note, Child.Initial_Child, Title);
         if Get_Current_Page (Note) = Page_Num (Note, Child.Initial_Child) then
            Set_Title (Child.MDI.Docks (Side), Title);
         end if;
      end if;

      --  Update the menu, if it exists

      if Child.Menu_Item /= null then
         --  Since we don't want to use Gtk.Type_Conversion in this package,
         --  the simplest is to destroy and recreate the label associated
         --  with the menu_item.
         Gtk_New (Label, Title);
         Set_Alignment (Label, 0.0, 0.5);
         Set_Accel_Widget (Label, Child.Menu_Item);
         Remove (Child.Menu_Item, Get_Child (Child.Menu_Item));
         Add (Child.Menu_Item, Label);
         Show (Label);
      end if;
   end Set_Title;

   -------------------
   -- Get_MDI_Child --
   -------------------

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
      use Widget_List;

      Tmp   : Widget_List.Glist;
      Found : MDI_Child;

      procedure Action (W : access Gtk_Widget_Record'Class);
      --  Executed for each child in MDI

      ------------
      -- Action --
      ------------

      procedure Action (W : access Gtk_Widget_Record'Class) is
      begin
         if Found = null
           and then (MDI_Child (W).Initial = Gtk_Widget (Widget)
                     or else MDI_Child (W).Initial_Child = Gtk_Widget (Widget))
         then
            Found := MDI_Child (W);
         end if;
      end Action;

   begin
      Forall (MDI, Action'Unrestricted_Access);

      if Found = null then
         Tmp := First (MDI.Invisible_Items);

         while Tmp /= Null_List loop
            if MDI_Child (Get_Data (Tmp)).Initial = Gtk_Widget (Widget)
              or else MDI_Child (Get_Data (Tmp)).Initial_Child =
              Gtk_Widget (Widget)
            then
               return MDI_Child (Get_Data (Tmp));
            end if;

            Tmp := Next (Tmp);
         end loop;
      end if;

      return Found;
   end Find_MDI_Child;

   -----------------
   -- Raise_Child --
   -----------------

   procedure Raise_Child (Child : access MDI_Child_Record'Class) is
      Note : Gtk_Notebook;
   begin
      --  For an embedded item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Embedded then
         Note := Gtk_Notebook (Child.MDI.Docks (Docked_Side (Child)).Initial);
         Set_Page (Note, Page_Num (Note, Child.Initial_Child));

      elsif Realized_Is_Set (Child) then
         Gdk_Raise (Get_Window (Child));

         if Child.State = Floating then
            Gdk_Raise
              (Get_Window (Gtk_Window (Get_Parent (Child.Initial_Child))));
         end if;

         --  Make sure the docks always stay on top

         for J in Left .. Bottom loop
            if Child.MDI.Docks (J) /= null then
               Gdk_Raise (Get_Window (Child.MDI.Docks (J)));
            end if;
         end loop;
      end if;

      if Child.MDI.Docks (None) /= null
        and then Realized_Is_Set (Child.MDI.Docks (None))
      then
         Lower (Get_Window (Child.MDI.Docks (None)));
      end if;
   end Raise_Child;

   ----------------------
   -- Update_Dock_Menu --
   ----------------------

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Dock_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
         Set_Active
           (Child.MDI.Dock_Menu_Item,
            Child.State = Embedded and then Docked_Side (Child) /= None);
         Set_Sensitive (Child.MDI.Dock_Menu_Item, Child.Dock /= None);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
      end if;
   end Update_Dock_Menu;

   -----------------------
   -- Update_Float_Menu --
   -----------------------

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Float_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
         Set_Active (Child.MDI.Float_Menu_Item, Child.State = Floating);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
      end if;
   end Update_Float_Menu;

   --------------------
   -- Activate_Child --
   --------------------

   procedure Activate_Child (Child : access MDI_Child_Record'Class) is
      Old  : MDI_Child := Child.MDI.Focus_Child;
      C    : MDI_Child := MDI_Child (Child);
      S    : Dock_Side;
      Note : Gtk_Notebook;

   begin
      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old then
         return;
      end if;

      if C.State = Docked then
         Note := Gtk_Notebook (C.Initial);
         C := Find_MDI_Child
           (C.MDI, Get_Nth_Page (Note, Get_Current_Page (Note)));
      end if;

      Child.MDI.Focus_Child := C;

      if Old /= null then
         if Old.State = Embedded then
            S := Docked_Side (Old);

            if Realized_Is_Set (C.MDI.Docks (S)) then
               Draw_Child (C.MDI.Docks (S), Full_Area);
            end if;
         end if;

         if Realized_Is_Set (Old) then
            Draw_Child (Old, Full_Area);
         end if;
      end if;

      --  Must be Child, so that the appropriate page is shown in the dock
      Raise_Child (Child);

      if C.State = Embedded then
         S := Docked_Side (C);

         if Realized_Is_Set (C.MDI.Docks (S)) then
            Draw_Child (C.MDI.Docks (S), Full_Area);
         end if;
      end if;

      if Realized_Is_Set (C) then
         Draw_Child (C, Full_Area);
      end if;

      Update_Dock_Menu (C);
      Update_Float_Menu (C);

      if Child.MDI.Float_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (C.MDI.Float_Menu_Item, C.MDI.Float_Menu_Item_Id);
         Set_Active (C.MDI.Float_Menu_Item, C.State = Floating);
         Gtk.Handlers.Handler_Unblock
           (C.MDI.Float_Menu_Item, C.MDI.Float_Menu_Item_Id);
      end if;

      --  Active the menu for C (first set Focus_Child to avoid infinite loops)

      if C.Menu_Item /= null then
         Set_Active (C.Menu_Item, True);
      end if;
   end Activate_Child;

   ----------------------
   -- Cascade_Children --
   ----------------------

   procedure Cascade_Children (MDI : access MDI_Window_Record) is
      Xmin  : constant Gint := Constrain_X (MDI, 0);
      Ymin  : constant Gint := Constrain_Y (MDI, 0);
      W     : Gint := Constrain_X (MDI, MDI.MDI_Width, 0);
      H     : Gint := Constrain_Y (MDI, MDI.MDI_Height, 0);
      List  : Widget_List.Glist := Children (MDI);
      C     : MDI_Child;
      Tmp   : Widget_List.Glist;
      Level : Gint := 0;
      Num_Children : Gint := 0;

      procedure Position_Child (Child : access MDI_Child_Record'Class);

      procedure Position_Child (Child : access MDI_Child_Record'Class) is
      begin
         Child.X := Xmin + Level;
         Child.Y := Ymin + Level;
         Move (MDI, Child, Child.X, Child.Y);
         Child.Uniconified_Width := W;
         Child.Uniconified_Height := H;
         Set_USize (Child, Child.Uniconified_Width, Child.Uniconified_Height);
         Level := Level + Title_Bar_Height;
      end Position_Child;

   begin
      Tmp := First (List);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         Tmp := Next (Tmp);
      end loop;

      W := W - (Num_Children - 1) * Title_Bar_Height;
      H := H - (Num_Children - 1) * Title_Bar_Height;

      Tmp := First (List);

      while Tmp /= Null_List loop
         C := MDI_Child (Get_Data (Tmp));

         if C.State = Normal and then C /= MDI.Focus_Child then
            Position_Child (C);
         end if;

         Tmp := Next (Tmp);
      end loop;

      if MDI.Focus_Child /= null and then MDI.Focus_Child.State = Normal then
         Position_Child (MDI.Focus_Child);
      end if;

      Free (List);
   end Cascade_Children;

   -----------------------
   -- Tile_Horizontally --
   -----------------------

   procedure Tile_Horizontally (MDI : access MDI_Window_Record) is
      Xmin  : constant Gint := Constrain_X (MDI, 0);
      Ymin  : constant Gint := Constrain_Y (MDI, 0);
      W     : constant Gint := Constrain_X (MDI, MDI.MDI_Width, 0);
      H     : constant Gint := Constrain_Y (MDI, MDI.MDI_Height, 0);
      List  : Widget_List.Glist := Children (MDI);
      C     : MDI_Child;
      Tmp   : Widget_List.Glist;
      Level : Gint := 0;
      Num_Children : Gint := 0;

   begin
      Tmp := First (List);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         Tmp := Next (Tmp);
      end loop;

      Tmp := First (List);

      while Tmp /= Null_List loop
         C := MDI_Child (Get_Data (Tmp));

         if C.State = Normal then
            C.X := Xmin + Level;
            C.Y := Ymin;
            Move (MDI, C, C.X, C.Y);
            C.Uniconified_Width := W / Num_Children;
            C.Uniconified_Height := H;
            Set_USize (C, C.Uniconified_Width, C.Uniconified_Height);
            Level := Level + (W / Num_Children);
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (List);
      Queue_Resize (MDI);
   end Tile_Horizontally;

   ---------------------
   -- Tile_Vertically --
   ---------------------

   procedure Tile_Vertically (MDI : access MDI_Window_Record) is
      Xmin  : constant Gint := Constrain_X (MDI, 0);
      Ymin  : constant Gint := Constrain_Y (MDI, 0);
      W     : constant Gint := Constrain_X (MDI, MDI.MDI_Width, 0);
      H     : constant Gint := Constrain_Y (MDI, MDI.MDI_Height, 0);
      List  : Widget_List.Glist := Children (MDI);
      C     : MDI_Child;
      Tmp   : Widget_List.Glist;
      Level : Gint := 0;
      Num_Children : Gint := 0;

   begin
      Tmp := First (List);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).State = Normal then
            Num_Children := Num_Children + 1;
         end if;

         Tmp := Next (Tmp);
      end loop;

      Tmp := First (List);

      while Tmp /= Null_List loop
         C := MDI_Child (Get_Data (Tmp));

         if C.State = Normal then
            C.X := Xmin;
            C.Y := Ymin + Level;
            Move (MDI, C, C.X, C.Y);
            C.Uniconified_Width := W;
            C.Uniconified_Height := H / Num_Children;
            Set_USize (C, C.Uniconified_Width, C.Uniconified_Height);
            Level := Level + (H / Num_Children);
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (List);
      Queue_Resize (MDI);
   end Tile_Vertically;

   ------------------
   -- Delete_Child --
   ------------------

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      --  Gtk_Window children are handled differently (see Float_Child)
      pragma Assert
        (not (MDI_Child (Child).Initial.all in Gtk_Window_Record'Class));
      return Return_Callback.Emit_By_Name
        (MDI_Child (Child).Initial, "delete_event", Event);
   end Delete_Child;

   -----------------
   -- Float_Child --
   -----------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class;
      Float : Boolean)
   is
      Win : Gtk_Window;
   begin
      if Child.State /= Floating and then Float then
         Minimize_Child (Child, False);
         Dock_Child (Child, False);

         if Child.Initial.all in Gtk_Window_Record'Class then
            Reparent (Child.Initial_Child, Gtk_Window (Child.Initial));
            Show_All (Child.Initial);
            Set_Default_Size
              (Gtk_Window (Child.Initial),
               Child.Uniconified_Width, Child.Uniconified_Height);

         else
            Gtk_New (Win, Window_Toplevel);
            Reparent (Child.Initial_Child, Win);
            Set_Title (Win, Child.Title.all);
            Show_All (Win);

            --  Delete_Event should be forwarded to the child, not to the
            --  toplevel window
            Return_Callback.Object_Connect
              (Win, "delete_event",
               Return_Callback.To_Marshaller (Delete_Child'Access), Child);
            Set_Default_Size
              (Win, Child.Uniconified_Width, Child.Uniconified_Height);
         end if;

         --  If not already in the list
         Ref (Child);
         Widget_List.Prepend
           (Child.MDI.Invisible_Items, Gtk_Widget (Child));

         --  If the child belonged directly to the MDI (and not to one of the
         --  docks for instance)
         if Get_Parent (Child) = Gtk_Widget (Child.MDI) then
            Remove (Child.MDI, Child);
         end if;

         Child.State := Floating;
         Activate_Child (Child);

      elsif Child.State = Floating and then not Float then
         --  Remove the widget from the list of embedded children
         Gtk.Layout.Put
           (Gtk_Layout_Record (Child.MDI.all)'Access, Child, Child.X, Child.Y);
         Widget_List.Remove (Child.MDI.Invisible_Items, Gtk_Widget (Child));
         Unref (Child);
         Child.State := Normal;

         Win := Gtk_Window (Get_Parent (Child.Initial_Child));
         Reparent (Child.Initial_Child, Gtk_Box (Get_Child (Child)));

         if Gtk_Widget (Child.Initial) = Gtk_Widget (Win) then
            Hide (Child.Initial);
         else
            Destroy (Win);
         end if;

         --  If all items are maximized, add Child to the notebook
         if Child.MDI.Docks (None) /= null then
            Put_In_Notebook (Child.MDI, None, Child);
         end if;

         Show_All (Child);
      end if;

      Update_Float_Menu (Child);
   end Float_Child;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean is
   begin
      return Child.State = Floating;
   end Is_Floating;

   ------------------------
   -- Move_Dock_Notebook --
   ------------------------

   procedure Move_Dock_Notebook
     (MDI : access MDI_Window_Record'Class; Last : Dock_Side)
   is
      Priorities : Priorities_Array renames MDI.Priorities;
      Order      : array (0 .. 4) of Dock_Side;
      W, H, X, Y : Gint;
      Req        : Gtk_Requisition;
      Pos, K     : Integer;
      Side       : Dock_Side;

   begin
      pragma Assert (Last = None or else MDI.Docks (Last) /= null);

      --  The middle one is always resized last.
      Order (Order'Last) := None;

      Pos := Order'First;

      for J in Priorities'Range loop
         K := 0;

         while K <= Pos - 1 loop
            if Priorities (Order (K)) > Priorities (J) then
               Order (K + 1 .. Pos) := Order (K .. Pos - 1);
               exit;
            end if;

            K := K + 1;
         end loop;

         Order (K) := J;
         Pos := Pos + 1;
      end loop;

      for J in Order'Range loop
         Side := Order (J);

         if MDI.Docks (Side) /= null then
            X := 0;
            Y := 0;
            Size_Request (MDI.Docks (Side), Req);
            W := Req.Width;
            H := Req.Height;

            case Side is
               when Left =>
                  H := MDI.MDI_Height;

               when Right =>
                  X := MDI.MDI_Width - Req.Width;
                  H := MDI.MDI_Height;

               when Bottom =>
                  Y := MDI.MDI_Height - Req.Height;
                  W := MDI.MDI_Width;

               when Top =>
                  W := MDI.MDI_Width;

               when None =>
                  X := Constrain_X (MDI, 0, 0);
                  Y := Constrain_Y (MDI, 0, 0);
                  W := Constrain_X (MDI, X + MDI.MDI_Width, 0) - X;
                  H := Constrain_Y (MDI, Y + MDI.MDI_Height, 0) - Y;
            end case;

            case Side is
               when Bottom | Top =>
                  if MDI.Docks (Left) /= null
                    and then Priorities (Left) <= Priorities (Side)
                  then
                     X := MDI.Docks (Left).Uniconified_Width;
                     W := W - X;
                  end if;

                  if MDI.Docks (Right) /= null
                    and then Priorities (Right) <= Priorities (Side)
                  then
                     W := W - MDI.Docks (Right).Uniconified_Width;
                  end if;

               when Left | Right =>
                  if MDI.Docks (Top) /= null
                    and then Priorities (Top) < Priorities (Side)
                  then
                     Y := MDI.Docks (Top).Uniconified_Height;
                     H := H - Y;
                  end if;

                  if MDI.Docks (Bottom) /= null
                    and then Priorities (Bottom) < Priorities (Side)
                  then
                     H := H - MDI.Docks (Bottom).Uniconified_Height;
                  end if;

               when None => null;
            end case;

            MDI.Docks (Side).Uniconified_Width := W;
            MDI.Docks (Side).Uniconified_Height := H;
            MDI.Docks (Side).X := X;
            MDI.Docks (Side).Y := Y;
         end if;
      end loop;

      --  The bottom and top docks should never overlap.
      --  Priority is given to the one last moved by the user

      if Last = Bottom and then MDI.Docks (Top) /= null then
         MDI.Docks (Top).Uniconified_Height :=
           Gint'Min (MDI.Docks (Bottom).Y, MDI.Docks (Top).Uniconified_Height);

      elsif Last = Top
        and then MDI.Docks (Bottom) /= null
        and then MDI.Docks (Bottom).Y < MDI.Docks (Top).Uniconified_Height
      then
         MDI.Docks (Bottom).Uniconified_Height :=
           MDI.Docks (Bottom).Uniconified_Height
           + MDI.Docks (Bottom).Y
           - MDI.Docks (Top).Uniconified_Height;
         MDI.Docks (Bottom).Y := MDI.Docks (Top).Uniconified_Height;
      end if;

      --  Likewise for the left and right docks

      if Last = Right and then MDI.Docks (Left) /= null then
         MDI.Docks (Left).Uniconified_Width :=
           Gint'Min (MDI.Docks (Left).Uniconified_Width, MDI.Docks (Right).X);

      elsif Last = Left
        and then MDI.Docks (Right) /= null
        and then MDI.Docks (Right).X < MDI.Docks (Left).Uniconified_Width
      then
         MDI.Docks (Right).Uniconified_Width :=
           MDI.Docks (Right).Uniconified_Width
           + MDI.Docks (Right).X
           - MDI.Docks (Left).Uniconified_Width;
         MDI.Docks (Right).X := MDI.Docks (Left).Uniconified_Width;
      end if;

      --  Do the actual resizing and moving

      for J in Dock_Side'Range loop
         if MDI.Docks (J) /= null then
            Set_USize (MDI.Docks (J),
                       MDI.Docks (J).Uniconified_Width,
                       MDI.Docks (J).Uniconified_Height);
            Move (MDI, MDI.Docks (J), MDI.Docks (J).X, MDI.Docks (J).Y);
         end if;
      end loop;

      --  Take care of the icons
      --  declare
      --     List : Widget_List.Glist := Children (MDI);
      --     Tmp : Widget_List.Glist := First (List);
      --     C2 : MDI_Child;
      --  begin
      --     while Tmp /= Null_List loop
      --        C2 := MDI_Child (Get_Data (Tmp));
      --        if C2.State = Iconified then
      --           C2.X := Constrain_X (MDI, C2.X);
      --           C2.Y := Constrain_Y (MDI, C2.Y);
      --           Move (MDI, C2, C2.X, C2.Y);
      --        end if;
      --        Tmp := Next (Tmp);
      --     end loop;
      --     Free (List);
      --  end;
   end Move_Dock_Notebook;

   ------------------------
   -- Docked_Switch_Page --
   ------------------------

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class)
   is
      Child : MDI_Child := MDI_Child (Docked_Child);
      C     : MDI_Child;
      Note  : Gtk_Notebook := Gtk_Notebook (Child.Initial);

   begin
      if Get_Current_Page (Note) /= -1 then
         Free (Child.Title);
         C := Find_MDI_Child
           (Child.MDI, Get_Nth_Page (Note, Get_Current_Page (Note)));
         Child.Title := new String' (C.Title.all);
         Activate_Child (C);
      end if;
   end Docked_Switch_Page;

   ---------------------
   -- Put_In_Notebook --
   ---------------------

   procedure Put_In_Notebook
     (MDI   : access MDI_Window_Record'Class;
      Side  : Dock_Side;
      Child : access MDI_Child_Record'Class)
   is
      Note  : Gtk_Notebook;
      Label : Gtk_Label;

   begin
      --  Create the notebook if it doesn't exist

      if MDI.Docks (Side) = null then
         Gtk_New (Note);
         Set_Tab_Pos (Note, Pos_Bottom);
         Set_Show_Tabs (Note, False);
         Set_Show_Border (Note, False);
         Gtk_New (MDI.Docks (Side), Note, Is_Dock => True, Side => Side);
         Put (MDI, MDI.Docks (Side));
         Widget_Callback.Object_Connect
           (Note, "switch_page",
            Widget_Callback.To_Marshaller (Docked_Switch_Page'Access),
            MDI.Docks (Side), After => True);

      else
         Note := Gtk_Notebook (MDI.Docks (Side).Initial);
         Set_Show_Tabs (Note, True);
      end if;

      --  If the child was already in a notebook (for instance for maximized
      --  items), remove it from notebook

      if Child.State = Embedded then
         Dock_Child (Child, False);
      end if;

      --  Embed the contents of the child into the notebook, and mark
      --  Child as embedded, so that we can't manipulate it afterwards.
      Gtk_New (Label, Child.Title.all);
      Ref (Child.Initial_Child);
      Remove (Gtk_Box (Get_Child (Child)), Child.Initial_Child);
      Append_Page (Note, Child.Initial_Child, Label);
      Unref (Child.Initial_Child);

      --  Remove the child from the MDI, but keep it in a list so that we
      --  can restore it in exactly the same state. Don't this if Child is
      --  already in the list
      Ref (Child);
      Widget_List.Prepend (MDI.Invisible_Items, Gtk_Widget (Child));

      if Get_Parent (Child) /= null then
         Remove (MDI, Child);
      end if;

      --  The size of the notebook will be reinitialized based on its new
      --  contents
      Set_USize (MDI.Docks (Side), -1, -1);

      Set_Page (Note, -1);
      Show_All (MDI.Docks (Side));
      Move_Dock_Notebook (MDI, Side);
      Child.State := Embedded;
   end Put_In_Notebook;

   --------------------------
   -- Remove_From_Notebook --
   --------------------------

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side)
   is
      Note : Gtk_Notebook := Gtk_Notebook (Child.MDI.Docks (Side).Initial);
   begin
      pragma Assert (Child.State = Embedded);
      Remove_Page (Note, Page_Num (Note, Child.Initial_Child));
      Unparent (Child.Initial_Child);

      if Get_Nth_Page (Note, 0) = null then
         Destroy (Child.MDI.Docks (Side));
         Child.MDI.Docks (Side) := null;
      else
         Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
      end if;

      Child.State := Normal;
   end Remove_From_Notebook;

   ----------------
   -- Dock_Child --
   ----------------

   procedure Dock_Child
     (Child : access MDI_Child_Record'Class;
      Dock  : Boolean := True;
      Side  : Dock_Side := None)
   is
      MDI : MDI_Window := Child.MDI;
      S   : Dock_Side;

   begin
      if Side /= None then
         Child.Dock := Side;
      end if;

      if Child.State /= Docked
        and then (Child.State /= Embedded or else Docked_Side (Child) = None)
        and then Dock
        and then Child.Dock /= None
      then
         Float_Child (Child, False);
         Minimize_Child (Child, False);
         Put_In_Notebook (MDI, Child.Dock, Child);

      elsif Child.State = Embedded and then not Dock then
         --  Reassign the widget to Child instead of the notebook
         S := Docked_Side (Child);
         Ref (Child.Initial_Child);
         Remove_From_Notebook (Child, S);
         Add (Gtk_Box (Get_Child (Child)), Child.Initial_Child);
         Unref (Child.Initial_Child);
         Child.State := Normal;

         --  If all items are maximized, maximize this one tool, unless
         --  it was already maximized.
         --  ??? Since this also takes care of removing it from the current
         --  notebook, we could save some time.

         if Child.MDI.Docks (None) /= null and then S /= None then
            Put_In_Notebook (Child.MDI, None, Child);
         else
            Child.X := Constrain_X (MDI, Child.X);
            Child.Y := Constrain_Y (MDI, Child.Y);
            Gtk.Layout.Put
              (Gtk_Layout_Record (MDI.all)'Access, Child, Child.X, Child.Y);
         end if;

         Show_All (Child);
         Activate_Child (Child);

         --  Remove the widget from the list of embedded children
         Widget_List.Remove (MDI.Invisible_Items, Gtk_Widget (Child));
         Unref (Child);
      end if;

      Update_Dock_Menu (Child);
   end Dock_Child;

   -------------------
   -- Set_Dock_Side --
   -------------------

   procedure Set_Dock_Side
     (Child : access MDI_Child_Record'Class; Side  : Dock_Side)
   is
      S : Dock_Side;
   begin
      --  If the child is already docked on another side, change it.
      if Child.State = Embedded then
         S := Docked_Side (Child);

         if Side /= S and then S /= None then
            Dock_Child (Child, False);
            Child.Dock := Side;
            Dock_Child (Child, True, Side);

         else
            Child.Dock := Side;
         end if;
      else
         Child.Dock := Side;
      end if;

      Update_Dock_Menu (Child);
   end Set_Dock_Side;

   --------------------
   -- Minimize_Child --
   --------------------

   procedure Minimize_Child
     (Child : access MDI_Child_Record'Class; Minimize : Boolean)
   is
      MDI         : MDI_Window := Child.MDI;
      Icon_Height : constant Gint := Title_Bar_Height + 2 * Border_Thickness;

   begin
      if Child.State /= Iconified and then Minimize then
         Float_Child (Child, False);
         Dock_Child (Child, False);
         Child.Uniconified_X := Child.X;
         Child.Uniconified_Y := Child.Y;
         Set_USize (Child, Icons_Width, Icon_Height);
         Child.State := Iconified;

         declare
            List : Widget_List.Glist := Children (MDI);
            Tmp  : Widget_List.Glist := First (List);
            C2   : MDI_Child;

         begin
            Child.X := Constrain_X (MDI, 0);
            Child.Y := (Constrain_Y (MDI, MDI.MDI_Height) / Icon_Height)
              * Icon_Height;

            while Tmp /= Null_List loop
               C2 := MDI_Child (Get_Data (Tmp));

               if C2 /= MDI_Child (Child) and then C2.State = Iconified then
                  if C2.Y mod Icon_Height = 0
                    and then C2.Y <= Child.Y
                  then
                     if C2.X + Icons_Width >= MDI.MDI_Width then
                        Child.X := Constrain_X (MDI, 0);
                        Child.Y := C2.Y - Icon_Height;
                     elsif C2.X + Icons_Width > Child.X then
                        Child.X := C2.X + Icons_Width;
                        Child.Y := C2.Y;
                     end if;
                  end if;
               end if;

               Tmp := Next (Tmp);
            end loop;

            Free (List);
         end;

         Move (MDI, Child, Child.X, Child.Y);
         Activate_Child (Child);
         Hide (Child.Maximize_Button);

      elsif Child.State = Iconified and then not Minimize then
         Set_USize (Child, Child.Uniconified_Width, Child.Uniconified_Height);
         Child.X := Constrain_X (MDI, Child.Uniconified_X);
         Child.Y := Constrain_Y (MDI, Child.Uniconified_Y);
         Child.State := Normal;
         Move (MDI, Child, Child.X, Child.Y);

         --  If all items are maximized, add Child to the notebook
         if Child.MDI.Docks (None) /= null then
            Put_In_Notebook (Child.MDI, None, Child);
         end if;

         Activate_Child (Child);
         Show (Child.Maximize_Button);
      end if;
   end Minimize_Child;

   -----------------------
   -- Maximize_Children --
   -----------------------

   procedure Maximize_Children
     (MDI : access MDI_Window_Record; Maximize : Boolean := True)
   is
      use Widget_List;

      List : Widget_List.Glist;
      Tmp  : Widget_List.Glist;
      C    : MDI_Child;

   begin
      if Maximize then
         List := Children (MDI);
         Tmp := First (List);

         while Tmp /= Null_List loop
            C := MDI_Child (Get_Data (Tmp));
            Tmp := Next (Tmp);

            if C.State = Normal then
               Put_In_Notebook (MDI, None, C);
            end if;
         end loop;

         Free (List);

      else
         Tmp := First (MDI.Invisible_Items);

         while Tmp /= Null_List loop
            C := MDI_Child (Get_Data (Tmp));
            Tmp := Next (Tmp);

            if C.State = Embedded and then Docked_Side (C) = None then
               Dock_Child (C, False);
            end if;
         end loop;
      end if;
   end Maximize_Children;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Child : access MDI_Child_Record) return Gtk_Widget is
   begin
      return Gtk_Widget (Child.Initial_Child);
   end Get_Widget;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window (Child : access MDI_Child_Record) return Gtk_Window is
   begin
      if Child.Initial.all in Gtk_Window_Record'Class then
         return Gtk_Window (Child.Initial);
      else
         return null;
      end if;
   end Get_Window;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child
   is
      Note : Gtk_Notebook;
   begin
      if MDI.Focus_Child /= null and then MDI.Focus_Child.State = Docked then
         Note := Gtk_Notebook (MDI.Focus_Child.Initial);
         return Find_MDI_Child
           (MDI, Get_Nth_Page (Note, Get_Current_Page (Note)));
      end if;

      return MDI.Focus_Child;
   end Get_Focus_Child;

   ----------------
   -- Cascade_Cb --
   ----------------

   procedure Cascade_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Cascade_Children (MDI_Window (MDI));
   end Cascade_Cb;

   ---------------
   -- Tile_H_Cb --
   ---------------

   procedure Tile_H_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Horizontally (MDI_Window (MDI));
   end Tile_H_Cb;

   ---------------
   -- Tile_V_Cb --
   ---------------

   procedure Tile_V_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Vertically (MDI_Window (MDI));
   end Tile_V_Cb;

   -----------------
   -- Maximize_Cb --
   -----------------

   procedure Maximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), True);
   end Maximize_Cb;

   -------------------
   -- Unmaximize_Cb --
   -------------------

   procedure Unmaximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), False);
   end Unmaximize_Cb;

   -------------
   -- Dock_Cb --
   -------------

   procedure Dock_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Dock_Child (C, C.State /= Embedded or else Docked_Side (C) = None);
      end if;
   end Dock_Cb;

   --------------
   -- Float_Cb --
   --------------

   procedure Float_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Float_Child (C, C.State /= Floating);
      end if;
   end Float_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Close_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := MDI_Window (MDI).Focus_Child;
         --  Close automatically gets the contents of docks, instead of the
         --  dock itself
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Close_Child (C);
      end if;
   end Close_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Focus_Cb (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      if Get_Active (C.Menu_Item) then
         Activate_Child (C);
      end if;
   end Focus_Cb;

   --------------------------
   -- Menu_Entry_Destroyed --
   --------------------------

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class) is
   begin
      MDI_Child (Child).Menu_Item := null;
   end Menu_Entry_Destroyed;

   -----------------------
   -- Create_Menu_Entry --
   -----------------------

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class) is
      G : Widget_SList.GSlist;
   begin
      if Child.Menu_Item = null and then Child.State /= Docked then
         G := Child.MDI.Menu_Item_Group;
         Gtk_New (Child.Menu_Item, G, Child.Title.all);
         Append (Child.MDI.Menu, Child.Menu_Item);
         Set_Active
           (Child.Menu_Item, MDI_Child (Child) = Child.MDI.Focus_Child);
         Show_All (Child.Menu_Item);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "activate",
            Widget_Callback.To_Marshaller (Focus_Cb'Access), Child,
            After => True);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "destroy",
            Widget_Callback.To_Marshaller (Menu_Entry_Destroyed'Access),
            Child);
         Child.MDI.Menu_Item_Group := Group (Child.Menu_Item);
      end if;
   end Create_Menu_Entry;

   --------------------
   -- Menu_Destroyed --
   --------------------

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class) is
   begin
      MDI_Window (MDI).Menu := null;
      MDI_Window (MDI).Dock_Menu_Item := null;
      MDI_Window (MDI).Float_Menu_Item := null;
      MDI_Window (MDI).Menu_Item_Group := Widget_SList.Null_List;
   end Menu_Destroyed;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
     (MDI : access MDI_Window_Record) return Gtk.Menu.Gtk_Menu
   is
      Item  : Gtk_Menu_Item;
      Child : MDI_Child;

   begin
      if MDI.Menu = null then
         Gtk_New (MDI.Menu);

         Gtk_New (Item, "Cascade");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Cascade_Cb'Access), MDI);

         Gtk_New (Item, "Tile Horizontally");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_H_Cb'Access), MDI);

         Gtk_New (Item, "Tile Vertically");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_V_Cb'Access), MDI);

         Gtk_New (Item, "Maximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Maximize_Cb'Access), MDI);

         Gtk_New (Item, "Unmaximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Unmaximize_Cb'Access), MDI);

         Gtk_New (Item, "Arrange Icons");
         Append (MDI.Menu, Item);
         Set_Sensitive (Item, False);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Dock_Menu_Item, "Docked");
         Append (MDI.Menu, MDI.Dock_Menu_Item);
         Set_Active (MDI.Dock_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Embedded);
         MDI.Dock_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Dock_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Dock_Cb'Access), MDI);

         Gtk_New (MDI.Float_Menu_Item, "Floating");
         Append (MDI.Menu, MDI.Float_Menu_Item);
         Set_Active (MDI.Float_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Floating);
         MDI.Float_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Float_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Float_Cb'Access), MDI);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (Item, "Close");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Close_Cb'Access), MDI);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         declare
            Tmp : Widget_List.Glist := First (Children (MDI));
         begin
            while Tmp /= Null_List loop
               Child := MDI_Child (Get_Data (Tmp));
               Create_Menu_Entry (Child);
               Tmp := Next (Tmp);
            end loop;

            Tmp := First (MDI.Invisible_Items);
            while Tmp /= Null_List loop
               Child := MDI_Child (Get_Data (Tmp));
               Create_Menu_Entry (Child);
               Tmp := Next (Tmp);
            end loop;
         end;

         Widget_Callback.Object_Connect
           (MDI.Menu, "destroy",
            Widget_Callback.To_Marshaller (Menu_Destroyed'Access), MDI);
      end if;

      Show_All (MDI.Menu);
      return MDI.Menu;
   end Create_Menu;

   -----------------------
   -- Create_Child_Menu --
   -----------------------

   function Create_Child_Menu
     (Child : access MDI_Child_Record'Class) return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Item  : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;

   begin
      Gtk_New (Menu);

      Gtk_New (Item, "Cascade");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Horizontally");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Vertically");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Maximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Unmaximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Arrange Icons");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Check, "Docked");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Embedded or else Child.State = Docked);
      Set_Sensitive (Check, Child.Dock /= None);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Dock_Cb'Access), Child);

      Gtk_New (Check, "Floating");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Floating);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Float_Cb'Access), Child);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Item, "Close");
      Append (Menu, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Close_Cb'Access), Child);

      Show_All (Menu);
      return Menu;
   end Create_Child_Menu;

   --------------------
   -- Set_Priorities --
   --------------------

   procedure Set_Priorities
     (MDI : access MDI_Window_Record; Prio : Priorities_Array) is
   begin
      MDI.Priorities := Prio;
   end Set_Priorities;

end Gtkada.MDI;
