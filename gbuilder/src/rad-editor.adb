with Glib;              use Glib;
with Gdk;               use Gdk;
with Gdk.Color;         use Gdk.Color;
with Gdk.Drawable;      use Gdk.Drawable;
with Gdk.Event;         use Gdk.Event;
with Gdk.GC;            use Gdk.GC;
with Gdk.Pixmap;        use Gdk.Pixmap;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Window;        use Gdk.Window;
with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Container;     use Gtk.Container;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Fixed;         use Gtk.Fixed;
with Gtk.Handlers;
with Gtk.Layout;        use Gtk.Layout;
with Gtk.Main;          use Gtk.Main;
with Gtk.Menu_Bar;      use Gtk.Menu_Bar;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Notebook;      use Gtk.Notebook;
with Gtk.Packer;        use Gtk.Packer;
with Gtk.Style;         use Gtk.Style;
with Gtk.Viewport;      use Gtk.Viewport;
with Gtk.Window;        use Gtk.Window;
with Gtkada.Handlers;   use Gtkada.Handlers;
with RAD.Debug;         use RAD.Debug;
with RAD.GB_Widget;     use RAD.GB_Widget;
with RAD.Pixmaps;       use RAD.Pixmaps;

package body RAD.Editor is

   ---------------
   -- Constants --
   ---------------

   Min_Widget_Width          : constant := 16;
   Min_Widget_Height         : constant := 16;
   Max_Initial_Widget_Width  : constant := 300;
   Max_Initial_Widget_Height : constant := 200;
   Default_Widget_Width      : constant := 50;
   Default_Widget_Height     : constant := 50;

   Placeholder_Width         : constant := 16;
   Placeholder_Height        : constant := 16;

   --  The size of the selection handles in the corners of widgets
   Corner_Width              : constant := 7;
   Corner_Height             : constant := 7;

   --  The grid (for fixed containers)

   type Editor_Grid_Style is (Grid_Dots, Grid_Lines);

   Show_Grid         : constant Boolean := True;
   Grid_Horz_Spacing : constant Gint := 8;
   Grid_Vert_Spacing : constant Gint := 8;
   Grid_Style        : constant Editor_Grid_Style := Grid_Dots;

   Selected_Widgets  : Widget_List.Glist;
   --  List of selected widgets
   --  ??? global variable

   Global_Selection  : Boolean := True;
   --  ???? Temporary variable for preliminary tests

   procedure Add_Mouse_Signals_Recursive
     (Widget : access Gtk_Widget_Record'Class);
   --  Add signals related to the mouse recursively in widget and its children

   function Check_Ignore_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Return True if the event should be ignored.
   --  Currently always return False.

   procedure Draw_Grid (Widget : access Gtk_Widget_Record'Class);

   procedure Draw_Widget (Widget : access Gtk_Widget_Record'Class);

   procedure Draw_Widget_Focus (Widget : access Gtk_Widget_Record'Class);

   function Expose_Widget
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Expose) return Boolean;

   function Get_Widget_Window
     (Parent : Gtk_Widget;
      Widget : access Gtk_Widget_Record'Class) return Gdk.Gdk_Window;
   --  Return the window to draw on for a given widget.
   --  SPECIAL CODE: Gnome_Dock_Item widgets use a different window when
   --  floating.

   procedure Get_Event_Widget
     (Widget       : access Gtk_Widget_Record'Class;
      Window       : Gdk.Gdk_Window;
      X, Y         : Gint;
      X_Out        : out Gint;
      Y_Out        : out Gint;
      Event_Widget : out Gtk_Widget);
   --  This function is passed a widget which has received a mouse event, and
   --  the coordinates of that event. It returns the widget which the event is
   --  really meant for (which could be a descendent of the given widget), and
   --  the position of the event in the widget's allocated area.

   function Is_Placeholder
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Return True if Widget is a placeholder (i.e is not have a real widget)

   function On_Button_Press
     (Signal_Widget : access Gtk_Widget_Record'Class;
      Event         : Gdk_Event_Button) return Boolean;

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   function On_Enter_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing) return Boolean;

   function On_Key_Press_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean;

   function On_Key_Release_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean;

   function On_Leave_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing) return Boolean;

   function On_Motion_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Motion) return Boolean;

   procedure On_Size_Allocate
     (Widget     : access Gtk_Widget_Record'Class;
      Allocation : Gtk_Allocation_Access);

   procedure On_Widget_Realize (Widget : access Gtk_Widget_Record'Class);
   --  Add signals to widgets to allow manipulation, e.g. selecting/drawing

   procedure Paint_Selection
     (Window : Gdk.Gdk_Window; GC : Gdk.Gdk_GC; X, Y, Width, Height : Gint);

   procedure Paint_Selections (Widget : access Gtk_Widget_Record'Class);

   procedure Paint_Widget (Widget : access Gtk_Widget_Record'Class);
   --  Handler for draw events on a widget.
   --  This procedure will draw GUI builder artifacts on top of any widget

   package Allocation_Marshaller is new
     Widget_Callback.Marshallers.Generic_Marshaller
       (Gtk_Allocation_Access, To_Allocation);

   ----------------------
   -- Add_Draw_Signals --
   ----------------------

   procedure Add_Mouse_Signals_Recursive
     (Widget : access Gtk_Widget_Record'Class) is
   begin
      pragma Debug (Message ("Adding mouse signals to: " & Get_Name (Widget)));

      --  ??? We don't add signals to menu items, since it currently makes it
      --  impossible to popup the menus in a menubar.

      if Widget.all in Gtk_Menu_Item_Record'Class then
         return;
      end if;

      --  Ensure that the event mask is set so we get button press & release
      --  events.

      if not No_Window_Is_Set (Widget) then
         if not Realized_Is_Set (Widget) then
            Add_Events (Widget, Button_Press_Mask or Button_Release_Mask);
         end if;
      else
         --  Here we set the event mask for the main window of a widget,
         --  but widgets can have more than one window. How do we get all the
         --  windows of a widget ???

         Set_Events (Get_Window (Widget),
           Get_Events (Get_Window (Widget)) or
             Button_Press_Mask or Button_Release_Mask);
      end if;

      Return_Callback.Connect
        (Widget, "button_press_event",
         Return_Callback.To_Marshaller (On_Button_Press'Access));
      Return_Callback.Connect
        (Widget, "button_release_event",
         Return_Callback.To_Marshaller (On_Button_Release'Access));

      --  We connect to these so we can stop widgets getting them while we are
      --  dragging/resizing. It stops widgets changing state, i.e.
      --  normal/active and so cuts down on flickering a bit.

      Return_Callback.Connect
        (Widget, "enter_notify_event",
         Return_Callback.To_Marshaller (On_Enter_Notify'Access));
      Return_Callback.Connect
        (Widget, "leave_notify_event",
         Return_Callback.To_Marshaller (On_Leave_Notify'Access));

      Children_Foreach (Widget, Add_Mouse_Signals_Recursive'Access);
   end Add_Mouse_Signals_Recursive;

   -----------------------
   -- Add_Mouse_Signals --
   -----------------------

   --  We need to be careful about passing events on to widgets, especially
   --  with regard to mouse grabs - in a Gtk_Entry the mouse is grabbed while
   --  selecting text, and this can cause all sorts of problems for RAD.

   procedure Add_Mouse_Signals (Widget : access Gtk_Widget_Record'Class) is
   begin
      --  Widgets without windows will not get events directly from X Windows,
      --  but they may have child widgets which pass events up to them, e.g.
      --  a Gtk_Combo has a Gtk_Entry which will get X events.
      --  This doesn't matter too much since we have to call a function to
      --  figure out which widget the event is for anyway.

      Add_Mouse_Signals_Recursive (Widget);

      Widget_Callback.Connect
        (Widget, "realize",
         Widget_Callback.To_Marshaller (On_Widget_Realize'Access),
         After => True);
   end Add_Mouse_Signals;

   ----------------------
   -- Add_Key_Signals --
   ----------------------

   procedure Add_Key_Signals (Widget : access Gtk_Widget_Record'Class) is
   begin
      --  We only add key signal handlers to windows.

      if not (Widget.all in Gtk_Window_Record'Class) then
         return;
      end if;

      Return_Callback.Connect
        (Widget, "key_press_event",
         Return_Callback.To_Marshaller (On_Key_Press_Event'Access));
      Return_Callback.Connect
        (Widget, "key_release_event",
         Return_Callback.To_Marshaller (On_Key_Release_Event'Access));
   end Add_Key_Signals;

   ----------------------
   -- Add_Draw_Signals --
   ----------------------

   procedure Add_Draw_Signals (Widget : access Gtk_Widget_Record'Class) is
   begin
      --  ??? Note that we set Pointer_Motion_Hint_Mask here. This may not
      --  be wise since widgets may be designed to work with normal motion
      --  events only.

      if not No_Window_Is_Set (Widget) then
         Add_Events (Widget,
           Exposure_Mask or Pointer_Motion_Mask
             or Button1_Motion_Mask
             or Pointer_Motion_Hint_Mask);
      end if;

      Return_Callback.Connect
        (Widget, "expose_event",
         Return_Callback.To_Marshaller (Expose_Widget'Access),
         After => True);
      Widget_Callback.Connect (Widget, "draw",
         Widget_Callback.To_Marshaller (Draw_Widget'Access),
         After => True);
      Widget_Callback.Connect
        (Widget, "size_allocate",
         Allocation_Marshaller.To_Marshaller (On_Size_Allocate'Access),
         After => True);

      --  Needed for button, others?
      Widget_Callback.Connect
        (Widget, "draw_default",
         Widget_Callback.To_Marshaller (Draw_Widget_Focus'Access),
         After => True);
      Widget_Callback.Connect
        (Widget, "draw_focus",
         Widget_Callback.To_Marshaller (Draw_Widget_Focus'Access),
         After => True);

      --  ??? mouse signal - This also needs to be added to all children.
      Return_Callback.Connect
        (Widget, "motion_notify_event",
         Return_Callback.To_Marshaller (On_Motion_Notify'Access));

      --  Needed for scrolled window, clist? & possibly other widgets

      if Widget.all in Gtk_Container_Record'Class then
         Forall (Gtk_Container (Widget), Add_Draw_Signals'Access);
      end if;
   end Add_Draw_Signals;

   ------------------------
   -- Check_Ignore_Event --
   ------------------------

   function Check_Ignore_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean is
   begin
      return False;
   end Check_Ignore_Event;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid (Widget : access Gtk_Widget_Record'Class) is
      GC     : constant Gdk.Gdk_GC :=
        Get_Dark_GC (Get_Style (Widget), State_Normal);
      Width  : constant Gint := Gint (Get_Allocation_Width (Widget));
      Height : constant Gint := Gint (Get_Allocation_Height (Widget));
      Gridx, Gridy, Origin_X, Origin_Y : Gint;
      Window : Gdk.Gdk_Window;

   begin
      if Widget.all in Gtk_Layout_Record'Class then
         Window := Get_Bin_Window (Gtk_Layout (Widget));
         Origin_X := Grid_Horz_Spacing -
           Gint (Get_Xoffset (Gtk_Layout (Widget))) mod Grid_Horz_Spacing;
         Origin_Y := Grid_Vert_Spacing -
           Gint (Get_Yoffset (Gtk_Layout (Widget))) mod Grid_Vert_Spacing;
      else
         Window := Get_Window (Widget);
         Origin_X := 0;
         Origin_Y := 0;
      end if;

      --  Note: should we take the Border_Width into account? - i.e. start the
      --  grid inside the border. It makes it awkward if you change the border
      --  size.

      if not Show_Grid then
         return;
      end if;

      if Grid_Style = Grid_Dots then
         Gridx := Origin_X;

         while Gridx < Width loop
            Gridy := Origin_Y;

            while Gridy < Height loop
               Draw_Point (Window, GC, Gridx, Gridy);
               Gridy := Gridy + Grid_Vert_Spacing;
            end loop;

            Gridx := Gridx + Grid_Horz_Spacing;
         end loop;
      else
         Gridx := Origin_X;
         while Gridx < Width loop
            Draw_Line (Window, GC, Gridx, 0, Gridx, Height);
            Gridx := Gridx + Grid_Horz_Spacing;
         end loop;

         Gridy := Origin_Y;
         while Gridy < Height loop
            Draw_Line (Window, GC, 0, Gridy, Width, Gridy);
            Gridy := Gridy + Grid_Vert_Spacing;
         end loop;
      end if;
   end Draw_Grid;

   -----------------
   -- Draw_Widget --
   -----------------

   procedure Draw_Widget (Widget : access Gtk_Widget_Record'Class) is
   begin
      pragma Debug (Message ("In Draw_Widget: " & Get_Name (Widget)));

      Paint_Widget (Widget);
      --  ??? Gtk.Handlers.Emit_Stop_By_Name (Widget, "draw");
   end Draw_Widget;

   -----------------------
   -- Draw_Widget_Focus --
   -----------------------

   procedure Draw_Widget_Focus (Widget : access Gtk_Widget_Record'Class) is
   begin
      pragma Debug
        (Message ("In Draw_Widget_Focus: " & Get_Name (Widget) &
           " Parent: " & Get_Name (Get_Parent (Widget))));
      Paint_Widget (Widget);
   end Draw_Widget_Focus;

   -------------------
   -- Expose_Widget --
   -------------------

   function Expose_Widget
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Expose) return Boolean is
   begin
      pragma Debug
        (Message ("In Expose_Widget: " & Get_Name (Widget)));
      pragma Debug
        (Message ("Area x:" & Get_Area (Event).X'Img &
           " y:"      & Get_Area (Event).Y'Img &
           " width:"  & Get_Area (Event).Width'Img &
           " height:" & Get_Area (Event).Height'Img));

      --  Ignore spurious exposes before widget is positioned.

      if Get_Allocation_X (Widget) = -1
        or else Get_Allocation_Y (Widget) = -1
      then
         return False;
      end if;

      Paint_Widget (Widget);

      --  ??? GTK bug workaround - try to stop multiple exposes of
      --  placeholders, which happens because the drawing area doesn't have a
      --  Draw function and the default widget Real_Draw function emits an
      --  expose event.

      if Is_Placeholder (Widget) then
         Gtk.Handlers.Emit_Stop_By_Name (Widget, "expose_event");
      end if;

      return False;
   end Expose_Widget;

   ----------------------
   -- Get_Event_Widget --
   ----------------------

   procedure Get_Event_Widget
     (Widget       : access Gtk_Widget_Record'Class;
      Window       : Gdk.Gdk_Window;
      X, Y         : Gint;
      X_Out        : out Gint;
      Y_Out        : out Gint;
      Event_Widget : out Gtk_Widget)
   is
      Found_Child      : Gtk_Widget;
      Win_X, Win_Y,
      Saved_X, Saved_Y : Gint;
      Found_X, Found_Y : Gint := 0;
      Parent_Window    : Gdk.Gdk_Window;
      Parent           : constant Gtk_Widget := Get_Parent (Widget);
      Local_Widget     : Gtk_Widget;
      Local_Window     : Gdk.Gdk_Window;

      procedure Find_Child_At (Widget : access Gtk_Widget_Record'Class);
      --  Find widget at a given point (X_Out, Y_Out)
      --  Set Found_Child in case of success.
      --  Note: returns last found child if children overlap

      procedure Find_Notebook_Tab (Widget : access Gtk_Widget_Record'Class);
      --  Check if point (X_Out, Y_Out) is in the given notebook's tabs.
      --  Set Found_Child in case of success.

      procedure Find_Child_At (Widget : access Gtk_Widget_Record'Class) is
      begin
         pragma Debug
           (Message ("In Find_Child_At: " & Get_Name (Widget) &
              " X:" & Get_Allocation_X (Widget)'Img &
              " Y:" & Get_Allocation_Y (Widget)'Img &
              " W:" & Get_Allocation_Width (Widget)'Img &
              " H:" & Get_Allocation_Height (Widget)'Img));

         --  Notebook pages are visible but not mapped if they are not showing.

         if Drawable_Is_Set (Widget)
           and then Get_Allocation_X (Widget) <= X_Out
           and then Get_Allocation_Y (Widget) <= Y_Out
           and then Get_Allocation_X (Widget) +
             Gint (Get_Allocation_Width (Widget)) > X_Out
           and then Get_Allocation_Y (Widget) +
             Gint (Get_Allocation_Height (Widget)) > Y_Out
         then
            pragma Debug (Message ("found child: " & Get_Name (Widget)));
            Found_Child := Widget.all'Access;
         end if;
      end Find_Child_At;

      procedure Find_Notebook_Tab (Widget : access Gtk_Widget_Record'Class) is
         use Page_List;

         Children : Glist;
         Page     : Gtk_Notebook_Page;

      begin
         Children := Get_Children (Gtk_Notebook (Widget));

         while Children /= Null_List loop
            Page := Get_Data (Children);

            if          (Get_Allocation_X (Page) <= X_Out)
               and then (Get_Allocation_Y (Page) <= Y_Out)
               and then (Get_Allocation_X (Page) +
                 Gint (Get_Allocation_Width (Page)) > X_Out)
               and then (Get_Allocation_Y (Page) +
                 Gint (Get_Allocation_Height (Page)) > Y_Out)
            then
               --  Make sure this is a Gb_Widget.
               --  ??? if Is_GB_Widget (Get_Tab_Label (Page))
               Found_Child := Get_Tab_Label (Page);
            end if;

            Children := Next (Children);
         end loop;
      end Find_Notebook_Tab;

   begin
      pragma Debug
        (Message ("Original: " & Get_Name (Widget) &
           " X:" & X'Img & " Y:" & Y'Img));

      --  GTK bug workaround??? - need to translate coords if mouse button was
      --  pressed in a child window.
      --  Remember widgets can have other windows besides their main one, and
      --  when dragging the event may be sent to the parent's window?

      --  SPECIAL CODE: Gnome_Dock_Item widgets which are floating use a
      --  different window.

      if Parent /= null then
         pragma Debug
           (Message ("  Parent: " & Get_Name (Parent)));

         --  ??? Gnome
         --  if Widget.all in Gnome_Dock_Item_Record'Class
         --    and then Is_Floating (Widget)
         --  then
         --     Parent_Window := Get_Float_Window (Widget);
         --  elsif Parent.all in Gnome_Dock_Item_Record'Class
         --    and then Is_Floating (Parent)
         --  then
         --     Parent_Window := Get_Float_Window (Parent);
         if False then
            null;
         else
            Parent_Window := Get_Window (Parent);
         end if;
      else
         Parent_Window := Get_Window (Widget);
      end if;

      X_Out := X;
      Y_Out := Y;
      Local_Window := Window;

      while Local_Window /= null and then Local_Window /= Parent_Window loop
         Get_Position (Local_Window, Win_X, Win_Y);

         pragma Debug
           (Message ("  adding X:" & Win_X'Img & " Y:" & Win_Y'Img));

         X_Out := X_Out + Win_X;
         Y_Out := Y_Out + Win_Y;
         Local_Window := Get_Parent (Local_Window);
      end loop;

      if Local_Window /= Parent_Window then
         pragma Debug (Message ("  Get_Event_Widget - unknown window"));
         Event_Widget := null;
         return;
      end if;

      pragma Debug
        (Message ("  Translated X:" & X_Out'Img & " Y:" & Y_Out'Img));

      --  We now have correct coordinates relative to the parent's window,
      --  i.e. in the same coordinate space as the widget's allocation.
      --  Now we find out which widget this event is really for.
      --  We step down the widget tree, trying to find the widget at the given
      --  position. We have to translate coordinates for children of widgets
      --  with windows. We may need to use Bin_Window for viewport.

      if Is_GB_Widget (Widget) or else Is_Placeholder (Widget) then
         pragma Debug (Message ("Found child: " & Get_Name (Widget)));

         Event_Widget := Widget.all'Access;
         Found_X := X_Out;
         Found_Y := Y_Out;
      end if;

      --  Save the widget and the x & y coords.
      Local_Widget := Widget.all'Access;
      Saved_X := X_Out;
      Saved_Y := Y_Out;

      --  Now we want to convert the coordinates into the widget's childrens'
      --  coordinate space, so if the widget has a window, we have to subtract
      --  the position of it (since the child coordinates are relative to
      --  that). Viewports need special treatment.

      if not No_Window_Is_Set (Widget) and then Parent /= null then
         Get_Position (Get_Window (Widget), Win_X, Win_Y);
         X_Out := X_Out - Win_X;
         Y_Out := Y_Out - Win_Y;

         --  SPECIAL CODE: need to translate to bin_window for a viewport.

         if Widget.all in Gtk_Viewport_Record'Class then
            Get_Position (Get_Bin_Window (Gtk_Layout (Widget)), Win_X, Win_Y);
            X_Out := X_Out - Win_X;
            Y_Out := Y_Out - Win_Y;
         end if;

         pragma Debug
           (Message ("  Translated X:" & X_Out'Img & " Y:" & Y_Out'Img));
      end if;

      loop
         exit when not (Local_Widget.all in Gtk_Container_Record'Class)
           or else Local_Widget.all in Gtk_Menu_Bar_Record'Class;

         Found_Child := null;

         pragma Debug (Message ("...Finding child widget"));

         Forall
           (Gtk_Container (Local_Widget), Find_Child_At'Unrestricted_Access);

         --  SPECIAL CODE - Check for notebook tabs.

         if Local_Widget.all in Gtk_Notebook_Record'Class then
            Find_Notebook_Tab (Local_Widget);
         end if;

         exit when Found_Child = null;

         pragma Debug
           (Message ("Found child: " & Get_Name (Found_Child)));

         Local_Widget := Found_Child;

         if Is_GB_Widget (Local_Widget)
           or else Is_Placeholder (Local_Widget)
         then
            Event_Widget := Local_Widget;
            Found_X := X_Out;
            Found_Y := Y_Out;
         end if;

         if not No_Window_Is_Set (Local_Widget) then
            Get_Position (Get_Window (Local_Widget), Win_X, Win_Y);
            X_Out := X_Out - Win_X;
            Y_Out := Y_Out - Win_Y;

            --  SPECIAL CODE: need to translate to bin_window for a viewport.

            if Local_Widget.all in Gtk_Viewport_Record'Class then
               Get_Position
                 (Get_Bin_Window (Gtk_Viewport (Local_Widget)), Win_X, Win_Y);
               X_Out := X_Out - Win_X;
               Y_Out := Y_Out - Win_Y;
            end if;

            pragma Debug
              (Message ("  Translated X:" & X_Out'Img & " Y:" & Y_Out'Img));
         end if;
      end loop;

      --  If we haven't found a Gb_Widget yet, try moving up the hierarchy.

      Local_Widget := Widget.all'Access;
      X_Out := Saved_X;
      Y_Out := Saved_Y;

      while Event_Widget = null
        and then Get_Parent (Local_Widget) /= null
      loop
         Local_Widget := Get_Parent (Local_Widget);

         pragma Debug (Message ("  Trying parent: " & Get_Name (Local_Widget) &
           " X:" & X_Out'Img & " Y:" & Y_Out'Img));

         if Is_GB_Widget (Local_Widget)
           or else Is_Placeholder (Local_Widget)
         then
            Event_Widget := Local_Widget;
            Found_X := X_Out;
            Found_Y := Y_Out;

         elsif not No_Window_Is_Set (Widget) then
            Get_Position (Get_Window (Local_Widget), Win_X, Win_Y);
            X_Out := X_Out + Win_X;
            Y_Out := Y_Out + Win_Y;

            --  SPECIAL CODE; use bin_window for viewport.

            if Local_Widget.all in Gtk_Viewport_Record'Class then
               Get_Position
                 (Get_Bin_Window (Gtk_Viewport (Local_Widget)), Win_X, Win_Y);
               X_Out := X_Out + Win_X;
               Y_Out := Y_Out + Win_Y;
            end if;

            pragma Debug
              (Message ("  Translated X:" & X_Out'Img & " Y:" & Y_Out'Img));
         end if;
      end loop;

      if Event_Widget = null then
         return;
      end if;

      X_Out := Found_X - Get_Allocation_X (Event_Widget);
      Y_Out := Found_Y - Get_Allocation_Y (Event_Widget);

      pragma Debug (Message ("  Event widget: " & Get_Name (Event_Widget) &
        " X:" & X_Out'Img & " Y:" & Y_Out'Img));
   end Get_Event_Widget;

   -----------------------
   -- Get_Widget_Window --
   -----------------------

   function Get_Widget_Window
     (Parent : Gtk_Widget;
      Widget : access Gtk_Widget_Record'Class) return Gdk.Gdk_Window is
   begin
      --  ??? Gnome
      --  if Widget.all in Gnome_Dock_Item_Record'Class
      --    and then Is_Floating (Gnome_Dock_Item (Widget))
      --  then
      --     return Get_Float_Window (Gnome_Dock_Item (Widget));
      --  end if;

      --  if Parent /= null
      --    and then Parent.all in Gnome_Dock_Item_Record'Class
      --    and then Is_Floating (Gnome_Dock_Item (Parent))
      --  then
      --     return Get_Float_Window (Gnome_Dock_Item (Parent));
      --  end if;

      if Parent /= null then
         return Get_Window (Parent);
      else
         return Get_Window (Widget);
      end if;
   end Get_Widget_Window;

   --------------------
   -- Is_Placeholder --
   --------------------

   function Is_Placeholder
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      --  ??? Get_Data...
      return True;
   end Is_Placeholder;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Signal_Widget : access Gtk_Widget_Record'Class;
      Event         : Gdk_Event_Button) return Boolean
   is
      Widget  : Gtk_Widget;
      X, Y    : Gint;
      Handled : Boolean := False;

   begin
      pragma Debug
        (Message ("In On_Button_Press widget: " & Get_Name (Signal_Widget)));

      Get_Event_Widget
        (Signal_Widget, Get_Window (Event),
         Gint (Get_X (Event)), Gint (Get_Y (Event)), X, Y, Widget);

      if Widget = null then
         return False;
      end if;

      if Check_Ignore_Event (Widget, Event) then
         return False;
      end if;

      pragma Debug (Message ("...Checking which button pressed"));

      --  We only want single button press events.

      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         --  ??? if Glade_Palette.Is_Selector_On (Glade_Palette) then
         if True then
            pragma Debug (Message ("...Selecting widget"));

            --  Handled := Select_Widget (Widget, Get_State (Event), X, Y);

            --  if Handled then
            --     Gtk.Handlers.Emit_Stop_By_Name
            --       (Signal_Widget, "button_press_event");
            --  end if;

            return Handled;

         elsif Widget.all in Gtk_Fixed_Record'Class
           or else Widget.all in Gtk_Layout_Record'Class
         then
            --  Fixed/Layout but not GnomeCanvas ???
            --  ??? Add_Widget_To_Fixed (Widget, X, Y);
            Gtk.Handlers.Emit_Stop_By_Name
              (Signal_Widget, "button_press_event");
            return True;

         elsif Is_Placeholder (Widget) then
            --  ??? Placeholder_Replace (Widget);
            Gtk.Handlers.Emit_Stop_By_Name
              (Signal_Widget, "button_press_event");
            return True;

         elsif Widget.all in Gtk_Packer_Record'Class then
            --  ??? Add_Widget_To_Container (Widget);
            Gtk.Handlers.Emit_Stop_By_Name
              (Signal_Widget, "button_press_event");
            return True;
         end if;

      elsif Get_Button (Event) = 3 then
         --  ??? GB_Widget.Show_Popup_Menu (Widget, Event);
         Gtk.Handlers.Emit_Stop_By_Name (Signal_Widget, "button_press_event");
         return True;
      end if;

      return False;
   end On_Button_Press;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean is
   begin
      pragma Debug (Message ("In On_Button_Release"));

      --  ??? if Dragging_Widget /= null then
      --     Drag_Action := GB_Drag_None;
      --     pragma Debug (Message ("  removing grab"));
      --     Grab_Remove (Dragging_Widget);
      --     Dragging_Widget := null;
      --  end if;

      return False;
   end On_Button_Release;

   ---------------------
   -- On_Enter_Notify --
   ---------------------

   function On_Enter_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing) return Boolean is
   begin
      --  ??? if Dragging_Widget /= null then
      --     Gtk.Handlers.Emit_Stop_By_Name (Widget, "enter_notify_event");
      --  end if;

      return False;
   end On_Enter_Notify;

   ------------------------
   -- On_Key_Press_Event --
   ------------------------

   function On_Key_Press_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean is
   begin
      pragma Debug (Message ("In On_Key_Press_Event"));
      return False;
   end On_Key_Press_Event;

   --------------------------
   -- On_Key_Release_Event --
   --------------------------

   function On_Key_Release_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean is
   begin
      pragma Debug (Message ("In On_Key_Release_Event"));
      return False;
   end On_Key_Release_Event;

   ---------------------
   -- On_Leave_Notify --
   ---------------------

   function On_Leave_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing) return Boolean is
   begin
      --  ??? if Dragging_Widget /= null then
      --     Gtk.Handlers.Emit_Stop_By_Name (Widget, "leave_notify_event");
      --  end if;

      return False;
   end On_Leave_Notify;

   ----------------------
   -- On_Motion_Notify --
   ----------------------

   function On_Motion_Notify
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Motion) return Boolean is
   begin
      --  ??? if (Get_State (Event) and Button1_Mask) /= 0 then
      --     return Do_Drag_Action (Widget, Event);
      --  else
      --     return Set_Cursor (Widget, Event);
      --  end if;

      return False;
   end On_Motion_Notify;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
     (Widget     : access Gtk_Widget_Record'Class;
      Allocation : Gtk_Allocation_Access)
   is
      --  Widget_Data : ??? := Get_Data (Widget, GB_Widget_Data_Key);
   begin
      --  Reset the flag, since the size is allocated now. Note that wdata may
      --  be null as widget may be a placeholder.

      --  if Widget_Data /= null then
      --     Widget_Data.Flags :=
      --       Widget_Data.Flags and (not GB_Size_Not_Allocated);
      --  end if;

      pragma Debug
        (Message ("In On_Size_Allocate: " & Get_Name (Widget) &
           ". x:" & Allocation.X'Img &
           " y:" & Allocation.Y'Img &
           " width:" & Allocation.Width'Img &
           " height:" & Allocation.Height'Img));

      --  if Property_Get_Widget = Widget then
      --     GB_Widget.Show_Position_Properties (Widget);
      --  end if;
      null;
   end On_Size_Allocate;

   -----------------------
   -- On_Widget_Realize --
   -----------------------

   Placeholder_Pixmap : Gdk.Gdk_Pixmap := null;
   --  Pixmap used to set the background for placeholder widgets
   --  ??? Global variable

   procedure On_Widget_Realize (Widget : access Gtk_Widget_Record'Class) is
      Mask : Gdk.Gdk_Bitmap := null;
   begin
      pragma Debug
        (Message ("In On_Widget_Realize widget: " & Get_Name (Widget)));

      if Is_Placeholder (Widget) then
         --  Create placeholder pixmap if it hasn't already been created.
         --  There may be a problem with multi-depth displays.

         if Placeholder_Pixmap = null then
            Create_From_Xpm_D
              (Placeholder_Pixmap,
               Get_Window (Widget), Mask, Null_Color, placeholder_xpm);

            if Placeholder_Pixmap = null then
               --  Use a color instead ???
               null;
            end if;
         end if;

         if Placeholder_Pixmap /= null then
            Set_Back_Pixmap (Get_Window (Widget), Placeholder_Pixmap, False);
         end if;
      end if;
   end On_Widget_Realize;

   ---------------------
   -- Paint_Selection --
   ---------------------

   procedure Paint_Selection
     (Window : Gdk.Gdk_Window; GC : Gdk.Gdk_GC; X, Y, Width, Height : Gint) is
   begin
      pragma Debug (Message ("In Paint_Selection"));

      --  Paint the four corner handles, if there is enough room.

      if Width > Corner_Width and then Height > Corner_Height then
         Draw_Rectangle (Window, GC, True, X, Y, Corner_Width, Corner_Height);
         Draw_Rectangle (Window, GC, True, X, Y + Height - Corner_Height,
                         Corner_Width, Corner_Height);
         Draw_Rectangle (Window, GC, True, X + Width - Corner_Width, Y,
                         Corner_Width, Corner_Height);
         Draw_Rectangle (Window, GC, True, X + Width - Corner_Width,
                         Y + Height - Corner_Height,
                         Corner_Width, Corner_Height);
      end if;

      --  Paint the box around the widget.

      Draw_Rectangle (Window, GC, False, X, Y, Width - 1, Height - 1);
   end Paint_Selection;

   ----------------------
   -- Paint_Selections --
   ----------------------

   procedure Paint_Selections (Widget : access Gtk_Widget_Record'Class) is
      Ancestor   : Gtk_Widget;
      Window     : Gdk.Gdk_Window;
      GC         : Gdk.Gdk_GC;
      X, Y, W, H : Gint;

      use Widget_List;

   begin
      pragma Debug (Message ("In Paint_Selections: " & Get_Name (Widget)));

      GC := Get_Black_GC (Get_Style (Widget));
      Set_Subwindow (GC, Include_Inferiors);
      Ancestor := Widget.all'Access;

      while Ancestor /= null loop
         --  if Find (Selected_Widgets, Ancestor) /= Null_List then
         if Global_Selection then  --  ???? temporary testing code
            Window := Get_Widget_Window (Get_Parent (Ancestor), Ancestor);

            if Get_Parent (Ancestor) /= null then
               X := Get_Allocation_X (Ancestor);
               Y := Get_Allocation_Y (Ancestor);
               W := Gint (Get_Allocation_Width (Ancestor));
               H := Gint (Get_Allocation_Height (Ancestor));

            else
               X := 0;
               Y := 0;
               Get_Size (Window, W, H);
            end if;

            Paint_Selection (Window, GC, X, Y, W, H);
         end if;

         --  SPECIAL CODE: Don't try to paint selection boxes around
         --  ancestors of a viewport, as the coordinates are completely
         --  different.

         exit when Ancestor /= Widget.all'Access
           and then Ancestor.all in Gtk_Viewport_Record'Class;

         Ancestor := Get_Parent (Ancestor);
      end loop;

      --  Reset gc - maybe we should remember the current setting

      Set_Subwindow (GC, Clip_By_Children);
   end Paint_Selections;

   ------------------
   -- Paint_Widget --
   ------------------

   procedure Paint_Widget (Widget : access Gtk_Widget_Record'Class) is
      Allocation_Width  : constant Guint := Get_Allocation_Width (Widget);
      Allocation_Height : constant Guint := Get_Allocation_Height (Widget);
      Light_GC          : Gdk.Gdk_GC;
      Dark_GC           : Gdk.Gdk_GC;
      W, H              : Gint;
      Window            : constant Gdk.Gdk_Window := Get_Window (Widget);

   begin
      pragma Debug
        (Message ("Painting widget: " & Get_Name (Widget) &
                  " W:" & Allocation_Width'Img &
                  " H:" & Allocation_Height'Img));

      --  Check widget is drawable in case it has been deleted.

      if not Drawable_Is_Set (Widget) then
         return;
      end if;

      --  Don't try to draw anything if the width or height of the widget is 0.

      if Allocation_Width = 0 or else Allocation_Height = 0 then
         return;
      end if;

      --  If widget is a placeholder, draw the placeholder pixmap in it and a
      --  3D border around it.

      if Is_Placeholder (Widget) then
         Light_GC := Get_Light_GC (Get_Style (Widget), State_Normal);
         Dark_GC := Get_Dark_GC (Get_Style (Widget), State_Normal);
         Get_Size (Window, W, H);

         --  This should not be needed. ??? Except maybe under NT
         --  Clear (Window);

         Draw_Line (Window, Light_GC, 0, 0, W - 1, 0);
         Draw_Line (Window, Light_GC, 0, 0, 0, H - 1);
         Draw_Line (Window, Dark_GC, 0, H - 1, W - 1, H - 1);
         Draw_Line (Window, Dark_GC, W - 1, 0, W - 1, H - 1);
      end if;

      --  Draw grid for fixed containers

      if Widget.all in Gtk_Fixed_Record'Class
        or else Widget.all in Gtk_Layout_Record'Class
      then
         Draw_Grid (Widget);
      end if;

      Paint_Selections (Widget);
   end Paint_Widget;

end RAD.Editor;
