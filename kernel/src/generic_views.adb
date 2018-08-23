------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;

with Glib.Object;               use Glib, Glib.Object;
with XML_Utils;                 use XML_Utils;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Window;                use Gdk.Window;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Toggle_Tool_Button;    use Gtk.Toggle_Tool_Button;
with Gtk.Tool_Item;             use Gtk.Tool_Item;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Entry_Completion;   use Gtkada.Entry_Completion;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Ada.Tags;                  use Ada.Tags;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Stock_Icons;           use GPS.Stock_Icons;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;

with Config;                    use Config;

package body Generic_Views is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.GENERIC_VIEWS");
   No_Transient_Views : constant Trace_Handle :=
     Create ("GPS.INTERNAL.VIEWS_NO_TRANSIENT_VIEWS", Default => Off);

   function Has_Right_Expander
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
      return Gint;
   --  Return the index of the item or separator that right aligns items, or -1
   --  if there is none.

   procedure On_Destroy_View (View : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure On_Menu_Deactivate (View : access GObject_Record'Class);
   --  Called when the config menu is popped down, to restore the state of
   --  the config button (unpressed)

   procedure On_Filter_Changed (View : access Gtk_Widget_Record'Class);
   --  Callback for filter-canged signal

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Kernel
     (View   : not null access View_Record'Class;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      View.Kernel := Kernel_Handle (Kernel);
   end Set_Kernel;

   -----------------
   -- Set_Toolbar --
   -----------------

   procedure Set_Toolbar
     (View    : not null access View_Record'Class;
      Toolbar : access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      View.Toolbar := Gtk_Toolbar (Toolbar);
   end Set_Toolbar;

   -----------------
   -- Get_Toolbar --
   -----------------

   function Get_Toolbar
     (View    : not null access View_Record'Class)
      return Gtk.Toolbar.Gtk_Toolbar is
   begin
      return View.Toolbar;
   end Get_Toolbar;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (View : not null access View_Record'Class)
      return Filter_Panels.Filter_Panel is
   begin
      return View.Filter;
   end Get_Filter;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Self : not null access View_Record;
      Text : String)
   is
      use Filter_Panels;
   begin
      if Self.Filter /= null then
         Self.Filter.Set_Filter (Text);
      end if;
   end Set_Filter;

   ------------------
   -- Build_Search --
   ------------------

   procedure Build_Search
     (Self    : not null access View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      P       : not null access GPS.Kernel.Search.Kernel_Search_Provider'Class;
      Name                : Histories.History_Key;
      Case_Sensitive      : Boolean := False;
      Placeholder         : String := "search") is
   begin
      --  If the view already contains a search panel, don't create another
      --  one.
      if Self.Search /= null then
         return;
      end if;

      --  Create the tool item
      Self.Search := new Search_Panel_Record;
      Gtk.Tool_Item.Initialize (Self.Search);

      --  Create and initialize the completion entry. The provider
      --  is freed by the completion entry (see the
      --  Gtkada.Entry_Completion.Initialize doc).
      Gtkada.Entry_Completion.Gtk_New
        (Self                => Self.Search.Completion_Entry,
         Kernel              => Self.Kernel,
         Completion          => P,
         Name                => Name,
         Case_Sensitive      => Case_Sensitive,
         Completion_In_Popup => True,
         Placeholder         => Placeholder);
      Self.Search.Add (Self.Search.Completion_Entry);

      --  Ensure that Get_Can_Focus returns True, even if the
      --  completion entry has not been realized and/or mapped yet.
      --  This is safe because we know that Gtk_Entry widgets can
      --  receive the focus.
      Self.Search.Completion_Entry.Set_Can_Focus (True);

      --  Append it to the toolbar
      Self.Append_Toolbar (Toolbar     => Toolbar,
                           Item        => Self.Search,
                           Right_Align => True);
   end Build_Search;

   ------------------------------
   -- Override_Search_Provider --
   ------------------------------

   procedure Override_Search_Provider
     (Self : not null access View_Record;
      P    : not null access GPS.Kernel.Search.Kernel_Search_Provider'Class) is
   begin
      Self.Search.Completion_Entry.Set_Completion (P);
      Self.Search.Is_Provider_Overriden := True;
   end Override_Search_Provider;

   ---------------------------
   -- Reset_Search_Provider --
   ---------------------------

   procedure Reset_Search_Provider (Self : not null access View_Record) is
   begin
      Self.Search.Completion_Entry.Reset_Completion;
      Self.Search.Is_Provider_Overriden := False;
   end Reset_Search_Provider;

   -----------------------------------
   -- Is_Search_Provider_Overridden --
   -----------------------------------

   function Is_Search_Provider_Overridden
     (Self : not null access View_Record) return Boolean
   is
     (Self.Search.Is_Provider_Overriden);

   ------------------
   -- Build_Filter --
   ------------------

   procedure Build_Filter
     (Self        : not null access View_Record;
      Toolbar     : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Hist_Prefix : History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Panels.Filter_Options_Mask := 0;
      Name        : String := "")
   is
      use Filter_Panels;
   begin
      if Self.Filter /= null then
         return;
      end if;

      --  The filter will be automatically deallocated by gtk+, because it is
      --  put in the toolbar of the view, which is destroyed when the view is
      --  destroyed.

      Gtk_New
        (Self.Filter,
         Self.Kernel,
         Hist_Prefix,
         Tooltip,
         Placeholder,
         Options,
         Name);

      Widget_Callback.Object_Connect
        (Self.Filter,
         Filter_Panels.Signal_Filter_Changed,
         Widget_Callback.To_Marshaller (On_Filter_Changed'Access),
         Self);

      Self.Append_Toolbar (Toolbar, Self.Filter, Right_Align => True);
   end Build_Filter;

   ------------
   -- Kernel --
   ------------

   function Kernel
     (Self : not null access View_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Self.Kernel;
   end Kernel;

   ------------------------
   -- Has_Right_Expander --
   ------------------------

   function Has_Right_Expander
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
      return Gint
   is
      Count : constant Gint := Toolbar.Get_N_Items;
      Item  : Gtk_Tool_Item;
   begin
      for J in reverse 0 .. Count - 1 loop
         Item := Toolbar.Get_Nth_Item (J);
         if Item.Get_Expand then
            return J;
         end if;
      end loop;
      return -1;
   end Has_Right_Expander;

   --------------------
   -- Append_Toolbar --
   --------------------

   procedure Append_Toolbar
     (Self        : not null access View_Record;
      Toolbar     : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Item        : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
      Right_Align : Boolean := False;
      Homogeneous : Boolean := True)
   is
      pragma Unreferenced (Self);
      Sep : Gtk_Separator_Tool_Item;
      Loc : Gint;
   begin
      if Right_Align then
         if not Item.Get_Expand
            and then Has_Right_Expander (Toolbar) = -1
         then
            Gtk_New (Sep);
            Sep.Set_Draw (False);
            Sep.Set_Expand (True);
            Toolbar.Insert (Sep);
         end if;

         Toolbar.Insert (Item);

      else
         Loc := Has_Right_Expander (Toolbar);
         if Loc /= -1 then
            --  Insert before the item, to left align
            Toolbar.Insert (Item, Pos => Loc - 1);
         else
            Toolbar.Insert (Item, Pos => -1);
         end if;
      end if;

      Item.Set_Homogeneous (Homogeneous);
   end Append_Toolbar;

   ---------------------
   -- On_Destroy_View --
   ---------------------

   procedure On_Destroy_View (View : access Gtk_Widget_Record'Class) is
      V : constant Abstract_View_Access := Abstract_View_Access (View);
   begin
      if V.Config_Menu /= null then
         V.Config_Menu.Destroy;
         V.Config_Menu := null;
      end if;

      V.On_Destroy;
   end On_Destroy_View;

   -----------------------
   -- On_Filter_Changed --
   -----------------------

   procedure On_Filter_Changed (View : access Gtk_Widget_Record'Class)
   is
      V : constant Abstract_View_Access    := Abstract_View_Access (View);
      P : GPS.Search.Search_Pattern_Access := V.Filter.Get_Filter_Pattern;
   begin
      V.Filter_Changed (P);
   end On_Filter_Changed;

   ------------------------
   -- On_Menu_Deactivate --
   ------------------------

   procedure On_Menu_Deactivate (View : access GObject_Record'Class) is
      V  : constant Abstract_View_Access := Abstract_View_Access (View);
   begin
      if not V.In_Destruction then
         V.Config.Set_Active (False);
         V.Config_Menu := null;
      end if;
   end On_Menu_Deactivate;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is
      Module : Module_ID;

      Window_X_Hist_Key  : constant History_Key :=
        "window_x_" & History_Key (View_Name);
      Window_Y_Hist_Key  : constant History_Key :=
        "window_y_" & History_Key (View_Name);

      type Local_Formal_MDI_Child_Access
         is access all Local_Formal_MDI_Child'Class;

      type Open_Command is new Interactive_Command with null record;
      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type;
      --  Open the view

      type Toplevel_Box is new Gtk_Box_Record with record
         Initial : View_Access;
      end record;
      --  When using a local toolbar, the contents of the widget as set by the
      --  application is nested inside a box. We use a dedicated tagged type so
      --  that we can more easily find the child in the desktop by tag.

      procedure Create_If_Needed
        (Kernel          : access Kernel_Handle_Record'Class;
         Child           : out GPS_MDI_Child;
         View            : out View_Access;
         Toolbar_Id      : String := View_Name;
         Init            : access procedure
           (View : not null access Formal_View_Record'Class) := null;
         Is_Load_Desktop : Boolean := False);
      --  Create or reuse a view.

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access;
         Visible_Only : Boolean := False);
      --  Find any existing view.
      --  If Visible_Only is True, this function returns the view only if it's
      --  visible in the current perspective. Otherwise, the view will be
      --  returned and automatically put back in the current perspective.

      procedure Store_Position (View : View_Access);
      --  Store in history the position of the view's dialog

      procedure Get_Stored_Position
        (View           : View_Access;
         Position_Found : out Boolean;
         X, Y           : out Gint);
      --  Retrieve from history the position of the view's dialog.
      --  Position_Found is set to True iff the position was set, and, in this
      --  case, (X, Y) is set to the position.

      -------------------------
      -- Get_Stored_Position --
      -------------------------

      procedure Get_Stored_Position
        (View           : View_Access;
         Position_Found : out Boolean;
         X, Y           : out Gint)
      is
         Win     : Gtk_Widget := View.Get_Toplevel;
         Hist_X  : constant String_List_Access := Get_History
           (Get_History (View.Kernel).all, Window_X_Hist_Key);
         Hist_Y  : constant String_List_Access := Get_History
           (Get_History (View.Kernel).all, Window_Y_Hist_Key);

         Screen : Gdk_Screen;
         Monitor : Gint;
         Rect    : Gdk_Rectangle;
      begin
         Position_Found := False;

         if Hist_X = null or else Hist_Y = null then
            return;
         end if;

         --  Ensure the window is at least partially visible on the current
         --  screen.
         --  Screen.Get_{Width,Height} returns the total size for all monitors
         --    for instance 5760x1200
         --  So we need to look at the specific monitor that the window is on.

         if Win = null
           or else Win.all not in Gtk_Window_Record'Class
         then
            Win := Gtk_Widget (View.Kernel.Get_Main_Window);
            if Win = null then
               return;
            end if;
         end if;

         X := Gint'Value (Hist_X (Hist_X'First).all);
         Y := Gint'Value (Hist_Y (Hist_Y'First).all);

         Screen := Gtk_Window (Win).Get_Screen;

         Monitor := Screen.Get_Monitor_At_Point (X, Y);
         Screen.Get_Monitor_Geometry (Monitor, Rect);

         X := Gint'Min (Gint'Max (X, Rect.X), Rect.X + Rect.Width - 10);
         Y := Gint'Min (Gint'Max (Y, Rect.Y), Rect.Y + Rect.Height - 10);

         Position_Found := True;
      end Get_Stored_Position;

      --------------------
      -- Store_Position --
      --------------------

      procedure Store_Position (View : View_Access) is
         Child   : constant MDI_Child := Child_From_View (View);
         Win  : Gtk_Widget;
         X, Y : Gint;
      begin
         if Child = null then
            return;
         end if;

         --  Store the position of the floating window

         Win := View.Get_Toplevel;
         Get_Root_Origin (Get_Window (Win), X, Y);

         Add_To_History
           (Get_History (View.Kernel).all, Window_X_Hist_Key,
            Gint'Image (X));
         Add_To_History
           (Get_History (View.Kernel).all, Window_Y_Hist_Key,
            Gint'Image (Y));
      end Store_Position;

      -----------------------------
      -- On_Display_Local_Config --
      -----------------------------

      function On_Display_Local_Config
        (View  : access GObject_Record'Class;
         Event : Gdk_Event_Button) return Boolean
      is
         V     : constant Abstract_View_Access := Abstract_View_Access (View);
         Child : MDI_Child;
         Time_Before_Factory : Time;
      begin
         if Event.Button /= 1 then
            return False;
         end if;

         if Host = Windows then
            Time_Before_Factory := Clock;
         end if;

         if V.Config_Menu /= null then
            V.Config_Menu.Destroy;
         end if;

         Gtk_New (V.Config_Menu);
         V.Create_Menu (V.Config_Menu);

         Child := Child_From_View (View_Access (V));
         if Child /= null and then Child.Is_Floating then
            if Has_Children (V.Config_Menu) then
               V.Config_Menu.Add (Gtk_Separator_Menu_Item_New);
            end if;
            Append_Menu (V.Kernel, V.Config_Menu,
                         Label => "Unfloat",
                         Action => "unfloat view");
         end if;

         V.Config_Menu.Attach_To_Widget (V.Config, Detacher => null);
         V.Config_Menu.Show_All;

         --  See comments in GUI_Utils.Button_Press_For_Contextual_Menu

         if Host = Windows then
            Popup (V.Config_Menu,
                   Button        => 1,
                   Activate_Time => Event.Time
                   + Guint32 ((Clock - Time_Before_Factory) * 1000));
         else
            Popup (V.Config_Menu,
                   Button        => 1,
                   Activate_Time => Event.Time);
         end if;

         V.Config_Menu.On_Deactivate (On_Menu_Deactivate'Access, V);

         V.Config.Set_Active (True);
         return True;
      end On_Display_Local_Config;

      -----------
      -- Close --
      -----------

      procedure Close
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      is
         View : constant View_Access := Retrieve_View (Kernel);

      begin
         if View /= null then
            Close_Child (Child_From_View (View));
         end if;
      end Close;

      ---------------------
      -- Child_From_View --
      ---------------------

      function Child_From_View
        (View : not null access Formal_View_Record'Class)
         return access Local_Formal_MDI_Child'Class
      is
      begin
         if Local_Config or else Local_Toolbar then
            return Access_Local_Formal_MDI_Child (Find_MDI_Child
              (Get_MDI (View.Kernel),
               View.Get_Parent));  --  the box
         else
            return Access_Local_Formal_MDI_Child
              (Find_MDI_Child (Get_MDI (View.Kernel), View));
         end if;
      end Child_From_View;

      -----------------------
      -- Get_Actual_Widget --
      -----------------------

      overriding function Get_Actual_Widget
        (Self : not null access Local_Formal_MDI_Child) return Gtk_Widget
      is
         W : constant Gtk_Widget := Get_Widget (Self);
      begin
         if Local_Toolbar or else Local_Config then
            return Gtk_Widget (Toplevel_Box (W.all).Initial);
         else
            return W;
         end if;
      end Get_Actual_Widget;

      ----------
      -- Find --
      ----------

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access;
         Visible_Only : Boolean := False)
      is
         T   : Ada.Tags.Tag;
         MDI : MDI_Window;
      begin
         if Local_Toolbar or else Local_Config then
            T := Toplevel_Box'Tag;
         else
            T := Formal_View_Record'Tag;
         end if;

         MDI := Get_MDI (Kernel);
         View := null;

         if MDI /= null then
            Child := GPS_MDI_Child
              (MDI.Find_MDI_Child_By_Tag (T, Visible_Only => Visible_Only));
            if Child /= null then
               View := View_From_Child (Child);
            end if;
         end if;
      end Find;

      ------------------------------
      -- On_Delete_Floating_Child --
      ------------------------------

      function On_Delete_Floating_Child
        (Self : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
      is
         View : constant View_Access := View_Access (Self);
      begin
         Store_Position (View);

         return False;
      end On_Delete_Floating_Child;

      -----------------------------
      -- On_Close_Floating_Child --
      -----------------------------

      procedure On_Close_Floating_Child
        (Self : access Gtk_Widget_Record'Class)
      is
         View : constant View_Access := View_Access (Self);
      begin
         Store_Position (View);

         --  We have just stored the position of the view: call Close_Child
         --  to let the MDI close the view.
         Child_From_View (View).Close_Child;

         --  Give the focus back to the main Window, since this is not always
         --  done by the window manager (e.g. under Windows)

         Gdk.Window.Gdk_Raise (View.Kernel.Get_Main_Window.Get_Window);
      end On_Close_Floating_Child;

      --------------------
      -- On_Float_Child --
      --------------------

      procedure On_Float_Child (Child : access Gtk_Widget_Record'Class) is
         Self   : constant Local_Formal_MDI_Child_Access :=
           Local_Formal_MDI_Child_Access (Child);
         View   : constant View_Access := View_From_Child (Self);
         V      : constant Abstract_View_Access := Abstract_View_Access (View);
         Req    : Gtk_Requisition;
      begin
         --  Show the 'Close' button if present
         if V.Close_Button /= null then
            V.Close_Button.Show_All;
         end if;

         Return_Callback.Object_Connect
           (View.Get_Toplevel, Gtk.Widget.Signal_Delete_Event,
            On_Delete_Floating_Child_Access, View);

         --  Set the size of the floating window
         View.Set_Size_Request (-1, -1);
         Size_Request (View, Req);
         View.Set_Size_Request (Req.Width, Req.Height);
      end On_Float_Child;

      -----------------------------
      -- On_Before_Unfloat_Child --
      -----------------------------

      procedure On_Before_Unfloat_Child
        (Child : access Gtk_Widget_Record'Class)
      is
         Self   : constant Local_Formal_MDI_Child_Access :=
           Local_Formal_MDI_Child_Access (Child);
         View   : constant View_Access := View_From_Child (Self);
         V      : constant Abstract_View_Access := Abstract_View_Access (View);
      begin
         --  Hide the 'Close' button if present
         if V.Close_Button /= null then
            V.Close_Button.Hide;
         end if;

         --  Store the position of the floating window
         Store_Position (View);
         View.Set_Size_Request (-1, -1);
      end On_Before_Unfloat_Child;

      ----------------------
      -- Create_If_Needed --
      ----------------------

      procedure Create_If_Needed
        (Kernel          : access Kernel_Handle_Record'Class;
         Child           : out GPS_MDI_Child;
         View            : out View_Access;
         Toolbar_Id      : String := View_Name;
         Init            : access procedure
           (View : not null access Formal_View_Record'Class) := null;
         Is_Load_Desktop : Boolean := False)
      is
         use Filter_Panels;

         Focus_Widget     : Gtk_Widget;
         Finalized_View   : Gtk_Widget;
         Abstract_View    : Abstract_View_Access;
         Button_Box_Frame : Gtk_Frame;
      begin
         if Reuse_If_Exist then
            Find (Kernel, Child, View);
            if View /= null then
               if Init /= null then
                  Init (View);
               end if;
               return;
            end if;
         end if;

         View := new Formal_View_Record;
         Set_Kernel (View, Kernel_Handle (Kernel));
         Focus_Widget := Initialize (View);

         --  Create the finalized view, creating its local toolbar if needed
         Finalized_View := Create_Finalized_View
           (View, Toolbar_Id => Toolbar_Id);

         Abstract_View := Abstract_View_Access (View);

         --  A simple check that the widget can indeed get the keyboard focus.
         --  If it can't, this might result in surprising behavior: for
         --  instance, clicking on a MDI tab will send a "child_selected"
         --  event with no module information set, and the current focus
         --  widget will not have changed, thus the menus are not correctly
         --  refreshed.

         if Focus_Widget = null then
            --  If no focus widget has been returned when calling Initialize,
            --  give the focus to the search/filter bar, if any.
            --  If the view does not have any search/filter bar, give it to the
            --  view itself.
            if Abstract_View.Search /= null then
               Focus_Widget :=
                 Gtk_Widget (Abstract_View.Search.Completion_Entry);
            elsif Abstract_View.Filter /= null then
               Focus_Widget := Abstract_View.Filter.Get_Focus_Widget;
            end if;
         end if;

         Assert
           (Me,
            Focus_Widget = null or else Focus_Widget.Get_Can_Focus,
            "Focus_Widget cannot in fact receive keyboard focus",
            Raise_Exception => False);

         --  Child does not exist yet, create it
         Child := new Local_Formal_MDI_Child;
         Child.Set_Toolbar (View.Get_Toolbar);
         Initialize (Child, Finalized_View,
                     Kernel         => Kernel,
                     Default_Width  => Default_Width,
                     Default_Height => Default_Height,
                     Focus_Widget   => Focus_Widget,
                     Flags          =>
                       (if Active (No_Transient_Views) then
                         MDI_Flags
                           and not (Float_As_Transient or Float_To_Main)
                        else
                           MDI_Flags),
                     Module         => Module,
                     Group          => Group,
                     Areas          => Areas);
         Set_Title (Child, View_Name, View_Name);

         --  Create the button box area at the bottom

         Gtk_New (Button_Box_Frame);
         View.Pack_End (Button_Box_Frame, Expand => False);
         Get_Style_Context (Button_Box_Frame).Add_Class
           ("dialog-action-box");

         Gtk_New
           (Abstract_View.Button_Box,
            Orientation_Horizontal);
         Abstract_View.Button_Box.Set_Layout (Buttonbox_End);
         Button_Box_Frame.Add (Abstract_View.Button_Box);

         --  Let the view create its own buttons if needed
         View.Create_Buttons_Area (Abstract_View.Button_Box);

         if Add_Close_Button_On_Float then
            Widget_Callback.Connect
              (Child, Signal_Float_Child, On_Float_Child_Access);
            Widget_Callback.Connect
              (Child, Signal_Before_Unfloat_Child,
               On_Before_Unfloat_Child_Access);

            Gtk_New (Abstract_View.Close_Button, -"Close");
            Widget_Callback.Object_Connect
              (Abstract_View.Close_Button, Gtk.Button.Signal_Clicked,
               On_Close_Floating_Child_Access, View);
            Abstract_View.Button_Box.Add (Abstract_View.Close_Button);
         end if;

         Abstract_View.Button_Box.Show_All;

         --  Put the child in the MDI

         declare
            Found : Boolean := False;
            X, Y  : Gint := 0;
         begin
            if Position = Position_Float then
               --  If the child was floating, attempt to find the previous
               --  position in history.
               Get_Stored_Position (View, Found, X, Y);
            end if;

            Put (Get_MDI (Kernel), Child,
                 Initial_Position => Position,
                 Position_At_Mouse => not Found,
                 X => X, Y => Y);
         end;

         if Init /= null then
            Init (View);
         end if;

         if not Is_Load_Desktop then
            Save_Backup_Desktop (Kernel);
         end if;

         View.On_Create (Child);
      end Create_If_Needed;

      ----------------
      -- Get_Module --
      ----------------

      function Get_Module return GPS.Kernel.Modules.Module_ID is
      begin
         return Module;
      end Get_Module;

      ------------------
      -- Load_Desktop --
      ------------------

      function Load_Desktop
        (MDI  : MDI_Window;
         Node : Node_Ptr;
         User : Kernel_Handle) return MDI_Child
      is
         pragma Unreferenced (MDI);
         View         : View_Access;
         Child        : GPS_MDI_Child;
      begin
         if Node.Tag.all = Module_Name then
            Create_If_Needed (User, Child, View, Is_Load_Desktop => True);
            Load_From_XML (View, Node);
            return MDI_Child (Child);
         end if;
         return null;
      end Load_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      overriding function Save_Desktop
        (Self : not null access Local_Formal_MDI_Child) return Node_Ptr
      is
         Widget : constant Gtk_Widget := Get_Widget (Self);
         N : Node_Ptr;
         Tb : constant Boolean := Local_Toolbar or else Local_Config;
      begin
         --  Test inherited first, in case the user has provided his own. This
         --  is also needed when a module is written in python, since
         --  python_module.adb in that case uses its own mdi_child to call the
         --  python function

         N := Formal_MDI_Child (Self.all).Save_Desktop;
         if N /= null then
            return N;
         end if;

         if Tb and then Widget.all in Toplevel_Box'Class then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (Toplevel_Box (Widget.all).Initial, N);
            return N;

         elsif not Tb
           and then Widget.all in Formal_View_Record'Class
         then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (View_Access (Widget), N);
            return N;
         end if;
         return null;
      end Save_Desktop;

      -------------
      -- Execute --
      -------------

      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         Ignore : View_Access;
         pragma Unreferenced (Self, Ignore);
      begin
         Ignore := Get_Or_Create_View (Get_Kernel (Context.Context));
         return Commands.Success;
      end Execute;

      -------------------
      -- Retrieve_View --
      -------------------

      function Retrieve_View
        (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
         Visible_Only : Boolean := False)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
      begin
         Find
           (Kernel,
            Child        => Child,
            View         => View,
            Visible_Only => Visible_Only);

         return View;
      end Retrieve_View;

      -------------------
      -- Reset_Toolbar --
      -------------------

      procedure Reset_Toolbar
        (View       : not null access Formal_View_Record'Class;
         Toolbar_Id : String := View_Name)
      is
         Toolbar : Gtk_Toolbar := View.Get_Toolbar;
         Config  : Gtk_Toggle_Tool_Button;
      begin
         if Toolbar /= null then
            Trace (Me, "Create toolbar, from id=" & Toolbar_Id);

            Create_Toolbar (View.Kernel, Toolbar, Id => Toolbar_Id);
            Toolbar.Set_Icon_Size (Icon_Size_Local_Toolbar);
            Get_Style_Context (Toolbar).Add_Class ("gps-local-toolbar");

            View.Create_Toolbar (Toolbar);
            View.Set_Toolbar (Toolbar);

            --  If View needs a local config menu, create it
            if Local_Config then
               Gtk_New (Config);
               View_Record (View.all).Config := Config;
               Config.Set_Icon_Name ("gps-config-menu-symbolic");
               Config.Set_Name ("local-config");
               Config.Set_Tooltip_Text (-"Configuration panel");
               View.Append_Toolbar (Toolbar, Config, Right_Align => True);
               Config.Get_Child.On_Button_Press_Event
                 (On_Display_Local_Config_Access, View);
            end if;

            Toolbar.Show_All;

            --  Force a refresh to recompute visibility of the toolbar buttons
            View.Kernel.Context_Changed (View.Kernel.Get_Current_Context);
         end if;
      end Reset_Toolbar;

      ---------------------------
      -- Create_Finalized_View --
      ---------------------------

      function Create_Finalized_View
        (View       : not null access Formal_View_Record'Class;
         Toolbar_Id : String := View_Name) return Gtk_Widget
      is
         Box            : Gtk_Box;
         Toolbar        : Gtk_Toolbar;
      begin
         --  If no local toolbar is needed, either to contain a custom toolbar
         --  or for a local config menu, return View.
         if not Local_Toolbar and then not Local_Config then
            return Gtk_Widget (View);
         end if;

         --  If View needs a local toolbar, create it
         Box := new Toplevel_Box;
         Initialize_Vbox (Box);
         Toplevel_Box (Box.all).Initial := View_Access (View);

         Gtk_New (Toolbar);
         Box.Pack_Start (Toolbar, Expand => False, Fill => False);
         Box.Pack_Start (View, Expand => True, Fill => True);

         View.Set_Toolbar (Toolbar);
         Reset_Toolbar (View, Toolbar_Id);

         View.On_Destroy (On_Destroy_View'Access);

         return Gtk_Widget (Box);
      end Create_Finalized_View;

      ------------------------
      -- Get_Or_Create_View --
      ------------------------

      function Get_Or_Create_View
        (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
         Focus      : Boolean := True;
         Toolbar_Id : String := View_Name;
         Init       : access procedure
            (View : not null access Formal_View_Record'Class) := null)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         Existed_Before : Boolean := False;
         View         : View_Access;
      begin
         if Active (No_Transient_Views) then
            Find (Kernel, Child, View);
            if Child /= null then
               Existed_Before := True;
            end if;
         end if;

         Create_If_Needed
            (Kernel, Child, View, Toolbar_Id => Toolbar_Id, Init => Init);

         if Focus then
            Raise_Child (Child);
            Set_Focus_Child (Child);

            if Active (No_Transient_Views) then
               --  If this mode is active, this means we are on an old X11
               --  implementation, where the Present() called by Raise_Child
               --  above might not work. Force the presentation here.
               if Child.Is_Floating
                 and then Existed_Before
               then
                  declare
                     Window : constant Gtk_Window :=
                       Gtk_Window (View.Get_Toplevel);
                     X, Y : Gint;
                  begin
                     Window.Get_Position (X, Y);
                     --  This is of course a hack, but the only way (I found)
                     --  to convince Xming to raise a window to the front.
                     Window.Hide;
                     Window.Show_All;
                     Window.Move (X, Y);
                  end;
               end if;
            end if;

            --  ??? browsers used to do the following:
            --  Add_Navigation_Location (Kernel, -"Call graph Browser");
         end if;

         if Child = null then
            return null;
         else
            return View;
         end if;
      end Get_Or_Create_View;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
         ID          : GPS.Kernel.Modules.Module_ID := null) is
      begin
         if ID = null then
            Module := new Module_ID_Record;
         else
            Module := ID;
         end if;

         if Commands_Category /= "" then
            Register_Action
              (Kernel, "open " & View_Name,
               new Open_Command, "Open (or reuse if it already exists) the '"
               & View_Name & "' view", null, Commands_Category);
         end if;

         Register_Module
           (Module      => Module,
            Kernel      => Kernel,
            Module_Name => Module_Name,
            Priority    => GPS.Kernel.Modules.Default_Priority);
         Register_Desktop_Functions (null, Load_Desktop_Access);
      end Register_Module;
   end Simple_Views;

end Generic_Views;
