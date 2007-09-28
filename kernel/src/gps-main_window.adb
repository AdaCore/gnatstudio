-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007, AdaCore              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Scripts.Gtkada;       use GNAT.Scripts.Gtkada;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with Gdk.Dnd;                   use Gdk.Dnd;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Generic_Properties;   use Glib.Generic_Properties;
with Glib.Object;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Values;               use Glib.Values;

with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Object;                use Gtk.Object;
with Gtk.Progress_Bar;          use Gtk.Progress_Bar;
with Gtk.Rc;                    use Gtk.Rc;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;

with Pango.Font;                use Pango.Font;

with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with GUI_Utils;
with Projects;                  use Projects;
with Remote;                    use Remote;
with Traces;                    use Traces;
with User_Interface_Tools;

package body GPS.Main_Window is

   Me : constant Debug_Handle := Create ("GPS.Main_Window");

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String ("preferences_changed"));
   Class_Record : GObject_Class := Uninitialized_Class;

   Force_Cst      : aliased constant String := "force";
   Msg_Cst        : aliased constant String := "msg";
   Param1_Cst     : aliased constant String := "param1";
   Exit_Status_Cst : aliased constant String := "status";
   Exit_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Force_Cst'Access,
      2 => Exit_Status_Cst'Access);
   Save_Windows_Parameters : constant Cst_Argument_List :=
     (1 => Force_Cst'Access);
   Dialog_Cmd_Parameters   : constant Cst_Argument_List :=
     (1 => Msg_Cst'Access);
   Input_Dialog_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Msg_Cst'Access,
      2 => Param1_Cst'Access);

   Vertically_Cst : aliased constant String := "vertically";
   Name_Cst       : aliased constant String := "name";
   Child_Cst      : aliased constant String := "child";
   Float_Cst      : aliased constant String := "float";
   Reuse_Cst      : aliased constant String := "reuse";
   Visible_Only_Cst : aliased constant String := "visible_only";
   Short_Cst      : aliased constant String := "short";
   Get_Cmd_Parameters : constant Cst_Argument_List := (1 => Name_Cst'Access);
   Get_By_Child_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Child_Cst'Access);
   Float_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Float_Cst'Access);
   Split_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Vertically_Cst'Access, 2 => Reuse_Cst'Access);
   Next_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Visible_Only_Cst'Access);
   Name_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Short_Cst'Access);
   Rename_Cmd_Parameter : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Short_Cst'Access);

   type Tabs_Position_Preference is (Bottom, Top, Left, Right);
   for Tabs_Position_Preference'Size use Glib.Gint'Size;
   pragma Convention (C, Tabs_Position_Preference);
   package Tabs_Position_Properties is new Generic_Enumeration_Property
     ("Tabs_Position", Tabs_Position_Preference);

   type Tabs_Policy_Enum is (Never, Automatic, Always);
   for Tabs_Policy_Enum'Size use Glib.Gint'Size;
   pragma Convention (C, Tabs_Policy_Enum);
   package Show_Tabs_Policy_Properties is new Generic_Enumeration_Property
     ("Tabs_Policy", Tabs_Policy_Enum);

   type Toolbar_Icons_Size is (Hide_Toolbar, Small_Icons, Large_Icons);
   for Toolbar_Icons_Size'Size use Glib.Gint'Size;
   pragma Convention (C, Toolbar_Icons_Size);
   package Toolbar_Icons_Size_Properties is new Generic_Enumeration_Property
     ("Toobar_Icons", Toolbar_Icons_Size);

   Pref_Tabs_Policy     : Param_Spec_Enum;
   Pref_Tabs_Position   : Param_Spec_Enum;
   Pref_Toolbar_Style   : Param_Spec_Enum;
   Pref_Show_Statusbar  : Param_Spec_Boolean;

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the delete event.

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class);
   --  Called when the the main window is destroyed

   type MDI_Child_Selection_Command is new Interactive_Command with record
      Kernel       : Kernel_Handle;
      Move_To_Next : Boolean;
      Group        : Child_Group;
   end record;
   type MDI_Child_Selection_Command_Access is access all
     MDI_Child_Selection_Command'Class;
   function Execute
     (Command : access MDI_Child_Selection_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Check whether Event should activate the selection dialog for MDI
   --  children.

   type Window_Mode is
     (Split_H, Split_V, Clone, Reorder_Tab_Left, Reorder_Tab_Right,
      Move_To_Next_Tab, Move_To_Previous_Tab);
   type MDI_Window_Actions_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Mode   : Window_Mode;
   end record;
   type MDI_Window_Actions_Command_Access is access all
     MDI_Window_Actions_Command'Class;
   function Execute
     (Command : access MDI_Window_Actions_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Act on the layout of windows

   procedure Put_Animation (Main_Window : access GPS_Window_Record'Class);
   --  Add the animated icon in the main window.

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project is changed.

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handles shell commands defined in this package

   procedure Default_Window_Command_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for MDIWindow class

   type User_Interface is new User_Interface_Tools.User_Interface with record
      Main_Window : Gtk.Window.Gtk_Window;
   end record;
   --  Generic User Interface object

   function Query_User
     (UI            : User_Interface;
      Prompt        : String;
      Password_Mode : Boolean) return String;
   --  See inherited for documentation.

   ----------------
   -- Query_User --
   ----------------

   function Query_User
     (UI            : User_Interface;
      Prompt        : String;
      Password_Mode : Boolean) return String is
   begin
      return GUI_Utils.Query_User (UI.Main_Window, Prompt, Password_Mode);
   end Query_User;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access MDI_Window_Actions_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Child : MDI_Child;
      Iter  : Child_Iterator;
      Note  : Gtk_Notebook;
      Pos   : Gint;
   begin
      case Command.Mode is
         when Split_H =>
            Split (Get_MDI (Command.Kernel), Orientation_Horizontal,
                   After => True);
         when Split_V =>
            Split (Get_MDI (Command.Kernel), Orientation_Vertical,
                   After => True);
         when Clone =>
            declare
               Focus : constant MDI_Child :=
                 Get_Focus_Child (Get_MDI (Command.Kernel));
               N : MDI_Child;
               pragma Unreferenced (N);
            begin
               if Focus /= null then
                  N  := Dnd_Data (Focus, Copy => True);
               end if;
            end;
         when Reorder_Tab_Left =>
            Iter := First_Child (Get_MDI (Command.Kernel));
            Child := Get (Iter);
            if Child /= null then
               Note := Get_Notebook (Iter);
               Reorder_Child (Note, Child, Page_Num (Note, Child) - 1);
            end if;

         when Reorder_Tab_Right =>
            Iter := First_Child (Get_MDI (Command.Kernel));
            Child := Get (Iter);
            if Child /= null then
               Note := Get_Notebook (Iter);
               Pos := Page_Num (Note, Child) + 1;
               if Pos >= Get_N_Pages (Note) then
                  Pos := 0;
               end if;
               Reorder_Child (Note, Child, Pos);
            end if;

         when Move_To_Next_Tab =>
            Iter := First_Child (Get_MDI (Command.Kernel));
            Child := Get (Iter);
            if Child /= null then
               Next_Page (Get_Notebook (Iter));
            end if;

         when Move_To_Previous_Tab =>
            Iter := First_Child (Get_MDI (Command.Kernel));
            Child := Get (Iter);
            if Child /= null then
               Prev_Page (Get_Notebook (Iter));
            end if;

      end case;

      return Success;
   end Execute;

   -------------
   -- Anim_Cb --
   -------------

   function Anim_Cb (Kernel : Kernel_Handle) return Boolean is
      Window : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
   begin
      if Window.Animation_Iter = null then
         return False;

      elsif Advance (Window.Animation_Iter) then
         if Window.Animation_Image /= null then
            Set (Window.Animation_Image, Get_Pixbuf (Window.Animation_Iter));
         end if;
      end if;

      return True;
   end Anim_Cb;

   ---------------------------
   -- Display_Default_Image --
   ---------------------------

   procedure Display_Default_Image (Kernel : GPS.Kernel.Kernel_Handle) is
      Window : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
   begin
      if Window.Static_Image /= null then
         Set (Window.Animation_Image, Window.Static_Image);
      end if;
   end Display_Default_Image;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window      : out GPS_Window;
      Home_Dir         : String;
      Prefix_Directory : String) is
   begin
      Main_Window := new GPS_Window_Record;
      GPS.Main_Window.Initialize (Main_Window, Home_Dir, Prefix_Directory);
   end Gtk_New;

   ----------------------
   -- Confirm_And_Quit --
   ----------------------

   procedure Quit
     (Main_Window : access GPS_Window_Record'Class;
      Force       : Boolean := False;
      Status      : Integer := 0) is
   begin
      if Force or else Save_MDI_Children (Main_Window.Kernel) then
         Exit_GPS (Main_Window.Kernel, Status => Status);
      end if;
   end Quit;

   ---------------------
   -- Delete_Callback --
   ---------------------

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      Quit (GPS_Window (Widget));

      return True;
   end Delete_Callback;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Reset_Title (GPS_Window (Get_Main_Window (Kernel)));
   end On_Project_Changed;

   -------------------
   -- Put_Animation --
   -------------------

   procedure Put_Animation (Main_Window : access GPS_Window_Record'Class) is
      Throbber : constant String := Normalize_Pathname
        ("gps-animation.gif",
         Get_System_Dir (Main_Window.Kernel) & "/share/gps/");
      Image    : constant String := Normalize_Pathname
        ("gps-animation.png",
         Get_System_Dir (Main_Window.Kernel) & "/share/gps/");
      Error    : GError;
      Pixbuf   : Gdk_Pixbuf;

   begin
      if Is_Regular_File (Image) then
         Trace (Me, "loading gps-animation.png");
         Gdk_New_From_File (Main_Window.Static_Image, Image, Error);
         Gtk_New (Main_Window.Animation_Image, Main_Window.Static_Image);
         Add (Main_Window.Animation_Frame, Main_Window.Animation_Image);
      else
         Trace (Me, "gps-animation.png not found");
         return;
      end if;

      if Is_Regular_File (Throbber) then
         Trace (Me, "loading gps-animation.gif");
         Gdk_New_From_File (Main_Window.Animation, Throbber, Error);
         Main_Window.Animation_Iter := Get_Iter (Main_Window.Animation);
         Pixbuf := Get_Pixbuf (Main_Window.Animation_Iter);
         Set (Main_Window.Animation_Image, Pixbuf);
      else
         Trace (Me, "gps-animation.gif not found");
      end if;

      Show_All (Main_Window.Animation_Image);
   end Put_Animation;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Win    : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Pos    : Gtk_Position_Type;
      Policy : Show_Tabs_Policy_Enum;
   begin
      Gtk.Rc.Parse_String
        ("gtk-font-name=""" &
         To_String (Get_Pref (Default_Font)) &
         '"' & ASCII.LF);

      Gtk.Rc.Parse_String
        ("style ""gtk-default-tooltips-style""  {" & ASCII.LF
         & "  bg[NORMAL] = """
         & Get_Pref (Tooltip_Color) & """" & ASCII.LF
         & "}");

      case Toolbar_Icons_Size'Val (Get_Pref (Pref_Toolbar_Style)) is
         when Hide_Toolbar =>
            Set_Child_Visible (Win.Toolbar_Box, False);
            Hide_All (Win.Toolbar_Box);

         when Small_Icons  =>
            Set_Size_Request (Win.Toolbar_Box, -1, -1);
            Set_Child_Visible (Win.Toolbar_Box, True);
            Show_All (Win.Toolbar_Box);
            Set_Icon_Size (Win.Toolbar, Icon_Size_Small_Toolbar);

         when Large_Icons  =>
            Set_Size_Request (Win.Toolbar_Box, -1, -1);
            Set_Child_Visible (Win.Toolbar_Box, True);
            Show_All (Win.Toolbar_Box);
            Set_Icon_Size (Win.Toolbar, Icon_Size_Large_Toolbar);
      end case;

      if Get_Pref (Toolbar_Show_Text) then
         Set_Style (Get_Toolbar (Kernel), Toolbar_Both);
      else
         Set_Style (Get_Toolbar (Kernel), Toolbar_Icons);
      end if;

      case Tabs_Position_Preference'Val (Get_Pref (Pref_Tabs_Position)) is
         when Bottom => Pos := Pos_Bottom;
         when Right  => Pos := Pos_Right;
         when Top    => Pos := Pos_Top;
         when Left   => Pos := Pos_Left;
      end case;

      case Tabs_Policy_Enum'Val (Get_Pref (Pref_Tabs_Policy)) is
         when Automatic => Policy := Show_Tabs_Policy_Enum'(Automatic);
         when Never     => Policy := Show_Tabs_Policy_Enum'(Never);
         when Always    => Policy := Show_Tabs_Policy_Enum'(Always);
      end case;

      if Get_Pref (Pref_Show_Statusbar) then
         Show_All (Win.Statusbar);
         Set_Child_Visible (Win.Statusbar, True);
         Set_Size_Request (Win.Statusbar, 0, -1);
      else
         Hide_All (Win.Statusbar);
         Set_Child_Visible (Win.Statusbar, False);
         Set_Size_Request (Win.Statusbar, 0, 0);
      end if;

      Configure
        (Get_MDI (Kernel),
         Opaque_Resize     => Get_Pref (MDI_Opaque),
         Close_Floating_Is_Unfloat =>
           not Get_Pref (MDI_Destroy_Floats),
         Title_Font        => Get_Pref (Default_Font),
         Background_Color  => Get_Pref (MDI_Background_Color),
         Title_Bar_Color   => Get_Pref (MDI_Title_Bar_Color),
         Focus_Title_Color => Get_Pref (MDI_Focus_Title_Color),
         Draw_Title_Bars   => Get_Pref (Pref_Draw_Title_Bars),
         Show_Tabs_Policy  => Policy,
         Tabs_Position     => Pos);

      Set_All_Floating_Mode
        (Get_MDI (Kernel), Get_Pref (MDI_All_Floating));

      Use_Short_Titles_For_Floats
        (Get_MDI (Kernel), Get_Pref (MDI_Float_Short_Title));
   end Preferences_Changed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window      : access GPS_Window_Record'Class;
      Home_Dir         : String;
      Prefix_Directory : String)
   is
      Vbox      : Gtk_Vbox;
      Box1      : Gtk_Hbox;
      Progress  : Gtk.Progress_Bar.Gtk_Progress_Bar;
      Menu      : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;

   begin
      --  Initialize the window first, so that it can be used while creating
      --  the kernel, in particular calls to Push_State
      Gtk.Window.Initialize (Main_Window, Window_Toplevel);
      Initialize_Class_Record
        (Main_Window, Signals, Class_Record, Type_Name => "GpsMainWindow");

      Gtk_New
        (Main_Window.Kernel,
         Gtk_Window (Main_Window),
         Home_Dir, Prefix_Directory);

      Pref_Draw_Title_Bars := Param_Spec_Boolean
        (Gnew_Boolean
           (Name  => "Window-Draw-Title-Bars",
            Nick  => -"Show title bars",
            Blurb => -("Whether the windows should have their own title bars."
                       & " If this is disabled, then the notebooks tabs will"
                       & " be used to show the current window"),
            Default => True));
      Register_Property
        (Main_Window.Kernel, Param_Spec (Pref_Draw_Title_Bars), -"Windows");

      Pref_Tabs_Policy := Param_Spec_Enum
        (Show_Tabs_Policy_Properties.Gnew_Enum
           (Name  => "Window-Tabs-Policy",
            Nick  => -"Notebook tabs policy",
            Blurb => -"When the notebook tabs should be displayed",
            Default => Automatic));
      Register_Property
        (Main_Window.Kernel, Param_Spec (Pref_Tabs_Policy), -"Windows");

      Pref_Tabs_Position := Param_Spec_Enum
        (Tabs_Position_Properties.Gnew_Enum
           (Name  => "Window-Tabs-Position",
            Nick  => -"Notebook tabs position",
            Blurb => -("Where the tabs should be displayed relative to the"
                       & " notebooks"),
            Default => Bottom));
      Register_Property
        (Main_Window.Kernel, Param_Spec (Pref_Tabs_Position), -"Windows");

      Pref_Toolbar_Style := Param_Spec_Enum
        (Toolbar_Icons_Size_Properties.Gnew_Enum
           (Name    => "General-Toolbar-Style",
            Nick    => -"Tool bar style",
            Blurb   => -("Indicates how the tool bar should be displayed"),
            Default => Large_Icons));
      Register_Property
        (Main_Window.Kernel, Param_Spec (Pref_Toolbar_Style), -"General");

      Pref_Show_Statusbar := Param_Spec_Boolean
        (Gnew_Boolean
           (Name  => "Window-Show-Status-Bar",
            Nick  => -"Show status bar",
            Blurb => -("Whether the area at the bottom of the GPS window"
                       & " should be displayed. This area contains the"
                       & " progress bars while actions are taking place. The"
                       & " same information is available from the Task"
                       & " Manager"),
            Default => True));
      Register_Property
        (Main_Window.Kernel, Param_Spec (Pref_Show_Statusbar), -"General");

      Set_Policy (Main_Window, False, True, False);
      Set_Position (Main_Window, Win_Pos_None);
      Set_Modal (Main_Window, False);
      Set_Default_Size (Main_Window, 800, 700);

      Gtk_New (Main_Window.Main_Accel_Group);
      Add_Accel_Group (Main_Window, Main_Window.Main_Accel_Group);
      Gtk_New (Main_Window.MDI, Main_Window.Main_Accel_Group);

      Gtk_New_Vbox (Vbox, False, 0);
      Add (Main_Window, Vbox);

      Gtk_New_Hbox
        (Main_Window.Statusbar, Homogeneous => False, Spacing => 4);
      Set_Size_Request (Main_Window.Statusbar, 0, -1);

      --  Avoid resizing the main window whenever a label is changed.
      Set_Resize_Mode (Main_Window.Statusbar, Resize_Queue);

      Gtk_New (Progress);
      Set_Text (Progress, " ");
      --  ??? This is a tweak : it seems that the gtk progress bar doesn't
      --  have a size that is the same when it has text than when it does not,
      --  but we do want to insert and remove text from this bar, without
      --  the annoying change in size, so we make sure there is always some
      --  text displayed.

      Pack_Start (Main_Window.Statusbar, Progress, False, False, 0);

      --  ??? We set the default width to 0 so that the progress bar appears
      --  only as a vertical separator.
      --  This should be removed when another way to keep the size of the
      --  status bar acceptable is found.
      Set_Size_Request (Progress, 0, -1);
      Pack_End (Vbox, Main_Window.Statusbar, False, False, 0);

      Gtk_New_Hbox (Main_Window.Menu_Box, False, 0);
      Pack_Start (Vbox, Main_Window.Menu_Box, False, False);

      Gtk_New (Main_Window.Menu_Bar);
      Pack_Start (Main_Window.Menu_Box, Main_Window.Menu_Bar);

      Gtk_New_With_Mnemonic (Menu_Item, -"_File");
      Append (Main_Window.Menu_Bar, Menu_Item);
      Gtk_New (Menu);
      Set_Accel_Group (Menu, Main_Window.Main_Accel_Group);
      Set_Submenu (Menu_Item, Menu);

      Gtk_New_With_Mnemonic (Menu_Item, -"_Window");
      Append (Main_Window.Menu_Bar, Menu_Item);
      Set_Submenu (Menu_Item, Create_Menu
                   (Main_Window.MDI, Accel_Path_Prefix => "<gps>"));

      Setup_Toplevel_Window (Main_Window.MDI, Main_Window);

      Gtk_New_Vbox (Main_Window.Toolbar_Box, False, 0);
      Pack_Start (Vbox, Main_Window.Toolbar_Box, False, False, 0);

      Gtk_New_Hbox (Box1);
      Pack_Start (Main_Window.Toolbar_Box, Box1);
      Gtk_New (Main_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Main_Window.Toolbar, True);
      Pack_Start (Box1, Main_Window.Toolbar, True, True);

      Gtk_New (Main_Window.Animation_Frame);
      Set_Shadow_Type (Main_Window.Animation_Frame, Shadow_None);
      Pack_End
        (Main_Window.Menu_Box, Main_Window.Animation_Frame, False, False);
      Put_Animation (Main_Window);

      Add (Vbox, Main_Window.MDI);

      Widget_Callback.Connect (Main_Window, Signal_Destroy, On_Destroy'Access);

      Add_Hook (Main_Window.Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "main_window.preferences_changed");
      Preferences_Changed (Main_Window.Kernel);

      --  Make sure we don't display the toolbar until we have actually loaded
      --  the preferences and checked whether the user wants it or not. This is
      --  to avoid flickering
      Set_Size_Request (Main_Window.Toolbar_Box, -1, 0);
      Set_Child_Visible (Main_Window.Toolbar_Box, False);
      Hide_All (Main_Window.Toolbar_Box);

      Add_Hook (Main_Window.Kernel, Project_Changed_Hook,
                Wrapper (On_Project_Changed'Access),
                Name => "main_window.projet_changed");

      Return_Callback.Object_Connect
        (Main_Window, Gtk.Widget.Signal_Delete_Event,
         Delete_Callback'Access,
         Gtk_Widget (Main_Window),
         After => False);

      --  Support for Win32 WM_DROPFILES drag'n'drop

      Gtk.Dnd.Dest_Set
        (Main_Window, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Main_Window, Signal_Drag_Data_Received,
         Drag_Data_Received'Access, Kernel_Handle (Main_Window.Kernel));

      --  Set the generic user interface
      User_Interface_Tools.Set_User_Interface
        (new User_Interface'(Main_Window => Gtk_Window (Main_Window)));
   end Initialize;

   -------------------
   -- Register_Keys --
   -------------------

   procedure Register_Keys (Main_Window : access GPS_Window_Record'Class) is
      MDI_Class        : constant Class_Type := New_Class
        (Main_Window.Kernel, "MDI");
      MDI_Window_Class : constant Class_Type := New_Class
        (Main_Window.Kernel, "MDIWindow", Get_GUI_Class (Main_Window.Kernel));
      Command          : MDI_Child_Selection_Command_Access;
      Command2         : MDI_Window_Actions_Command_Access;
   begin
      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Move_To_Next := True;
      Command.Group        := Group_Any;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to next window",
         Command     => Command,
         Category    => "MDI",
         Description =>
           -("Select the next window in GPS. Any key binding should use a"
             & " modifier such as control for best usage of this function."));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to next window",
         Default_Key => "alt-Tab");

      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Move_To_Next := False;
      Command.Group        := Group_Any;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to previous window",
         Command     => Command,
         Category    => "MDI",
         Description =>
           -("Select the previous window in GPS. Any key binding should use a"
             & " modifier such as control for best usage of this function."));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to previous window",
         Default_Key => "alt-shift-ISO_Left_Tab");

      Command              := new MDI_Child_Selection_Command;
      Command.Kernel       := Main_Window.Kernel;
      Command.Group        := Group_Default;
      Command.Move_To_Next := True;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Select other window",
         Command     => Command,
         Category    => "MDI",
         Description =>
         -("Select the next splitted window in the central area of GPS."));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Split_H;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split horizontally",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two horizontally"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Split_V;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split vertically",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two vertically"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Clone;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Clone window",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Create a duplicate of the current window if possible. Not all"
           & " windows support this operation."));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Reorder_Tab_Left;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move tab to left",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Move the current notebook tab one position to the left, within"
           & " the notebook (cyclic)"));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move tab to left",
         Default_Key => "shift-control-alt-Left");

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Reorder_Tab_Right;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move tab to right",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Move the current notebook tab one position to the right, within"
           & " the notebook (cyclic)"));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move tab to right",
         Default_Key => "shift-control-alt-Right");

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Move_To_Next_Tab;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to next tab",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Move to the next tab in the current notebook"));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to next tab",
         Default_Key => "control-alt-Right");

      Command2        := new MDI_Window_Actions_Command;
      Command2.Kernel := Main_Window.Kernel;
      Command2.Mode   := Move_To_Previous_Tab;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to previous tab",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Move to the previous tab in the current notebook"));
      Bind_Default_Key
        (Kernel      => Main_Window.Kernel,
         Action      => "Move to previous tab",
         Default_Key => "control-alt-Left");

      Register_Command
        (Main_Window.Kernel, "dialog",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "yes_no_dialog",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "input_dialog",
         Minimum_Args => 2,
         Maximum_Args => 100,
         Class         => MDI_Class,
         Static_Method => True,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "save_all",
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "exit",
         Minimum_Args => 0,
         Maximum_Args => Exit_Cmd_Parameters'Length,
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Main_Window.Kernel, Constructor_Method,
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "split",
         Class         => MDI_Window_Class,
         Maximum_Args  => 2,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "float",
         Maximum_Args  => 1,
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "is_floating",
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "raise_window",
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "get_child",
         Class          => MDI_Window_Class,
         Handler        => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "next",
         Class          => MDI_Window_Class,
         Maximum_Args   => 1,
         Handler        => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "name",
         Maximum_Args   => 1,
         Class          => MDI_Window_Class,
         Handler        => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "rename",
         Minimum_Args   => 1,
         Maximum_Args   => 2,
         Class          => MDI_Window_Class,
         Handler        => Default_Window_Command_Handler'Access);

      Register_Command
        (Main_Window.Kernel, "get",
         Class         => MDI_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "children",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "get_by_child",
         Class         => MDI_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "current",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "hide",
         Class         => MDI_Class,
         Static_Method => True,
         HanDler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "show",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
   end Register_Keys;

   ------------------------------------
   -- Default_Window_Command_Handler --
   ------------------------------------

   procedure Default_Window_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use Glib.Object;
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      MDI_Window_Class : constant Class_Type :=
        New_Class (Kernel, "MDIWindow");
      Inst   : constant Class_Instance := Nth_Arg (Data, 1, MDI_Window_Class);
      Child  : constant MDI_Child := MDI_Child (GObject'(Get_Data (Inst)));
      Widget : Gtk_Widget;
      Result : Class_Instance;
   begin
      if Child = null then
         Set_Error_Msg (Data, "MDIWindow no longer exists");

      elsif Command = Constructor_Method then
         Set_Error_Msg (Data, "Cannot build instances of MDIWindow");

      elsif Command = "split" then
         Name_Parameters (Data, Split_Cmd_Parameters);
         if Get_State (Child) = Normal then
            Set_Focus_Child (Child);
            if Nth_Arg (Data, 2, True) then
               Split (Get_MDI (Kernel),
                      Orientation       => Orientation_Vertical,
                      Reuse_If_Possible => Nth_Arg (Data, 3, False),
                      After             => False);
            else
               Split (Get_MDI (Kernel),
                      Orientation       => Orientation_Horizontal,
                      Reuse_If_Possible => Nth_Arg (Data, 3, False),
                      After             => False);
            end if;
         end if;

      elsif Command = "float" then
         Name_Parameters (Data, Float_Cmd_Parameters);
         Float_Child (Child, Nth_Arg (Data, 2, True));

      elsif Command = "is_floating" then
         Set_Return_Value (Data, Is_Floating (Child));

      elsif Command = "raise_window" then
         Raise_Child (Child, Give_Focus => True);

      elsif Command = "name" then
         Name_Parameters (Data, Name_Cmd_Parameters);
         if Nth_Arg (Data, 2, False) then
            Set_Return_Value (Data, Get_Short_Title (Child));
         else
            Set_Return_Value (Data, Get_Title (Child));
         end if;

      elsif Command = "rename" then
         Name_Parameters (Data, Rename_Cmd_Parameter);
         Set_Title (Child, Nth_Arg (Data, 2), Nth_Arg (Data, 3, ""));

      elsif Command = "next" then
         Name_Parameters (Data, Next_Cmd_Parameters);
         declare
            Child2 : MDI_Child;
            Iter   : Child_Iterator := First_Child (Get_MDI (Kernel));
            Return_Next : Boolean := False;
            Visible_Only : constant Boolean := Nth_Arg (Data, 2, True);
         begin
            loop
               Child2 := Get (Iter);

               if Child2 = null then
                  Iter := First_Child (Get_MDI (Kernel));
                  Return_Next := True;
                  Child2 := Get (Iter);
               end if;

               exit when Return_Next
                 and then (not Visible_Only or else Is_Raised (Child2));

               if Child2 = Child then
                  exit when Return_Next;  --  We already traversed all
                  Return_Next := True;
               end if;

               Next (Iter);
            end loop;

            Result := New_Instance (Get_Script (Data), MDI_Window_Class);
            Set_Data (Result, GObject (Child2));
            Set_Return_Value (Data, Result);
         end;

      elsif Command = "get_child" then
         Widget := Get_Widget (Child);
         Result := Get_Instance (Get_Script (Data), Widget);
         if Result /= No_Class_Instance then
            Set_Return_Value (Data, Result);
         else
            Result := New_Instance (Get_Script (Data), Get_GUI_Class (Kernel));
            Set_Data (Result, GObject (Widget));
            Set_Return_Value (Data, Result);
         end if;
      end if;
   end Default_Window_Command_Handler;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use Glib.Object;
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      MDI_Window_Class : Class_Type;
      Child  : MDI_Child;
      Inst   : Class_Instance;
   begin
      if Command = "exit" then
         Name_Parameters (Data, Exit_Cmd_Parameters);
         Quit (GPS_Window (Get_Main_Window (Kernel)),
               Force => Nth_Arg (Data, 1, False),
               Status => Nth_Arg (Data, 2, 0));
      elsif Command = "save_all" then
         Name_Parameters (Data, Save_Windows_Parameters);

         if not Save_MDI_Children
           (Kernel, No_Children, Nth_Arg (Data, 1, False))
         then
            Set_Error_Msg (Data, -"Cancelled by user");
         end if;

      elsif Command = "get"
        or else Command = "get_by_child"
        or else Command = "current"
      then
         if Command = "get" then
            Name_Parameters (Data, Get_Cmd_Parameters);
            Child := Find_MDI_Child_By_Name
              (Get_MDI (Kernel), Nth_Arg (Data, 1));
         elsif Command = "get_by_child" then
            Name_Parameters (Data, Get_By_Child_Cmd_Parameters);
            Child := Find_MDI_Child
              (Get_MDI (Kernel),
               Widget => Gtk_Widget
                 (GObject'
                    (Get_Data (Nth_Arg (Data, 1, Get_GUI_Class (Kernel))))));
         else
            Child := Get_Focus_Child (Get_MDI (Kernel));
         end if;

         if Child = null then
            Set_Return_Value (Data, No_Class_Instance);
         else
            Inst := Get_Instance (Get_Script (Data), Child);
            if Inst = No_Class_Instance then
               MDI_Window_Class := New_Class (Kernel, "MDIWindow");
               Inst := New_Instance (Get_Script (Data), MDI_Window_Class);
               Set_Data (Inst, GObject (Child));
            end if;
            Set_Return_Value (Data, Inst);
         end if;

      elsif Command = "children" then
         declare
            Iter : Child_Iterator := First_Child (Get_MDI (Kernel));
            Class : constant Class_Type := New_Class (Kernel, "MDIWindow");
         begin
            Set_Return_Value_As_List (Data);
            while Get (Iter) /= null loop
               Child := Get (Iter);
               Inst := Get_Instance (Get_Script (Data), Child);
               if Inst = No_Class_Instance then
                  Inst := New_Instance (Get_Script (Data), Class);
                  Set_Data (Inst, GObject (Child));
               end if;
               Set_Return_Value (Data, Inst);
               Next (Iter);
            end loop;
         end;

      elsif Command = "dialog" then
         Name_Parameters (Data, Dialog_Cmd_Parameters);

         declare
            Result : Message_Dialog_Buttons;
            pragma Unreferenced (Result);
         begin
            Result := Message_Dialog
              (Msg     => Nth_Arg (Data, 1),
               Buttons => Button_OK,
               Justification => Justify_Left,
               Parent  => Get_Current_Window (Kernel));
         end;

      elsif Command = "yes_no_dialog" then
         Name_Parameters (Data, Dialog_Cmd_Parameters);
         Set_Return_Value
           (Data, Message_Dialog
            (Msg           => Nth_Arg (Data, 1),
             Buttons       => Button_Yes + Button_No,
             Justification => Justify_Left,
             Dialog_Type   => Confirmation,
             Parent        => Get_Current_Window (Kernel)) = Button_Yes);

      elsif Command = "input_dialog" then
         declare
            Dialog : Gtk_Dialog;
            Label  : Gtk_Label;
            Group  : Gtk_Size_Group;
            Button : Gtk_Widget;

            type Ent_Array
               is array (2 .. Number_Of_Arguments (Data)) of Gtk_Entry;
            Ent : Ent_Array;

            procedure Create_Entry (N : Natural);
            --  Create the Nth entry. N must be in Ent_Array'Range.

            ------------------
            -- Create_Entry --
            ------------------

            procedure Create_Entry (N : Natural) is
               Arg   : constant String := Nth_Arg (Data, N);
               Index : Natural := Arg'First;
               Hbox  : Gtk_Hbox;
            begin
               Gtk_New_Hbox (Hbox, Homogeneous => False);
               Pack_Start (Get_Vbox (Dialog), Hbox, Padding => 3);

               while Index <= Arg'Last loop
                  exit when Arg (Index) = '=';

                  Index := Index + 1;
               end loop;

               if Arg'First <= Index - 1 then
                  Gtk_New (Label, Arg (Arg'First .. Index - 1) & ':');
                  Set_Alignment (Label, 0.0, 0.5);
                  Add_Widget (Group, Label);
                  Pack_Start (Hbox, Label, Expand => False, Padding => 3);
               end if;

               Gtk_New (Ent (N));
               Set_Text (Ent (N), Arg (Index + 1 .. Arg'Last));

               Set_Activates_Default (Ent (N),  True);
               Pack_Start (Hbox, Ent (N), Padding => 10);
            end Create_Entry;

         begin
            Name_Parameters (Data, Input_Dialog_Cmd_Parameters);

            Gtk_New (Label);
            Set_Markup (Label, Nth_Arg (Data, 1));

            Gtk_New
              (Dialog,
               Title  => Get_Text (Label),
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal);

            Set_Alignment (Label, 0.0, 0.5);
            Pack_Start
              (Get_Vbox (Dialog), Label, Expand => True, Padding => 10);

            Gtk_New (Group);

            for Num in Ent'Range loop
               Create_Entry (Num);
            end loop;

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            Set_Return_Value_As_List (Data);

            if Run (Dialog) = Gtk_Response_OK then
               for Num in Ent'Range loop
                  Set_Return_Value (Data, Get_Text (Ent (Num)));
               end loop;
            end if;

            Destroy (Dialog);
         end;

      elsif Command = "hide" then
         declare
            Iterator : Child_Iterator := First_Child (Get_MDI (Kernel));
            Child    : MDI_Child;
         begin
            loop
               Child := Get (Iterator);

               exit when Child = null;

               Hide (Child);
               Next (Iterator);
            end loop;

            Hide (Gtk_Widget (Get_Main_Window (Kernel)));
         end;

      elsif Command = "show" then
         declare
            Iterator : Child_Iterator := First_Child (Get_MDI (Kernel));
            Child    : MDI_Child;
         begin
            loop
               Child := Get (Iterator);

               exit when Child = null;

               Show (Child);
               Next (Iterator);
            end loop;

            Show (Gtk_Widget (Get_Main_Window (Kernel)));
         end;
      end if;
   end Default_Command_Handler;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access MDI_Child_Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      if Command.Group /= Group_Any then
         Check_Interactive_Selection_Dialog
           (Get_MDI (Command.Kernel), null,
            Move_To_Next            => Command.Move_To_Next,
            Only_Group              => Command.Group);
      else
         Check_Interactive_Selection_Dialog
           (Get_MDI (Command.Kernel), Context.Event,
            Move_To_Next            => Command.Move_To_Next,
            Only_Group              => Command.Group);
      end if;
      return Success;
   end Execute;

   --------------
   -- GPS_Name --
   --------------

   function GPS_Name (Window : access GPS_Window_Record) return String is
   begin
      if Window.Public_Version then
         return "GPS";
      else
         return "GPS Pro";
      end if;
   end GPS_Name;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class) is
      Win : constant GPS_Window := GPS_Window (Main_Window);
   begin
      if Win.Animation /= null then
         Unref (Win.Animation);
      end if;

      if Win.Animation_Iter /= null then
         Unref (Win.Animation_Iter);
      end if;

      if Win.Static_Image /= null then
         Unref (Win.Static_Image);
      end if;

      if Main_Level > 0 then
         Main_Quit;
      end if;
   end On_Destroy;

   ------------------
   -- Load_Desktop --
   ------------------

   procedure Load_Desktop (Window : access GPS_Window_Record'Class) is
      Was_Loaded : Boolean;
      pragma Unreferenced (Was_Loaded);
   begin
      Was_Loaded := Load_Desktop (Window.Kernel);
   end Load_Desktop;

   -----------------
   -- Reset_Title --
   -----------------

   procedure Reset_Title
     (Window : access GPS_Window_Record;
      Info   : String := "")
   is
      function Info_Str return String;
      --  Returns the info string, if set

      function Remote_Str return String;
      --  Returns the remote string, if set

      --------------
      -- Info_Str --
      --------------

      function Info_Str return String is
      begin
         if Info /= "" then
            return " - " & Info;
         end if;

         return "";
      end Info_Str;

      ----------------
      -- Remote_Str --
      ----------------

      function Remote_Str return String is
      begin
         if not Is_Local (Build_Server) then
            return " on " & Get_Nickname (Build_Server);
         end if;

         return "";
      end Remote_Str;
   begin
      Set_Title
        (Window, GPS_Name (Window) &
         (-" - GNAT Programming Studio (project: ") &
         Project_Name (Get_Project (Window.Kernel)) &
         Remote_Str & ')' & Info_Str);
   end Reset_Title;

end GPS.Main_Window;
