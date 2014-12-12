------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Command_Line;

with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with Cairo;                     use Cairo;
with Gdk.Display;               use Gdk.Display;
with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Screen;                use Gdk.Screen;

with Glib;                      use Glib;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Glib.Properties;
with Glib.Values;               use Glib.Values;

with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Settings;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Style_Provider;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk.Css_Provider;          use Gtk.Css_Provider;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Gtk.Scrolled_Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Style;
with Gtkada.Types;

with Pango.Font;                use Pango.Font;

with Config;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences.Enums; use Default_Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel;                use GPS.Kernel;
with GUI_Utils;
with Remote;                    use Remote;
with Task_Manager;              use Task_Manager;
with User_Interface_Tools;

package body GPS.Main_Window is
   Me : constant Trace_Handle := Create ("MAIN");

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String ("preferences_changed"));
   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Force_Cst      : aliased constant String := "force";
   Msg_Cst        : aliased constant String := "msg";
   Param1_Cst     : aliased constant String := "param1";
   Exit_Status_Cst : aliased constant String := "status";
   File_Filter_Cst : aliased constant String := "file_filter";
   Exit_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Force_Cst'Access,
      2 => Exit_Status_Cst'Access);
   Save_Windows_Parameters : constant Cst_Argument_List :=
     (1 => Force_Cst'Access);
   Dialog_Cmd_Parameters   : constant Cst_Argument_List :=
                               (1 => Msg_Cst'Access);
   File_Selector_Cmd_Parameters : constant Cst_Argument_List :=
                                   (1 => File_Filter_Cst'Access);
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
   New_View_Cst   : aliased constant String := "new_view";
   Get_Cmd_Parameters : constant Cst_Argument_List := (1 => Name_Cst'Access);
   Get_By_Child_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Child_Cst'Access);
   Float_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Float_Cst'Access);
   Split_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Vertically_Cst'Access, 2 => Reuse_Cst'Access,
      3 => New_View_Cst'Access);
   Next_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Visible_Only_Cst'Access);
   Name_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Short_Cst'Access);
   Rename_Cmd_Parameter : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Short_Cst'Access);

   type Toolbar_Icons_Size
      is (Text_Only, Text_And_Icons, Small_Icons, Large_Icons);
   package Toolbar_Icons_Size_Preferences is new
     Default_Preferences.Enums.Generics (Toolbar_Icons_Size);

   Pref_Toolbar_Style  : Toolbar_Icons_Size_Preferences.Preference;

   Theme_Specific_Css_Provider : Gtk_Css_Provider;
   --  Provider for the gps-<theme>.css file

   Tooltips_Background_Provider : Gtk.Css_Provider.Gtk_Css_Provider;
   --  Global variable used to override the background color for tooltips

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the delete event

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences have changed

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class);
   --  Called when the the main window is destroyed

   type MDI_Child_Selection_Command is new Interactive_Command with record
      Move_To_Next : Boolean;
      Group        : Child_Group;
   end record;
   type MDI_Child_Selection_Command_Access is access all
     MDI_Child_Selection_Command'Class;
   overriding function Execute
     (Command : access MDI_Child_Selection_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Check whether Event should activate the selection dialog for MDI
   --  children.

   type Window_Mode is
     (Split_H, Split_V, Clone, Reorder_Tab_Left, Reorder_Tab_Right,
      Move_To_Next_Tab, Move_To_Previous_Tab);
   type MDI_Window_Actions_Command is new Interactive_Command with record
      Mode   : Window_Mode;
   end record;
   type MDI_Window_Actions_Command_Access is access all
     MDI_Window_Actions_Command'Class;
   overriding function Execute
     (Command : access MDI_Window_Actions_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  Act on the layout of windows

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project is changed

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

   function On_Draw_Toolbar_Box
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean;
   --  Drawing the background of the toolbar box, so that the main progress
   --  bar and omni-search have a proper background.

   overriding function Query_User
     (UI            : User_Interface;
      Prompt        : String;
      Password_Mode : Boolean) return String;
   --  See inherited for documentation

   function Prepare_Quit
     (Main_Window : access GPS_Window_Record'Class)
      return Boolean;
   --  Prepare GPS for quitting the main window.
   --  It will save all current windows, and run the hooks.
   --  Returns False if quitting should be prevented at this time.

   ----------------
   -- Query_User --
   ----------------

   overriding function Query_User
     (UI            : User_Interface;
      Prompt        : String;
      Password_Mode : Boolean) return String is
   begin
      return GUI_Utils.Query_User (UI.Main_Window, Prompt, Password_Mode);
   end Query_User;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access MDI_Window_Actions_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Child : MDI_Child;
      Iter  : Child_Iterator;
      Note  : Gtk_Notebook;
      Pos   : Gint;
   begin
      case Command.Mode is
         when Split_H =>
            Split (Get_MDI (Kernel), Orientation_Horizontal,
                   Mode => After);
         when Split_V =>
            Split (Get_MDI (Kernel), Orientation_Vertical,
                   Mode  => After);
         when Clone =>
            declare
               Focus : constant MDI_Child :=
                 Get_Focus_Child (Get_MDI (Kernel));
               N : MDI_Child;
               pragma Unreferenced (N);
            begin
               if Focus /= null then
                  N  := Dnd_Data (Focus, Copy => True);
               end if;
            end;
         when Reorder_Tab_Left =>
            Iter := First_Child (Get_MDI (Kernel));
            Child := Get (Iter);
            if Child /= null then
               Note := Get_Notebook (Iter);
               Reorder_Child (Note, Child, Page_Num (Note, Child) - 1);
            end if;

         when Reorder_Tab_Right =>
            Iter := First_Child (Get_MDI (Kernel));
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
            Iter := First_Child (Get_MDI (Kernel));
            Child := Get (Iter);
            if Child /= null then
               Note := Get_Notebook (Iter);
               if Get_Current_Page (Note) = Get_N_Pages (Note) - 1 then
                  Set_Current_Page (Note, 0);
               else
                  Next_Page (Note);
               end if;
            end if;

         when Move_To_Previous_Tab =>
            Iter := First_Child (Get_MDI (Kernel));
            Child := Get (Iter);
            if Child /= null then
               Note := Get_Notebook (Iter);
               if Get_Current_Page (Note) = 0 then
                  Set_Current_Page (Note, Get_N_Pages (Note) - 1);
               else
                  Prev_Page (Note);
               end if;
            end if;

      end case;

      return Success;
   end Execute;

   ------------------
   -- Prepare_Quit --
   ------------------

   function Prepare_Quit
     (Main_Window : access GPS_Window_Record'Class)
      return Boolean
   is
      Data : aliased Exit_Before_Action_Hooks_Args :=
        (Hooks_Data with null record);
   begin
      if Save_MDI_Children (Main_Window.Kernel)
        and then Run_Hook_Until_Failure
          (Main_Window.Kernel,
           Before_Exit_Action_Hook, Data'Unchecked_Access)
      then
         --  Need to save the desktop here, while the MDI still belongs to a
         --  toplevel window, since otherwise we can't save the size or status
         --  (maximized or not) of the latter. So can't be done in On_Destroy.
         if Save_Desktop_On_Exit.Get_Pref then
            Save_Desktop (Main_Window.Kernel);
         end if;

         --  We are about to tell the main window to destroy itself: set the
         --  destruction flag in the kernel to avoid, for instance, computing
         --  menus (that might have been destroyed) in reaction to context
         --  changes.
         Main_Window.Kernel.Set_Destruction_Flag (True);

         return True;
      else
         return False;
      end if;
   end Prepare_Quit;

   ----------
   -- Quit --
   ----------

   procedure Quit
     (Main_Window : access GPS_Window_Record'Class;
      Force       : Boolean := False;
      Status      : Integer := 0) is
   begin
      --  Calling Prepare_Quit my be calling GPS.exit(), which would call Quit
      --  again. We need to protect against this.

      if not Main_Window.Is_Destroyed then
         Main_Window.Is_Destroyed := True;

         if Force or else Prepare_Quit (Main_Window) then
            Increase_Indent (Me, "Requesting application quit");
            Ada.Command_Line.Set_Exit_Status
              (Ada.Command_Line.Exit_Status (Status));

            --  Destroying the last window will also result in quitting the
            --  application. If we call directly Application.Quit, we are
            --  bypassing the On_Destroy callback
            --  However, the GPS testsuite contains lots of tests that call
            --  GPS.exit() as part of the gps_started callback. If we destroy
            --  the window, it means that the other hooks no longer have access
            --  to the GUI, and that results in a lot of errors (for instance
            --  python scripts no longer have a console).

            --  Destroy (Main_Window);
            Main_Window.Application.Quit;
         end if;
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
      return not Prepare_Quit (GPS_Window (Widget));
   end Delete_Callback;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Reset_Title (GPS_Window (Get_Main_Window (Kernel)));
   end On_Project_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      P     : constant Preference := Get_Pref (Data);
      Dead   : Boolean;
      pragma Unreferenced (Dead);
      Theme : Theme_Descr;
      Err   : aliased GError;
      Theme_Css : Virtual_File;

   begin
      if P = null
        or else P = Preference (Gtk_Theme)
      then
         Theme := Gtk_Theme.Get_Pref;

         if Theme.Directory /= null then
            Trace (Me, "Setting gtk+ theme to '"
                   & Theme.Name.all & "', directory='"
                   & Theme.Directory.all & "'");
            Glib.Properties.Set_Property
              (Gtk.Settings.Get_Default,
               Gtk.Settings.Gtk_Theme_Name_Property,
               Theme.Directory.all);
         end if;

         Glib.Properties.Set_Property
           (Gtk.Settings.Get_Default,
            Gtk.Settings.Gtk_Application_Prefer_Dark_Theme_Property,
            Theme.Dark);

         if Theme_Specific_Css_Provider = null then
            Gtk_New (Theme_Specific_Css_Provider);
            Gtk.Style_Context.Add_Provider_For_Screen
              (Get_Default_Screen (Get_Default),
               +Theme_Specific_Css_Provider,
               Priority => Gtk.Style_Provider.Priority_Settings);
         end if;

         Theme_Css := Kernel.Get_Home_Dir.Create_From_Dir
           ("gps-" & (+Theme.Name.all) & ".css");
         if not Theme_Css.Is_Regular_File then
            Trace (Me, "No " & Theme_Css.Display_Full_Name & " found");
            Theme_Css := Kernel.Get_Share_Dir.Create_From_Dir
              ("gps-" & (+Theme.Name.all) & ".css");
         end if;

         if not Theme_Css.Is_Regular_File then
            Trace (Me, "No " & Theme_Css.Display_Full_Name & " found");
         elsif not Theme_Specific_Css_Provider.Load_From_Path
           (+Theme_Css.Full_Name.all, Err'Access)
         then
            Trace (Me, "Error loading " & Theme_Css.Display_Full_Name & ": "
                   & Get_Message (Err));
         else
            Trace (Me, "Loaded " & Theme_Css.Display_Full_Name);
         end if;
      end if;

      if P = null
        or else P = Preference (Default_Font)
      then
         --  ??? This creates a new css_provider every time prefs are changed.
         Gtkada.Style.Load_Css_String
           ("* { font: " & To_String (Default_Font.Get_Pref_Font) & "}",
            Priority => Gtk.Style_Provider.Priority_Theme);
      end if;

      if P = null
        or else P = Preference (Pref_Toolbar_Style)
      then
         case Toolbar_Icons_Size'(Pref_Toolbar_Style.Get_Pref) is
         when Text_Only =>
            Get_Toolbar (Kernel).Set_Icon_Size (Icon_Size_Menu);
            Get_Toolbar (Kernel).Set_Style (Toolbar_Text);

         when Text_And_Icons =>
            Get_Toolbar (Kernel).Set_Icon_Size (Icon_Size_Menu);
            Get_Toolbar (Kernel).Set_Style (Toolbar_Both);

         when Small_Icons =>
            Get_Toolbar (Kernel).Set_Icon_Size (Icon_Size_Menu);
            Get_Toolbar (Kernel).Set_Style (Toolbar_Icons);

         when Large_Icons =>
            Get_Toolbar (Kernel).Set_Icon_Size (Icon_Size_Large_Toolbar);
            Get_Toolbar (Kernel).Set_Style (Toolbar_Icons);
         end case;
      end if;

      if P = null
        or else P = Preference (Tooltips_Background)
      then
         if Tooltips_Background.Get_Pref = White_RGBA then
            --  Fallback to default color
            if Tooltips_Background_Provider /= null then
               Remove_Provider_For_Screen
                 (Get_Default_Screen (Get_Default),
                  +Tooltips_Background_Provider);
               Tooltips_Background_Provider := null;
            end if;

         else
            if Tooltips_Background_Provider = null then
               Gtk_New (Tooltips_Background_Provider);
               Add_Provider_For_Screen
                 (Get_Default_Screen (Get_Default),
                  +Tooltips_Background_Provider,
                  Priority => Gtk.Style_Provider.Priority_User);
               Unref (Tooltips_Background_Provider);
            end if;

            if not Tooltips_Background_Provider.Load_From_Data
              ("@define-color theme_tooltip_bg_color "
               & To_String (Tooltips_Background.Get_Pref) & ";",
               Err'Access)
            then
               Trace (Me, "Error setting tooltip color: "
                      & Get_Message (Err));
            end if;
         end if;
      end if;

      Configure_MDI (Kernel, P);
   end Preferences_Changed;

   -------------------------
   -- On_Draw_Toolbar_Box --
   -------------------------

   function On_Draw_Toolbar_Box
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo_Context) return Boolean is
   begin
      Render_Background
        (Get_Style_Context (Self),
         Cr, 0.0, 0.0,
         Gdouble (Get_Allocated_Width (Self)),
         Gdouble (Get_Allocated_Height (Self)));
      return False;
   end On_Draw_Toolbar_Box;

   -----------------
   -- On_Focus_In --
   -----------------

   function On_Focus_In (W : access Gtk_Widget_Record'Class) return Boolean;
   function On_Focus_In (W : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Check_Monitored_Files_In_Background (GPS_Window (W).Kernel);
      return False;
   end On_Focus_In;

   ------------
   -- Kernel --
   ------------

   function Kernel
     (Self : not null access GPS_Window_Record'Class)
      return GPS.Kernel.Kernel_Handle
   is
   begin
      return Self.Application.Kernel;
   end Kernel;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out GPS_Window;
      Application : not null access GPS_Application_Record'Class;
      Menubar     : not null access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class)
   is
      Vbox      : Gtk_Vbox;
   begin
      Main_Window := new GPS_Window_Record;
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Application_Window.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "GpsMainWindow");
      Glib.Object.G_New (Main_Window, Class_Record);

      Main_Window.Application := Application;
      Application.Add_Window (Main_Window);

      Application.Kernel.Set_Main_Window (Main_Window);

      Pref_Toolbar_Style := Toolbar_Icons_Size_Preferences.Create
        (Get_Preferences (Application.Kernel),
         Name    => "GPS6-General-Toolbar-Style",
         Label   => -"Toolbar style",
         Page    => -"Windows",
         Doc     => -("Indicates how the tool bar should be displayed"),
         Default => Small_Icons);

      --  Use Win_Pos_Center, as the default Win_Pos_None is translated on many
      --  window managers as "top-left" corner, which may cause issues with
      --  taskbars.
      Set_Position (Main_Window, Win_Pos_Center);
      Set_Modal (Main_Window, False);
      Set_Default_Size (Main_Window, 800, 700);

      Gtk_New (Main_Window.Main_Accel_Group);
      Add_Accel_Group (Main_Window, Main_Window.Main_Accel_Group);

      GPS.Kernel.MDI.Gtk_New
        (MDI    => Main_Window.MDI,
         Kernel => Application.Kernel,
         Group  => Main_Window.Main_Accel_Group);

      Gtk_New_Vbox (Vbox, False, 0);
      Add (Main_Window, Vbox);

      Main_Window.Menu_Bar := Gtk_Menu_Bar (Menubar);
      Pack_Start (Vbox, Main_Window.Menu_Bar, False);

      Setup_Toplevel_Window (Main_Window.MDI, Main_Window);

      Gtk_New_Hbox (Main_Window.Toolbar_Box, False, 0);
      Main_Window.Toolbar_Box.Set_Name ("toolbar-box");
      Pack_Start (Vbox, Main_Window.Toolbar_Box, False, False, 0);
      Get_Style_Context (Main_Window.Toolbar_Box).Add_Class ("toolbar");
      Main_Window.Toolbar_Box.On_Draw (On_Draw_Toolbar_Box'Access);

      Add (Vbox, Main_Window.MDI);

      Widget_Callback.Connect (Main_Window, Signal_Destroy, On_Destroy'Access);

      Add_Hook (Application.Kernel, Preference_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "main_window.preferences_changed");

      Add_Hook (Application.Kernel, Project_Changed_Hook,
                Wrapper (On_Project_Changed'Access),
                Name => "main_window.projet_changed");

      Return_Callback.Object_Connect
        (Main_Window, Gtk.Widget.Signal_Delete_Event,
         Delete_Callback'Access,
         Gtk_Widget (Main_Window),
         After => False);

      Return_Callback.Connect
        (Main_Window, Signal_Focus_In_Event, On_Focus_In'Access);

      --  Support for Win32 WM_DROPFILES drag'n'drop

      Gtk.Dnd.Dest_Set
        (Main_Window, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Main_Window, Signal_Drag_Data_Received,
         Drag_Data_Received'Access, Application.Kernel);

      --  Set the generic user interface
      User_Interface_Tools.Set_User_Interface
        (new User_Interface'(Main_Window => Gtk_Window (Main_Window)));

      Main_Window.Toolbar := Create_Toolbar (Application.Kernel, Id => "main");
      Main_Window.Toolbar_Box.Pack_Start (Main_Window.Toolbar);

      Preferences_Changed (Application.Kernel, Data => null);
   end Gtk_New;

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

      Command              := new MDI_Child_Selection_Command;
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

      Command              := new MDI_Child_Selection_Command;
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
      Command2.Mode   := Split_H;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split horizontally",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two horizontally"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Split_V;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Split vertically",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two vertically"));

      Command2        := new MDI_Window_Actions_Command;
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
      Command2.Mode   := Reorder_Tab_Left;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move tab to left",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Move the current notebook tab one position to the left, within"
           & " the notebook (cyclic)"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Reorder_Tab_Right;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move tab to right",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Move the current notebook tab one position to the right, within"
           & " the notebook (cyclic)"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Move_To_Next_Tab;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to next tab",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Move to the next tab in the current notebook"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Move_To_Previous_Tab;
      Register_Action
        (Main_Window.Kernel,
         Name        => "Move to previous tab",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Move to the previous tab in the current notebook"));

      Register_Command
        (Main_Window.Kernel, "dialog",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "yes_no_dialog",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "file_selector",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "input_dialog",
         Minimum_Args  => 2,
         Maximum_Args  => 100,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
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
        (Main_Window.Kernel, "version",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Default_Command_Handler'Access);

      Register_Command
        (Main_Window.Kernel, Constructor_Method,
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "split",
         Class        => MDI_Window_Class,
         Maximum_Args => 3,
         Handler      => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "float",
         Maximum_Args => 1,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "is_floating",
         Class       => MDI_Window_Class,
         Handler     => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "raise_window",
         Class       => MDI_Window_Class,
         Handler     => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "get_child",
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "next",
         Class        => MDI_Window_Class,
         Maximum_Args => 1,
         Handler      => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "name",
         Maximum_Args => 1,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Register_Command
        (Main_Window.Kernel, "rename",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);

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
         Handler       => Default_Command_Handler'Access);
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
      Child2 : MDI_Child;
      Klass  : Class_Type;
      Widget : Gtk_Widget;
      Orientation : Gtk_Orientation;
      Result : Class_Instance;
      Reuse  : Boolean;
   begin
      if Child = null then
         Set_Error_Msg (Data, "MDIWindow no longer exists");

      elsif Command = Constructor_Method then
         Set_Error_Msg (Data, "Cannot build instances of MDIWindow");

      elsif Command = "split" then
         Name_Parameters (Data, Split_Cmd_Parameters);
         if Get_State (Child) = Normal then
            Raise_Child (Child);
            Child2 := Dnd_Data (Child, Copy => Nth_Arg (Data, 3, False));
            Set_Focus_Child (Child2);

            if Nth_Arg (Data, 2, True) then
               Orientation := Orientation_Vertical;
            else
               Orientation := Orientation_Horizontal;
            end if;

            Reuse := Nth_Arg (Data, 3, False);

            if Reuse then
               Split (Get_MDI (Kernel),
                      Orientation       => Orientation,
                      Mode              => Any_Side_Reuse);
            else
               Split (Get_MDI (Kernel),
                      Orientation       => Orientation,
                      Mode              => Before);
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

            Result := Create_MDI_Window_Instance
              (Get_Script (Data), Kernel, Child2);
            Set_Return_Value (Data, Result);
         end;

      elsif Command = "get_child" then
         Widget := Get_Widget (Child);
         Result := Get_Instance (Get_Script (Data), Widget);

         if Result = No_Class_Instance then
            if Child.all in GPS_MDI_Child_Record'Class then
               Klass := GPS_MDI_Child (Child).Get_Child_Class;
               if Klass = No_Class then
                  Klass := Get_GUI_Class (Kernel);
               end if;
            else
               Klass := Get_GUI_Class (Kernel);
            end if;

            Result := New_Instance (Get_Script (Data), Klass);
            Set_Data (Result, GObject (Widget));
         end if;

         Set_Return_Value (Data, Result);
      end if;
   end Default_Window_Command_Handler;

   --------------------------------
   -- Create_MDI_Window_Instance --
   --------------------------------

   function Create_MDI_Window_Instance
     (Script : not null access Scripting_Language_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class;
      Child  : access MDI_Child_Record'Class) return Class_Instance
   is
      MDI_Window_Class : Class_Type;
      Inst : Class_Instance;
   begin
      if Child = null then
         return No_Class_Instance;
      end if;

      Inst := Get_Instance (Script, Child);
      if Inst = No_Class_Instance then
         MDI_Window_Class := New_Class (Kernel, "MDIWindow");
         Inst := New_Instance (Script, MDI_Window_Class);
         Set_Data (Inst, Glib.Object.GObject (Child));
      end if;
      return Inst;
   end Create_MDI_Window_Instance;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Inst : Class_Instance) return MDI_Child is
   begin
      return MDI_Child (Glib.Object.GObject'(Get_Data (Inst)));
   end Get_Child;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use Glib.Object;
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Child  : MDI_Child;
      Inst   : Class_Instance;
   begin
      if Command = "exit" then
         Name_Parameters (Data, Exit_Cmd_Parameters);
         Quit (GPS_Window (Get_Main_Window (Kernel)),
               Force => Nth_Arg (Data, 1, False),
               Status => Nth_Arg (Data, 2, 0));

      elsif Command = "version" then
         Set_Return_Value (Data, Config.Version);

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

         Inst := Create_MDI_Window_Instance (Get_Script (Data), Kernel, Child);
         Set_Return_Value (Data, Inst);

      elsif Command = "children" then
         declare
            Iter : Child_Iterator := First_Child (Get_MDI (Kernel));
         begin
            Set_Return_Value_As_List (Data);
            while Get (Iter) /= null loop
               Child := Get (Iter);
               Inst := Create_MDI_Window_Instance
                 (Get_Script (Data), Kernel, Child);
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

      elsif Command = "file_selector" then
         Name_Parameters (Data, File_Selector_Cmd_Parameters);

         declare
            Result : GNATCOLL.VFS.Virtual_File;
         begin
            if Number_Of_Arguments (Data) = 0 then
               Result := Select_File (Parent => Get_Current_Window (Kernel));
            else
               Result := Select_File
                 (File_Pattern => Nth_Arg (Data, 1),
                  Parent       => Get_Current_Window (Kernel));
            end if;

            Set_Return_Value (Data, Create_File (Get_Script (Data), Result));
         end;

      elsif Command = "input_dialog" then
         declare
            use Gtk.Text_View;
            use Gtk.Text_Buffer;

            Dialog : Gtk_Dialog;
            Label  : Gtk_Label;
            Group  : Gtk_Size_Group;
            Button : Gtk_Widget;

            type Ent_Array
               is array (2 .. Number_Of_Arguments (Data)) of Gtk_Entry;
            Ent : Ent_Array;

            type Text_View_Array
               is array (2 .. Number_Of_Arguments (Data)) of Gtk_Text_View;

            Text : Text_View_Array;

            procedure Create_Entry (N : Natural);
            --  Create the Nth entry. N must be in Ent_Array'Range

            ------------------
            -- Create_Entry --
            ------------------

            procedure Create_Entry (N : Natural) is
               Multiline_Prefix : constant String := "multiline:";
               Is_Multiline     : Boolean := False;

               Arg   : constant String := Nth_Arg (Data, N);
               Index : Natural := Arg'First;
               First : Natural := Arg'First;
               Hbox  : Gtk_Hbox;
            begin
               Gtk_New_Hbox (Hbox, Homogeneous => False);
               Pack_Start (Get_Content_Area (Dialog), Hbox, Padding => 3);

               while Index <= Arg'Last loop
                  exit when Arg (Index) = '=';

                  Index := Index + 1;
               end loop;

               if Index - Arg'First > Multiline_Prefix'Length then
                  First := First + Multiline_Prefix'Length;

                  if Arg (Arg'First .. First - 1) = Multiline_Prefix then
                     Is_Multiline := True;
                  else
                     First := Arg'First;
                  end if;
               end if;

               if First <= Index - 1 then
                  Gtk_New (Label, Arg (First .. Index - 1) & ':');

                  Set_Alignment (Label, 0.0, 0.5);
                  Add_Widget (Group, Label);
                  Pack_Start (Hbox, Label, Expand => False, Padding => 3);
               end if;

               if Is_Multiline then
                  declare
                     Buffer   : Gtk_Text_Buffer;
                     Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
                  begin
                     Gtk_New (Buffer);
                     Gtk_New (Text (N), Buffer);
                     Buffer.Set_Text (Arg (Index + 1 .. Arg'Last));
                     Gtk.Scrolled_Window.Gtk_New (Scrolled);
                     Scrolled.Add (Text (N));
                     Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
                     Scrolled.Set_Shadow_Type (Shadow_In);
                     Pack_Start (Hbox, Scrolled, Padding => 10);
                  end;
               else
                  Gtk_New (Ent (N));
                  Set_Text (Ent (N), Arg (Index + 1 .. Arg'Last));

                  Set_Activates_Default (Ent (N),  True);
                  Pack_Start (Hbox, Ent (N), Padding => 10);
               end if;
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
              (Get_Content_Area (Dialog),
               Label, Expand => True, Padding => 10);

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
                  if Ent (Num) /= null then
                     Set_Return_Value (Data, Get_Text (Ent (Num)));
                  else
                     Set_Return_Value
                       (Data,
                        Glib.Properties.Get_Property
                          (Text (Num).Get_Buffer,
                           Gtk.Text_Buffer.Text_Property));
                  end if;
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

   overriding function Execute
     (Command : access MDI_Child_Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      if Command.Group /= Group_Any then
         Check_Interactive_Selection_Dialog
           (Get_MDI (Kernel), null,
            Move_To_Next            => Command.Move_To_Next,
            Only_Group              => Command.Group);
      else
         Check_Interactive_Selection_Dialog
           (Get_MDI (Kernel), Context.Event,
            Move_To_Next            => Command.Move_To_Next,
            Only_Group              => Command.Group);
      end if;
      return Success;
   end Execute;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class) is
      Win : constant GPS_Window := GPS_Window (Main_Window);
   begin
      --  All tasks should be interrupted before the main window is closed
      --  since they may need to access their consoles.

      Task_Manager.Interrupt_All_Tasks (Get_Task_Manager (Win.Kernel));
   end On_Destroy;

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
         V : Virtual_File;
      begin
         if Info /= "" then
            V := Create (+Info);
            if V.Is_Regular_File then
               return " - " & V.Display_Base_Name
                 & " - " & (+V.Dir_Name);
            else
               return " - " & Info;
            end if;
         end if;

         return "";
      end Info_Str;

      ----------------
      -- Remote_Str --
      ----------------

      function Remote_Str return String is
      begin
         if Is_Local (Build_Server) then
            return "";
         else
            return " on " & Get_Nickname (Build_Server);
         end if;
      end Remote_Str;

   begin
      Set_Title
        (Window, "GPS" & Info_Str & " - "
         & Get_Project (Window.Kernel).Name & " project" & Remote_Str);
   end Reset_Title;

   ----------------------
   -- Is_Any_Menu_Open --
   ----------------------

   function Is_Any_Menu_Open
     (Window : access GPS_Window_Record) return Boolean
   is
      use Gtk.Widget.Widget_List;
      L : Glist := First (Window.Menu_Bar.Get_Children);
      Menu : Gtk_Widget;
   begin
      while L /= Null_List loop
         Menu := Gtk_Menu_Item (Get_Data (L)).Get_Submenu;
         L := Next (L);
         if Menu /= null and then Menu.Is_Visible then
            return True;
         end if;
      end loop;
      return False;
   end Is_Any_Menu_Open;

end GPS.Main_Window;
