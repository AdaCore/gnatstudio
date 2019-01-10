------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GNATCOLL.JSON;
with GNATCOLL.Scripts.Gtkada;   use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Strings;              use GNAT.Strings;

with Cairo;                     use Cairo;
with Gdk.Display;               use Gdk.Display;
with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Screen;                use Gdk.Screen;
with Gdk.Window;                use Gdk.Window;

with Glib.Main;
with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Glib.Properties;
with Glib.Values;               use Glib.Values;

with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Settings;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Style_Provider;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Css_Provider;          use Gtk.Css_Provider;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Multiline_Entry;    use Gtkada.Multiline_Entry;
with Gtkada.Style;

with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;

with Config;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences.Enums; use Default_Preferences;
with Dialog_Utils;              use Dialog_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel;                use GPS.Kernel;
with GPS.VCS;                   use GPS.VCS;
with GPS.Properties;            use GPS.Properties;
with GUI_Utils;
with Informational_Popups;      use Informational_Popups;
with Task_Manager;              use Task_Manager;
with User_Interface_Tools;

package body GPS.Main_Window is
   Me : constant Trace_Handle := Create ("GPS.MAIN.WINDOW");

   --  This trace doesn't use the naming convention for backward compatibility:
   --  most customers add this trace in traces.cfg
   Store_Window_Positions : constant Trace_Handle :=
     Create ("STORE_WINDOW_POSITIONS", On);

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

   Name_Cst       : aliased constant String := "name";
   Child_Cst      : aliased constant String := "child";
   Float_Cst      : aliased constant String := "float";
   Visible_Only_Cst : aliased constant String := "visible_only";
   Short_Cst      : aliased constant String := "short";
   Get_Cmd_Parameters : constant Cst_Argument_List := (1 => Name_Cst'Access);
   Get_By_Child_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Child_Cst'Access);
   Float_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Float_Cst'Access);
   Next_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Visible_Only_Cst'Access);
   Name_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Short_Cst'Access);
   Rename_Cmd_Parameter : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Short_Cst'Access);

   type Window_Size_Property is new Property_Record with record
      Width, Height : Gint;
   end record;
   overriding procedure Save
     (Self  : access Window_Size_Property;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
      (Self : in out Window_Size_Property;
      Value : GNATCOLL.JSON.JSON_Value);
   --  Save the preferred size of a window in the persistent properties

   type Toolbar_Icons_Size
      is (Text_Only, Text_And_Icons, Small_Icons, Large_Icons);
   package Toolbar_Icons_Size_Preferences is new
     Default_Preferences.Enums.Generics (Toolbar_Icons_Size);

   Pref_Toolbar_Style  : Toolbar_Icons_Size_Preferences.Preference;
   Window_Title_Pref   : String_Preference;

   Theme_Specific_Css_Provider : Gtk_Css_Provider;
   --  Provider for the gps-<theme>.css file

   function Delete_Callback
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the delete event

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   procedure On_Destroy (Main_Window : access Gtk_Widget_Record'Class);
   --  Called when the main window is destroyed

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

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Project_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project is changed

   procedure On_Float_Child (Win : access Gtk_Widget_Record'Class);
   --  Called when a child window is floated

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

   procedure Set_Toolbar_Style
     (App       : not null access GPS_Application_Record'Class;
      Icon_Size : Gtk.Enums.Gtk_Icon_Size;
      Style     : Gtk.Enums.Gtk_Toolbar_Style);
   --  Change the style for all toolbars in the application

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

   function On_Focus_In (W : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the main window gains or loses focus.

   type Delete_Event_Data is record
      Kernel : access Kernel_Handle_Record'Class;
      Name   : Unbounded_String;
   end record;

   package Delete_Events is new Gtk.Handlers.User_Return_Callback
      (Widget_Type => Gtk_Window_Record,
       Return_Type => Boolean,
       User_Type   => Delete_Event_Data);
   function On_Delete
      (Win   : access Gtk_Window_Record'Class;
       Event : Gdk_Event;
       Data  : Delete_Event_Data) return Boolean;
   --  Called when a window is deleted, to store its size in the properties
   --  and be able to restore it later on.

   package Hide_Events is new Gtk.Handlers.User_Callback
      (Widget_Type => Gtk_Window_Record,
       User_Type   => Delete_Event_Data);
   procedure On_Hide
      (Win   : access Gtk_Window_Record'Class;
       Data  : Delete_Event_Data);
   --  Called when a window is hidden, to store its size in the properties
   --  and be able to restore ir later on.

   procedure Update_Perspectives_In_Selector
     (Self : access Gtk_Widget_Record'Class);
   --  Update the current perspective in the perspectives selector

   procedure On_Switch_Perspective (Self : access Gtk_Widget_Record'Class);
   --  Called when the user selects a new perspective via the selector

   --------------------------
   -- For_All_Open_Windows --
   --------------------------

   procedure For_All_Open_Windows
     (App      : not null access Gtk_Application_Record'Class;
      Callback : not null access procedure
        (Win : not null access GPS_Application_Window_Record'Class))
   is
      use Widget_List;
      List : Widget_List.Glist := App.Get_Windows;  --  Do not free
   begin
      while List /= Null_List loop
         Callback (GPS_Application_Window (Get_Data (List)));
         List := Next (List);
      end loop;
   end For_All_Open_Windows;

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
      return Boolean is
   begin
      if Save_MDI_Children (Main_Window.Kernel)
         and then Before_Exit_Action_Hook.Run (Main_Window.Kernel)
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
         else
            Main_Window.Is_Destroyed := False;
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
      Main_Window : constant GPS_Window := GPS_Window (Widget);
   begin
      if Prepare_Quit (Main_Window) then
         Main_Window.Application.Quit;
         return False;
      end if;

      return True;
   end Delete_Callback;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Project_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Reset_Title (Kernel);
   end Execute;

   -----------------------
   -- Set_Toolbar_Style --
   -----------------------

   procedure Set_Toolbar_Style
     (App       : not null access GPS_Application_Record'Class;
      Icon_Size : Gtk.Enums.Gtk_Icon_Size;
      Style     : Gtk.Enums.Gtk_Toolbar_Style)
   is
      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class);
      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class) is
      begin
         if W.Toolbar /= null then
            W.Toolbar.Set_Icon_Size (Icon_Size);
            W.Toolbar.Set_Style (Style);
         end if;
      end Internal;
   begin
      For_All_Open_Windows (App, Internal'Access);
   end Set_Toolbar_Style;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      Dead   : Boolean;
      pragma Unreferenced (Dead, Self);
      Theme : Theme_Descr;
      Err   : aliased GError;
      Theme_Css : Virtual_File;

   begin
      if Pref = null
        or else Pref = Preference (Gtk_Theme)
      then
         Theme := Gtk_Theme.Get_Pref;

         declare
            Theme_Name      : constant String := To_String (Theme.Name);
            Theme_Directory : constant String := To_String (Theme.Directory);
         begin
            Trace (Me, "Setting gtk+ theme to '"
                   & Theme_Name & "', directory='"
                   & Theme_Directory & "'");
            Glib.Properties.Set_Property
              (Gtk.Settings.Get_Default,
               Gtk.Settings.Gtk_Theme_Name_Property,
               Theme_Directory);

            Glib.Properties.Set_Property
              (Gtk.Settings.Get_Default,
               Gtk.Settings.Gtk_Application_Prefer_Dark_Theme_Property,
               Theme.Dark);

            if Theme_Specific_Css_Provider = null then
               Gtk_New (Theme_Specific_Css_Provider);
               Gtk.Style_Context.Add_Provider_For_Screen
                 (Get_Default_Screen (Get_Default),
                  +Theme_Specific_Css_Provider,
                  Priority => Gtk.Style_Provider.Priority_Theme);
            end if;

            Theme_Css := Kernel.Get_Home_Dir.Create_From_Dir
              ("gps-" & (+Theme_Name) & ".css");

            if not Theme_Css.Is_Regular_File then
               Trace (Me, "No " & Theme_Css.Display_Full_Name & " found");
               Theme_Css := Kernel.Get_Share_Dir.Create_From_Dir
                 ("gps-" & (+Theme_Name) & ".css");
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
         end;
      end if;

      if Pref = null
        or else Pref = Preference (Default_Font)
      then
         declare
            Default_Font_Val    : constant Pango_Font_Description :=
                                    Default_Font.Get_Pref;
            Default_Font_Family : constant String :=
                                    Get_Family (Default_Font_Val);
            Default_Font_Size   : constant String :=
                                    Gint'Image
                                      (Pango.Enums.To_Pixels
                                         (Get_Size (Default_Font_Val)));
            Default_Font_Style  : constant String :=
                                    Get_Style_As_String (Default_Font_Val);
         begin
            --  ??? This creates a new css_provider every time prefs are
            --  changed.
            Gtkada.Style.Load_Css_String
              ("* { font-family: " & Default_Font_Family & ";"
               & ASCII.LF
               & " font-size: " & Default_Font_Size & "pt;"
               & ASCII.LF
               & " font-style: " & Default_Font_Style & ";}",
               Priority => Gtk.Style_Provider.Priority_Theme);
         end;
      end if;

      if Pref = null
        or else Pref = Preference (Window_Title_Pref)
      then
         Reset_Title (Kernel);
      end if;

      if Pref = null
        or else Pref = Preference (Pref_Toolbar_Style)
      then
         case Toolbar_Icons_Size'(Pref_Toolbar_Style.Get_Pref) is
         when Text_Only =>
            Set_Toolbar_Style (GPS_Application (Kernel.Get_Application),
                               Icon_Size_Menu, Toolbar_Text);

         when Text_And_Icons =>
            Set_Toolbar_Style (GPS_Application (Kernel.Get_Application),
                               Icon_Size_Menu, Toolbar_Both);

         when Small_Icons =>
            Set_Toolbar_Style (GPS_Application (Kernel.Get_Application),
                               Icon_Size_Menu, Toolbar_Icons);

         when Large_Icons =>
            Set_Toolbar_Style (GPS_Application (Kernel.Get_Application),
                               Icon_Size_Large_Toolbar, Toolbar_Icons);
         end case;
      end if;

      Configure_MDI (Kernel, Pref);
   end Execute;

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

   function On_Focus_In (W : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Check_Monitored_Files_In_Background (GPS_Window (W).Kernel);
      return False;
   end On_Focus_In;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access GPS_Application_Window_Record'Class;
      Application : not null access GPS_Application_Record'Class)
   is
   begin
      Gtk.Application_Window.Initialize (Self, Application);

      Self.Application := Application;
      Application.Add_Window (Self);

      Gtk_New_Vbox (Self.Main_Box, Homogeneous => False);
      Self.Add (Self.Main_Box);
   end Initialize;

   -------------------------------------
   -- Update_Perspectives_In_Selector --
   -------------------------------------

   procedure Update_Perspectives_In_Selector
     (Self : access Gtk_Widget_Record'Class)
   is
      Main : constant GPS_Window := GPS_Window (Self);
      Current : constant String := Main.MDI.Current_Perspective;
      List : constant String_List_Access :=  --  do not free
        Main.MDI.List_Of_Perspectives;
   begin
      Main.Perspective_Selector.Clear_Items;

      if List /= null then
         for L of List.all loop
            Main.Perspective_Selector.Add_Item (Item => L.all);
         end loop;
      end if;

      Main.Perspective_Selector.Select_Item (Current);
   end Update_Perspectives_In_Selector;

   --------------------
   -- Setup_Menu_Bar --
   --------------------

   procedure Setup_Menu_Bar
     (Self : not null access GPS_Application_Window_Record'Class) is
   begin
      Install_Menus (Self.Application.Kernel, Menubar => Self.Menu_Bar);
      if Self.Menu_Bar /= null then
         Self.Main_Box.Pack_Start (Self.Menu_Bar, Expand => False);
      end if;

      Gtk_New_Hbox (Self.Toolbar_Box, False, 0);
      Self.Toolbar_Box.Set_Name ("toolbar-box");
      Self.Main_Box.Pack_Start (Self.Toolbar_Box, False, False, 0);
      Get_Style_Context (Self.Toolbar_Box).Add_Class ("toolbar");
      Self.Toolbar_Box.On_Draw (On_Draw_Toolbar_Box'Access);

      Create_Toolbar (Self.Application.Kernel, Self.Toolbar, Id => "main");
      Self.Toolbar_Box.Pack_Start (Self.Toolbar);
   end Setup_Menu_Bar;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out GPS_Window;
      Application : not null access GPS_Application_Record'Class)
   is
      P    : access On_Pref_Changed;
   begin
      Main_Window := new GPS_Window_Record;
      GPS.Main_Window.Initialize (Main_Window, Application);

      Application.Kernel.Set_Main_Window (Main_Window);

      --  Useful on Mac OS X, to present the application to the user
      Glib.Main.Activate_Application;

      Pref_Toolbar_Style := Toolbar_Icons_Size_Preferences.Create
        (Get_Preferences (Application.Kernel),
         Path    => -"General/Custom Styles:Other",
         Name    => "GPS6-General-Toolbar-Style",
         Label   => -"Toolbar style",
         Doc     => -"Style the toolbar.",
         Default => Small_Icons);

      Window_Title_Pref := Application.Kernel.Get_Preferences.Create
        (Path  => -"Windows:Main Window",
         Name  => "window-title",
         Label => "Window title",
         Doc   => "Title to use for the GPS window." & ASCII.LF
           & "The following macros are expanded dynamically:" & ASCII.LF
           & GPS.Kernel.Macros.Doc,
         Default => "GPS - %ts - %fd - %P project");

      --  Use Win_Pos_Center, as the default Win_Pos_None is translated on many
      --  window managers as "top-left" corner, which may cause issues with
      --  taskbars.
      Set_Position (Main_Window, Win_Pos_Center);
      Set_Geometry_Hints
        (Window          => Main_Window,
         Geometry_Widget => null,
         Geometry        => Gdk_Geometry'
           (Min_Width  => 1280,
            Min_Height => 720,
            others     => <>),
         Geom_Mask       => Hint_Base_Size);

      Gtk_New (Main_Window.Main_Accel_Group);
      Add_Accel_Group (Main_Window, Main_Window.Main_Accel_Group);

      GPS.Kernel.MDI.Gtk_New
        (MDI    => Main_Window.MDI,
         Kernel => Application.Kernel,
         Group  => Main_Window.Main_Accel_Group);
      Setup_Toplevel_Window (Main_Window.MDI, Main_Window);

      Widget_Callback.Object_Connect
         (Main_Window.MDI, Signal_Float_Child, On_Float_Child'Access,
          Slot_Object => Main_Window);

      Main_Window.Setup_Menu_Bar;
      Main_Window.Main_Box.Add (Main_Window.MDI);

      Widget_Callback.Connect (Main_Window, Signal_Destroy, On_Destroy'Access);

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

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

      P := new On_Pref_Changed;
      Preferences_Changed_Hook.Add (P);
      P.Execute (Application.Kernel, null);
   end Gtk_New;

   ---------------------------
   -- On_Switch_Perspective --
   ---------------------------

   procedure On_Switch_Perspective (Self : access Gtk_Widget_Record'Class) is
      B : constant GPS_Window := GPS_Window (Self);
      Selected : constant String := B.Perspective_Selector.Get_Selected_Item;
   begin
      Load_Perspective (B.Kernel, Selected);
   end On_Switch_Perspective;

   --------------------------------
   -- Setup_Perspective_Selector --
   --------------------------------

   procedure Setup_Perspective_Selector
     (Self : not null access GPS_Window_Record'Class)
   is
      P : Gtkada_Combo_Tool_Button;
   begin
      Gtk_New (P, "", Click_Pops_Up => True);
      Self.Perspective_Selector := P;

      P.Set_Tooltip_Text (-"Change the current perspective (layout of views)");
      Widget_Callback.Object_Connect
        (Self.MDI, Signal_Perspective_Changed,
         Update_Perspectives_In_Selector'Access, Self);
      Widget_Callback.Object_Connect
        (Self.MDI, Signal_Perspectives_Added,
         Update_Perspectives_In_Selector'Access, Self);
      Widget_Callback.Object_Connect
        (Self.Perspective_Selector,
         Gtkada.Combo_Tool_Button.Signal_Selection_Changed,
         On_Switch_Perspective'Access, Self);

      Self.Toolbar_Box.Pack_End (P, Expand => False, Fill => False);
   end Setup_Perspective_Selector;

   ------------------------
   -- Setup_VCS_Selector --
   ------------------------

   procedure Setup_VCS_Selector
     (Self : not null access GPS_Window_Record'Class)
   is
      VCS : constant Abstract_VCS_Repository_Access := Self.Kernel.VCS;
      W   : Gtk_Widget;
   begin
      if VCS /= null then
         W := VCS.Get_VCS_Selector;
         if W /= null then
            Self.Toolbar_Box.Pack_End (W, False, False, Padding => 4);
         end if;
      end if;
   end Setup_VCS_Selector;

   --------------------
   -- On_Float_Child --
   --------------------

   procedure On_Float_Child (Win : access Gtk_Widget_Record'Class) is
      W : constant GPS_Window := GPS_Window (Win);
   begin
      --  Report a context change, so that the menus for the new
      --  floating window are properly updated
      W.Kernel.Refresh_Context;
      Reset_Title (W.Kernel);
   end On_Float_Child;

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
      Kernel           : constant Kernel_Handle := Main_Window.Kernel;
   begin
      Command              := new MDI_Child_Selection_Command;
      Command.Move_To_Next := True;
      Command.Group        := Group_Any;
      Register_Action
        (Kernel,
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
        (Kernel,
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
        (Kernel,
         Name        => "Select other window",
         Command     => Command,
         Category    => "MDI",
         Description =>
         -("Select the next splitted window in the central area of GPS."));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Split_H;
      Register_Action
        (Kernel,
         Name        => "Split horizontally",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two horizontally"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Split_V;
      Register_Action
        (Kernel,
         Name        => "Split vertically",
         Command     => Command2,
         Category    => "MDI",
         Description => -("Split the current window in two vertically"));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Clone;
      Register_Action
        (Kernel,
         Name        => "Clone window",
         Command     => Command2,
         Category    => "MDI",
         Description =>
         -("Create a duplicate of the current window if possible. Not all"
           & " windows support this operation."));

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Reorder_Tab_Left;
      Register_Action
        (Kernel,
         Name         => "Move tab to left",
         Command      => Command2,
         Category     => "MDI",
         Description  =>
           -("Move the current notebook tab one position to the left, within"
           & " the notebook (cyclic)"),
         For_Learning => True);

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Reorder_Tab_Right;
      Register_Action
        (Kernel,
         Name         => "Move tab to right",
         Command      => Command2,
         Category     => "MDI",
         Description  =>
         -("Move the current notebook tab one position to the right, within"
           & " the notebook (cyclic)"),
         For_Learning => True);

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Move_To_Next_Tab;
      Register_Action
        (Kernel,
         Name         => "Move to next tab",
         Command      => Command2,
         Category     => "MDI",
         Description  => -("Move to the next tab in the current notebook"),
         For_Learning => True);

      Command2        := new MDI_Window_Actions_Command;
      Command2.Mode   := Move_To_Previous_Tab;
      Register_Action
        (Kernel,
         Name         => "Move to previous tab",
         Command      => Command2,
         Category     => "MDI",
         Description  => -("Move to the previous tab in the current notebook"),
         For_Learning => True);

      Kernel.Scripts.Register_Command
        ("present_main_window",
         Params        => No_Params,
         Handler       => Default_Command_Handler'Access,
         Class         => MDI_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        ("get_main_window",
         Params        => No_Params,
         Handler       => Default_Command_Handler'Access,
         Class         => MDI_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        ("dialog",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("yes_no_dialog",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("information_popup",
         Params        => (1  => Param ("text", Optional => True),
                           2  => Param ("icon", Optional => True)),
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("file_selector",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("directory_selector",
         Params        => (1  => Param ("base_dir", Optional => True)),
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("input_dialog",
         Minimum_Args  => 2,
         Maximum_Args  => 100,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("combo_selection_dialog",
         Class        => MDI_Class,
         Params       => (1 => Param ("title"),
                          2 => Param ("message"),
                          3 => Param ("choices"),
                          4 => Param ("combo_label", Optional => True)),
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("save_all",
         Maximum_Args  => 1,
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("exit",
         Minimum_Args => 0,
         Maximum_Args => Exit_Cmd_Parameters'Length,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("version",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("getenv",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("setenv",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Handler      => Default_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class         => MDI_Window_Class,
         Handler       => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("split",
         Class        => MDI_Window_Class,
         Params       => (2 => Param ("vertically", Optional => True),
                          3 => Param ("reuse",      Optional => True),
                          4 => Param ("new_view",   Optional => True)),
         Handler      => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("float",
         Maximum_Args => 1,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_floating",
         Class       => MDI_Window_Class,
         Handler     => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("raise_window",
         Class       => MDI_Window_Class,
         Handler     => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("close",
         Params      => (1 => Param ("force", Optional => True)),
         Class       => MDI_Window_Class,
         Handler     => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_child",
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("next",
         Class        => MDI_Window_Class,
         Maximum_Args => 1,
         Handler      => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("name",
         Maximum_Args => 1,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("rename",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => MDI_Window_Class,
         Handler      => Default_Window_Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("get",
         Class         => MDI_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("children",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_by_child",
         Class         => MDI_Class,
         Static_Method => True,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("current",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("hide",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("show",
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("load_perspective",
         Params        => (1 => Param ("name")),
         Class         => MDI_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("current_perspective",
         Class          => MDI_Class,
         Static_Method  => True,
         Handler        => Default_Command_Handler'Access);
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
      Result : Class_Instance;
   begin
      if Child = null then
         Set_Error_Msg (Data, "MDIWindow no longer exists");

      elsif Command = Constructor_Method then
         Set_Error_Msg (Data, "Cannot build instances of MDIWindow");

      elsif Command = "split" then
         if Child.Get_State = Normal then
            Raise_Child (Child);
            Child2 := Dnd_Data (Child, Copy => Data.Nth_Arg (4, False));
            Set_Focus_Child (Child2);

            Get_MDI (Kernel).Split
               (Child       => Child2,
                Orientation =>
                   (if Data.Nth_Arg (2, True)
                    then Orientation_Vertical else Orientation_Horizontal),
                Mode        =>
                   (if Data.Nth_Arg (3, False)
                    then Any_Side_Reuse else Before));
         end if;

      elsif Command = "float" then
         Name_Parameters (Data, Float_Cmd_Parameters);
         Float_Child (Child, Nth_Arg (Data, 2, True));

      elsif Command = "is_floating" then
         Set_Return_Value (Data, Is_Floating (Child));

      elsif Command = "close" then
         Close_Child (Child, Data.Nth_Arg (2, False));

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
              (Get_Script (Data), Child2);
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
         MDI_Window_Class := New_Class (Get_Kernel (Script), "MDIWindow");
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
      if Command = "present_main_window" then
         Get_Main_Window (Kernel).Present;

      elsif Command = "get_main_window" then
         declare
            Result : Class_Instance;
         begin

            Result := New_Instance (Get_Script (Data), Get_GUI_Class (Kernel));

            Set_Data (Result, GObject (Get_Main_Window (Kernel)));
            Set_Return_Value (Data, Result);
         end;

      elsif Command = "getenv" then
         declare
            Str : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv
               (Nth_Arg (Data, 1));
         begin
            if Str = null then
               Set_Return_Value (Data, String'(""));
            else
               Set_Return_Value (Data, Str.all);
               GNAT.OS_Lib.Free (Str);
            end if;
         end;

      elsif Command = "setenv" then
         GNAT.OS_Lib.Setenv (Nth_Arg (Data, 1), Nth_Arg (Data, 2));

      elsif Command = "exit" then
         Name_Parameters (Data, Exit_Cmd_Parameters);
         Quit (GPS_Window (Get_Main_Window (Kernel)),
               Force => Nth_Arg (Data, 1, False),
               Status => Nth_Arg (Data, 2, 0));

      elsif Command = "version" then
         Set_Return_Value (Data, To_String (Config.Version));

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
            Child := Find_MDI_Child_From_Widget
              (Gtk_Widget
                 (GObject'
                    (Get_Data (Nth_Arg (Data, 1, Get_GUI_Class (Kernel))))));
         else
            Child := Get_Focus_Child (Get_MDI (Kernel));
         end if;

         Inst := Create_MDI_Window_Instance (Get_Script (Data), Child);
         Set_Return_Value (Data, Inst);

      elsif Command = "children" then
         declare
            Iter : Child_Iterator := First_Child (Get_MDI (Kernel));
         begin
            Set_Return_Value_As_List (Data);
            while Get (Iter) /= null loop
               Child := Get (Iter);
               Inst := Create_MDI_Window_Instance
                 (Get_Script (Data), Child);
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

      elsif Command = "information_popup" then
         Display_Informational_Popup
            (Parent    => Get_Main_Window (Kernel),
             Icon_Name => Data.Nth_Arg (2, ""),
             Text      => Data.Nth_Arg (1, ""));

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

      elsif Command = "directory_selector" then
         declare
            Base_Dir : constant Filesystem_String := Data.Nth_Arg (1, "");
            Result   : GNATCOLL.VFS.Virtual_File;
         begin
            Result := Select_Directory
              (Base_Directory => Create (Base_Dir),
               Parent         => Get_Current_Window (Kernel));

            Set_Return_Value (Data, Create_File (Get_Script (Data), Result));
         end;

      elsif Command = "input_dialog" then
         declare
            Dialog       : Gtk_Dialog;
            Main_View    : Dialog_View;
            Group_Widget : Dialog_Group_Widget;
            Button       : Gtk_Widget;
            Message      : constant String := Data.Nth_Arg (1);

            type Ent_Array
               is array (2 .. Number_Of_Arguments (Data)) of Gtk_Entry;
            Ent : Ent_Array;

            type Text_View_Array
            is array (2 .. Number_Of_Arguments (Data))
              of Gtkada_Multiline_Entry;

            Text : Text_View_Array;

            procedure Create_Entry (N : Natural; Focus : Boolean);
            --  Create the Nth entry. N must be in Ent_Array'Range.
            --  Give it the focus if Focus.

            ------------------
            -- Create_Entry --
            ------------------

            procedure Create_Entry (N : Natural; Focus : Boolean) is
               Multiline_Prefix : constant String := "multiline:";
               Is_Multiline     : Boolean := False;

               Arg   : constant String := Nth_Arg (Data, N);
               Index : Natural := Arg'First;
               First : Natural := Arg'First;
            begin
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

               --  If the parameter is multiline, create a multiline entry

               if Is_Multiline then
                  Gtk_New (Text (N));
                  Text (N).Set_Text (Arg (Index + 1 .. Arg'Last));

                  Group_Widget.Create_Child
                    (Text (N),
                     Label       => Arg (First .. Index - 1),
                     Same_Height => False);

                  if Focus then
                     Dialog.Set_Focus (Text (N));
                  end if;
               else
                  Gtk_New (Ent (N));
                  Ent (N).Set_Text (Arg (Index + 1 .. Arg'Last));
                  Ent (N).Set_Activates_Default (True);

                  Group_Widget.Create_Child
                    (Ent (N),
                     Label => Arg (First .. Index - 1));

                  if Focus then
                     Dialog.Set_Focus (Ent (N));
                  end if;
               end if;
            end Create_Entry;

         begin
            Name_Parameters (Data, Input_Dialog_Cmd_Parameters);

            --  Create the dialog

            Gtk_New
              (Dialog,
               Title  => Message,
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal);
            Set_Default_Size_From_History
              (Dialog,
               Name   => Message,
               Kernel => Kernel,
               Width  => 350,
               Height => 250);

            --  Create the main view with the message

            Main_View := Create_Dialog_View_With_Message (Message);
            Dialog.Get_Content_Area.Pack_Start (Main_View);

            --  Create an initial group widget to group all the entries

            Group_Widget := new Dialog_Group_Widget_Record;
            Initialize
              (Group_Widget,
               Parent_View         => Main_View,
               Allow_Multi_Columns => False);

            --  Create the entries for each parameter, giving the focus to the
            --  first entry.

            for Num in Ent'Range loop
               Create_Entry (Num, Focus => Num = Ent'First);
            end loop;

            --  Add the buttons

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            --  Show the dialog and set its focus widget to null to avoid
            --  the selection of the group widget's row.

            Dialog.Show_All;

            Set_Return_Value_As_List (Data);

            --   Run the dialog and return the values set in the entries

            if Run (Dialog) = Gtk_Response_OK then
               for Num in Ent'Range loop
                  if Ent (Num) /= null then
                     Set_Return_Value (Data, Get_Text (Ent (Num)));
                  else
                     Set_Return_Value
                       (Data, Text (Num).Get_Text);
                  end if;
               end loop;
            end if;

            Destroy (Dialog);
         end;

      elsif Command = "combo_selection_dialog" then
         declare
            Dialog       : Gtk_Dialog;
            Button       : Gtk_Widget;
            Main_View    : Dialog_View;
            Group_Widget : Dialog_Group_Widget;
            Combo        : Gtk_Combo_Box_Text;
            Title        : constant String := Data.Nth_Arg (1);
            Message      : constant String := Data.Nth_Arg (2);
            List         : constant List_Instance := Data.Nth_Arg (3);
            Combo_Label  : constant String := Data.Nth_Arg (4, "");
            Choices      : Unbounded_String_Array
              (1 .. List.Number_Of_Arguments);

         begin
            for J in Choices'Range loop
               Choices (J) := Nth_Arg (List, J);
            end loop;

            --  Create the dialog

            Gtk_New
              (Dialog,
               Title  => Title,
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal);
            Set_Default_Size_From_History
              (Dialog,
               Name   => Title,
               Kernel => Kernel,
               Width  => 200,
               Height => 100);

            --  Add the Ok and Cancel buttons

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            --  Create the dialog's main view with the associated message

            Main_View := Create_Dialog_View_With_Message (Message);
            Dialog.Get_Content_Area.Pack_Start (Main_View);

            --  Create the combo box and its associated group widget

            Group_Widget := new Dialog_Group_Widget_Record;
            Initialize (Group_Widget,
                        Parent_View         => Main_View,
                        Allow_Multi_Columns => False);

            Gtk_New (Combo);
            Group_Widget.Create_Child (Combo, Label => Combo_Label);

            --  Append all the given choices to the combo box

            for Choice of Choices loop
               Combo.Append_Text (To_String (Choice));
            end loop;

            --  Select the first choice by default

            Combo.Set_Active (0);

            --  Show the dialog and set its focus widget to null to avoid
            --  the selection of the group widget's row

            Dialog.Show_All;

            --  Return the selected choice if the user clicks on OK

            if Run (Dialog) = Gtk_Response_OK then
               Data.Set_Return_Value (Combo.Get_Active_Text);
            end if;

            Dialog.Destroy;
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
      elsif Command = "load_perspective" then
         declare
            Name : constant String := Nth_Arg (Data, 1);
         begin
            Load_Perspective (Kernel, Name);
         end;

      elsif Command = "current_perspective" then
         Data.Set_Return_Value (Get_MDI (Kernel).Current_Perspective);
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
     (Kernel  : not null access Kernel_Handle_Record'Class)
   is
      Context : Selection_Context;

      function Callback (Param : String; Quoted : Boolean) return String;
      function Callback (Param : String; Quoted : Boolean) return String is
         Done : aliased Boolean := False;
      begin
         return GPS.Kernel.Macros.Substitute
           (Param, Context, Quoted, Done'Access);
      end Callback;

      MDI  : MDI_Window;
      C    : MDI_Child;
      Win  : Gtk_Window;
   begin
      if Kernel.Is_In_Destruction then
         return;
      end if;

      MDI := Get_MDI (Kernel);
      if MDI /= null then
         C := MDI.Get_Focus_Child;

         --  During a drag-and-drop operation, the toplevel might be a
         --  Gtk_Notebook rather than a window (for instance when moving the
         --  Outline from the bottom dock to the left of the editor.

         if C /= null
            and then C.Get_Widget.Get_Toplevel.all in Gtk_Window_Record'Class
         then
            Win := Gtk_Window (C.Get_Widget.Get_Toplevel);
         end if;
      end if;

      if Win = null then
         Win := Kernel.Get_Main_Window;
      end if;

      if Win /= null then
         Context := Kernel.Get_Current_Context;
         Win.Set_Title
           (GNATCOLL.Templates.Substitute
              (Str      => Window_Title_Pref.Get_Pref,
               Callback => Callback'Unrestricted_Access));
      end if;
   end Reset_Title;

   ----------------------
   -- Is_Any_Menu_Open --
   ----------------------

   function Is_Any_Menu_Open
     (App : not null access GPS_Application_Record'Class) return Boolean
   is
      use Gtk.Widget.Widget_List;
      Result : Boolean := False;

      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class);
      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class)
      is
         Children : Glist;  --  Must be freed
         L        : Glist;
         Menu     : Gtk_Widget;
      begin
         if not Result and then W.Menu_Bar /= null then
            Children := W.Menu_Bar.Get_Children;
            L := First (Children);
            while L /= Null_List loop
               Menu := Gtk_Menu_Item (Get_Data (L)).Get_Submenu;
               L := Next (L);
               if Menu /= null and then Menu.Is_Visible then
                  Free (Children);
                  Result := True;
                  return;
               end if;
            end loop;
            Free (Children);
         end if;
      end Internal;
   begin
      For_All_Open_Windows (App, Internal'Access);
      return Result;
   end Is_Any_Menu_Open;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
      (Win   : access Gtk_Window_Record'Class;
       Event : Gdk_Event;
       Data  : Delete_Event_Data) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      --  Call the On_Hide procedure which will actually save the size of the
      --  window, but only when the window is still visible, to avoid saving
      --  wrong sizes.

      if Win.Is_Visible then
         On_Hide (Win, Data => Data);
      end if;

      return False;
   end On_Delete;

   -------------
   -- On_Hide --
   -------------

   procedure On_Hide
      (Win   : access Gtk_Window_Record'Class;
       Data  : Delete_Event_Data)
   is
      Prop          : access Window_Size_Property;
      Width, Height : Gint;
   begin
      Win.Get_Size (Width, Height);

      if Active (Me) then
         Trace (Me, "Storing new size for window "
            & To_String (Data.Name)
            & " width=" & Width'Img
            & " height=" & Height'Img);
      end if;

      Prop := new Window_Size_Property'
         (Property_Record with
          Width  => Width,
          Height => Height);
      Set_Property
         (Data.Kernel,
          Key        => To_String (Data.Name),
          Name       => "size",
          Property   => Prop,
          Persistent => True);
   end On_Hide;

   -----------------------------------
   -- Set_Default_Size_From_History --
   -----------------------------------

   procedure Set_Default_Size_From_History
      (Win    : not null access Gtk_Window_Record'Class;
       Name   : String;
       Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Width, Height : Gint)
   is
      Prop  : Window_Size_Property;
      Found : Boolean;
      Data  : Delete_Event_Data;
   begin
      if not Active (Store_Window_Positions) then
         Trace (Me, "Default size for " & Name & " from default");
         Win.Set_Default_Size (Width, Height);
         return;
      end if;

      Get_Property
         (Property => Prop,
          Key      => Name,
          Name     => "size",
          Found    => Found);

      --  ??? Can we check that the size is small enough for the screen ? We
      --  do not know yet what screen the window will be displayed on (since
      --  we are calling before that).

      --  ??? Should we use Gtk.Window.Resize instead, so that this works even
      --  after the window has been created ? Then we would know the screen
      --  the window is on.

      if Found then
         Trace (Me, "Default size for " & Name & " from properties");
         Win.Set_Default_Size (Prop.Width, Prop.Height);
      else
         Trace (Me, "Default size for " & Name & " from default");
         Win.Set_Default_Size (Width, Height);
      end if;

      --  Start monitoring delete and hide events of the window so that we
      --  can store the size in the properties file.

      Data.Kernel := Kernel;
      Data.Name   := To_Unbounded_String (Name);
      Delete_Events.Connect
         (Win, Gtk.Widget.Signal_Delete_Event,
          Delete_Events.To_Marshaller (On_Delete'Access), Data);
      Hide_Events.Connect
         (Win, Gtk.Widget.Signal_Hide,
          Hide_Events.To_Marshaller (On_Hide'Access), Data);
   end Set_Default_Size_From_History;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Window_Size_Property;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;
   begin
      Value.Set_Field ("width", Image (Integer (Self.Width), Min_Width => 0));
      Value.Set_Field
        ("height", Image (Integer (Self.Height), Min_Width => 0));
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Window_Size_Property;
      Value : GNATCOLL.JSON.JSON_Value) is
   begin
      Self.Width  := Gint'Value (Value.Get ("width"));
      Self.Height := Gint'Value (Value.Get ("height"));
   end Load;

end GPS.Main_Window;
