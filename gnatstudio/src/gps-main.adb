------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2023, AdaCore                     --
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
with Ada.Exceptions;                   use Ada.Exceptions;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Text_IO;                      use Ada.Text_IO;

with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;
with GNATCOLL.Memory;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Scripts.Python;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;               use GNATCOLL.VFS_Utils;
with VSS.Strings.Conversions;

with Glib;
with Glib.Application;                 use Glib.Application;
with Glib.Main;
with Glib.Object;                      use Glib.Object;
with Glib.Option;                      use Glib.Option;
with Glib.Properties;                  use Glib.Properties;

with Gdk.Main;
with Gdk.Pixbuf;                       use Gdk.Pixbuf;

with Gtk;                              use Gtk;
with Gtk.Application;                  use Gtk.Application;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Icon_Theme;                   use Gtk.Icon_Theme;
with Gtk.Image;                        use Gtk.Image;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Overlay;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Style_Provider;               use Gtk.Style_Provider;
with Gtk.Widget;                       use Gtk.Widget;
with Gdk.Window;
with Gtk.Window;                       use Gtk.Window;

with Fontconfig;                       use Fontconfig;

with Gtkada.Application;               use Gtkada.Application;
with Gtkada.Dialogs;                   use Gtkada.Dialogs;
with Gtkada.MDI;                       use Gtkada.MDI;
with Gtkada.Style;

with Config;                           use Config;
with Default_Preferences;              use Default_Preferences;
with Default_Preferences.Assistants;   use Default_Preferences.Assistants;
with GPS.Callbacks;                    use GPS.Callbacks;
with GPS.Initialization;               use GPS.Initialization;
with GPS.Intl;                         use GPS.Intl;
with GPS.Globals;                      use GPS.Globals;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Actions;               use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;             use GPS.Kernel.Clipboard;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.Contexts;              use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;                use GPS.Kernel.Custom;
with GPS.Kernel.Custom.GUI;            use GPS.Kernel.Custom.GUI;
with GPS.Kernel.Entities;              use GPS.Kernel.Entities;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Shell;
with GPS.Kernel.Modules;               use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;            use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Preferences_Views;     use GPS.Kernel.Preferences_Views;
with GPS.Kernel.Project;               use GPS.Kernel.Project;
with GPS.Kernel.Remote;
with GPS.Kernel.Scripts;               use GPS.Kernel.Scripts;
with GPS.Kernel.Scripts.Hooks;
with GPS.Kernel.Style_Manager.Shell;
with GPS.Kernel.Task_Manager;          use GPS.Kernel.Task_Manager;
with GPS.Kernel.Xref;
with GPS.Stock_Icons;
with GPS.Main_Window;                  use GPS.Main_Window;
with GPS.Menu;
with GPS.Search.GUI;
with GUI_Utils;                        use GUI_Utils;
with OS_Utils;                         use OS_Utils;
with Projects;                         use Projects;
with Project_Templates.GPS;            use Project_Templates.GPS;
with Remote;                           use Remote;
with Src_Editor_Buffer;
with String_Utils;
with Welcome_Dialogs;                  use Welcome_Dialogs;
with Welcome_View;                     use Welcome_View;

--  Modules registered by GNAT Studio

with Ada_Module;
with Aliases_Module;
with Bookmark_Views;
with Browsers.Call_Graph;
with Browsers.Canvas;
with Browsers.Dependency_Items;
with Browsers.Elaborations;
with Browsers.Entities;
with Browsers.Scripts;
with Browsers.Projects;
with Revision_Views;
with Buffer_Views;
with Builder_Module;
with Builder_Facility_Module;
with Call_Graph_Views;
with Casing_Exceptions;
with Clipboard_Views;
with Code_Analysis_Module;
with CodePeer.Module;
with Codefix_Module;
with Command_Window;
with Cpp_Module;
with Custom_Module;
with GNAThub.Module;
with GNAThub.Module.Shell;
with External_Editor_Module;
with GNATTest_Module;
with GPS.Location_View;
with GVD_Module;
with GVD.Assembly_View;
with GVD.Breakpoints;
with GVD.Breakpoints_List;
with GVD.Call_Stack;
with GVD.Dialogs;
with GVD.Memory_View;
with GVD.Preferences;
with GVD.Types;
with GVD.Variables.View;
with GVD.Registers_View;
with DAP.Module;
with Help_Module;
with KeyManager_Module;
with KeyManager_Module.Macros;
with Toolchains_Module;
with Ada_Semantic_Tree_Module;
with LAL.Module;
with Language_Handlers.Assistants;
with Learn.Views;
with Log_File_Views;
with GPS.LSP_Module;
with Memory_Usage_Views.Module;
with Navigation_Module;
with Outline_View;
with Outline_View_Provider_Semantic_Trees;
with Project_Explorers;
with Project_Explorers_Files;
with Project_Properties;
with Project_Viewers;
with Python_Module;
with Refactoring_Module;
with Remote.Rsync;
with Remote_Module;
with Scenario_Views;
with Shell_Script;
with Socket_Module;
with Src_Editor_Module;
with Src_Editor_Module.Construct_Formatter;
with Switches_Chooser.Scripts;
with Toolchains_Editor;
with VCS2.Module;
with VFS_Module;
with Vdiff2_Module;
with Vsearch;
with Ada_Semantic_Tree.Lang;
with GPS.Traces;
with GPS.Valgrind;
with Serial_Ports_Views;
with Xref;

with DAP.Modules.Preferences;

procedure GPS.Main is

   use type Glib.Gint;

   Pid_Image  : constant String := String_Utils.Image (Get_Process_Id);
   Gtk_Errors : constant Trace_Handle := Create
     ("GPS.MAIN.GTK");

   Refactor_Trace         : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Refactor",
             GNATCOLL.Traces.On);
   Python_Trace           : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Python",
             GNATCOLL.Traces.On);
   Learn_Trace            : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Learn",
             GNATCOLL.Traces.On);
   Call_Graph_Trace       : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Call_Graph",
             GNATCOLL.Traces.On);
   Dependency_Trace       : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Dependency",
             GNATCOLL.Traces.On);
   Project_Browser_Trace  : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Project_Browser",
             GNATCOLL.Traces.On);
   Browsers_Trace         : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Browsers",
             GNATCOLL.Traces.On);
   Entities_Browser_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Entities_Browser",
             GNATCOLL.Traces.On);
   Revision_Views_Trace   : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Revision_Views",
             GNATCOLL.Traces.On);
   Aliases_Trace          : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Aliases",
             GNATCOLL.Traces.On);
   Project_Explorer_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Project_Explorer",
             GNATCOLL.Traces.On);
   Files_Explorer_Trace   : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Files_Explorer",
             GNATCOLL.Traces.On);
   VCS2_Trace             : constant Trace_Handle :=
     Create ("GPS.VCS.MODULE",
             GNATCOLL.Traces.On);
   External_Editor_Trace  : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_External_Editor",
             GNATCOLL.Traces.On);
   Custom_Trace           : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Custom",
             GNATCOLL.Traces.On);
   Project_Templates_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Project_Templates",
             GNATCOLL.Traces.On);
   Code_Analysis_Trace    : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Code_Analysis",
             GNATCOLL.Traces.On);
   GNAThub_Trace          : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_GNAThub",
             GNATCOLL.Traces.On);
   CodePeer_Trace         : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_CodePeer",
             GNATCOLL.Traces.On);
   Codefix_Trace          : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Codefix",
             GNATCOLL.Traces.On);
   Builder_Trace          : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Builder",
             GNATCOLL.Traces.On);
   GVD_Trace              : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_GVD",
             GNATCOLL.Traces.On);
   GNATTest_Trace         : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_GNATTest",
             GNATCOLL.Traces.On);
   Startup_Trace          : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Startup",
             GNATCOLL.Traces.On);
   VFS_Trace              : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_VFS",
             GNATCOLL.Traces.On);
   Help_Trace             : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Help",
             GNATCOLL.Traces.On);
   Scenario_View_Trace    : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_SCENARIO",
             GNATCOLL.Traces.On);
   Project_Viewer_Trace   : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Project_Viewer",
             GNATCOLL.Traces.On);
   Project_Properties_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Project_Properties",
             GNATCOLL.Traces.On);
   CPP_Trace              : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_CPP",
             GNATCOLL.Traces.On);
   Outline_View_Trace     : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Outline",
             GNATCOLL.Traces.On);
   Call_Graph_View_Trace  : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Call_Graph_View",
             GNATCOLL.Traces.On);
   Clipboard_View_Trace   : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Clipboard_Vview",
             GNATCOLL.Traces.On);
   Toolchains_Trace       : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Toolchains",
             GNATCOLL.Traces.On);
   Toolchains_Editor_Trace  : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Toolchains_Editor",
             GNATCOLL.Traces.On);
   Elaboration_Browser_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_Elaboration_Browser",
             GNATCOLL.Traces.On);

   Debugger_GDB_Trace : constant Trace_Handle :=
     Create ("MODULE.Debugger_GDB", GNATCOLL.Traces.Off);
   Debugger_GDB_Pretty_Printer_Trace : constant Trace_Handle :=
     Create ("MODULE.Debugger_GDB_Pretty_Printer", GNATCOLL.Traces.Off);
   --  for testing gvd with pretty printer on
   Debugger_GDB_MI_Trace : constant Trace_Handle :=
     Create ("MODULE.Debugger_GDB_MI", GNATCOLL.Traces.Off);
   Debugger_LLDB_Trace : constant Trace_Handle :=
     Create ("MODULE.Debugger_LLDB", GNATCOLL.Traces.Off);
   Debugger_DAP_Trace : constant Trace_Handle :=
     Create ("MODULE.Debugger_DAP", GNATCOLL.Traces.Off);

   --  If any of these debug handles is active, the correponding module
   --  is loaded.

   Timeout_Id                 : Glib.Main.G_Source_Id;
   pragma Unreferenced (Timeout_Id);

   Application_Class_Record : aliased Glib.Object.Ada_GObject_Class;
   --  A custom child of GtkApplication

   procedure Startup_Callback
     (Application : access Gapplication_Record'Class);
   --  Handler for the ::startup signal, emitted by the application

   procedure Activate_Callback
     (Application : access Gapplication_Record'Class);
   --  Handler for the ::activate signal, emitted by the application

   function On_GPS_Started return Boolean;
   --  Called when GNAT Studio is started and visible on the screen

   function Command_Line_Callback
     (Application  : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
   return Glib.Gint;
   --  Handler for the ::command-line signal, emitted by the application

   procedure File_Open_Callback
     (Application : Gtkada.Application.Gtkada_Application;
      Files       : Gtkada.Application.GFile_Array);
   --  Handler for the ::open signal, emitted by the application

   procedure Shutdown_Callback
     (Application : access Gapplication_Record'Class);

   procedure Set_Project_Name;
   --  Set the project name from the command line switch

   procedure Error_Message (Message : String; Save : Boolean);
   --  Display the "Fatal error" message

   procedure Display_Splash_Screen;
   --  Display the GNAT Studio splash screen

   procedure Load_CSS;
   --  Load the GNAT Studio global and local CSS files

   function Finish_Setup return Boolean;
   --  Finish the set up of GNAT Studio, while the main loop is running

   procedure Execute_Batch (Batch : String; As_File : Boolean);
   --  Execute a batch command (either loading the file Batch if As_File is
   --  true, or as a standard command otherwise).

   procedure Default_Gtk_Mer
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Called when an Ada callback raises an exception, to log it.

   procedure Load_Fonts (Kernel : Kernel_Handle);
   --  Load the fonts that ship by default with GNAT Studio

   procedure Trace_With_Python_Backtrace
    (Handle : not null access GNATCOLL.Traces.Trace_Handle_Record'Class;
     E      : Ada.Exceptions.Exception_Occurrence);
   --  Trace unexpected exception with Python backtrace when available.

   procedure Update_Splash_Progress_Label (Text : String);
   --  Update the splash screen's progress label, displaying the given text.

   ---------------------------------
   -- Trace_With_Python_Backtrace --
   ---------------------------------

   procedure Trace_With_Python_Backtrace
    (Handle : not null access GNATCOLL.Traces.Trace_Handle_Record'Class;
     E      : Ada.Exceptions.Exception_Occurrence)
   is
      PBT : constant String := GNATCOLL.Scripts.Python.Python_Backtrace;

   begin
      if PBT /= "" then
         Trace
           (Handle,
            E,
            "Unexpected exception: Python backtrace: " & ASCII.LF & PBT);

      else
         Trace (Handle, E, "Unexpected exception: ");
      end if;
   end Trace_With_Python_Backtrace;

   ----------------------------------
   -- Update_Splash_Progress_Label --
   ----------------------------------

   procedure Update_Splash_Progress_Label (Text : String)
   is
      Ignored : Boolean;
   begin
      --  The splash screen might not exist while running the testsuite or if
      --  the corresponding preference has been disabled.

      if Splash /= null and then Splash.Progress_Label /= null then
         Splash.Progress_Label.Set_Text (Text);

         --  Make sure that Gtk+ paints the new label text.
         Gdk.Main.Flush;
         while Gtk.Main.Events_Pending loop
            Ignored := Gtk.Main.Main_Iteration;
         end loop;
      end if;
   end Update_Splash_Progress_Label;

   ----------------------
   -- Startup_Callback --
   ----------------------

   procedure Startup_Callback
     (Application : access Gapplication_Record'Class)
   is
      pragma Unreferenced (Application);
   begin
      Gtk.Handlers.Set_On_Exception
        (Default_Gtk_Mer'Unrestricted_Access);

      OS_Utils.Install_Ctrl_C_Handler (Callbacks.Ctrl_C_Handler'Access);
   end Startup_Callback;

   -----------------------
   -- Activate_Callback --
   -----------------------

   procedure Activate_Callback
     (Application : access Gapplication_Record'Class)
   is
      pragma Unreferenced (Application);
   begin
      --  This callback is here to make GLib happy. Nothing has really to be
      --  done on the activate signal, as GNAT Studio allows multiple
      --  instances.
      Trace (Main_Trace, "GApplication Activated");
   end Activate_Callback;

   ----------------
   -- Load_Fonts --
   ----------------

   procedure Load_Fonts (Kernel : Kernel_Handle) is
      Files : File_Array_Access;

      Fonts_Dir : Virtual_File;
      Result    : Boolean;
   begin
      Fonts_Dir := Create_From_Dir (Get_Share_Dir (Kernel), "fonts");

      if not Fonts_Dir.Is_Directory then
         Trace (Main_Trace, "Not a directory: " & (+Fonts_Dir.Full_Name.all));
         return;
      end if;

      Files := Fonts_Dir.Read_Dir (Files_Only);

      for F in Files'Range loop
         declare
            File : constant String := +Files (F).Full_Name.all;
            Ext  : String (1 .. 4);
         begin
            if File'Length > 4 then
               Ext := File (File'Last - 3 .. File'Last);
               if Ext = ".ttf"
                 or else Ext = ".otf"
                 or else Ext = ".ttc"
               then
                  Trace (Main_Trace, "Adding font: " & File);
                  Result := App_Font_Add_File (File);
                  if not Result then
                     Insert (Kernel, -"Could not add font: " & File);
                  end if;
               end if;
            end if;
         end;
      end loop;

      Unchecked_Free (Files);

   exception
      when E : others =>
         Trace (Main_Trace, E);
   end Load_Fonts;

   ---------------------------
   -- Command_Line_Callback --
   ---------------------------

   function Command_Line_Callback
     (Application  : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Glib.Gint
   is
      App : constant GPS_Application := GPS_Application (Application);
      Tmp, Ignored : Boolean;
      pragma Unreferenced (Command_Line, Tmp);

   begin
      --  Create the kernel and prepare the menu model

      if GPS_Command_Line.Do_Exit then
         App.Quit;
         return 0;
      end if;

      Gtk_New
        (Handle           => App.Kernel,
         Application      => App,
         Home_Dir         => GNATStudio_Home_Dir,
         Prefix_Directory => Prefix_Dir,
         Log_Dir          => GPS_Log_Dir);

      --  Display the splash screen, if needed, while we continue loading
      Display_Splash_Screen;

      Update_Splash_Progress_Label ("Loading preferences...");
      Create_MDI_Preferences (App.Kernel);

      --  Load the fonts

      Update_Splash_Progress_Label ("Loading fonts...");
      Load_Fonts (App.Kernel);

      --  Register the stock icons

      Update_Splash_Progress_Label ("Loading icons...");
      GPS.Stock_Icons.Register_Stock_Icons (App.Kernel, Prefix_Dir);

      --  Finally create the main window, and setup the project

      Update_Splash_Progress_Label ("Creating the main window...");
      GPS.Main_Window.Gtk_New (GPS_Main, App);
      GPS_Main.Kernel.Set_Ignore_Saved_Scenario_Values (Ignore_Saved_Values);

      App.Kernel.Set_Original_Environment (Env);

      Set_Project_Name;

      if Is_Regular_File
        (Create_From_Dir (Prefix_Dir, "share/gnatstudio/gps-pro.txt"))
      then
         GPS_Main.Public_Version := False;
      end if;

      Update_Splash_Progress_Label ("Registering menus...");
      GPS.Menu.Register_Common_Menus (App.Kernel);

      Kernel_Callback.Connect
        (Get_MDI (App.Kernel), Signal_Child_Title_Changed,
         Title_Changed'Access, App.Kernel);

      --  Load CSS must be loaded *after* the call to GPS.Main_Window.Gtk_New
      --  above, since that call loads the theme (Adwaita) and the CSS file
      --  may refer to the theme, for instance for calls such as
      --    @define-color my_color shade(@theme_bg_color, 1.10);
      Load_CSS;

      Tmp := Finish_Setup;

      return 0;
   end Command_Line_Callback;

   ------------------------
   -- File_Open_Callback --
   ------------------------

   procedure File_Open_Callback
     (Application : Gtkada.Application.Gtkada_Application;
      Files       : Gtkada.Application.GFile_Array)
   is
      pragma Unreferenced (Application);
      Started : constant Boolean := GPS_Main /= null;

   begin
      if Started then
         Deiconify (GPS_Main);
      end if;

      for File of Files loop
         declare
            Path : constant Glib.UTF8_String :=
                     Gtkada.Application.Get_Path (File);
            File : constant GNATCOLL.VFS.Virtual_File :=
                     GNATCOLL.VFS.Create
                       (GNATCOLL.VFS_Utils.Normalize_Pathname
                          (+Path,
                           Resolve_Links =>
                              not Preferences.Trusted_Mode.Get_Pref));
         begin
            if File_Extension (File) = Project_File_Extension then
               Trace
                 (Main_Trace, "... opening project " &
                    Display_Full_Name (File));

               if not Started then
                  Project_Name := File;
               else
                  Load_Project (GPS_Main.Kernel, File);
               end if;

            else
               Trace
                 (Main_Trace, "... opening file " &
                    Display_Full_Name (File));

               --  Else, this is handled at the command line level
               if not Started then
                  Files_To_Open.Append
                    (File_To_Open'
                       (File         => To_Unbounded_String (+File.Full_Name),
                        Line         => 1,
                        From_Project => False));
               else
                  Open_File_Action_Hook.Run
                    (GPS_Main.Kernel,
                     File            => File,
                     Project         => No_Project,
                     New_File        => False,
                     Is_Load_Desktop => True);
               end if;
            end if;
         end;
      end loop;

   exception
      when E : others =>
         Trace_With_Python_Backtrace (Main_Trace, E);
   end File_Open_Callback;

   -----------------------
   -- Shutdown_Callback --
   -----------------------

   procedure Shutdown_Callback
     (Application : access Gapplication_Record'Class)
   is
      use type GVD.Types.Debugger_Type;

      Kernel  : constant Kernel_Handle := GPS_Application (Application).Kernel;
      Log_File : Virtual_File;
      Pid_File : Virtual_File;
      Project  : Project_Type;
      Success  : Boolean;

   begin
      --  At this stage, the GUI has already been destroyed, so we must not
      --  reference GPS_Main.

      if Kernel = null then
         return;
      end if;

      Increase_Indent (Main_Trace, "Shutdown");

      Log_File := Create_From_Dir (Get_Home_Dir (Kernel), +"log.txt");
      Pid_File := Create_From_Dir
        (Get_Home_Dir (Kernel), +("log." & Pid_Image & ".txt"));
      Project  := Get_Project (Kernel);

      Set_Destruction_Flag (Kernel, True);

      --  We want to close the debuggers first, to avoid saving debugger
      --  consoles in the desktop.

      if GVD.Preferences.Debugger_Kind.Get_Pref = GVD.Types.DAP then
         DAP.Module.Terminate_Debuggers;
      else
         GVD_Module.Debug_Terminate (Kernel);
      end if;

      Get_Messages_Container (Kernel).Save;
      Get_Messages_Container (Kernel).Clear;

      if Get_Registry (Kernel).Tree.Status = Default then
         Trace (Main_Trace, "Remove default project on disk, no longer used");
         Delete (Project_Path (Project), Success);
      end if;

      Destroy (Kernel);
      GPS_Main := null;
      GNATCOLL.Traces.Finalize;

      --  In case of a normal exit, rename log.<pid>.txt as log.txt to avoid
      --  generating a new log file for each session; this way we still
      --  keep the log file in case of post mortem analysis.
      --  In case of unexpected exit, keep the log file under its original
      --  name, so that it does not get erased by the next session and can
      --  be reported.

      if not Unexpected_Exception
        and then Is_Regular_File (Pid_File)
      then
         Delete (Log_File, Success);
         Rename (Pid_File, Log_File, Success);
      end if;

      GPS_Command_Line.Context.Free;
      Free (Startup_Dir);
      Free (Batch_File);
      Free (Batch_Script);
      Free (Tools_Host);
      Free (GPS.Globals.Target);
      Free (Protocol);
      Free (Passed_Project_Name);
      Free (Program_Args);

      if Memory_Monitor then
         GNATCOLL.Memory.Dump
           (Size   => Memory_Stack_Depth,
            Report => GNATCOLL.Memory.All_Reports);
      end if;
   end Shutdown_Callback;

   --------------------
   -- On_GPS_Started --
   --------------------

   function On_GPS_Started return Boolean is
   begin
      Gps_Started_Hook.Run (GPS_Main.Kernel);

      --  Load the custom after running the 'gps_started' to be sure that
      --  actions that get registered while running this hook are available.
      --  This also ensures that the user key shortcuts will override
      --  everything else set so far.

      KeyManager_Module.Load_Custom_Keys (GPS_Main.Kernel);

      --  A number of actions are created in reaction to the hook above:
      --  if there is a menu described in menus.xml corresponding to such
      --  an action, this menu will remain greyed out until the first context
      --  change. We force a context refresh here to refresh these menus.
      Refresh_Context (GPS_Main.Kernel);

      return False;
   end On_GPS_Started;

   ----------------------
   -- Set_Project_Name --
   ----------------------

   procedure Set_Project_Name is
   begin
      if Passed_Project_Name /= null then
         Project_Name :=
           Create
             (Normalize_Pathname
                  (Filesystem_String (Passed_Project_Name.all),
                   Resolve_Links =>
                   not GPS.Kernel.Preferences.Trusted_Mode.Get_Pref));

         if not Is_Regular_File (Project_Name) then
            if Is_Regular_File
              (+(Full_Name (Project_Name) & Project_File_Extension))
            then
               Project_Name := Create
                 (Normalize_Pathname
                    (Full_Name (Project_Name) & Project_File_Extension,
                     Resolve_Links =>
                     not GPS.Kernel.Preferences.Trusted_Mode.Get_Pref));
               Trace
                 (Main_Trace, "Found project: " &
                    Display_Full_Name (Project_Name));
            else
               --  Keep Project_Name even if it is invalid, we will look
               --  for it later on the project path, but the latter is
               --  not known yet at this point
               if Equal (File_Extension (Project_Name),
                         Project_File_Extension)
               then
                  Project_Name :=
                 Create_From_Base (Base_Name => +Passed_Project_Name.all);
               else
                  Project_Name :=
                    Create_From_Base
                      (Base_Name =>
                       +Passed_Project_Name.all & Project_File_Extension);
               end if;

               Trace
                 (Main_Trace, "Project not found in current dir: "
                  & Project_Name.Display_Base_Name);
            end if;
         else
            Trace (Main_Trace, "Found project: " &
                     Display_Full_Name (Project_Name));
         end if;

         Free (Passed_Project_Name);
      else
         --  If only a file was passed in the command line and this is a
         --  project file => load it
         if Integer (Files_To_Open.Length) = 1 then
            declare
               Name : constant String       :=
                 To_String (Files_To_Open.First_Element.File);
               File : constant Virtual_File :=
                 Create
                   (Normalize_Pathname
                      (Filesystem_String (Name),
                       Resolve_Links =>
                          not GPS.Kernel.Preferences.Trusted_Mode.Get_Pref));
            begin
               if Equal (File_Extension (File), Project_File_Extension) then
                  Project_Name := File;
                  --  We are loading the project, don't open it in the editor
                  Files_To_Open.Clear;
               end if;
            end;
         end if;
      end if;
   end Set_Project_Name;

   ---------------------------
   -- Display_Splash_Screen --
   ---------------------------

   procedure Display_Splash_Screen is
      File   : constant Virtual_File :=
        Create_From_Dir
          (Prefix_Dir, "share/gnatstudio/gnatstudio-splash.png");
      Image  : Gtk_Image;
      Ignored : Boolean;
      Overlay : Gtk.Overlay.Gtk_Overlay;
   begin
      if not Hide_GPS
        and then Splash_Screen.Get_Pref
        and then File.Is_Regular_File
      then
         Splash := new GPS_Splash_Screen_Record;
         Initialize (Splash, Window_Popup);

         Gtk.Overlay.Gtk_New (Overlay);
         Splash.Add (Overlay);

         Splash.Set_Type_Hint (Gdk.Window.Window_Type_Hint_Splashscreen);
         Splash.Set_Hexpand (False);
         Splash.Set_Vexpand (False);
         Set_Property (Splash, Decorated_Property, False);
         Set_Position (Splash, Win_Pos_Center);
         Gtk_New (Image, Filename => +File.Full_Name);
         Overlay.Add (Image);

         Gtk_New (Splash.Progress_Label, "Initializing GNAT Studio...");
         Splash.Progress_Label.Set_Alignment (0.5, 0.8);
         Overlay.Add_Overlay (Splash.Progress_Label);

         Splash.Show_All;

         --  The following is required for the splash screen to be fully
         --  painted before the startup process continues.
         Splash.Show_Now;
         Splash.Set_Keep_Above (False);
         Image.Show_Now;
         Gdk.Main.Flush;
         while Gtk.Main.Events_Pending loop
            Ignored := Gtk.Main.Main_Iteration;
         end loop;
      end if;
   end Display_Splash_Screen;

   --------------
   -- Load_CSS --
   --------------

   procedure Load_CSS is
      Global : constant Virtual_File :=
        Prefix_Dir.Create_From_Dir ("share/gnatstudio/gps.css");
      Local  : constant Virtual_File :=
        GNATStudio_Home_Dir.Create_From_Dir ("gps.css");
   begin
      if Global.Is_Regular_File then
         Trace (Main_Trace, "Loading " & Global.Display_Full_Name);
         Gtkada.Style.Load_Css_File
           (Global.Display_Full_Name, Put_Line'Access,
            Priority_User);
      end if;

      if Local.Is_Regular_File then
         Trace (Main_Trace, "Loading " & Local.Display_Full_Name);
         Gtkada.Style.Load_Css_File
           (Local.Display_Full_Name, Put_Line'Access,
            Priority_User);
      end if;
   end Load_CSS;

   ------------------
   -- Finish_Setup --
   ------------------

   function Finish_Setup return Boolean is

      use type GVD.Types.Debugger_Type;

      Auto_Load_Project : Boolean := True;
      Empty_Project     : Boolean := False;
      File_Opened       : Boolean := False;
      Idle_Id           : Glib.Main.G_Source_Id;
      Project           : Project_Type;
      Icon              : Gdk_Pixbuf;
      pragma Unreferenced (Idle_Id);

      procedure Setup_Debug (Empty_Project : out Boolean);
      --  Load appropriate debugger project and set up debugger-related
      --  properties.
      --  Empty_Project is set to True if an empty project has been loaded
      --  for that purpose (i.e: when no -P option has been given).

      function Setup_Project return Boolean;
      --  When no project has been specified explicitely by the user,
      --  look for a project on the current directory, or use the welcome
      --  dialog
      --  Return False if set up was aborted and GNAT Studio should exit.

      procedure Load_Sources;
      --  Load all the source files given on the command line

      -----------------
      -- Setup_Debug --
      -----------------

      procedure Setup_Debug (Empty_Project : out Boolean) is
      begin
         Empty_Project := False;
         File_Opened := True;
         Auto_Load_Project := False;

         if Project_Name /= No_File and then Is_Regular_File (Project_Name)
         then
            --  Do not clear to keep the welcome message on kernel's console
            Load_Project (GPS_Main.Kernel, Project_Name, Clear => False);
            Project := Get_Project (GPS_Main.Kernel);
         else
            Empty_Project := True;
            Load_Empty_Project (GPS_Main.Kernel);
            Project := Get_Project (GPS_Main.Kernel);
            Get_Registry (GPS_Main.Kernel).Tree.Set_Status (From_Executable);
            Xref.Project_Changed (GPS_Main.Kernel.Databases);
         end if;

         --  Project will be overridden when the executable is loaded
         Load_Sources;

         --  Attempt to set the IDE'Program_Host and IDE'Communication_Protocol
         --  project attributes if specified in the command line.
         --  If the project is not editable, just catch the exception and
         --  pursue without setting the attributes.
         begin
            if GPS.Globals.Target /= null then
               Project.Set_Attribute
                 (Scenario  => All_Scenarios,
                  Attribute => Program_Host_Attribute,
                  Value     => GPS.Globals.Target.all);
            end if;

            if Protocol /= null then
               Project.Set_Attribute
                 (Scenario  => All_Scenarios,
                  Attribute => Protocol_Attribute,
                  Value     => Protocol.all);

            end if;

         exception
            when GNATCOLL.Projects.Project_Not_Editable =>
            Trace
              (Main_Trace,
               "Can't edit project attributes needed for "
               & "--debug and --target options: '"
               & Project.Name
               & "'' is not editable");
         end;

         --  No project was specified on the command line: set the default
         --  languages of the empty project (Ada and C++ to cover most of
         --  cases).
         if Empty_Project then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Languages_Attribute,
               Values    =>
                 (new String'("ada"), new String'("c"), new String'("c++")));
         end if;

         Project.Set_Modified (False);

         Recompute_View (GPS_Main.Kernel);
      end Setup_Debug;

      -------------------
      -- Setup_Project --
      -------------------

      function Setup_Project return Boolean is
         Current : constant Virtual_File := Get_Current_Dir;
         Files   : File_Array_Access;
      begin
         Auto_Load_Project := False;
         Files := Read_Dir (Current, Files_Only);

         for J in Files'Range loop
            if Equal (File_Extension (Files (J)), Project_File_Extension) then
               if Project_Name = No_File then
                  Auto_Load_Project := True;
                  Project_Name := Files (J);
               else
                  Auto_Load_Project := False;
                  exit;
               end if;
            end if;
         end loop;

         Unchecked_Free (Files);

         --  If only one project file was found in the current directory, do
         --  not open the welcome dialog. Likewise if we are loading a script,
         --  or if source files have been specified on the command line.

         if Auto_Load_Project then
            return True;
         end if;

         Load_Sources;

         if File_Opened then
            return True;
         end if;

         if Batch_File /= null then
            Load_Default_Project
              (GPS_Main.Kernel, Get_Current_Dir,
               Load_Default_Desktop => True,
               Clear                => False);
            return True;
         end if;

         --  Remove the splash screen, since it conflicts with the
         --  welcome dialog.

         if Splash /= null then
            Destroy (Splash);
            Splash := null;
         end if;

         --  Display the welcome dialog if no project has been automatically
         --  loaded.

         declare
            Actions : constant Welcome_Dialog_Action_Array :=
              (1 =>
                 Create
                   (Callback  => Display_Project_Templates_Assistant'Access,
                    Label     => "Create new project",
                    Icon_Name => "gps-add-symbolic"),
               2 =>
                 Create
                   (Callback  => Display_Open_Project_Dialog'Access,
                    Label     => "Open project",
                    Icon_Name => "gps-open-file-symbolic"),
               3 =>
                 Create
                   (Callback  => Display_Open_Alire_Crate_Dialog'Access,
                    Label     => "Open Alire crate",
                    Icon_Name => "alr-logo-symbolic"),
               4 =>
                 Create
                   (Callback  => Load_Default_Project'Access,
                    Label     => "Start with default project",
                    Icon_Name => "gps-run-symbolic"));
         begin
            case Display_Welcome_Dialog
                   (GPS_Main.Kernel,
                    Actions => (if Is_Alire_Available (GPS_Main.Kernel) then
                     Actions else Actions (1 .. 2) & Actions (Actions'Last)))
            is
               when Quit_GPS =>
                  GPS_Main.Application.Quit;
                  return False;

               when Project_Loaded =>
                  --  Desktop was already loaded when the project itself
                  --  was loaded.
                  return True;
            end case;
         end;
      exception
         when E : others =>
            Unexpected_Exception := True;
            Trace (Main_Trace, E);
            if not Hide_GPS then
               Error_Message
                 (Message => "Unexpected fatal error during project load.",
                  Save    => False);
            end if;

            GPS_Main.Application.Quit;

            return False;
      end Setup_Project;

      ------------------
      -- Load_Sources --
      ------------------

      procedure Load_Sources is
      begin
         if not Files_To_Open.Is_Empty then
            --  If no project has been loaded yet, load a default project
            --  and desktop before opening source editors.

            if not Auto_Load_Project and then not File_Opened then
               Load_Default_Project
                 (GPS_Main.Kernel, Get_Current_Dir,
                  Load_Default_Desktop => True,
                  Clear                => False);
            end if;

            for File_Item of Files_To_Open loop
               File_Opened := True;
               Open_File_Action_Hook.Run
                 (GPS_Main.Kernel,
                  File            =>
                    Create
                      (Normalize_To_OS_Case (+To_String (File_Item.File)),
                       GPS_Main.Kernel,
                       Use_Source_Path => File_Item.From_Project,
                       Use_Object_Path => False),
                  Focus           => True,
                  Project         => No_Project,
                  Line            => File_Item.Line,
                  Is_Load_Desktop => True);
            end loop;

            --  Load a dummy project, in case the wizard needs to be launched

            if not Auto_Load_Project and then not File_Opened then
               Load_Empty_Project (GPS_Main.Kernel);
            end if;
         end if;
      end Load_Sources;

   begin
      --  Freeze the preferences to avoid overwriting preferences.xml too early
      Freeze_Preferences (GPS_Main.Kernel);

      Update_Splash_Progress_Label ("Loading modules...");

      --  Register the Learn module and the associated view
      Learn.Register_Module (GPS_Main.Kernel);

      --  Register the actions learn provider
      GPS.Kernel.Actions.Register_Actions_Learn_Provider (GPS_Main.Kernel);

      if Active (Learn_Trace) then
         --  Register the Learn view once we know that all the learn providers
         --  have been registered.
         Learn.Views.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (CodePeer_Trace) then
         CodePeer.Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Register the default filters, so that other modules can create
      --  contextual menus

      GPS.Kernel.Contexts.Register_Default_Filters (GPS_Main.Kernel);

      --  Register this module first, in case someone needs to print a message
      --  in the console right away

      GPS.Kernel.Console.Register_Module (GPS_Main.Kernel);

      GPS.Kernel.MDI.Register_Module (GPS_Main.Kernel);

      --  Register the locations view before all the modules that register a
      --  highlighting category. Otherwise, when loading the desktop, the
      --  locations view might create highligthting with categories that don't
      --  exist.

      GPS.Location_View.Register_Module (GPS_Main.Kernel);

      --  Register all modules (scripting languages must be registered first)

      Shell_Script.Register_Module (GPS_Main.Kernel);

      if Active (Python_Trace) then
         Python_Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Register the standard hooks. Other modules were able to connect to
      --  these earlier anyway, but these add shell commands, and therefore
      --  must be loaded after the script modules

      GPS.Kernel.Hooks.Register_Hooks (GPS_Main.Kernel);
      GPS.Kernel.Scripts.Hooks.Register_Module (GPS_Main.Kernel);
      GPS.Valgrind.Register_Module (GPS_Main.Kernel);

      Register_Default_Script_Commands (GPS_Main.Kernel);

      --  Enable the old xrefs engine relying on gnatinspect if the LSP support
      --  for Ada is not enabled.

      if not GPS.LSP_Module.LSP_Ada_Support_Is_Active then
         GPS.Kernel.Xref.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Kernel.Messages.Register_Module (GPS_Main.Kernel);
      GPS.Kernel.Messages.Shell.Register_Commands (GPS_Main.Kernel);
      GPS.Kernel.Style_Manager.Shell.Register_Commands (GPS_Main.Kernel);

      if Remote_Module.Remote_Module_Trace.Is_Active then
         --  Register this very early so that other modules can access remote
         --  files. Note that we need the scripting capabilities to be
         --  initialized before the remote mode.
         Remote_Module.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Kernel.Remote.Register_Module (GPS_Main.Kernel);
      Remote.Rsync.Register_Module (GPS_Main.Kernel);

      GPS.Location_View.Register_Commands (GPS_Main.Kernel);
      GPS.Kernel.Clipboard.Register_Module (GPS_Main.Kernel);

      --  We then must register the keymanager, so that other modules can
      --  register their keys

      KeyManager_Module.Register_Module (GPS_Main.Kernel);
      KeyManager_Module.Macros.Register_Module (GPS_Main.Kernel);
      Command_Window.Register_Module (GPS_Main.Kernel);
      Register_Keys (GPS_Main);

      Vsearch.Register_Module (GPS_Main.Kernel);

      Welcome_View.Register_Module (GPS_Main.Kernel);

      if Active (Help_Trace) then
         Help_Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Initialize the aliases module before the src_editor, since the
      --  latter registers special expansion functions

      if Active (Aliases_Trace) then
         Aliases_Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Initialize Src_Editor_Module early, since many modules depend
      --  on the editor API, and also during finalization, modules may need
      --  to access marks that are handled by this module.

      Src_Editor_Module.Register_Module (GPS_Main.Kernel);
      Src_Editor_Module.Construct_Formatter.Register_Module (GPS_Main.Kernel);

      --  Initialize the outline view

      if Active (Outline_View_Trace) then
         Outline_View.Register_Module (GPS_Main.Kernel);
         Outline_View_Provider_Semantic_Trees.Register_Module
           (GPS_Main.Kernel);
      end if;

      if Active (Call_Graph_View_Trace) then
         Call_Graph_Views.Register_Module (GPS_Main.Kernel);
      end if;

      --  Initialize LSP module

      GPS.LSP_Module.Register_Module (GPS_Main.Kernel);

      --  Create the Formatter provider preferences

      Src_Editor_Module.Create_Preferences (GPS_Main.Kernel);

      --  Initialize the ada semantic tree module

      Ada_Semantic_Tree_Module.Register_Module
        (GPS_Main.Kernel,
         Create_From_Dir (Prefix_Dir, "share/gnatstudio/predefined_ada.xml"));

      GPS.Kernel.Entities.Register_Module (GPS_Main.Kernel);

      Browsers.Canvas.Register_Actions (GPS_Main.Kernel);

      if Active (Call_Graph_Trace)
        and then not GPS.LSP_Module.LSP_Ada_Support_Is_Active
      then
         Browsers.Call_Graph.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Clipboard_View_Trace) then
         Clipboard_Views.Register_Module (GPS_Main.Kernel);
      end if;

      Serial_Ports_Views.Register_Module (GPS_Main.Kernel);

      if Active (Dependency_Trace) then
         Browsers.Dependency_Items.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Files_Explorer_Trace) then
         Project_Explorers_Files.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Browsers_Trace) then
         Browsers.Scripts.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Browser_Trace) then
         Browsers.Projects.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Entities_Browser_Trace)
        and then not GPS.LSP_Module.LSP_Ada_Support_Is_Active
      then
         Browsers.Entities.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Elaboration_Browser_Trace) then
         Browsers.Elaborations.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Revision_Views_Trace) then
         Revision_Views.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Viewer_Trace) then
         Project_Viewers.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Explorer_Trace) then
         Project_Explorers.Register_Module (GPS_Main.Kernel);
      end if;

      Project_Properties.Register_Module_Reader (GPS_Main.Kernel);
      if Active (Project_Properties_Trace) then
         Project_Properties.Register_Module_Writer (GPS_Main.Kernel);
      end if;

      if Active (GNATTest_Trace) then
         GNATTest_Module.Register_Module (GPS_Main.Kernel);
      end if;

      KeyManager_Module.Register_Key_Menu (GPS_Main.Kernel);

      if Active (External_Editor_Trace) then
         External_Editor_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Builder_Trace) then
         Builder_Facility_Module.Register_Module (GPS_Main.Kernel);
         Builder_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Toolchains_Trace) then
         Toolchains_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Toolchains_Editor_Trace) then
         Toolchains_Editor.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (GVD_Trace) then
         GVD.Preferences.Debugger_Kind :=
           GVD.Preferences.Debugger_Kind_Preferences.Create
             (Manager => Get_Preferences (GPS_Main.Kernel),
              Name    => "GPS6-Debugger-Debugger-Kind",
              Label   => -"Debugger kind",
              Path    => "Debugger:General",
              Doc     =>
                -("Prefered kind of debugger spawned by GNAT Studio."
                  & " Project file settings may override this."),
              Default => GVD.Types.Gdb_MI);

         --  MODULE.Debugger_* traces that are used in testing
         if Active (Debugger_GDB_Trace)
           or else Active (Debugger_GDB_Pretty_Printer_Trace)
         then
            GVD.Preferences.Debugger_Kind.Set_Pref
              (GPS_Main.Kernel.Get_Preferences, "Gdb");

         elsif Active (Debugger_GDB_MI_Trace) then
            GVD.Preferences.Debugger_Kind.Set_Pref
              (GPS_Main.Kernel.Get_Preferences, "Gdb_MI");

         elsif Active (Debugger_LLDB_Trace) then
            GVD.Preferences.Debugger_Kind.Set_Pref
              (GPS_Main.Kernel.Get_Preferences, "LLDB");

         elsif Active (Debugger_DAP_Trace) then
            GVD.Preferences.Debugger_Kind.Set_Pref
              (GPS_Main.Kernel.Get_Preferences, "DAP");
         end if;

         if not Create ("GPS.DEBUGGING.DAP_MODULE").Is_Active then
            GVD.Preferences.Debugger_Kind.Hide (GVD.Types.DAP);
            if GVD.Preferences.Debugger_Kind.Get_Pref = GVD.Types.DAP then
               GVD.Preferences.Debugger_Kind.Set_Pref
                 (GPS_Main.Kernel.Get_Preferences, "Gdb_MI");
            end if;
         end if;

         if GVD.Preferences.Debugger_Kind.Get_Pref = GVD.Types.DAP then
            DAP.Module.Register_Module (GPS_Main.Kernel);

            declare
               use VSS.Strings;
            begin
               if DAP_GDB_Adapter /= Empty_Virtual_String then
                  DAP.Modules.Preferences.DAP_Adapter.Set_Pref
                    (GPS_Main.Kernel.Get_Preferences,
                     VSS.Strings.Conversions.To_UTF_8_String
                       (DAP_GDB_Adapter));
               end if;
            end;

         else
            GVD_Module.Register_Module (GPS_Main.Kernel);
            GVD.Breakpoints_List.Register_Module (GPS_Main.Kernel);
            GVD.Variables.View.Register_Module (GPS_Main.Kernel);
            GVD.Dialogs.Register_Module (GPS_Main.Kernel);
            GVD.Assembly_View.Register_Module (GPS_Main.Kernel);
            GVD.Breakpoints.Register_Module (GPS_Main.Kernel);
            GVD.Call_Stack.Register_Module (GPS_Main.Kernel);
            GVD.Memory_View.Register_Module (GPS_Main.Kernel);
            GVD.Registers_View.Register_Module (GPS_Main.Kernel);
         end if;
      end if;

      Vdiff2_Module.Register_Module (GPS_Main.Kernel);

      if Active (Scenario_View_Trace) then
         Scenario_Views.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Search.GUI.Register_Module (GPS_Main.Kernel);

      GPS.Main_Window.Setup_Perspective_Selector (GPS_Main);

      GPS.Kernel.Task_Manager.Register_Module (GPS_Main.Kernel);

      if Active (VCS2_Trace) then
         VCS2.Module.Register_Module (GPS_Main.Kernel);
         GPS.Main_Window.Setup_VCS_Selector (GPS_Main);
      end if;

      if Active (VFS_Trace) then
         VFS_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Codefix_Trace) then
         Codefix_Module.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Kernel.Preferences.Register_Module (GPS_Main.Kernel);
      GPS.Kernel.Preferences_Views.Register_Module (GPS_Main.Kernel);

      if Active (Custom_Trace) then
         Custom_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Templates_Trace) then
         Project_Templates.GPS.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Refactor_Trace) then
         Refactoring_Module.Register_Module
           (GPS_Main.Kernel, GPS.LSP_Module.LSP_Ada_Support_Is_Active);
      end if;

      if Active (Code_Analysis_Trace) then
         Code_Analysis_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (GNAThub_Trace) then
         GNAThub.Module.Register_Module (GPS_Main.Kernel);
         GNAThub.Module.Shell.Register_Commands (GPS_Main.Kernel);
      end if;

      --  Register the supported languages and their associated LI handlers

      Ada_Module.Register_Module (GPS_Main.Kernel);

      LAL.Module.Register_Module
        (GPS_Main.Kernel,
         (LAL.Use_LAL_In_Editor    =>
            Use_LAL_In_Outline.Get_Pref
            or else Use_LAL_In_Indent.Get_Pref
            or else Use_External_Highlighting.Get_Pref
                    = GPS.Kernel.Preferences.LAL,
          LAL.Use_LAL_In_Outline   => Use_LAL_In_Outline.Get_Pref,
          LAL.Use_LAL_In_Shell     => Use_LAL_In_Shell.Get_Pref,
          LAL.Use_LAL_In_Info      => Use_LAL_In_Info.Get_Pref,
          LAL.Use_LAL_In_GNATHUB   => Use_LAL_In_GNATHUB.Get_Pref,
          LAL.Use_LAL_In_COV       => Use_LAL_In_COV.Get_Pref,
          LAL.Use_LAL_In_Indent    => Use_LAL_In_Indent.Get_Pref,
          LAL.Use_LAL_In_Highlight =>
            Use_External_Highlighting.Get_Pref = GPS.Kernel.Preferences.LAL),
         Legacy => Ada_Semantic_Tree.Lang.Ada_Tree_Lang);

      if Active (CPP_Trace) then
         Cpp_Module.Register_Module (GPS_Main.Kernel);
      end if;

      Casing_Exceptions.Register_Module (GPS_Main.Kernel);

      Switches_Chooser.Scripts.Register_Module (GPS_Main.Kernel);

      --  Load these last, since this requires the collaboration of other
      --  modules

      Bookmark_Views.Register_Module (GPS_Main.Kernel);

      Buffer_Views.Register_Module (GPS_Main.Kernel);

      Language_Handlers.Assistants.Register_Module (GPS_Main.Kernel);

      Memory_Usage_Views.Module.Register_Module (GPS_Main.Kernel);

      GPS.Kernel.Custom.Parse_Startup_Scripts_List (GPS_Main.Kernel);

      --  Load system files.
      --  This must be done before loading the Navigation module, since that
      --  module relies on icons defined in custom files.

      if Active (Custom_Trace) then
         Load_System_Custom_Files (GPS_Main.Kernel);
      end if;

      --  Load the default key theme before the python plugins, so that the
      --  latter can override

      KeyManager_Module.Load_Key_Theme (GPS_Main.Kernel);

      --  Load the customization files before loading the actual projects,
      --  so that the usual hooks are taken into account right from the
      --  beginning

      Update_Splash_Progress_Label ("Loading python plug-ins...");

      if Active (Python_Trace) then
         Python_Module.Load_System_Python_Startup_Files (GPS_Main.Kernel);
      end if;

      --  The system wide custom files have been loaded, load the user's ones

      if Active (Custom_Trace) then
         Load_User_Custom_Files (GPS_Main.Kernel);
      end if;

      if Active (Python_Trace) then
         Python_Module.Load_User_Python_Startup_Files (GPS_Main.Kernel);
      end if;

      --  Register the GPS.Kernel.Custom.GUI afer loading both system and user
      --  custom files so that each custom file is known before registering all
      --  the 'Plugins' subpages in the preferences editor dialog.

      if Active (Startup_Trace) then
         GPS.Kernel.Custom.GUI.Register_Module (GPS_Main.Kernel);
      end if;

      Navigation_Module.Register_Module (GPS_Main.Kernel);

      if Server_Mode then
         Socket_Module.Register_Module (GPS_Main.Kernel, Port_Number);
      end if;

      --  Load preferences, but only after loading custom files, to make sure
      --  the themes loaded at startup are still overridden by the user's
      --  local choices. Note that the preferences have already been loaded
      --  once before, to take into account the splash screen pref for instance

      GPS.Traces.Register_Module (GPS_Main.Kernel);

      Load_Preferences (GPS_Main.Kernel);
      Thaw_Preferences (GPS_Main.Kernel);

      Update_Splash_Progress_Label ("Loading project...");

      --  Show the preferences assistant dialog if the user don't have any
      --  GNAT Studio home directory yet.
      if Show_Preferences_Assistant or else Auto_Run_Assistant.Active then
         --  Remove the splash screen, since it conflicts with the preferences
         --  assistant dialog.
         if Splash /= null then
            Destroy (Splash);
            Splash := null;
         end if;

         Display_Preferences_Assistant
           (GPS_Main.Kernel,
            Pages =>
              (1 =>
                 Create
                   (Pref_Page =>
                      GPS_Main.Kernel.Get_Preferences.Get_Registered_Page
                        ("Color Theme Assistant"),
                    Label     => "Set the color theme",
                    Message   =>
                      "The color theme can be changed later via "
                      & "<b>Edit/Preferences/Color Theme</b>."),
               2 =>
                 Create
                   (Pref_Page =>
                      GPS_Main.Kernel.Get_Preferences.Get_Registered_Page
                        ("Key shortcuts theme"),
                    Label     => "Select a key shortcuts theme",
                    Message   =>
                      "Key shortcuts can be changed later "
                      & "later via "
                      & "<b>Edit/Preferences/General/Key Shortcuts</b>."),
               3 =>
                 Create
                   (Pref_Page =>
                      GPS_Main.Kernel.Get_Preferences.Get_Registered_Page
                        ("Preferences Assistant General"),
                    Label     => "Set general settings",
                    Message   =>
                      "These preferences can be changed "
                      & "later via <b>Edit/Preferences</b>"),
               4 =>
                 Create
                   (Pref_Page =>
                      GPS_Main.Kernel.Get_Preferences.Get_Registered_Page
                        ("Preferences Assistant Plugins"),
                    Label     => "Select your plugins",
                    Message   =>
                      "Enabled plugins can be changed later "
                      & "via <b>Edit/Preferences/Plugins</b>.")));

         --  The list of plugins to load at startup may have been changed by
         --  the user: reload the system custom files (XML and Python plugins)
         --  with the potentially modified list.

         GPS.Kernel.Custom.Load_No_Autoload_Custom_Files (GPS_Main.Kernel);
         Python_Module.Load_No_Autoload_Python_Plugins (GPS_Main.Kernel);
      end if;

      --  All/most actions are now loaded, we can reset the toolbars.
      --  We must have the keybindings in place already, since the clean up of
      --  the menus also looks up the actions, and their keyshortcut is loaded
      --  at that point.
      --  This order prevents some flickering in the toolbar (since otherwise
      --  we would first be displaying empty buttons until their icon is
      --  loaded).

      GPS.Kernel.Modules.UI.Start_Monitoring_Menus (GPS_Main.Kernel);
      Update_Menus_And_Buttons (GPS_Main.Kernel);

      --  Set default icon for dialogs and windows

      Icon :=
        Gtk.Icon_Theme.Get_Default.Load_Icon_For_Scale
          ("gnatstudio_logo",
           32,
           Scale => GPS_Main.Get_Scale_Factor,
           Flags => 0,
           Error => null);

      if Icon /= null then
         Set_Default_Icon (Icon);
      end if;

      --  Print a welcome message in the console, but before parsing the error
      --  messages, so that these are visible

      declare
         About_File     : constant Virtual_File :=
           Create_From_Dir (Prefix_Dir, "share/gnatstudio/about.txt");
         About_Contents : GPS.Globals.String_Access :=
           (if About_File.Is_Regular_File then About_File.Read_File else null);
      begin
         if About_Contents = null then
            About_Contents := new String'("");
         end if;
         GPS_Main.Kernel.Insert
           (-"Welcome to GNAT Studio "
            & To_String (Config.Version)
            & " ("
            & Config.Source_Date
            & (-") hosted on ")
            & Config.Target
            & ASCII.LF
            & "(c) 2001-"
            & Config.Current_Year
            & " AdaCore"
            & ASCII.LF);
         Free (About_Contents);
      end;

      --  Apply the preferences to the MDI. In particular, we want to set the
      --  default position for notebook tabs, since they can be overridden by
      --  the desktop (and changing that default later on would switch all
      --  notebooks)

      Configure_MDI (GPS_Main.Kernel);

      --  When the hooks have been registered, in particular context_changed,
      --  we can start monitoring focus change.
      Kernel_Callback.Connect
        (Get_MDI (GPS_Main.Kernel),
         Signal_Child_Selected,
         Child_Selected'Access,
         GPS_Main.Kernel);

      --  Take config file from GPR_CONFIG environment if any
      declare
         GPR_CONFIG : constant String :=
           GPS_Main.Kernel.Get_Original_Environment.Value ("GPR_CONFIG");
      begin
         if Config_Files.Config_File = GNATCOLL.VFS.No_File
           and then GPR_CONFIG /= ""
         then
            Config_Files.Config_File :=
              Create_From_Base (+GPR_CONFIG, Get_Current_Dir.Full_Name);
         end if;
      end;

      --  Setup config files before we load projects

      Get_Registry (GPS_Main.Kernel).Environment.Set_Config_File
        (Config_Files.Config_File);
      Get_Registry (GPS_Main.Kernel).Environment.Set_Automatic_Config_File
        (Config_Files.Autoconf);

      --  Set scenario variables
      declare
         use Cmd_Line_Scenario_Vars_Maps;
         C : Cmd_Line_Scenario_Vars_Maps.Cursor :=
           Cmd_Line_Scenario_Vars.First;
      begin
         while Has_Element (C) loop
            Get_Registry (GPS_Main.Kernel).Environment.
              Change_Environment (Key (C), Element (C));
            Next (C);
         end loop;
      end;

      if GPS.Globals.Build_Tree_Dir /= No_File then
         Ensure_Directory (GPS.Globals.Build_Tree_Dir);
         Get_Registry (GPS_Main.Kernel).Environment.Set_Build_Tree_Dir
           (GPS.Globals.Build_Tree_Dir.Full_Name);
      end if;

      if GPS.Globals.Root_Dir /= No_File then
         Ensure_Directory (GPS.Globals.Root_Dir);
         Get_Registry (GPS_Main.Kernel).Environment.Set_Root_Dir
           (GPS.Globals.Root_Dir.Full_Name);
      end if;

      if Config_Files.DB_Dirs /= null then
         for D of Config_Files.DB_Dirs.all loop
            Get_Registry (GPS_Main.Kernel).Environment.Add_Config_Dir (D);
         end loop;
      end if;

      if Config_Files.Autoconf
        and then Config_Files.Config_File = GNATCOLL.VFS.No_File
      then
         Get_Registry (GPS_Main.Kernel).Environment.Set_Config_File
           (Create_From_Base ("auto.cgpr", Get_Current_Dir.Full_Name.all));
      end if;

      --  We now make sure we have a project loaded, so that opening editors
      --  will work correctly.

      --  If no project has been specified on the command line, try to open
      --  the first one in the current directory (if any).

      if Program_Args /= null then
         --  --debug has been specified
         --  Load project, and set debugger-related project properties

         Setup_Debug (Empty_Project => Empty_Project);

      else
         if Project_Name /= No_File and then not Is_Regular_File (Project_Name)
         then
            --  We can finally search on ADA_PROJECT_PATH, which is now known

            declare
               P : constant Virtual_File :=
                 Locate_Regular_File
                   (Base_Name (Project_Name),
                    Get_Registry (GPS_Main.Kernel)
                      .Environment
                      .Predefined_Project_Path);
            begin
               if P /= No_File then
                  Project_Name := P;
               else
                  --  Keep the user project, which will display an error in
                  --  GNAT Studio.
                  null;
               end if;
            end;
         end if;

         if Project_Name = No_File then
            if Server_Mode then
               Auto_Load_Project := True;
               Load_Empty_Project (GPS_Main.Kernel);
               Load_Sources;

            else
               if not Setup_Project then
                  return False;
               end if;
            end if;
         end if;
      end if;

      Create_Or_Load_Backup_Desktop (GPS_Main.Kernel);
      if Auto_Load_Project and then Project_Name /= No_File then
         --  Do not clear to keep the welcome message on kernel's console
         Load_Project (GPS_Main.Kernel, Project_Name, Clear => False);
         Load_Sources;
      end if;

      if not File_Opened and then not Has_User_Desktop (GPS_Main.Kernel) then
         Display_Welcome_View (GPS_Main.Kernel);
      end if;

      if Splash /= null then
         Destroy (Splash);
      end if;

      --  After loading the project, force remote host assignment if
      --  Tools_Host is not null

      if Tools_Host /= null then
         for S in Distant_Server_Type'Range loop
            GPS.Kernel.Remote.Assign
              (GPS_Main.Kernel, S, Tools_Host.all, Reload_Prj => True);
         end loop;
      end if;

      --  Disable tip of the day pop up window is --hide is
      --  specified.

      if Hide_GPS then
         Set_Pref (Tip_Of_The_Day, GPS_Main.Kernel, False);
      end if;

      --  Load the preferences set when creating the kernel
      --  This needs to be done after all the graphical eleents have been
      --  created, to be sure they are realized and will take the preferences
      --  into account.

      Preferences_Changed_Hook.Run (GPS_Main.Kernel, null);

      if not Hide_GPS then
         declare
            Ignored : Boolean;
         begin
            --  Ensure the main window is raised above any existing window
            GPS_Main.Set_Keep_Above (Setting => True);
            GPS_Main.Present;
            GPS_Main.Show_Now;
            while Gtk.Main.Events_Pending loop
               Ignored := Gtk.Main.Main_Iteration;
            end loop;
            GPS_Main.Set_Keep_Above (Setting => False);
         end;
      end if;

      --  Check if we have some autosaved files in the opened editors
      Src_Editor_Buffer.Check_Auto_Saved_Files (GPS_Main.Kernel);

      if Program_Args /= null then
         --  Initialize the debugger after having executed scripts if any,
         --  so that it is possible to set up the environment before starting
         --  a debug session.
         --  Needs to be done after the call to Show, so that the GNAT Studio
         --  window already has a proper size, otherwise we might end up with
         --  windows with height=0 or width=0
         if GVD.Preferences.Debugger_Kind.Get_Pref = GVD.Types.DAP then
            declare
               File : Virtual_File := Create_From_Base (+Program_Args.all);
            begin
               --  If we are on Windows ahd the file does not exist, try to
               --  append the file extension.
               --  This can be useful on Cygwin, where the extension if often
               --  omitted.
               if Config.Host = Windows and then not File.Is_Regular_File then
                  File := Create_From_Base (+Program_Args.all & ".exe");
               end if;

               GNATCOLL.VFS.Normalize_Path (File);
               DAP.Module.Initialize_Debugger
                 (Kernel  => GPS_Main.Kernel,
                  Project =>
                    (if Empty_Project then No_Project
                     else Get_Project (GPS_Main.Kernel)),
                  File    => File);
            end;
         else
            GVD_Module.Initialize_Debugger
              (Kernel => GPS_Main.Kernel, Args => Program_Args.all);
         end if;
      end if;

      --  Execute the startup scripts now, even though it is recommended that
      --  they connect to the GPS_Started hook if they have graphical actions
      --  to do
      --  This has to be launched after the call to Show, otherwise, the
      --  mini-loop launched in the trace function of the python module
      --  displatchs FOCUS_CHANGE, even if keyboard never been ungrab. This
      --  causes the editor to be uneditable on some cases on windows.

      if Batch_Script /= null then
         Execute_Batch (Batch_Script.all, As_File => False);
      end if;

      if Batch_File /= null then
         Execute_Batch (Batch_File.all, As_File => True);
         Free (Batch_File);
      end if;

      Idle_Id := Glib.Main.Idle_Add (On_GPS_Started'Unrestricted_Access);
      Log_File_Views.Register_Module (GPS_Main.Kernel);

      return False;
   end Finish_Setup;

   -------------------
   -- Execute_Batch --
   -------------------

   procedure Execute_Batch (Batch : String; As_File : Boolean) is
      Script   : Scripting_Language;
      Errors   : Boolean;
      Start    : Integer := Batch'First;
   begin
      Trace (Main_Trace, "Execute_Batch: " & Batch);
      for J in Batch'Range loop
         if Batch (J) = ':' then
            Script := Lookup_Scripting_Language
              (GPS_Main.Kernel.Scripts, Batch (Batch'First .. J - 1));
            Start := J + 1;

            if Script = null then
               if As_File then
                  Insert
                    (GPS_Main.Kernel,
                     -"Language unknown for --load command line switch",
                     Mode => Error);
               else
                  Insert
                    (GPS_Main.Kernel,
                     -"Language unknown for --script command line switch",
                     Mode => Error);
               end if;
               return;
            end if;

            exit;
         end if;
      end loop;

      if Script = null then
         --  Assume language is python
         Script := Lookup_Scripting_Language
           (GPS_Main.Kernel.Scripts, "python");
      end if;

      if As_File then
         Execute_File
           (Script   => Script,
            Filename => Normalize_Pathname
              (Batch (Start .. Batch'Last), Startup_Dir.all),
            Show_Command => False,
            Errors   => Errors);
      else
         GNATCOLL.Scripts.Execute_Command
           (Script   => Script,
            CL       => Parse_String
              (Batch (Start .. Batch'Last),
               Command_Line_Treatment (Script)),
                  Errors   => Errors);
      end if;

   exception
      when E : others =>
         if As_File then
            Insert (GPS_Main.Kernel,
                    -"Error when executing the script for -batch switch",
                    Mode => Error);
         else
            Insert (GPS_Main.Kernel,
                    -"Error when executing the script for --script switch",
                    Mode => Error);
         end if;

         Trace_With_Python_Backtrace (Main_Trace, E);
   end Execute_Batch;

   ---------------------
   -- Default_Gtk_Mer --
   ---------------------

   procedure Default_Gtk_Mer
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Trace_With_Python_Backtrace (Gtk_Errors, Occurrence);
   end Default_Gtk_Mer;

   -------------------
   -- Error_Message --
   -------------------

   procedure Error_Message (Message : String; Save : Boolean) is
      Log_File : Virtual_File;
      Pid_File : Virtual_File;
      Str      : Virtual_File;
      Button   : Message_Dialog_Buttons;
      Msg      : constant String :=
        (if Save
         then Message
         & ASCII.LF
         & "You will be asked to save modified files before GNAT Studio exits"
         else Message);
      Dead     : Boolean;
      pragma Unreferenced (Dead, Button);

   begin
      --  Error before we created the main window is likely while parsing
      --  command line switches.
      if GPS_Main = null then
         Report_Error (Message);
         return;
      end if;

      Log_File := Create_From_Dir (Get_Home_Dir (GPS_Main.Kernel), "log");
      Pid_File := Create_From_Dir
         (Get_Home_Dir (GPS_Main.Kernel), +("log." & Pid_Image));

      if Is_Regular_File (Pid_File) then
         Str := Pid_File;
      else
         Str := Log_File;
      end if;

      begin
         if Active (Testsuite_Handle) then
            Put_Line ("Error message generated: " & Msg);
         else

            Button := GPS_Message_Dialog
              (Msg
               & ASCII.LF
               & "Please report with contents of " & Str.Display_Full_Name,
               Error, Button_OK,
               Title         => -"Fatal Error",
               Justification => Justify_Left,
               Parent        => GPS_Main.Kernel.Get_Main_Window);
         end if;

         if Save then
            Dead := Save_MDI_Children (GPS_Main.Kernel, Force => False);
         end if;

         --  When GNAT Studio is in inconsistent state it can be impossible to
         --   create a new dialog catch the exception here.
      exception
         when others =>
            Put_Line (Message);
            Shutdown_Callback (GPS_Main.Application);
      end;

   end Error_Message;

   Registered  : Boolean;
   Status      : Glib.Gint;
   Application : GPS_Application;
   pragma Unreferenced (Registered);

begin
   --  Process the "--version" command line switch separately to make it
   --  available even without any display.

   for J in 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (J) = "--version"
        or else Ada.Command_Line.Argument (J) = "-v"
      then
         declare
            Version : constant String :=
              "GNAT Studio " & To_String (Config.Version) & " ("
              & Config.Source_Date & ") hosted on "
              & Config.Target;
         begin
            Put_Line (Version);
         end;

         GPS_Command_Line.Do_Exit := True;
      end if;
   end loop;

   if GPS_Command_Line.Do_Exit then
      return;
   end if;

   --  Under all platforms, prevent the creation of a dbus session: this serves
   --  no purpose, breaks the testsuite, slows down the startup of the first
   --  GNAT Studio instance, and is flaky under Windows XP.

   --  If, for some obscure reason, there is a DBUS address specified,
   --  allow it.
   declare
      Bus_Addr : GPS.Globals.String_Access :=
        Getenv ("DBUS_SESSION_BUS_ADDRESS");
   begin
      if Bus_Addr = null
        or else Bus_Addr.all = ""
      then
         Setenv ("DBUS_SESSION_BUS_ADDRESS", "null");
      end if;
      Free (Bus_Addr);
   end;

   --  Create and setup a Gtk Application. We create our own class so that we
   --  can override the local_command_Line virtual method, and thus do our own
   --  handling of --help. Otherwise, since we haven't yet registered our
   --  switches, --help would not be able to list them

   Glib.Object.Initialize_Class_Record
      (Ancestor     => Gtk.Application.Get_Type,
       Class_Record => Application_Class_Record,
       Type_Name    => "GPSApplication",
       Class_Init   => Application_Class_Init'Unrestricted_Access);

   Application := new GPS_Application_Record;
   G_New (Application, Application_Class_Record.The_Type);
   Application.Initialize
     ("com.adacore.GPS",
      Glib.Application.G_Application_Handles_Open +

        --  Arguments are handled at two levels:
        --  - locally in Application.Run (and local_command_line)
        --  - remotely in Command_Line_Callback
        Glib.Application.G_Application_Handles_Command_Line +
        Glib.Application.G_Application_Send_Environment +
        Glib.Application.G_Application_Non_Unique,

      --  Files specified on the command line are opened via On_Open below
      Gtkada_Application_Handles_Open +
        Gtkada_Application_OSX_FullScreen);

   Application.Set_Default;

   if Config.Host = Config.Unix and then not Config.Darwin_Target then
      --  On systems with DBus and an existing DBus configuration
      --  for the system's gtk, we don't want to open a session as
      --  we're using a different gtk library, that may have
      --  incompatibilities or missing modules.
      Glib.Properties.Set_Property
        (Application, Register_Session_Property, False);
   else
      Glib.Properties.Set_Property
        (Application, Register_Session_Property, True);
   end if;

   Application.On_Startup (Startup_Callback'Unrestricted_Access);
   Application.On_Activate (Activate_Callback'Unrestricted_Access);
   Application.On_Command_Line (Command_Line_Callback'Unrestricted_Access);
   Application.On_Open (File_Open_Callback'Unrestricted_Access);
   Application.On_Shutdown (Shutdown_Callback'Unrestricted_Access);

   Registered := Application.Register (Cancellable => null);

   Status := Application.Run;

   if Status /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Status));
   end if;

exception
   when E : others =>
      Unexpected_Exception := True;
      Trace_With_Python_Backtrace (Main_Trace, E);
      Error_Message
        (Message =>
            "Unexpected fatal error, GNAT Studio is in an inconsistent state"
         & ASCII.LF & Exception_Information (E),
         Save    => True);
end GPS.Main;
