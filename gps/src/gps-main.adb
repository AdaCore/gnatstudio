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

with System;
with Interfaces.C.Strings;

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Environment_Variables;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with ALI;
with Interfaces.C;              use Interfaces.C;

with GNAT.Command_Line;                use GNAT.Command_Line;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;           use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;
with GNAT.Strings;
with GNATCOLL.Memory;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;               use GNATCOLL.VFS_Utils;

with Glib;
with Glib.Application;                 use Glib.Application;
with Glib.Error;                       use Glib.Error;
with Glib.Main;
with Glib.Messages;                    use Glib.Messages;
with Glib.Object;                      use Glib.Object;
with Glib.Option;                      use Glib.Option;
with Glib.Properties;                  use Glib.Properties;

with Gdk.Main;
with Gdk.Pixbuf;                       use Gdk.Pixbuf;
with Gdk.Window;

with Gtk;                              use Gtk;
with Gtk.Application;                  use Gtk.Application;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Icon_Theme;                   use Gtk.Icon_Theme;
with Gtk.Image;                        use Gtk.Image;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Menu_Bar;                     use Gtk.Menu_Bar;
with Gtk.Menu_Item;                    use Gtk.Menu_Item;
with Gtk.Settings;                     use Gtk.Settings;
with Gtk.Style_Provider;               use Gtk.Style_Provider;
with Gtk.Widget;                       use Gtk.Widget;
with Gtk.Window;                       use Gtk.Window;
with Gtk_Utils;                        use Gtk_Utils;

with Gtkada.Application;               use Gtkada.Application;
with Gtkada.Bindings;                  use Gtkada.Bindings;
with Gtkada.Dialogs;                   use Gtkada.Dialogs;
with Gtkada.Intl;
with Gtkada.MDI;                       use Gtkada.MDI;
with Gtkada.Style;

with Config;                           use Config;
with Default_Preferences;              use Default_Preferences;
with GPS.Callbacks;                    use GPS.Callbacks;
with GPS.Environments;                 use GPS.Environments;
with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Clipboard;             use GPS.Kernel.Clipboard;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.Contexts;              use GPS.Kernel.Contexts;
with GPS.Kernel.Custom;                use GPS.Kernel.Custom;
with GPS.Kernel.Entities;              use GPS.Kernel.Entities;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Shell;
with GPS.Kernel.Modules;               use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;            use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Project;               use GPS.Kernel.Project;
with GPS.Kernel.Remote;
with GPS.Kernel.Scripts;               use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;        use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Style_Manager.Shell;
with GPS.Kernel.Task_Manager;          use GPS.Kernel.Task_Manager;
with GPS.Kernel.Timeout;               use GPS.Kernel.Timeout;
with GPS.Kernel.Xref;
with GPS.Stock_Icons;
with GPS.Main_Window;                  use GPS.Main_Window;
with GPS.Menu;
with GPS.Search.GUI;
with OS_Utils;                         use OS_Utils;
with Projects;                         use Projects;
with Remote;                           use Remote;
with Src_Editor_Box;                   use Src_Editor_Box;
with String_Utils;
with Trace_Support;                    use Trace_Support;
with Task_Manager.GUI;
with Welcome;                          use Welcome;
with Welcome_Page;                     use Welcome_Page;

--  Modules registered by GPS

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
with Project_Templates.GPS;
with External_Editor_Module;
with GNATStack.Module;
with GNATTest_Module;
with GPS.Location_View;
with GVD_Module;
with Help_Module;
with KeyManager_Module;
with KeyManager_Module.Macros;
with Toolchains_Module;
with Ada_Semantic_Tree_Module;
with Navigation_Module;
with Outline_View;
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
with Startup_Module;
with Switches_Chooser.Scripts;
with Theme_Manager_Module;
with Toolchains_Editor;
with VCS_Module;
with VFS_Module;
with Vdiff2_Module;
with Vsearch;
with Language.Libclang;

procedure GPS.Main is
   package ICS renames Interfaces.C.Strings;
   use ICS;
   use type Glib.Gint;

   Me         : constant Trace_Handle := Create ("GPS");
   Pid_Image  : constant String := String_Utils.Image (Get_Process_Id);
   Gtk_Errors : constant Trace_Handle := Create ("GTK");

   Memory_Monitor : Boolean;
   Memory_Stack_Depth : constant := 3;
   --  Stack depth for GNATCOLL.Memory

   Refactor_Trace         : constant Trace_Handle :=
                              Create ("MODULE.Refactor", GNATCOLL.Traces.On);
   Python_Trace           : constant Trace_Handle :=
                              Create ("MODULE.Python", GNATCOLL.Traces.On);
   Call_Graph_Trace       : constant Trace_Handle :=
                              Create ("MODULE.Call_Graph", GNATCOLL.Traces.On);
   Dependency_Trace       : constant Trace_Handle :=
                              Create ("MODULE.Dependency", GNATCOLL.Traces.On);
   Project_Browser_Trace  : constant Trace_Handle :=
                        Create ("MODULE.Project_Browser", GNATCOLL.Traces.On);
   Browsers_Trace         : constant Trace_Handle :=
                        Create ("MODULE.Browsers", GNATCOLL.Traces.On);
   Entities_Browser_Trace : constant Trace_Handle :=
                        Create ("MODULE.Entities_Browser", GNATCOLL.Traces.On);
   Revision_Views_Trace   : constant Trace_Handle :=
                        Create ("MODULE.Revision_Views", GNATCOLL.Traces.On);
   Aliases_Trace          : constant Trace_Handle :=
                        Create ("MODULE.Aliases", GNATCOLL.Traces.On);
   Project_Explorer_Trace : constant Trace_Handle :=
                        Create ("MODULE.Project_Explorer", GNATCOLL.Traces.On);
   Files_Explorer_Trace   : constant Trace_Handle :=
                        Create ("MODULE.Files_Explorer", GNATCOLL.Traces.On);
   External_Editor_Trace  : constant Trace_Handle :=
                        Create ("MODULE.External_Editor", GNATCOLL.Traces.On);
   VCS_Trace              : constant Trace_Handle :=
                        Create ("MODULE.VCS", GNATCOLL.Traces.On);

   Custom_Trace           : constant Trace_Handle :=
                        Create ("MODULE.Custom", GNATCOLL.Traces.On);
   Project_Templates_Trace : constant Trace_Handle :=
                       Create ("MODULE.Project_Templates", GNATCOLL.Traces.On);
   Code_Analysis_Trace    : constant Trace_Handle :=
                        Create ("MODULE.Code_Analysis", GNATCOLL.Traces.On);
   CodePeer_Trace         : constant Trace_Handle :=
                              Create ("MODULE.CodePeer", GNATCOLL.Traces.On);
   GNATStack_Trace        : constant Trace_Handle :=
                              Create ("MODULE.GNATStack", GNATCOLL.Traces.On);
   Codefix_Trace          : constant Trace_Handle :=
                              Create ("MODULE.Codefix", GNATCOLL.Traces.On);
   Builder_Trace          : constant Trace_Handle :=
                              Create ("MODULE.Builder", GNATCOLL.Traces.On);
   GVD_Trace              : constant Trace_Handle :=
                              Create ("MODULE.GVD", GNATCOLL.Traces.On);
   GNATTest_Trace         : constant Trace_Handle :=
                              Create ("MODULE.GNATTest", GNATCOLL.Traces.On);
   Startup_Trace          : constant Trace_Handle :=
                              Create ("MODULE.Startup", GNATCOLL.Traces.On);
   VFS_Trace              : constant Trace_Handle :=
                              Create ("MODULE.VFS", GNATCOLL.Traces.On);
   Help_Trace             : constant Trace_Handle :=
                              Create ("MODULE.Help", GNATCOLL.Traces.On);
   Scenario_View_Trace    : constant Trace_Handle :=
                              Create ("MODULE.SCENARIO", GNATCOLL.Traces.On);
   Project_Viewer_Trace   : constant Trace_Handle :=
                      Create ("MODULE.Project_Viewer", GNATCOLL.Traces.On);
   Project_Properties_Trace : constant Trace_Handle :=
                      Create ("MODULE.Project_Properties", GNATCOLL.Traces.On);
   CPP_Trace              : constant Trace_Handle :=
                              Create ("MODULE.CPP", GNATCOLL.Traces.On);
   Outline_View_Trace     : constant Trace_Handle :=
                              Create ("MODULE.Outline", GNATCOLL.Traces.On);
   Call_Graph_View_Trace  : constant Trace_Handle :=
                      Create ("MODULE.Call_Graph_View", GNATCOLL.Traces.On);
   Clipboard_View_Trace   : constant Trace_Handle :=
                      Create ("MODULE.Clipboard_Vview", GNATCOLL.Traces.On);
   Toolchains_Trace       : constant Trace_Handle :=
                      Create ("MODULE.Toolchains", GNATCOLL.Traces.On);
   Toolchains_Editor_Trace  : constant Trace_Handle :=
                      Create ("MODULE.Toolchains_Editor", GNATCOLL.Traces.On);
   Elaboration_Browser_Trace : constant Trace_Handle :=
     Create ("MODULE.Elaboration_Browser", GNATCOLL.Traces.On);

   --  If any of these debug handles is active, the correponding module
   --  is loaded.

   subtype String_Access is GNAT.Strings.String_Access;

   type File_To_Open is record
      File : Unbounded_String;
      Line : Natural := 1;
      From_Project : Boolean := False;
   end record;

   package File_To_Open_Vectors is new Ada.Containers.Vectors
     (Positive, File_To_Open);

   Home_Dir               : Virtual_File;
   Prefix_Dir             : Virtual_File;
   GPS_Home_Dir           : Virtual_File;
   Batch_File             : String_Access;
   Batch_Script           : String_Access;
   Hide_GPS               : Boolean := False;
   Tools_Host             : String_Access;
   Target                 : String_Access;
   Protocol               : String_Access;
   Debugger_Name          : String_Access;
   Startup_Dir            : String_Access;
   Passed_Project_Name    : String_Access;
   Program_Args           : String_Access;
   Server_Mode            : Boolean := False;
   Port_Number            : Natural := 0;

   GPS_Main               : GPS_Window;
   Application            : GPS_Application;
   Project_Name           : Virtual_File := No_File;
   Splash                 : Gtk_Window;
   Files_To_Open          : File_To_Open_Vectors.Vector;
   Unexpected_Exception   : Boolean := False;
   Env                    : GPS.Environments.Environment;

   Timeout_Id             : Glib.Main.G_Source_Id;
   pragma Unreferenced (Timeout_Id);

   function Local_Command_Line
      (Self        : System.Address;
       Arguments   : access chars_ptr_array_access;
       Exit_Status : access Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Local_Command_Line);
   --  override gtk+ builtin virtual method for an application.
   --  This makes sure that we can do our own handling of --hep

   procedure Application_Class_Init (Self : GObject_Class);
   pragma Convention (C, Application_Class_Init);

   Application_Class_Record : aliased Glib.Object.Ada_GObject_Class;
   --  A custom child of GtkApplication

   procedure Startup_Callback
     (Application : access Gapplication_Record'Class);
   --  Handler for the ::startup signal, emitted by the application

   procedure Activate_Callback
     (Application : access Gapplication_Record'Class);
   --  Handler for the ::activate signal, emitted by the application

   function On_GPS_Started return Boolean;
   --  Called when GPS is started and visible on the screen

   function Command_Line_Callback
     (Application  : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
   return Glib.Gint;
   --  Handler for the ::command-line signal, emitted by the application

   procedure Initialize_Environment_Variables
     (Command_Line : access Gapplication_Command_Line_Record'Class);
   --  Sanitize, sets and take into account various environment variables, and
   --  initialize GPS according to them.

   procedure Initialize_Low_Level (Status_Code : out Glib.Gint);
   --  Initializes the low-level gtk, python, traces layers
   --  This needs to be performed after environment variable initialisation.

   type GPS_Option_Context is record
      Context : Glib.Option.Goption_Context;

      Do_Exit : Boolean := False;
      --  Set to True if GPS should exit after parsing command line switches

      Line  : Positive := 1;
      --  Line to use when opening files from the command line.
   end record;
   GPS_Command_Line : GPS_Option_Context;
   --  Handling of command line

   procedure Build_Command_Line;
   --  Initialize the variable GPS_Command_Line.

   function On_Switch
      (Option_Name : ICS.chars_ptr;
       Value       : ICS.chars_ptr;
       Data        : System.Address;  --  ignored
       Error       : access Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, On_Switch);
   --  General callback for switch handling from GApplication

   function On_File_Switch
     (Option_Name : ICS.chars_ptr;
      Value       : ICS.chars_ptr;
      Data        : System.Address;
      Error       : access Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, On_File_Switch);
   --  General callback for file opening handling from GApplication

   procedure Handle_X_Switch (Val : String);
   --  Handles the -X command line switch

   procedure File_Open_Callback
     (Application : Gtkada.Application.Gtkada_Application;
      Files       : Gtkada.Application.GFile_Array);
   --  Handler for the ::open signal, emitted by the application

   procedure Shutdown_Callback
     (Application : access Gapplication_Record'Class);

   procedure Set_Project_Name;
   --  Set the project name from the command line switch

   procedure Error_Message (Message : String);
   --  Display the "Fatal error" message

   procedure Display_Splash_Screen;
   --  Display the GPS splash screen

   procedure Load_CSS;
   --  Load the GPS global and local CSS files

   function Finish_Setup (Data : Process_Data) return Boolean;
   --  Finish the set up of GPS, while the main loop is running

   procedure Execute_Batch (Batch : String; As_File : Boolean);
   --  Execute a batch command (either loading the file Batch if As_File is
   --  true, or as a standard command otherwise).

   procedure Default_Gtk_Mer
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Called when an Ada callback raises an exception, to log it.

   --------------------------------------
   -- Initialize_Environment_Variables --
   --------------------------------------

   procedure Initialize_Environment_Variables
     (Command_Line : access Gapplication_Command_Line_Record'Class)
   is
      function Getenv (Var : String) return String;
      function Get_Cwd return String;
      --  proxies for the services in the command line, usable even when no
      --  command line is passed

      function Getenv (Var : String) return String is
         Str : String_Access;
      begin
         if Command_Line = null then
            Str := GNAT.OS_Lib.Getenv (Var);
            return S : constant String := Str.all do
               Free (Str);
            end return;
         else
            return Command_Line.Getenv (Var);
         end if;
      end Getenv;

      function Get_Cwd return String is
      begin
         if Command_Line = null then
            return Get_Current_Dir.Display_Full_Name;
         else
            return Command_Line.Get_Cwd;
         end if;
      end Get_Cwd;

   begin
      --  Reset the environment that was set before GPS was started (since
      --  starting GPS will generally imply a change in LD_LIBRARY_PATH to
      --  point to the right libraries

      declare
         Tmp : constant String := Getenv ("GPS_STARTUP_LD_LIBRARY_PATH");
      begin
         if Tmp /= "" then
            Setenv ("LD_LIBRARY_PATH", Tmp);
         end if;
      end;

      declare
         Tmp : constant String := Getenv ("GPS_STARTUP_DYLD_LIBRARY_PATH");
      begin
         if Tmp /= "" then
            Setenv ("DYLD_LIBRARY_PATH", Tmp);
         end if;
      end;

      declare
         Tmp : constant String :=
           Getenv ("GPS_STARTUP_DYLD_FALLBACK_LIBRARY_PATH");
      begin
         if Tmp /= "" then
            Setenv ("DYLD_FALLBACK_LIBRARY_PATH", Tmp);
         end if;
      end;

      declare
         Charset : constant String := Getenv ("CHARSET");
      begin
         if Charset = "" then
            --  Gtk+ does not like if CHARSET is not defined.
            --  Need to set CHARSET *before* calling Gtk.Main.Init, so cannot
            --  use Get_Pref here.

            Setenv ("CHARSET", Config.Default_Charset);
         end if;
      end;

      Startup_Dir := new String'(Get_Cwd);

      --  Set the TERM variable to a dummy value, since we only know how to
      --  handle simple terminals

      Setenv ("TERM", "dumb");

      declare
         Home : constant String := Getenv ("GPS_HOME");
      begin
         if Home /= "" then
            Home_Dir := Create (+Home);
         else
            Home_Dir := Get_Home_Directory;
         end if;
      end;

      GPS_Home_Dir := Create_From_Dir (Home_Dir, ".gps");
      Ensure_Directory (GPS_Home_Dir);

      declare
         Prefix : constant String := Getenv ("GPS_ROOT");
      begin
         if Prefix /= "" then
            Prefix_Dir := Create (+Prefix);
         end if;
      end;

      if Prefix_Dir = No_File then
         declare
            Prefix : constant String := Executable_Location;
         begin
            --  Check whether we are running the installed gps, or locally from
            --  the development environment.

            if Prefix'Length < 4
              or else Prefix (Prefix'Last - 3 .. Prefix'Last - 1) /= "obj"
            then
               Prefix_Dir := Create (+Prefix);
            else
               Prefix_Dir := Create (+Config.Prefix);
            end if;

         end;
      end if;

      declare
         Tmp     : constant String := Getenv ("PATH");
         Prefix  : constant String := Prefix_Dir.Display_Full_Name;
         Bin     : constant String :=
           Prefix &
           (if Prefix (Prefix'Last) /= Directory_Separator
            then (1 => Directory_Separator) else "") &
           "bin";

      begin
         --  Command_Line.Getenv is unfortunately case sensitive, which is
         --  incompatible with Windows where the PATH environment variable
         --  might be called "Path", so add an extra safety guard by using
         --  Ada.Environment_Variables.Value which is case insensitive.
         --  ??? Consider fixing Command_Line.Getenv

         if Tmp /= "" then
            Setenv ("PATH", Tmp & Path_Separator & Bin);
         else
            Setenv
              ("PATH",
               Ada.Environment_Variables.Value ("PATH")
               & Path_Separator & Bin);
         end if;
      exception
         --  Value may raise Constraint_Error if PATH is not set, nothing
         --  to do in this case
         when Constraint_Error => null;
      end;

      --  Python startup path

      declare
         Python_Path : constant String := Getenv ("PYTHONPATH");
         New_Val : String_Access;
      begin
         if Python_Path = "" then
            New_Val := new String'
              (+Create_From_Dir (Prefix_Dir, "share/gps/python").Full_Name);
         else
            New_Val := new String'
              (+To_Path
                 (From_Path (+Python_Path) &
                  (1 => Create_From_Dir (Prefix_Dir, "share/gps/python"))));
         end if;

         Setenv ("PYTHONPATH", New_Val.all);
         Trace (Me, "PYTHONPATH=" & New_Val.all);
         Free (New_Val);
      end;

      declare
         Equal  : Natural;  --  Position of '=' in NAME=VALUE env string
         Prefix : constant String := "GPS_STARTUP_";
      begin
         Env := new Environment_Record;

         if Command_Line /= null then
            declare
               List : GNAT.Strings.String_List := Command_Line.Get_Environ;
            begin
               for J in List'Range loop
                  if Head (List (J).all, Prefix'Length) = Prefix then
                     Equal := Index (List (J).all, "=");

                     declare
                        Name : constant String :=
                        List (J) (Prefix'Length + List (J)'First .. Equal - 1);
                     begin
                        Env.Append
                          (Name        => Name,
                           Users_Value =>
                              List (J) (Equal + 1 .. List (J)'Last),
                           GPS_Value   => Getenv (Name));
                     end;
                  end if;
               end loop;
               Free (List);
            end;
         end if;
      end;
   end Initialize_Environment_Variables;

   --------------------------
   -- Initialize_Low_Level --
   --------------------------

   procedure Initialize_Low_Level (Status_Code : out Glib.Gint) is
      Ignored     : Log_Handler_Id;
      pragma Unreferenced (Ignored);

   begin
      Gtkada.Intl.Setlocale;
      Gtkada.Intl.Bind_Text_Domain
        ("gps", +Create_From_Dir (Prefix_Dir, "share/locale").Full_Name);
      Gtkada.Intl.Text_Domain ("gps");

      --  Redirect all default Gtk+ logs to our own trace mechanism

      Ignored := Log_Set_Handler
        ("", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("GLib", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("GLib-GObject", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("Pango", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("Atk", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("GdkPixbuf", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("Gdk", Log_Level_Mask, Gtk_Log'Access);
      Ignored := Log_Set_Handler
        ("Gtk", Log_Level_Mask, Gtk_Log'Access);

      declare
         Plug_Ins           : constant Virtual_File :=
                                Create_From_Dir (GPS_Home_Dir, "plug-ins");
         Gnatinspect_Traces : constant Virtual_File :=
                                Create_From_Dir (GPS_Home_Dir,
                                                 "gnatinspect_traces.cfg");
         File               : Writable_File;

      begin
         if not Is_Directory (GPS_Home_Dir) then
            Make_Dir (GPS_Home_Dir);

            --  Create a default configuration file for the traces.
            --  This should be left while GPS is considered as not fully
            --  stable.

            File := Create_From_Dir (GPS_Home_Dir, "traces.cfg").Write_File;
            Write (File,
                   ">log.$$" & ASCII.LF &
                     "+" & ASCII.LF &
                     "*.EXCEPTIONS=yes" & ASCII.LF &
                     "DEBUG.COLORS=no" & ASCII.LF &
                     "DEBUG.ABSOLUTE_TIME=yes" & ASCII.LF &
                     "DEBUG.ELAPSED_TIME=no" & ASCII.LF &
                     "DEBUG.STACK_TRACE=no" & ASCII.LF &
                     "DEBUG.LOCATION=no" & ASCII.LF &
                     "DEBUG.ENCLOSING_ENTITY=no");
            Close (File);
         end if;

         if not Gnatinspect_Traces.Is_Regular_File then
            --  Make sure gnatinspect will never try to write to stdout. This
            --  works around an issue in gnatcoll-traces, where handles that
            --  are enabled by default would write to stdout in no config file
            --  is parsed to override this.
            File := Gnatinspect_Traces.Write_File;
            Write (File, ">log_gnatinspect");
            Close (File);
         end if;

         if not Is_Directory (Plug_Ins) then
            Make_Dir (Plug_Ins);
         end if;

      exception
         when VFS_Directory_Error =>
            Put_Line (Standard_Error,
                      (-"Cannot create config directory ") &
                      GPS_Home_Dir.Display_Full_Name & ASCII.LF);
            Status_Code := 1;
            return;
      end;

      declare
         Tmp : constant Virtual_File := Get_Tmp_Directory;
      begin
         if not Is_Directory (Tmp) then
            Put_Line (Standard_Error,
                      (-"Cannot access temporary directory ") &
                      Tmp.Display_Full_Name);
            Status_Code := 1;
            return;
         end if;
      end;

      --  Initialize the traces

      declare
         File : constant Virtual_File :=
                  Create_From_Dir (GPS_Home_Dir, "traces.cfg");
      begin
         --   Add the decorators first, so that the proper factory is
         --   used for them when parsing the config file.
         Trace_Support.Add_Trace_Decorators;

         GNATCOLL.Traces.Parse_Config_File
           (Filename     => No_File,
            Default      => File,
            On_Exception => GNATCOLL.Traces.Deactivate);
      exception
         when others =>
            Put_Line (Standard_Error,
                      (-"Cannot parse file ") & File.Display_Full_Name);
            Status_Code := 1;
            return;
      end;

      --  Check whether we should enable memory monitor. We do not use a
      --  constant for the trace_handle, since we must create it only after
      --  the call to Add_Trace_Decorators.
      Memory_Monitor := Active (Create ("DEBUG.ADA_MEMORY", Off));
      GNATCOLL.Memory.Configure
        (Activate_Monitor  => Memory_Monitor,
         Stack_Trace_Depth => Memory_Stack_Depth,
         Disable_Free      => False);

      Trace (Me, "GPS " & Config.Version & " (" & Config.Source_Date &
             ") hosted on " & Config.Target);
      Trace (Me, "Gtk+ static version: "
             & String_Utils.Image (Integer (Gtk.Major_Version)) & '.'
             & String_Utils.Image (Integer (Gtk.Minor_Version)) & '.'
             & String_Utils.Image (Integer (Gtk.Micro_Version)));
      Trace (Me, "Gtk+ dynamic version: "
             & String_Utils.Image (Gtk_Major_Version) & '.'
             & String_Utils.Image (Gtk_Minor_Version) & '.'
             & String_Utils.Image (Gtk_Micro_Version));

      Status_Code := 0;
   end Initialize_Low_Level;

   ---------------------
   -- Handle_X_Switch --
   ---------------------

   procedure Handle_X_Switch (Val : String) is
      Idx : constant Integer := Ada.Strings.Fixed.Index (Val, "=");
   begin
      if Idx >= Val'First then
         Ada.Environment_Variables.Set
            (Name  => Val (Val'First .. Idx - 1),
             Value => Val (Idx + 1 .. Val'Last));
      else
         Put_Line ("Invalid value for -X, should be VAR=VALUE");
         GPS_Command_Line.Do_Exit := True;
      end if;
   end Handle_X_Switch;

   ---------------
   -- On_Switch --
   ---------------

   function On_Switch
     (Option_Name : ICS.chars_ptr;
      Value       : ICS.chars_ptr;
      Data        : System.Address;
      Error       : access Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Data, Error);
      Switch : constant String := ICS.Value (Option_Name);

   begin
      if Switch = "--project" or else Switch = "-P" then
         --  Although this isn't costly, we must not resolve symbolic
         --  links for project names unless Fast Project Loading mode is
         --  disabled. Some users (IA27-014) and SCM have local links
         --  that point to a SCM cache directory (Rational Synergy), but
         --  directory names are still local. These users should use
         --  Trusted mode so that we do not resolve symbolic links

         Passed_Project_Name := new String'(ICS.Value (Value));

      elsif Switch = "--help"
        or else Switch = "-h"
        or else Switch = "--help-all"
      then
         declare
            --  Get_Help (False) will also print options that are not
            --  in the main group (such as Gtk+ options)
            --  Get_Help (True) will only print options from the main
            --  group
            Help : constant String :=
                     "GPS " &
                     Config.Version & " (" &
                     Config.Source_Date & ") hosted on " &
                     Config.Target & ASCII.LF &
                     ASCII.LF &
                     GPS_Command_Line.Context.Get_Help
              (Switch /= "--help-all", null);
         begin
            Put_Line (Help);
         end;

         GPS_Command_Line.Do_Exit := True;

      elsif Switch = "-X" then
         Handle_X_Switch (ICS.Value (Value));

      elsif Switch = "--version" or else Switch = "-v" then
         declare
            Version : constant String :=
                        "GPS " &
                        Config.Version & " (" &
                        Config.Source_Date & ") hosted on " &
                        Config.Target;
         begin
            Put_Line (Version);
         end;

         GPS_Command_Line.Do_Exit := True;

      elsif Switch = "--debug" then
         Free (Program_Args);

         if Value /= ICS.Null_Ptr then
            Program_Args := new String'(ICS.Value (Value));
         else
            Program_Args := new String'("");
         end if;

      elsif Switch = "--debugger" then
         Free (Debugger_Name);
         Debugger_Name := new String'(ICS.Value (Value));

         if Program_Args = null then
            --  --debugger implies --debug
            Program_Args := new String'("");
         end if;

      elsif Switch = "--hide" then
         Hide_GPS := True;

      elsif Switch = "--readonly" then
         Src_Editor_Box.Read_Only_By_Default;

      elsif Switch = "--host" then
         Free (Tools_Host);
         Tools_Host := new String'(ICS.Value (Value));

      elsif Switch = "--load" then
         Free (Batch_File);
         Batch_File := new String'(ICS.Value (Value));

      elsif Switch = "--eval" then
         Free (Batch_Script);
         Batch_Script := new String'(ICS.Value (Value));

      elsif Switch = "--server" then
         begin
            Port_Number := Natural'Value (ICS.Value (Value));
            Server_Mode := True;
         exception
            when Constraint_Error =>
               return 0;
         end;

      elsif Switch = "--target" then
         declare
            Param  : constant String := ICS.Value (Value);
            Column : constant Natural :=
                       Ada.Strings.Fixed.Index
                         (Param, ":", Ada.Strings.Backward);

         begin
            --  Param should be of the form target:protocol

            if Column = 0 then
               raise Invalid_Switch;
            end if;

            Free (Target);
            Free (Protocol);
            Target   :=
              new String '(Param (Param'First .. Column - 1));
            Protocol :=
              new String '(Param (Column + 1 .. Param'Last));
         end;

      elsif Switch = "--traceon" then
         GNATCOLL.Traces.Set_Active (Create (ICS.Value (Value)), True);

      elsif Switch = "--traceoff" then
         GNATCOLL.Traces.Set_Active (Create (ICS.Value (Value)), False);

      elsif Switch = "--tracefile" then
         GNATCOLL.Traces.Parse_Config_File
           (Filename => Create_From_Base (+ICS.Value (Value)));

      elsif Switch = "--tracelist" then
         GNATCOLL.Traces.Show_Configuration
           (Ada.Text_IO.Put_Line'Access);
         GPS_Command_Line.Do_Exit := True;
      end if;

      return 1;
   end On_Switch;

   --------------------
   -- On_File_Switch --
   --------------------

   function On_File_Switch
     (Option_Name : ICS.chars_ptr;
      Value       : ICS.chars_ptr;
      Data        : System.Address;
      Error       : access Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Option_Name, Data, Error);
      FName : constant String := ICS.Value (Value);
      Item  : File_To_Open;

   begin
      if FName (FName'First) = '-' then
         --  Ignore switches
         return 0;

      elsif FName (FName'First) = '=' then
         --  =<basename> means open from project
         Item.File :=
           To_Unbounded_String (FName (FName'First + 1 .. FName'Last));
         Item.Line := GPS_Command_Line.Line;
         GPS_Command_Line.Line := 1;
         Item.From_Project := True;
         Files_To_Open.Append (Item);

      elsif FName (FName'First) = '+' then
         --  +<line number> means open next file on command line at a
         --  specific line number
         GPS_Command_Line.Line :=
            Positive'Value (FName (FName'First + 1 .. FName'Last));

      else
         Item.File := To_Unbounded_String (FName);
         Item.Line := GPS_Command_Line.Line;
         GPS_Command_Line.Line := 1;
         Files_To_Open.Append (Item);
      end if;

      return 1;
   end On_File_Switch;

   ------------------------
   -- Build_Command_Line --
   ------------------------

   procedure Build_Command_Line is

      function To_Gchar (C : Character) return Glib.Gchar;
      function To_Gchar (C : Character) return Glib.Gchar is
      begin
         return Glib.Gchar (Interfaces.C.char'(Interfaces.C.To_C (C)));
      end To_Gchar;

      use type Glib.Gboolean;
      Opt_Project  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("project"),
                        Short_Name      => To_Gchar ('P'),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Load project file project or project.gpr"),
                        Arg_Description => ICS.New_String ("project"));
      Opt_Scenario  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("scenario"),
                        Short_Name      => To_Gchar ('X'),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Set the value of a scenario variable"),
                        Arg_Description => ICS.New_String ("var=value"));
      Opt_Help     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("help"),
                        Short_Name      => To_Gchar ('h'),
                        Flags           => G_Option_Flag_No_Arg,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Show this help message and exit"),
                        Arg_Description => ICS.Null_Ptr);
      Opt_Help_All : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("help-all"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => G_Option_Flag_No_Arg +
                                         G_Option_Flag_Hidden,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Show this help message with all options"),
                        Arg_Description => ICS.Null_Ptr);
      Opt_Version  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("version"),
                        Short_Name      => To_Gchar ('v'),
                        Flags           => G_Option_Flag_No_Arg,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Show the GPS version and exit"),
                        Arg_Description => ICS.Null_Ptr);
      Opt_Debug    : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("debug"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => G_Option_Flag_Optional_Arg +
                                         G_Option_Flag_Filename,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Start a debug session"),
                        Arg_Description => ICS.New_String ("[program]"));
      Opt_Debugger : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("debugger"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => G_Option_Flag_Filename,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Specify the debugger's command line"),
                        Arg_Description => ICS.New_String
                          ("debugger"));
      Opt_Hide     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("hide"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => G_Option_Flag_No_Arg,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Hide GPS main window"),
                        Arg_Description => ICS.Null_Ptr);
      Opt_Host     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("host"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Use tools_host to launch tools (e.g. gdb)"),
                        Arg_Description => ICS.New_String ("tools_host"));
      Opt_Target   : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("target"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Load program on machine TARG using " &
                           "protocol PRO"),
                        Arg_Description => ICS.New_String ("TARG:PRO"));
      Opt_Load     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("load"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Execute an external file written in " &
                           "the language lang"),
                        Arg_Description => ICS.New_String ("lang:file"));
      Opt_Eval     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("eval"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Execute a command written in the language " &
                           "lang (before --load)"),
                        Arg_Description => ICS.New_String ("lang:cmd"));
      Opt_Readonly : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("readonly"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => G_Option_Flag_No_Arg,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Open all files in read-only mode"),
                        Arg_Description => ICS.Null_Ptr);
      Opt_Server   : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("server"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Start GPS in server mode, opening a " &
                           "socket on the given port"),
                        Arg_Description => ICS.New_String ("port"));
      Opt_Traceon  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("traceon"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Activate traces for a specific debug stream"),
                        Arg_Description => ICS.New_String ("stream"));
      Opt_Traceoff : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => ICS.New_String ("traceoff"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => ICS.New_String
                          ("Disable traces for a specific debug stream"),
                        Arg_Description => ICS.New_String ("stream"));
      Opt_Tracefile : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => ICS.New_String ("tracefile"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => G_Option_Flag_Filename,
                         Arg             => G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => ICS.New_String
                           ("Load traces configuration from file"),
                         Arg_Description => ICS.New_String ("file"));
      Opt_Tracelist : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => ICS.New_String ("tracelist"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => G_Option_Flag_No_Arg,
                         Arg             => G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => ICS.New_String
                           ("List all available debug streams"),
                         Arg_Description => ICS.Null_Ptr);
      --  Option for remaining arguments
      Opt_Remaining : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => ICS.New_String (""),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => G_Option_Flag_Filename,
                         Arg             => G_Option_Arg_Callback,
                         Arg_Data        => On_File_Switch'Address,
                         Description     => ICS.Null_Ptr,
                         Arg_Description => ICS.Null_Ptr);
      Opt_Entries   : constant Glib.Option.GOption_Entry_Array :=
                        (Opt_Project,
                         Opt_Scenario,
                         Opt_Help,
                         Opt_Help_All,
                         Opt_Version,
                         Opt_Debug,
                         Opt_Debugger,
                         Opt_Hide,
                         Opt_Host,
                         Opt_Target,
                         Opt_Load,
                         Opt_Eval,
                         Opt_Readonly,
                         Opt_Server,
                         Opt_Traceon,
                         Opt_Traceoff,
                         Opt_Tracefile,
                         Opt_Tracelist,
                         Opt_Remaining,
                         Null_GOption_Entry);

      function Get_Gtk_Option_Group
        (Open_Default_Display : Glib.Gboolean)
            return Glib.Option.GOption_Group;
      pragma Import (C, Get_Gtk_Option_Group, "gtk_get_option_group");

   begin
      GPS_Command_Line.Context := Glib.Option.G_New
        ("[[+line1] source1] [[+line2] source2] ...");

      GPS_Command_Line.Context.Set_Summary
        ("source1, source2, ..." & ASCII.LF
           & "    Name of files to load. Start with '=' to load from project"
           & ASCII.LF
           & "    and use +line to go to <line> directly, e.g. +40 source1");

      GPS_Command_Line.Context.Add_Group (Get_Gtk_Option_Group (1));
      GPS_Command_Line.Context.Add_Main_Entries (Opt_Entries, "gps");

      --  Default Help implementation immediately returns. We'd like to
      --  perform cleanup beforehand
      GPS_Command_Line.Context.Set_Help_Enabled (False);
   end Build_Command_Line;

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
      --  done on the activate signal, as GPS allows multiple instances.
      Trace (Me, "GApplication Activated");
   end Activate_Callback;

   ---------------------------
   -- Command_Line_Callback --
   ---------------------------

   function Command_Line_Callback
     (Application  : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Glib.Gint
   is
      App     : constant GPS_Application := GPS_Application (Application);
      Menubar : Gtk_Menu_Bar;
      Data    : Process_Data;
      Tmp     : Boolean;
      pragma Unreferenced (Command_Line, Tmp);

   begin
      --  Create the kernel and prepare the menu model

      Gtk_New (App.Kernel, App, GPS_Home_Dir, Prefix_Dir);

      --  Display the splash screen, if needed, while we continue loading
      Display_Splash_Screen;
      Data := (App.Kernel, null, null, null, null, null, False);

      Create_MDI_Preferences (App.Kernel);
      Install_Menus
        (App.Kernel,
         App,
         Create_From_Base
           ("menus.xml", Get_Share_Dir (App.Kernel).Full_Name.all),
         Menubar => Menubar);

      --  Finally create the main window, and setup the project

      GPS.Main_Window.Gtk_New (GPS_Main, App, Menubar);

      GPS.Stock_Icons.Register_Stock_Icons (App.Kernel, Prefix_Dir);
      App.Kernel.Set_Environment (Env);

      --  Make the /Window menu dynamic.
      --  ??? This does not work with GtkApplication menu

      declare
         Menu_Item : Gtk_Menu_Item;
      begin
         Menu_Item := Find_Menu_Item (App.Kernel, -"/Window");
         Set_Submenu
           (Menu_Item, Kernel_Desktop.Create_Menu
              (GPS_Main.MDI,
               User         => App.Kernel,
               Registration =>
                 GPS.Kernel.Modules.UI.Register_MDI_Menu'Access));
      end;

      Set_Project_Name;

      if Is_Regular_File
        (Create_From_Dir (Prefix_Dir, "share/gps/gps-pro.txt"))
      then
         GPS_Main.Public_Version := False;
      end if;

      Reset_Title (GPS_Main);

      GPS.Menu.Register_Common_Menus (App.Kernel);

      Kernel_Callback.Connect
        (Get_MDI (App.Kernel), Signal_Child_Title_Changed,
         Title_Changed'Access, App.Kernel);

      --  Under Windows, pressing the primary button outside of the slider
      --  should jump by a page increment.

      if Config.Host = Windows then
         Set_Property
           (Gtk.Settings.Get_Default,
            Gtk_Primary_Button_Warps_Slider_Property,
            False);
      end if;

      --  Load CSS must be loaded *after* the call to GPS.Main_Window.Gtk_New
      --  above, since that call loads the theme (Adwaita) and the CSS file
      --  may refer to the theme, for instance for calls such as
      --    @define-color my_color shade(@theme_bg_color, 1.10);
      Load_CSS;

      Tmp := Finish_Setup (Data);

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
                 (Me, "... opening project " &
                    Display_Full_Name (File));

               if not Started then
                  Project_Name := File;
               else
                  Load_Project (GPS_Main.Kernel, File);
               end if;

            else
               Trace
                 (Me, "... opening file " &
                    Display_Full_Name (File));

               --  Else, this is handled at the command line level
               if not Started then
                  Files_To_Open.Append
                    ((File         => To_Unbounded_String (+File.Full_Name),
                      Line         => 1,
                      From_Project => False));
               else
                  Open_File_Editor
                    (GPS_Main.Kernel, File,
                     Project  => No_Project,  --  will choose most appropriate
                     New_File => False);
               end if;
            end if;
         end;
      end loop;

   exception
      when E : others =>
         Trace (Me, E);
   end File_Open_Callback;

   -----------------------
   -- Shutdown_Callback --
   -----------------------

   procedure Shutdown_Callback
     (Application : access Gapplication_Record'Class)
   is
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

      Increase_Indent (Me, "Shutdown");

      Log_File := Create_From_Dir (Get_Home_Dir (Kernel), +"log");
      Pid_File := Create_From_Dir
        (Get_Home_Dir (Kernel), +("log." & Pid_Image));
      Project  := Get_Project (Kernel);

      Set_Destruction_Flag (Kernel, True);

      --  We want to close the debuggers first, to avoid saving debugger
      --  consoles in the desktop.

      GVD_Module.Debug_Terminate (Kernel);

      Get_Messages_Container (Kernel).Save;
      Get_Messages_Container (Kernel).Clear;

      if Get_Registry (Kernel).Tree.Status = Default then
         Trace (Me, "Remove default project on disk, no longer used");
         Delete (Project_Path (Project), Success);
      end if;

      Free_Modules (Kernel);
      Destroy (Kernel);
      GNATCOLL.Traces.Finalize;

      --  Memory used by the xref database.
      --  ??? This should be done in the ali_reader package

      ALI.ALIs.Free;
      ALI.Units.Free;
      ALI.Withs.Free;
      ALI.Args.Free;
      ALI.Linker_Options.Free;
      ALI.Sdep.Free;
      ALI.Xref.Free;

      --  In case of a normal exit, rename log.<pid> as log to avoid
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

      Free (Startup_Dir);

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
      Run_Hook (Application.Kernel, GPS_Started_Hook);
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
                 (Me, "Found project: " &
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
                 (Me, "Project not found in current dir: "
                  & Project_Name.Display_Base_Name);
            end if;
         else
            Trace (Me, "Found project: " &
                     Display_Full_Name (Project_Name));
         end if;

         Free (Passed_Project_Name);
      end if;
   end Set_Project_Name;

   ---------------------------
   -- Display_Splash_Screen --
   ---------------------------

   procedure Display_Splash_Screen is
      File   : constant Virtual_File :=
                 Create_From_Dir (Prefix_Dir, "share/gps/gps-splash.png");
      Image  : Gtk_Image;
      Ignored : Boolean;
   begin
      if not Hide_GPS
        and then Splash_Screen.Get_Pref
        and then File.Is_Regular_File
      then
         Gtk_New (Splash, Window_Toplevel);
         Splash.Set_Type_Hint (Gdk.Window.Window_Type_Hint_Splashscreen);
         Splash.Set_Hexpand (False);
         Splash.Set_Vexpand (False);
         Set_Property (Splash, Decorated_Property, False);
         Set_Position (Splash, Win_Pos_Center);
         Gtk_New (Image, Filename => +File.Full_Name);
         Splash.Add (Image);
         Splash.Show_All;

         --  The following is required for the splash screen to be fully
         --  painted before the startup process continues.
         Splash.Show_Now;
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
        Prefix_Dir.Create_From_Dir ("share/gps/gps.css");
      Local  : constant Virtual_File :=
        GPS_Home_Dir.Create_From_Dir ("gps.css");
   begin
      if Global.Is_Regular_File then
         Trace (Me, "Loading " & Global.Display_Full_Name);
         Gtkada.Style.Load_Css_File
           (Global.Display_Full_Name, Put_Line'Access,
            Priority_Settings);
      end if;

      if Local.Is_Regular_File then
         Trace (Me, "Loading " & Local.Display_Full_Name);
         Gtkada.Style.Load_Css_File
           (Local.Display_Full_Name, Put_Line'Access,
            Priority_User);
      end if;
   end Load_CSS;

   ------------------
   -- Finish_Setup --
   ------------------

   function Finish_Setup (Data : Process_Data) return Boolean is
      Auto_Load_Project : Boolean := True;
      File_Opened       : Boolean := False;
      Idle_Id           : Glib.Main.G_Source_Id;
      Project           : Project_Type;
      Screen            : Welcome_Screen;
      Icon              : Gdk_Pixbuf;
      pragma Unreferenced (Data, Idle_Id);

      procedure Setup_Debug;
      --  Load appropriate debugger project and set up debugger-related
      --  properties.

      function Setup_Project return Boolean;
      --  When no project has been specified explicitely by the user,
      --  look for a project on the current directory, or use the welcome
      --  dialog
      --  Return False if set up was aborted and GPS should exit.

      procedure Load_Sources;
      --  Load all the source files given on the command line

      -----------------
      -- Setup_Debug --
      -----------------

      procedure Setup_Debug is
      begin
         File_Opened := True;
         Auto_Load_Project := False;

         if Project_Name /= No_File then
            --  Do not clear to keep the welcome message on kernel's console
            Load_Project (GPS_Main.Kernel, Project_Name, Clear => False);
            Project := Get_Project (GPS_Main.Kernel);
         else
            Load_Empty_Project (GPS_Main.Kernel);
            Project := Get_Project (GPS_Main.Kernel);
            Get_Registry (GPS_Main.Kernel).Tree.Set_Status (From_Executable);
         end if;

         --  Project will be overriden when the executable is loaded
         Load_Sources;

         if Debugger_Name /= null then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Debugger_Command_Attribute,
               Value     => Debugger_Name.all);
         end if;

         --  ??? re-enable this...
--           if Tools_Host /= null then
--              Project.Set_Attribute
--                (Scenario  => All_Scenarios,
--                 Attribute => Remote_Host_Attribute,
--                 Value     => Tools_Host.all);
--           end if;

         if Target /= null then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Program_Host_Attribute,
               Value     => Target.all);
         end if;

         if Protocol /= null then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Protocol_Attribute,
               Value     => Protocol.all);
         end if;

         Project.Set_Attribute
           (Scenario  => All_Scenarios,
            Attribute => Languages_Attribute,
            Values    =>
              (new String'("ada"), new String'("c"), new String'("c++")));

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

         --  Load the project selected by the user

         Gtk_New (Screen, GPS_Main.Kernel, Project_Name);

         --  Remove the splash screen, since it conflicts with the
         --  welcome dialog.

         if Splash /= null then
            Destroy (Splash);
            Splash := null;
         end if;

         --  If the user wants to quit immediately, so be it

         case Run_Welcome (Screen) is
            when Quit_GPS =>
               Destroy (Screen);
               GPS_Main.Application.Quit;

               return False;

            when Project_Loaded =>
               --  Desktop was already loaded when the project itself
               --  was loaded.
               null;
         end case;

         Destroy (Screen);

         return True;
      exception
         when E : others =>
            Unexpected_Exception := True;
            Trace (Me, E);
            if not Hide_GPS then
               Error_Message
                 ("Unexpected fatal error during project load.");
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
               Open_File_Editor
                 (GPS_Main.Kernel,
                  Create
                    (Normalize_To_OS_Case (+To_String (File_Item.File)),
                     GPS_Main.Kernel,
                     Use_Source_Path => File_Item.From_Project,
                     Use_Object_Path => False),
                  Project => No_Project,  --  will choose most appropriate
                  Line    => File_Item.Line);
               File_Opened := True;
            end loop;

            --  Load a dummy project, in case the wizard needs to be launched

            if not Auto_Load_Project and then not File_Opened then
               Load_Empty_Project (GPS_Main.Kernel);
            end if;
         end if;
      end Load_Sources;

   begin
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

      Register_Default_Script_Commands (GPS_Main.Kernel);

      GPS.Kernel.Xref.Register_Module (GPS_Main.Kernel);

      GPS.Kernel.Messages.Register_Module (GPS_Main.Kernel);
      GPS.Kernel.Messages.Shell.Register_Commands (GPS_Main.Kernel);
      GPS.Kernel.Style_Manager.Shell.Register_Commands (GPS_Main.Kernel);

      --  Register this very early so that other modules can access remote
      --  files. Note that we need the scripting capabilities to be initialized
      --  before the remote mode.

      Remote_Module.Register_Module (GPS_Main.Kernel);
      GPS.Kernel.Remote.Register_Module (GPS_Main.Kernel);
      Remote.Rsync.Register_Module (GPS_Main.Kernel);

      GPS.Location_View.Register_Commands (GPS_Main.Kernel);
      GPS.Kernel.Clipboard.Register_Commands (GPS_Main.Kernel);

      --  We then must register the keymanager, so that other modules can
      --  register their keys

      KeyManager_Module.Register_Module (GPS_Main.Kernel);
      KeyManager_Module.Macros.Register_Module (GPS_Main.Kernel);
      Command_Window.Register_Module (GPS_Main.Kernel);
      Register_Keys (GPS_Main);

      --  Register the standard hooks. Other modules were able to connect to
      --  these earlier anyway, but these add shell commands, and therefore
      --  must be loaded after the script modules

      Register_Action_Hooks (GPS_Main.Kernel);
      Register_Standard_Hooks (GPS_Main.Kernel);

      --  Load the theme manager module immediately, so that any customization
      --  file or module can provide its own themes.

      Theme_Manager_Module.Register_Module (GPS_Main.Kernel);

      Vsearch.Register_Module (GPS_Main.Kernel);

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

      --  Initialize the ada semantic tree module

      Ada_Semantic_Tree_Module.Register_Module
        (GPS_Main.Kernel,
         Create_From_Dir (Prefix_Dir, "share/gps/predefined_ada.xml"));

      GPS.Kernel.Entities.Register_Module (GPS_Main.Kernel);

      Browsers.Canvas.Register_Actions (GPS_Main.Kernel);

      if Active (Call_Graph_Trace) then
         Browsers.Call_Graph.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Call_Graph_View_Trace) then
         Call_Graph_Views.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Clipboard_View_Trace) then
         Clipboard_Views.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Dependency_Trace) then
         Browsers.Dependency_Items.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Files_Explorer_Trace) then
         Project_Explorers_Files.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Browser_Trace) then
         Browsers.Projects.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Browsers_Trace) then
         Browsers.Scripts.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Entities_Browser_Trace) then
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

      if Active (Outline_View_Trace) then
         Outline_View.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Explorer_Trace) then
         Project_Explorers.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Properties_Trace) then
         Project_Properties.Register_Module (GPS_Main.Kernel);
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
         GVD_Module.Register_Module (GPS_Main.Kernel);
      end if;

      Vdiff2_Module.Register_Module (GPS_Main.Kernel);

      if Active (Scenario_View_Trace) then
         Scenario_Views.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Search.GUI.Register_Module (GPS_Main.Kernel);
      Task_Manager.GUI.Register_Module (GPS_Main.Kernel);
      GPS.Kernel.Task_Manager.Register_Module (GPS_Main.Kernel);

      if Active (VCS_Trace) then
         VCS_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (VFS_Trace) then
         VFS_Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Register the libclang module for C and C++ semantic support

      Language.Libclang.Register_Module (GPS_Main.Kernel);

      if Active (Codefix_Trace) then
         Codefix_Module.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Kernel.Preferences.Register_Module (GPS_Main.Kernel);

      if Active (Custom_Trace) then
         Custom_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Project_Templates_Trace) then
         Project_Templates.GPS.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Refactor_Trace) then
         Refactoring_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (Code_Analysis_Trace) then
         Code_Analysis_Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (CodePeer_Trace) then
         CodePeer.Module.Register_Module (GPS_Main.Kernel);
      end if;

      if Active (GNATStack_Trace) then
         GNATStack.Module.Register_Module (GPS_Main.Kernel);
      end if;

      --  Register the supported languages and their associated LI handlers

      Ada_Module.Register_Module (GPS_Main.Kernel);

      if Active (CPP_Trace) then
         Cpp_Module.Register_Module (GPS_Main.Kernel);
      end if;

      Casing_Exceptions.Register_Module (GPS_Main.Kernel);

      Switches_Chooser.Scripts.Register_Module (GPS_Main.Kernel);

      --  Load these last, since this requires the collaboration of other
      --  modules

      Bookmark_Views.Register_Module (GPS_Main.Kernel);

      Buffer_Views.Register_Module (GPS_Main.Kernel);

      if Active (Startup_Trace) then
         Startup_Module.Register_Module (GPS_Main.Kernel);
      end if;

      GPS.Kernel.Custom.Parse_Startup_Scripts_List (GPS_Main.Kernel);

      --  Load system files.
      --  This must be done before loading the Navigation module, since that
      --  module relies on icons defined in custom files.

      if Active (Custom_Trace) then
         Load_System_Custom_Files (GPS_Main.Kernel);
      end if;

      --  Create a hook for when GPS is started

      Register_Hook_No_Args (GPS_Main.Kernel, GPS_Started_Hook);

      --  Load the default key theme before the python plug-ins, so that the
      --  latter can override

      KeyManager_Module.Load_Key_Theme (GPS_Main.Kernel);

      --  Load the customization files before loading the actual projects,
      --  so that the usual hooks are taken into account right from the
      --  beginning

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

      Navigation_Module.Register_Module (GPS_Main.Kernel);

      if Server_Mode then
         Socket_Module.Register_Module (GPS_Main.Kernel, Port_Number);
      end if;

      --  Load preferences, but only after loading custom files, to make sure
      --  the themes loaded at startup are still overriden by the user's
      --  local choices. Note that the preferences have already been loaded
      --  once before, to take into account the splash screen pref for instance

      Load_Preferences (GPS_Main.Kernel);

      --  Load the custom keys last, so that they override everything else set
      --  so far.
      KeyManager_Module.Load_Custom_Keys (GPS_Main.Kernel);

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

      Icon := Gtk.Icon_Theme.Get_Default.Load_Icon_For_Scale
         ("gps_32", 32,
          Scale => GPS_Main.Get_Scale_Factor,
          Flags => 0, Error => null);

      if Icon /= null then
         Set_Default_Icon (Icon);
      end if;

      --  Print a welcome message in the console, but before parsing the error
      --  messages, so that these are visible

      declare
         About_Contents : String_Access := Create_From_Dir
           (Prefix_Dir, "share/gps/about.txt").Read_File;
      begin
         if About_Contents = null then
            About_Contents := new String'("");
         end if;
         GPS_Main.Kernel.Insert
           (-"Welcome to GPS " & Config.Version &
              " (" & Config.Source_Date &
            (-") hosted on ") & Config.Target & ASCII.LF &
            (-"the GNAT Programming Studio") & ASCII.LF & About_Contents.all &
              "(c) 2001-" & Config.Current_Year & " AdaCore" & ASCII.LF);
         Free (About_Contents);
      end;

      --  Apply the preferences to the MDI. In particular, we want to set the
      --  default position for notebook tabs, since they can be overriden by
      --  the desktop (and changing that default later on would switch all
      --  notebooks)

      Configure_MDI (GPS_Main.Kernel);

      --  When the hooks have been registered, in particular context_changed,
      --  we can start monitoring focus change.
      Kernel_Callback.Connect
        (Get_MDI (GPS_Main.Kernel), Signal_Child_Selected,
         Child_Selected'Access, GPS_Main.Kernel);

      --  We now make sure we have a project loaded, so that opening editors
      --  will work correctly.

      --  If no project has been specified on the command line, try to open
      --  the first one in the current directory (if any).

      if Program_Args /= null then
         --  --debug has been specified
         --  Load project, and set debugger-related project properties

         Setup_Debug;

      else
         if Project_Name /= No_File
           and then not Is_Regular_File (Project_Name)
         then
            --  We can finally search on ADA_PROJECT_PATH, which is now known

            declare
               P : constant Virtual_File := Locate_Regular_File
                 (Base_Name (Project_Name),
                  Get_Registry (GPS_Main.Kernel)
                  .Environment.Predefined_Project_Path);
            begin
               if P /= No_File then
                  Project_Name := P;
               else
                  --  Keep the user project, which will display an error in
                  --  GPS.
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

      if Auto_Load_Project and then Project_Name /= No_File then
         --  Do not clear to keep the welcome message on kernel's console
         Load_Project (GPS_Main.Kernel, Project_Name, Clear => False);
         Load_Sources;
      end if;

      if not File_Opened
        and then not Has_User_Desktop (GPS_Main.Kernel)
      then
         Display_Welcome_Page (GPS_Main.Kernel);
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

      declare
         D : aliased Preference_Hooks_Args :=
           (Hooks_Data with Pref => null);
      begin
         Run_Hook (GPS_Main.Kernel, Preference_Changed_Hook, D'Access);
      end;

      if not Hide_GPS then
         GPS_Main.Present;
      end if;

      if Program_Args /= null then
         --  Initialize the debugger after having executed scripts if any,
         --  so that it is possible to set up the environment before starting
         --  a debug session.
         --  Needs to be done after the call to Show, so that the GPS window
         --  already has a proper size, otherwise we might end up with windows
         --  with height=0 or width=0
         GVD_Module.Initialize_Debugger (GPS_Main.Kernel, Program_Args.all);
      end if;

      --  Execute the startup scripts now, even though it is recommended that
      --  they connect to the GPS_Started_Hook if they have graphical actions
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

      --  Set the title of the GPS window
      Set_Main_Title
        (GPS_Main.Kernel, Get_Focus_Child (Get_MDI (GPS_Main.Kernel)));

      Idle_Id := Glib.Main.Idle_Add (On_GPS_Started'Unrestricted_Access);

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
      Trace (Me, "Execute_Batch: " & Batch);
      for J in Batch'Range loop
         if Batch (J) = ':' then
            Script := Lookup_Scripting_Language
              (Get_Scripts (GPS_Main.Kernel), Batch (Batch'First .. J - 1));
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
           (Get_Scripts (GPS_Main.Kernel), "python");
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
         Trace (Me, E);
   end Execute_Batch;

   -----------------------------------
   -- Default_Gtk_Mer --
   -----------------------------------

   procedure Default_Gtk_Mer
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Trace (Gtk_Errors, Occurrence);
   end Default_Gtk_Mer;

   -------------------
   -- Error_Message --
   -------------------

   procedure Error_Message (Message : String) is
      Log_File : Virtual_File;
      Pid_File : Virtual_File;
      Str      : Virtual_File;
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      --  Error before we created the main window is likely while parsing
      --  command line switches.
      if GPS_Main = null then
         Put_Line (Message);
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

      if Active (Testsuite_Handle) then
         Put_Line ("Error message generated: " & Message);
      else
         Button := Message_Dialog
           (Message
            & ASCII.LF
            & "Please report with contents of " & Str.Display_Full_Name,
            Error, Button_OK,
            Title         => -"Fatal Error",
            Justification => Justify_Left);
      end if;
   end Error_Message;

   ------------------------
   -- Local_Command_Line --
   ------------------------

   function Local_Command_Line
      (Self        : System.Address;
       Arguments   : access chars_ptr_array_access;
       Exit_Status : access Glib.Gint) return Glib.Gboolean
   is
      pragma Unreferenced (Self);
      Err     : Glib.Error.GError;
      Success : Boolean;
      A       : size_t := 0;
      B       : size_t := 0;
   begin
      --  Sanitize the environment variables, and perform various init from
      --  them
      Initialize_Environment_Variables (null);

      --  Now perform the low level initializations
      Initialize_Low_Level (Exit_Status.all);

      if Exit_Status.all /= 0 then
         return 1;
      end if;

      --  Handle switches before we connect to the X11 server
      Build_Command_Line;

      --  Goption does not handle case of arguments attached to the switch,
      --  so we need to handle those specially first.

      while Arguments.all (A) /= Null_Ptr loop
         declare
            Val : constant String := Value (Arguments.all (A));
            Handled : Boolean := False;
         begin
            --  Ignore when -P is separate, it will be handled later
            if Val'Length > 2 and then Val (Val'First) = '-' then
               if Val (Val'First + 1) = 'P' then
                  Passed_Project_Name :=
                     new String'(Val (Val'First + 2 .. Val'Last));
                  Handled := True;

               elsif Val (Val'First + 1) = 'X' then
                  Handle_X_Switch (Val (Val'First + 2 .. Val'Last));
                  Handled := True;
               end if;
            end if;

            --  Will preserve the argument
            if not Handled then
               if A /= B then
                  Arguments.all (B) := Arguments.all (A);
               end if;
               B := B + 1;
            end if;
         end;
         A := A + 1;
      end loop;
      Arguments.all (B) := Null_Ptr;

      --  Now the standard parsing

      GPS_Command_Line.Context.Parse (Arguments, Success, Err);

      if not Success then
         if Err /= null then
            Put_Line (Get_Message (Err));
         else
            Put_Line ("Error parsing command line switches");
         end if;
         GPS_Command_Line.Do_Exit := True;
      end if;

      if GPS_Command_Line.Do_Exit then
         Exit_Status.all := 0;
         return 1;  --  Suppress default handling
      end if;
      return 0;
   end Local_Command_Line;

   ----------------------------
   -- Application_Class_Init --
   ----------------------------

   procedure Application_Class_Init (Self : GObject_Class) is
   begin
      Set_Local_Command_Line (Self, Local_Command_Line'Unrestricted_Access);
   end Application_Class_Init;

   Dead        : Boolean;
   Registered  : Boolean;
   pragma Unreferenced (Registered);
   Status      : Glib.Gint;
   pragma Unreferenced (Dead);

begin
   --  Under Windows, prevent the creation of a dbus session: this serves
   --  no purpose, breaks the testsuite, slows down the startup of the first
   --  GPS instance, and is flaky under Windows XP.

   if Config.Host = Windows then
      --  If, for some obscure reason, there is a DBUS address specified,
      --  allow it.
      declare
         Bus_Addr : String_Access := Getenv ("DBUS_SESSION_BUS_ADDRESS");
      begin
         if Bus_Addr = null
           or else Bus_Addr.all = ""
         then
            Setenv ("DBUS_SESSION_BUS_ADDRESS", "null");
         end if;
         Free (Bus_Addr);
      end;
   end if;

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
      Trace (Me, E);
      if GPS_Main /= null then
         Error_Message
           ("Unexpected fatal error, GPS is in an inconsistent state"
            & ASCII.LF
            & "You will be asked to save modified files before GPS exits");
         Dead := Save_MDI_Children (GPS_Main.Kernel, Force => False);
      else
         Put_Line ("Unexpected fatal error, GPS is in an inconsistent state");
      end if;
end GPS.Main;
