-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Error;                use Glib.Error;
with Glib.Object;               use Glib.Object;
with Pango.Font;                use Pango.Font;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk;                       use Gtk;
with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Window;                use Gtk.Window;
with Gtk.Rc;

with Glide_Menu;
with Glide_Main_Window;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with String_Utils;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Gtkada.Intl;               use Gtkada.Intl;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;
with GVD.Types;
with OS_Utils;                  use OS_Utils;
with Projects.Editor;           use Projects.Editor;
with Projects.Registry;         use Projects;
with GNAT.Command_Line;         use GNAT.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;
with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Welcome;                   use Welcome;
with DDE;

--  Modules registered by GPS.
with Ada_Module;
with Aliases_Module;
with Aunit_Module;
with Browsers.Dependency_Items;
with Browsers.Projects;
with Browsers.Call_Graph;
with Browsers.Entities;
with Cpp_Module;
with External_Editor_Module;
with GVD_Module;
with Metrics_Module;
with Project_Explorers;
with Project_Explorers_Files;
with Project_Viewers;
with Shell_Script;
with Src_Editor_Module;
with VCS_Module;
with VCS.CVS;
with VCS.ClearCase;
with VFS_Module;
with Vdiff_Module;
with Builder_Module;
with Glide_Kernel.Console;
with Navigation_Module;
with Custom_Module;
with Vsearch_Ext;
with Help_Module;
with Codefix_Module;
with Python_Module;

procedure GPS is
   use Glide_Main_Window;

   Override_Gtk_Theme : constant Boolean := True;
   --  Set to False for a True integration with a pre installed Gtk+ 2.0
   --  environment.

   Me : constant Debug_Handle := Create ("GPS");

   subtype String_Access is GNAT.OS_Lib.String_Access;

   GPS            : Glide_Window;
   Directory      : Dir_Type;
   Str            : String (1 .. 1024);
   Last           : Natural;
   Project_Name   : String_Access;
   Prj            : String_Access;
   Button         : Message_Dialog_Buttons;
   pragma Unreferenced (Button);
   Home           : String_Access;
   Prefix         : String_Access;
   Dir            : String_Access;
   Target,
   Protocol,
   Debugger_Name  : String_Access;
   Splash         : Gtk_Window;
   Timeout_Id     : Timeout_Handler_Id;
   User_Directory_Existed : Boolean;
   pragma Unreferenced (Timeout_Id);
   Result         : Boolean;
   pragma Unreferenced (Result);

   procedure Init_Settings;
   --  Set up environment for GPS.

   procedure Display_Splash_Screen;
   --  Display the GPS splash screen

   function Finish_Setup (Data : Process_Data) return Boolean;
   --  Finish the set up of GPS, while the main loop is running.

   procedure Help;
   --  Display help on the standard output.

   procedure Ctrl_C_Handler;
   --  Handler for Ctrl-C events.

   function Clean_Parameter return String;
   --  Return a clean version of the parameter for command line switches, ie
   --  return the same thing as GNAT.Command_Line.Parameter, but strips the
   --  leading '=' if any, so that users can say '--log-level=4' for instance.

   procedure Child_Selected
     (MDI : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when a new child is selected

   ---------------------
   -- Clean_Parameter --
   ---------------------

   function Clean_Parameter return String is
      P : constant String := Parameter;
   begin
      if P'Length > 0 and then P (P'First) = '=' then
         return P (P'First + 1 .. P'Last);
      else
         return P;
      end if;
   end Clean_Parameter;

   ---------------------------
   -- Display_Splash_Screen --
   ---------------------------

   procedure Display_Splash_Screen is
      File   : constant String := Format_Pathname
        (GPS.Prefix_Directory.all & "/share/gps/gps-splash.jpg");
      Image  : Gtk_Image;
      Pixbuf : Gdk_Pixbuf;
      Error  : GError;

   begin
      if Get_Pref (GPS.Kernel, Splash_Screen)
        and then Is_Regular_File (File)
      then
         Gtk_New (Splash, Window_Popup);
         Set_Policy (Splash,
                     Allow_Shrink => False,
                     Allow_Grow   => False,
                     Auto_Shrink  => False);
         Set_Position (Splash, Win_Pos_Center);
         Gdk_New_From_File (Pixbuf, File, Error);
         Gtk_New (Image, Pixbuf);
         Add (Splash, Image);
         Show_All (Splash);
      end if;
   end Display_Splash_Screen;

   ----------
   -- Init --
   ----------

   procedure Init_Settings is
      Dir_Created : Boolean := False;
      File        : File_Type;
   begin
      --  Set the TERM variable to a dummy value, since we only know how to
      --  handle simple terminals

      Setenv ("TERM", "dumb");

      Home := Getenv ("GPS_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      if Home.all = "" then
         Free (Home);

         if Directory_Separator = '\' then
            Home := new String'("c:\");
         else
            Home := new String'("/");
         end if;
      end if;

      Prefix := Getenv ("GPS_ROOT");

      if Prefix.all = "" then
         Free (Prefix);
         Prefix := new String'(Executable_Location);

         if Prefix.all = "" then
            Free (Prefix);
            Prefix := new String'(GVD.Prefix);
         end if;
      end if;

      Bind_Text_Domain
        ("gps", Format_Pathname (Prefix.all & "/share/locale"));

      Dir := new String'(String_Utils.Name_As_Directory (Home.all) & ".gps");

      begin
         User_Directory_Existed := Is_Directory (Dir.all);

         if not User_Directory_Existed then
            Make_Dir (Dir.all);
            Button := Message_Dialog
              ((-"Created config directory ") & Dir.all,
               Information, Button_OK, Justification => Justify_Left);
            Dir_Created := True;

            --  Create a default configuration file for the traces.
            --  This should be left while GPS is considered as not fully
            --  stable.

            Create
              (File,
               Name => String_Utils.Name_As_Directory (Dir.all)
                 & "traces.cfg");
            Put_Line (File, ">log");
            Put_Line (File, "+");
            Put_Line (File, "DEBUG.COLORS=no");
            Put_Line (File, "DEBUG.ABSOLUTE_TIME=yes");
            Put_Line (File, "DEBUG.ELAPSED_TIME=no");
            Put_Line (File, "DEBUG.STACK_TRACE=no");
            Put_Line (File, "DEBUG.LOCATION=no");
            Put_Line (File, "DEBUG.ENCLOSING_ENTITY=no");
            Put_Line (File, "SRC_INFO.ALI=no");
            Put_Line (File, "CPP.INFO=no");
            Put_Line (File, "CPP.FAIL=no");
            Close (File);
         end if;

         if not
           Is_Directory (String_Utils.Name_As_Directory (Dir.all) & "sessions")
         then
            Make_Dir (String_Utils.Name_As_Directory (Dir.all) & "sessions");

            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ")
                  & String_Utils.Name_As_Directory (Dir.all) & "sessions",
                  Information, Button_OK, Justification => Justify_Left);
            end if;
         end if;

         if not Is_Directory
           (String_Utils.Name_As_Directory (Dir.all) & "customize")
         then
            Make_Dir (String_Utils.Name_As_Directory (Dir.all) & "customize");

            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ")
                  & String_Utils.Name_As_Directory (Dir.all) & "customize",
                  Information, Button_OK, Justification => Justify_Left);
            end if;
         end if;

      exception
         when Directory_Error =>
            Button := Message_Dialog
              ((-"Cannot create config directory ") & Dir.all & ASCII.LF &
               (-"Exiting..."),
               Error, Button_OK,
               Justification => Justify_Left);
            OS_Exit (1);
      end;
   end Init_Settings;

   ----------
   -- Help --
   ----------

   procedure Help is
      use ASCII;
   begin
      if GVD.Can_Output then
         Put_Line ("GPS " & GVD.Version & " (" & GVD.Source_Date & ")" &
                   (-", the GNAT Programming System."));
         Put_Line (-"Usage:");
         Put_Line
           (-"   gps [options] [-Pproject-file] [source1] [source2] ...");
         Put_Line (-"Options:");
         Put_Line (-"   --help              Show this help message and exit");
         Put_Line (-"   --version           Show the GPS version and exit");
         Put_Line (-"   --debug[=program]   Start a debug session");
         Put_Line
           (-"   --debugger debugger Specify the debugger's command line");
         Put_Line ((-"   --target=TARG:PRO   ") &
                   (-"Load program on machine TARG using protocol PRO"));
         New_Line;
         Put_Line (-("Source files are searched everywhere on the project's "
                   & " source path"));

      else
         Button := Message_Dialog
           ("GPS " & GVD.Version & " (" & GVD.Source_Date & ")" &
            (-", the GNAT Programming System.") & LF &
            (-"Usage:") & LF &
            (-"   gps [options] [-Pproject-file] [source1] [source2] ...") &
            LF & (-"Options:") & LF &
            (-"   --help              Show this help message and exit.") & LF &
            (-"   --version           Show the GPS version and exit.") & LF &
            (-"   --debug[=program]   Start a debug session") & LF &
            (-"   --debugger debugger Specify the debugger's command line") &
            LF &
            (-"   --target=TARG:PRO   ") &
            (-"Load program on machine TARG using protocol PRO") & LF &
            (-("Source files are searched everywhere on the project's" &
               " source path")),
            Information, Button_OK,
            Title => -"Help",
            Justification => Justify_Left);
      end if;
   end Help;

   --------------------
   -- Ctrl_C_Handler --
   --------------------

   procedure Ctrl_C_Handler is
   begin
      --  Ignore Ctrl-C events

      null;
   end Ctrl_C_Handler;

   ------------------
   -- Finish_Setup --
   ------------------

   function Finish_Setup (Data : Process_Data) return Boolean is
      System_Rc         : constant String :=
        Format_Pathname (Prefix.all & "/etc/gps/gtkrc");
      Rc                : constant String :=
        String_Utils.Name_As_Directory (Dir.all) & "gtkrc";
      Log               : constant String :=
        Get_Home_Dir (GPS.Kernel) & "debugger.log";
      Key               : constant String :=
        Get_Home_Dir (GPS.Kernel) & "custom_key";
      Auto_Load_Project : Boolean := True;
      File_Opened       : Boolean := False;
      Project           : Projects.Project_Type;
      Screen            : Welcome_Screen;
      pragma Unreferenced (Data);

      procedure Load_Sources;
      --  Load all the source files given on the command line.

      procedure Load_Sources is
      begin
         loop
            declare
               S : constant String := Get_Argument (Do_Expansion => True);
            begin
               exit when S = "";

               --  If no project has been loaded yet, load a default project
               --  and desktop before open source editors.

               if not Auto_Load_Project and then not File_Opened then
                  Load_Default_Project
                    (GPS.Kernel, Get_Current_Dir,
                     Load_Default_Desktop => True);
               end if;

               Open_File_Editor (GPS.Kernel, S, 1, 1, From_Path => True);
               File_Opened := True;
            end;
         end loop;

         --  Load a default project, in case the wizard needs to be
         --  launched. Do not load the desktop immediately, since this
         --  would display the GPS window at the same time as the welcome
         --  dialog.

         if not Auto_Load_Project and then not File_Opened then
            Load_Default_Project
              (GPS.Kernel, Get_Current_Dir,
               Load_Default_Desktop => False);
         end if;
      end Load_Sources;

   begin
      --  Parse the system's RC file
      if Is_Regular_File (System_Rc) then
         Trace (Me, "Parsing System RC file " & System_Rc);
         Gtk.Rc.Parse (System_Rc);
      end if;

      --  Parse the user's RC file
      if Is_Regular_File (Rc) then
         Trace (Me, "Parsing RC file " & Rc);
         Gtk.Rc.Parse (Rc);
      end if;

      --  Load the custom key bindings, if any
      if Is_Regular_File (Key) then
         Trace (Me, "Loading key bindings from " & Key);
         Gtk.Accel_Map.Load (Key);
      end if;

      --  ??? Should have a cleaner way of initializing Log_File

      GPS.Log_File := Create_File (Log, Fmode => Text);

      --  Register this module first, in case someone needs to print a message
      --  in the console right away

      Glide_Kernel.Console.Register_Module (GPS.Kernel);

      --  Register all modules (scripting languages must be registered first)

      Shell_Script.Register_Module (GPS.Kernel);
      Python_Module.Register_Module (GPS.Kernel);
      Register_Default_Script_Commands (GPS.Kernel);

      Vsearch_Ext.Register_Module (GPS.Kernel);
      Help_Module.Register_Module (GPS.Kernel);

      Navigation_Module.Register_Module (GPS.Kernel);
      Metrics_Module.Register_Module (GPS.Kernel);
      Browsers.Call_Graph.Register_Module (GPS.Kernel);
      Browsers.Dependency_Items.Register_Module (GPS.Kernel);
      Browsers.Projects.Register_Module (GPS.Kernel);
      Browsers.Entities.Register_Module (GPS.Kernel);
      Project_Viewers.Register_Module (GPS.Kernel);
      Aliases_Module.Register_Module (GPS.Kernel);
      Src_Editor_Module.Register_Module (GPS.Kernel);
      Project_Explorers_Files.Register_Module (GPS.Kernel);
      Project_Explorers.Register_Module (GPS.Kernel);
      External_Editor_Module.Register_Module (GPS.Kernel);
      GVD_Module.Register_Module (GPS.Kernel);
      Builder_Module.Register_Module (GPS.Kernel);
      Vdiff_Module.Register_Module (GPS.Kernel);
      VCS_Module.Register_Module (GPS.Kernel);
      VCS.CVS.Register_Module (GPS.Kernel);
      VCS.ClearCase.Register_Module (GPS.Kernel);
      Aunit_Module.Register_Module (GPS.Kernel);
      VFS_Module.Register_Module (GPS.Kernel);
      Codefix_Module.Register_Module (GPS.Kernel);

      Custom_Module.Register_Module (GPS.Kernel);

      --  Register the supported languages and their associated LI handlers.

      Ada_Module.Register_Module (GPS.Kernel);
      Cpp_Module.Register_Module (GPS.Kernel);

      --  Temporarily disable unimplemented menu items

      declare
         Navigate : constant String := '/' & (-"Navigate") & '/';
         Tools    : constant String := '/' & (-"Tools") & '/';

      begin
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"Goto Parent Unit")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"Start Of Statement")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"End Of Statement")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"Next Procedure")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"Previous Procedure")), False);

         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Tools & (-"Profile")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Tools & (-"Memory Analyzer")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Tools & (-"Generate API doc")), False);
      end;

      --  Print a welcome message in the console, but before parsing the error
      --  messages, so that these are visible

      Console.Insert
        (GPS.Kernel,
         -"Welcome to GPS " & GVD.Version & " (" & GVD.Source_Date &
         (-") hosted on ") & GVD.Target & ASCII.LF &
         (-"the GNAT Programming System") & ASCII.LF &
         "(c) 2001-2003 ACT-Europe");

      --  We now make sure we have a project loaded, so that opening editors
      --  will work correctly.

      --  If no project has been specified on the command line, try to open
      --  the first one in the current directory (if any).

      if GPS.Program_Args /= null then
         --  --debug has been specified
         --  Load default project, and set debugger-related project properties

         File_Opened := True;
         Auto_Load_Project := False;
         Load_Default_Project
           (GPS.Kernel, Get_Current_Dir, Load_Default_Desktop => True);
         Load_Sources;
         Project := Get_Project (GPS.Kernel);

         if Debugger_Name /= null then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => (1 .. 0 => No_Variable),
               Attribute          => Debugger_Command_Attribute,
               Value              => Debugger_Name.all);
         end if;

         if Target /= null then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => (1 .. 0 => No_Variable),
               Attribute          => Program_Host_Attribute,
               Value              => Target.all);
         end if;

         if Protocol /= null then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => (1 .. 0 => No_Variable),
               Attribute          => Protocol_Attribute,
               Value              => Protocol.all);
         end if;

         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => (1 .. 0 => No_Variable),
            Attribute          => Languages_Attribute,
            Values             =>
              (new String'("ada"), new String'("c"), new String'("c++")));

         Set_Project_Modified (Project, False);
         Recompute_View (GPS.Kernel);
         GVD_Module.Initialize_Debugger (GPS.Kernel);

      elsif Project_Name = null then
         Auto_Load_Project := False;
         Open (Directory, Get_Current_Dir);

         loop
            Read (Directory, Str, Last);

            exit when Last = 0;

            if File_Extension (Str (1 .. Last)) = Project_File_Extension then
               if Project_Name = null then
                  Auto_Load_Project := True;
                  Project_Name := new String'(Str (1 .. Last));
               else
                  Auto_Load_Project := False;
                  exit;
               end if;
            end if;
         end loop;

         Close (Directory);

         --  If only one project file was found in the current directory, do
         --  not open the welcome dialog.

         if not Auto_Load_Project then
            Load_Sources;

            if not File_Opened then
               --  Load the project selected by the user

               if Project_Name = null then
                  Gtk_New (Screen, GPS.Kernel, "");
               else
                  Gtk_New (Screen, GPS.Kernel, Project_Name.all);
               end if;

               --  Remove the splash screen, since it conflicts with the
               --  welcome dialog.

               if Splash /= null then
                  Destroy (Splash);
                  Splash := null;
               end if;

               --  If the user wants to quit immediately, so be it.

               case Run_Welcome (Screen) is
                  when Quit_GPS =>
                     Destroy (Screen);
                     Gtk.Main.Main_Quit;
                     return False;

                  when Project_Loaded =>
                     --  Desktop was already loaded when the project itself was
                     --  loaded.
                     null;
               end case;

               Destroy (Screen);
            end if;
         end if;
      end if;

      if Auto_Load_Project then
         if not Is_Regular_File (Project_Name.all)
           and then Is_Regular_File (Project_Name.all & Project_File_Extension)
         then
            Prj := Project_Name;
            Project_Name := new String'(Prj.all & Project_File_Extension);
            Free (Prj);
         end if;

         Load_Project (GPS.Kernel, Project_Name.all);
         Load_Sources;
      end if;

      if not File_Opened
        and then not Has_User_Desktop (GPS.Kernel)
      then
         Open_Html (GPS.Kernel, "gps-welcome.html");
         Maximize_Children (Get_MDI (GPS.Kernel));
      end if;

      if Splash /= null then
         Destroy (Splash);
      end if;

      return False;
   end Finish_Setup;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
     (MDI : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (MDI);
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context /= null then
         Context_Changed (Kernel, Context);
      end if;
   end Child_Selected;

begin
   Home := Getenv ("CHARSET");

   if Home.all = "" then
      --  Gtk+ does not like if CHARSET is not defined.
      Setenv ("CHARSET", "ISO-8859-1");
   end if;

   Free (Home);

   OS_Utils.Install_Ctrl_C_Handler (Ctrl_C_Handler'Unrestricted_Access);
   Projects.Registry.Initialize;

   Gtk.Main.Init;

   Init_Settings;

   --  Initialize the traces
   Traces.Parse_Config_File
     (Default => String_Utils.Name_As_Directory (Dir.all) & "traces.cfg");

   Gtk_New
     (GPS, "<gps>", Glide_Menu.Glide_Menu_Items.all, Dir.all, Prefix.all);
   Reset_Title (GPS);

   Kernel_Callback.Connect
     (Get_MDI (GPS.Kernel), "child_selected",
      Kernel_Callback.To_Marshaller (Child_Selected'Unrestricted_Access),
      GPS.Kernel);

   DDE.Register_DDE_Server (GPS.Kernel);

   GPS.Debug_Mode := True;
   GPS.Log_Level  := GVD.Types.Hidden;

   if Override_Gtk_Theme then
      declare
         Key_Theme : String := Key_Themes'Image
           (Key_Themes'Val (Get_Pref (GPS.Kernel, Key_Theme_Name)));
      begin
         String_Utils.Mixed_Case (Key_Theme);
         Gtk.Rc.Parse_String
           ("gtk-font-name=""" &
            To_String (Get_Pref (GPS.Kernel, Default_Font)) &
            '"' & ASCII.LF &
            "gtk-can-change-accels=" &
            Integer'Image
              (Boolean'Pos
                 (Get_Pref (GPS.Kernel, Can_Change_Accels))) & ASCII.LF &
            "gtk-key-theme-name=""" & Key_Theme & '"');
      end;
   end if;

   --  Switch parsing

   loop
      case Getopt ("-version -help P! -log-level= " &
                   "-debug? -debugger= -target=")
      is
         -- long option names --
         when '-' =>
            case Full_Switch (Full_Switch'First + 1) is
               -- --version --
               when 'v' =>
                  if GVD.Can_Output then
                     Put_Line ("GPS version " & GVD.Version & " (" &
                       GVD.Source_Date & ") hosted on " & GVD.Target);
                  else
                     Button := Message_Dialog
                       ("GPS version " & GVD.Version & " (" &
                        GVD.Source_Date & ") hosted on " & GVD.Target,
                        Information, Button_OK,
                        Title => -"Version",
                        Justification => Justify_Left);
                  end if;

                  OS_Exit (0);

               when 'h' =>
                  -- --help --
                  if Full_Switch = "-help" then
                     Help;
                     OS_Exit (0);
                  end if;

               -- --log-level --
               when 'l' =>
                  begin
                     GPS.Log_Level := GVD.Types.Command_Type'Val
                       (GVD.Types.Command_Type'Pos
                          (GVD.Types.Command_Type'Last) + 1 -
                          Integer'Value (Parameter));

                  exception
                     when Constraint_Error =>
                        if GVD.Can_Output then
                           Put_Line ("Invalid parameter to --log-level");
                        end if;

                        Help;
                        OS_Exit (-1);
                  end;

               when 'd' =>
                  --  --debug
                  if Full_Switch = "-debug" then
                     Free (GPS.Program_Args);
                     GPS.Program_Args := new String'(Clean_Parameter);

                  else
                     --  --debugger
                     Free (Debugger_Name);
                     Debugger_Name := new String'(Parameter);
                  end if;

               -- --target --
               when 't' =>
                  declare
                     Param  : constant String := Parameter;
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

               when others =>
                  null;
            end case;

         when 'P' =>
            Project_Name := new String'
              (Normalize_Pathname (Parameter));
            Trace (Me, "Found project: " & Parameter);

         when ASCII.NUL =>
            exit;

         when others =>
            null;
      end case;
   end loop;

   Display_Splash_Screen;

   if Splash = null then
      Timeout_Id := Process_Timeout.Add
        (1, Finish_Setup'Unrestricted_Access,
         (GPS.Kernel, null, null, null, null));
   else
      Timeout_Id := Process_Timeout.Add
        (1000, Finish_Setup'Unrestricted_Access,
         (GPS.Kernel, null, null, null, null));
   end if;

   begin
      Gtk.Main.Main;
   exception
      when E : others =>
         Trace (Me, "Unhandled exception: " & Exception_Information (E));
         Button := Message_Dialog
           ((-"Unexpected fatal error, GPS is in an inconsistent state") &
            ASCII.LF & (-"Please report with contents of ") &
            Get_Home_Dir (GPS.Kernel) & "log" & ASCII.LF & ASCII.LF &
            (-"You will be asked to save modified files before GPS exits"),
            Error, Button_OK,
            Title => -"Fatal Error",
            Justification => Justify_Left);
         Result := Save_All_MDI_Children (GPS.Kernel, Force => False);
   end;

   Gtk.Accel_Map.Save
     (String_Utils.Name_As_Directory (Dir.all) & "custom_key");

   Free_Modules (GPS.Kernel);

   --  Call Handlers_Destroy after Free_Modules and Glide_Page.Destroy,
   --  since some handlers are already disconnected by these functions, and
   --  only Handlers_Destroy know what handlers are still left and need to be
   --  disconnected.

   Handlers_Destroy (GPS.Kernel);

   --  Since the call to destroy below will free the animation at some point,
   --  we no longer want to access/update it past this point.

   GPS.Animation_Image := null;

   declare
      Kernel : Kernel_Handle;
   begin
      Kernel := GPS.Kernel;
      Destroy (GPS);
      Destroy (Kernel);
   end;

   Projects.Registry.Finalize;
   Traces.Finalize;

   Free (Home);
   Free (Dir);
   Free (Prefix);

exception
   when Invalid_Switch | GNAT.Command_Line.Invalid_Parameter =>
      if GVD.Can_Output then
         Put_Line ("Invalid command line");
      end if;

      Help;

   when E : others =>
      Trace (Me, "Unexpected exception: " & Exception_Information (E));
end GPS;
