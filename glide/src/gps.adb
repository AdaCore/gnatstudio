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
with Glib.Messages;             use Glib.Messages;
with Glib.Object;               use Glib.Object;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk;                       use Gtk;
with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Arguments;             use Gtk.Arguments;
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
with File_Utils;
with String_Utils;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Custom;       use Glide_Kernel.Custom;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
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
with GUI_Utils;                 use GUI_Utils;
with Remote_Connections;
with System;

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
--  with Vdiff2_Module;
with Builder_Module;
with Glide_Kernel.Console;
with Glide_Result_View;
with Navigation_Module;
with Custom_Module;
with Vsearch_Ext;
with Help_Module;
with Codefix_Module;
with Python_Module;
with KeyManager_Module;
--  with Docgen_Module;
with SSH_Protocol;

procedure GPS is
   use Glide_Main_Window;

   Me        : constant Debug_Handle := Create ("GPS");
   Gtk_Trace : constant Debug_Handle := Create ("Gtk+");
   Pid_Image : constant String := String_Utils.Image (Get_Process_Id);

   subtype String_Access is GNAT.OS_Lib.String_Access;

   GPS                    : Glide_Window;
   Directory              : Dir_Type;
   Str                    : String (1 .. 1024);
   Last                   : Natural;
   Home                   : String_Access;
   Project_Name           : String_Access;
   Prj                    : String_Access;
   Prefix                 : String_Access;
   Dir                    : String_Access;
   Batch_File             : String_Access;
   Batch_Script           : String_Access;
   Target                 : String_Access;
   Protocol               : String_Access;
   Debugger_Name          : String_Access;
   Startup_Dir            : String_Access;
   About_Contents         : GNAT.OS_Lib.String_Access;
   Splash                 : Gtk_Window;
   User_Directory_Existed : Boolean;
   Cleanup_Needed         : Boolean := False;
   Unexpected_Exception   : Boolean := False;
   Splash_Timeout         : Glib.Guint32 := 1000;

   Started                : Boolean := False;
   --  Whether the main loop is started.

   Button                 : Message_Dialog_Buttons;
   Result                 : Boolean;
   Timeout_Id             : Timeout_Handler_Id;
   pragma Unreferenced (Button, Result, Timeout_Id);

   procedure Init_Settings;
   --  Set up environment for GPS.

   procedure Main_Processing;
   --  Main GPS processing (launches a gtk+ main loop and handle unexpected
   --  exceptions).

   procedure Do_Cleanups;
   --  Perform clean ups and automatic saving before exiting.

   procedure Parse_Switches;
   --  Parse command line switches.

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

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle);
   --  Called when the title of an MDI child has changed

   procedure Execute_Batch (Batch : String; As_File : Boolean);
   --  Execute a batch command (either loading the file Batch if As_File is
   --  true, or as a standard command otherwise).

   procedure Gtk_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : String);
   --  Log level glib handler for redirecting Gtk+ messages to our log file.

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
      FD     : File_Descriptor;

   begin
      if Get_Pref (GPS.Kernel, Splash_Screen)
        and then Is_Regular_File (File)
      then
         FD := Open_Read (File, Binary);

         if About_Contents.all /= "" then
            Splash_Timeout := 4000;
         end if;

         Close (FD);
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

   -------------
   -- Gtk_Log --
   -------------

   procedure Gtk_Log
     (Log_Domain : String;
      Log_Level  : Log_Level_Flags;
      Message    : String) is
   begin
      if Log_Domain = "" then
         --  Ignore this message, to avoid generating too much noise.
         return;
      end if;

      if (Log_Level and Log_Level_Critical) /= 0 then
         Trace (Gtk_Trace, Log_Domain & "-CRITICAL: " & Message);
      elsif (Log_Level and Log_Level_Warning) /= 0 then
         Trace (Gtk_Trace, Log_Domain & "-WARNING: " & Message);
      else
         Trace (Gtk_Trace, Log_Domain & "-MISC: " & Message);
      end if;
   end Gtk_Log;

   ----------
   -- Init --
   ----------

   procedure Init_Settings is
      Dir_Created : Boolean := False;
      File        : File_Type;
      Charset     : String_Access;
      Make_Root   : String_Access;
      Tmp         : String_Access;
      Ignored     : Log_Handler_Id;
      pragma Unreferenced (Ignored);

   begin
      OS_Utils.Install_Ctrl_C_Handler (Ctrl_C_Handler'Unrestricted_Access);
      Projects.Registry.Initialize;

      --  Reset any artificial memory limit

      Setenv ("GNAT_MEMORY_LIMIT", "");

      Charset := Getenv ("CHARSET");

      if Charset.all = "" then
         --  Gtk+ does not like if CHARSET is not defined.
         --  Need to set CHARSET *before* calling Gtk.Main.Init, so cannot
         --  use Get_Pref here

         Setenv ("CHARSET", "ISO-8859-1");
      end if;

      Free (Charset);
      Startup_Dir := new String'(Get_Current_Dir);

      --  Set the TERM variable to a dummy value, since we only know how to
      --  handle simple terminals

      Setenv ("TERM", "dumb");

      Home := Getenv ("GPS_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      if Home'Length > 2
        and then Home (Home'First) = '%'
        and then Home (Home'Last) = '%'
      then
         --  Some Windows systems set %HOME% to another env variable, e.g.
         --  %USERPROFILE%

         Tmp := Home;
         Home := Getenv (Tmp (Tmp'First + 1 .. Tmp'Last - 1));
         Free (Tmp);
      end if;

      if Home.all = "" then
         Free (Home);

         if Directory_Separator = '\' then
            Home := new String'("c:\");
         else
            Home := new String'("/");
         end if;
      end if;

      Dir := new String'(File_Utils.Name_As_Directory (Home.all) & ".gps");

      Prefix := Getenv ("GPS_ROOT");

      if Prefix.all = "" then
         Free (Prefix);
         Prefix := new String'(Executable_Location);

         if Prefix.all = "" then
            Free (Prefix);
            Prefix := new String'(GVD.Prefix);
         end if;
      end if;

      --  Parse the config files
      Gtk.Rc.Add_Default_File
        (Format_Pathname (Prefix.all & "/etc/gps/gtkrc"));
      Gtk.Rc.Add_Default_File
        (File_Utils.Name_As_Directory (Dir.all) & "gtkrc");

      Gtk.Main.Init;

      --  Define MAKE_ROOT if needed, so that the generated makefiles can find
      --  Makefile.prolog and Makefile.generic

      Make_Root := Getenv ("MAKE_ROOT");

      if Make_Root.all = "" then
         Setenv ("MAKE_ROOT", Prefix.all);
         Free (Make_Root);
      end if;

      Bind_Text_Domain
        ("gps", Format_Pathname (Prefix.all & "/share/locale"));

      --  Redirect all default Gtk+ logs to our own trace mechanism

      Ignored := Log_Set_Handler
        ("", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("GLib", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("GLib-GObject", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("Pango", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("Atk", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("GdkPixbuf", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("Gdk", Log_Level_Mask, Gtk_Log'Unrestricted_Access);
      Ignored := Log_Set_Handler
        ("Gtk", Log_Level_Mask, Gtk_Log'Unrestricted_Access);

      declare
         Sessions : constant String :=
           File_Utils.Name_As_Directory (Dir.all) & "sessions";
         Customize : constant String :=
           File_Utils.Name_As_Directory (Dir.all) & "customize";

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
               Name => File_Utils.Name_As_Directory (Dir.all)
                 & "traces.cfg");
            Put_Line (File, ">log.$$");
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
            Put_Line (File, "PROJECTS.DEBUG=no");
            Close (File);
         end if;

         if not Is_Directory (Sessions) then
            Make_Dir (Sessions);

            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ") & Sessions,
                  Information, Button_OK, Justification => Justify_Left);
            end if;
         end if;

         if not Is_Directory (Customize) then
            Make_Dir (Customize);

            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ") & Customize,
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

      --  Initialize the traces

      Traces.Parse_Config_File
        (Default => File_Utils.Name_As_Directory (Dir.all) & "traces.cfg");
      Trace (Me, "GPS " & GVD.Version & " (" & GVD.Source_Date &
             ") hosted on " & GVD.Target);

      Gtk_New
        (GPS, "<gps>", Glide_Menu.Glide_Menu_Items.all, Dir.all, Prefix.all);

      About_Contents := Read_File
        (Format_Pathname (GPS.Prefix_Directory.all & "/share/gps/about.txt"));

      if About_Contents = null then
         About_Contents := new String'("");
      end if;

      if Is_Regular_File
        (Format_Pathname
           (GPS.Prefix_Directory.all & "/share/gps/gps-pro.txt"))
      then
         GPS.Public_Version := False;
      end if;

      Reset_Title (GPS);

      Glide_Menu.Register_Common_Menus (GPS.Kernel);

      Kernel_Callback.Connect
        (Get_MDI (GPS.Kernel), "child_selected",
         Kernel_Callback.To_Marshaller (Child_Selected'Unrestricted_Access),
         GPS.Kernel);
      Kernel_Callback.Connect
        (Get_MDI (GPS.Kernel), "child_title_changed",
         Title_Changed'Unrestricted_Access, GPS.Kernel);

      DDE.Register_DDE_Server (GPS.Kernel);

      GPS.Debug_Mode := True;
      GPS.Log_Level  := GVD.Types.Hidden;

      Parse_Switches;
      Display_Splash_Screen;

      if Splash = null then
         Timeout_Id := Process_Timeout.Add
           (1, Finish_Setup'Unrestricted_Access,
            (GPS.Kernel, null, null, null, System.Null_Address));
      else
         Timeout_Id := Process_Timeout.Add
           (Splash_Timeout, Finish_Setup'Unrestricted_Access,
            (GPS.Kernel, null, null, null, System.Null_Address));
      end if;
   end Init_Settings;

   --------------------
   -- Parse_Switches --
   --------------------

   procedure Parse_Switches is
   begin
      loop
         case Getopt ("-version -help P! -log-level= " &
                      "-debug? -debugger= -target= -load= -eval=")
         is
            -- long option names --
            when '-' =>
               case Full_Switch (Full_Switch'First + 1) is
                  -- --version --
                  when 'v' =>
                     if GVD.Can_Output then
                        Put_Line (GPS_Name (GPS) & " version " &
                                  GVD.Version & " (" &
                                  GVD.Source_Date & ") hosted on " &
                                  GVD.Target);
                     else
                        Button := Message_Dialog
                          (GPS_Name (GPS) & " version " & GVD.Version & " (" &
                           GVD.Source_Date & ") hosted on " & GVD.Target,
                           Information, Button_OK,
                           Title         => -"Version",
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
                     if Full_Switch = "-log-level" then
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
                              OS_Exit (1);
                        end;

                     elsif Full_Switch = "-load" then
                        --  --load
                        Free (Batch_File);
                        Batch_File := new String'(Parameter);
                     end if;

                  when 'd' =>
                     --  --debug
                     if Full_Switch = "-debug" then
                        Free (GPS.Program_Args);
                        GPS.Program_Args := new String'(Clean_Parameter);

                     else
                        --  --debugger
                        Free (Debugger_Name);
                        Debugger_Name := new String'(Parameter);

                        if GPS.Program_Args = null then
                           --  --debugger implies --debug
                           GPS.Program_Args := new String'("");
                        end if;
                     end if;

                  when 'e' =>
                     --  --eval
                     Free (Batch_Script);
                     Batch_Script := new String'(Parameter);

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

               if not Is_Regular_File (Project_Name.all)
                 and then Is_Regular_File
                   (Project_Name.all & Project_File_Extension)
               then
                  Prj := Project_Name;
                  Project_Name :=
                    new String'(Prj.all & Project_File_Extension);
                  Free (Prj);
               end if;

               Trace (Me, "Found project: " & Parameter);

            when ASCII.NUL =>
               exit;

            when others =>
               null;
         end case;
      end loop;

   exception
      when Invalid_Switch | GNAT.Command_Line.Invalid_Parameter =>
         if GVD.Can_Output then
            Put_Line ("Invalid command line");
         end if;

         Help;
         OS_Exit (1);
   end Parse_Switches;

   ----------
   -- Help --
   ----------

   procedure Help is
      use ASCII;
   begin
      --  We do not document the --log-level flag, since it will be replaced
      --  by the regular Traces mechanism.

      if GVD.Can_Output then
         Put_Line (GPS_Name (GPS) & " " & GVD.Version &
                   " (" & GVD.Source_Date & ")" &
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
         Put_Line
           (-"   --load=lang:file    Execute an external file written");
         Put_Line (-"                        in the language lang");
         Put_Line
           (-"   --eval=lang:cmd     Execute a command written in the");
         Put_Line (-"                        language lang. This is executed");
         Put_Line (-"                        before the --load command");

      else
         Button := Message_Dialog
           (GPS_Name (GPS) & " " & GVD.Version & " (" & GVD.Source_Date & ")" &
            (-", the GNAT Programming System.") & LF &
            (-"Usage:") & LF &
            (-"   gps [options] [-Pproject-file] [source1] [source2] ...") &
            LF & (-"Options:") & LF &
            (-"   --help              Show this help message and exit.") & LF &
            (-"   --version           Show the GPS version and exit.") & LF &
            (-"   --load=lang:file    Execute an external file written")
            & LF &
            (-"                        in the language lang") & LF &
            (-"   --eval=lang:cmd     Execute a command written in the")
            & LF &
            (-"                        language lang. This is executed") & LF &
            (-"                        before the --load command") & LF &
            (-"   --debug[=program]   Start a debug session") & LF &
            (-"   --debugger debugger Specify the debugger's command line") &
            LF &
            (-"   --target=TARG:PRO   ") &
            (-"Load program on machine TARG using protocol PRO"),
            Information, Button_OK,
            Title         => -"Help",
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

   -------------------
   -- Execute_Batch --
   -------------------

   procedure Execute_Batch (Batch : String; As_File : Boolean) is
      Executed : Boolean := False;
      Script   : Scripting_Language;
      Errors   : Boolean;
   begin
      for J in Batch'Range loop
         if Batch (J) = ':' then
            Script := Lookup_Scripting_Language
              (GPS.Kernel, Batch (Batch'First .. J - 1));

            if Script = null then
               exit;
            end if;

            if As_File then
               Execute_File
                 (Script             => Script,
                  Filename           => Normalize_Pathname
                    (Batch (J + 1 .. Batch'Last), Startup_Dir.all),
                  Display_In_Console => True,
                  Errors             => Errors);
            else
               Execute_Command
                 (Script             => Script,
                  Command            => Batch (J + 1 .. Batch'Last),
                  Display_In_Console => True,
                  Errors             => Errors);
            end if;

            Executed := True;
            exit;
         end if;
      end loop;

      if not Executed then
         if As_File then
            Insert (GPS.Kernel,
                    -"Language unknown for --load command line switch",
                    Mode => Error);
         else
            Insert (GPS.Kernel,
                    -"Language unknown for --script command line switch",
                    Mode => Error);
         end if;
      end if;

   exception
      when E : others =>
         if As_File then
            Insert (GPS.Kernel,
                    -"Error when executing the script for -batch switch",
                    Mode => Error);
         else
            Insert (GPS.Kernel,
                    -"Error when executing the script for --script switch",
                    Mode => Error);
         end if;
         Trace (Me, "Exception was " & Exception_Information (E));
   end Execute_Batch;

   ------------------
   -- Finish_Setup --
   ------------------

   function Finish_Setup (Data : Process_Data) return Boolean is
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
         New_Dir : constant String := Get_Current_Dir;
      begin
         --  Temporarily restore start-up dir, so that relative paths are
         --  properly computed

         Change_Dir (Startup_Dir.all);

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

               if S (S'First) = '=' then
                  Open_File_Editor
                    (GPS.Kernel,
                     Create (S (S'First + 1 .. S'Last),
                             GPS.Kernel,
                             Use_Source_Path => True,
                             Use_Object_Path => False));
               else
                  Open_File_Editor
                    (GPS.Kernel,
                     Create (S,
                             GPS.Kernel,
                             Use_Source_Path => False,
                             Use_Object_Path => False));
               end if;

               File_Opened := True;
            end;
         end loop;

         Change_Dir (New_Dir);

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
      Cleanup_Needed := True;

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

      --  Register the remote protocols early so that other modules can access
      --  remote files.

      SSH_Protocol.Register_Protocol;

      --  Register all modules (scripting languages must be registered first)

      Shell_Script.Register_Module (GPS.Kernel);
      Python_Module.Register_Module (GPS.Kernel);
      Register_Default_Script_Commands (GPS.Kernel);

      --  We then must register the keymanager, so that other modules can
      --  register their keys

      KeyManager_Module.Register_Module (GPS.Kernel);
      Register_Keys (GPS);

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
      Project_Explorers.Register_Module (GPS.Kernel);
      Project_Explorers_Files.Register_Module (GPS.Kernel);
      External_Editor_Module.Register_Module (GPS.Kernel);
      GVD_Module.Register_Module (GPS.Kernel);
      Builder_Module.Register_Module (GPS.Kernel);
--      Vdiff2_Module.Register_Module (GPS.Kernel);
      Vdiff_Module.Register_Module (GPS.Kernel);
      VCS_Module.Register_Module (GPS.Kernel);
      VCS.CVS.Register_Module (GPS.Kernel);
      VCS.ClearCase.Register_Module (GPS.Kernel);
      Aunit_Module.Register_Module (GPS.Kernel);
      VFS_Module.Register_Module (GPS.Kernel);
      Codefix_Module.Register_Module (GPS.Kernel);
      Glide_Kernel.Task_Manager.Register_Module (GPS.Kernel);
      Custom_Module.Register_Module (GPS.Kernel);
      Glide_Result_View.Register_Module (GPS.Kernel);
--        Docgen_Module.Register_Module (GPS.Kernel);

      --  Register the supported languages and their associated LI handlers.

      Ada_Module.Register_Module (GPS.Kernel);
      Cpp_Module.Register_Module (GPS.Kernel);

      --  Load system files

      Load_All_Custom_Files (GPS.Kernel);

      --  Temporarily disable unimplemented menu items

      declare
         Navigate : constant String := '/' & (-"Navigate") & '/';
         Tools    : constant String := '/' & (-"Tools") & '/';
      begin
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Navigate & (-"Goto Parent Unit")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Tools & (-"Profile")), False);
         Set_Sensitive (Find_Menu_Item
           (GPS.Kernel, Tools & (-"Memory Analyzer")), False);
      end;

      --  Print a welcome message in the console, but before parsing the error
      --  messages, so that these are visible

      Console.Insert
        (GPS.Kernel,
         -"Welcome to " & GPS_Name (GPS) & " " & GVD.Version &
         " (" & GVD.Source_Date &
         (-") hosted on ") & GVD.Target & ASCII.LF &
         (-"the GNAT Programming System") & ASCII.LF & About_Contents.all &
         "(c) 2001-2003 ACT-Europe");
      Free (About_Contents);

      --  We now make sure we have a project loaded, so that opening editors
      --  will work correctly.

      --  If no project has been specified on the command line, try to open
      --  the first one in the current directory (if any).

      if GPS.Program_Args /= null then
         --  --debug has been specified
         --  Load default project, and set debugger-related project properties

         File_Opened := True;
         Auto_Load_Project := False;

         if Project_Name /= null
           and then Is_Regular_File (Project_Name.all)
         then
            Load_Project (GPS.Kernel, Project_Name.all);
            Project := Get_Project (GPS.Kernel);
         else
            Load_Default_Project
              (GPS.Kernel, Get_Current_Dir, Load_Default_Desktop => True);
            Project := Get_Project (GPS.Kernel);
            Set_Status (Project, From_Executable);
         end if;

         --  Project will be overriden when the executable is loaded.
         Load_Sources;

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
         --  not open the welcome dialog. Likewise if we are loading a script,
         --  since we consider that the script is responsible for loading the
         --  appropriate project.

         if not Auto_Load_Project then
            if Batch_File /= null then
               Load_Default_Project (GPS.Kernel, Get_Current_Dir);
            else
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
                        --  Desktop was already loaded when the project itself
                        --  was loaded.
                        null;
                  end case;

                  Destroy (Screen);
               end if;
            end if;
         end if;
      end if;

      if Auto_Load_Project and then Project_Name /= null then
         Load_Project (GPS.Kernel, Project_Name.all);
         Load_Sources;
      end if;

      if not File_Opened
        and then not Has_User_Desktop (GPS.Kernel)
      then
         Open_Html (GPS.Kernel, Create_Html ("gps-welcome.html", GPS.Kernel));
         Maximize_Children (Get_MDI (GPS.Kernel));
      end if;

      if Splash /= null then
         Destroy (Splash);
      end if;

      Python_Module.Load_Python_Startup_Files (GPS.Kernel);

      if Batch_Script /= null then
         Execute_Batch (Batch_Script.all, As_File => False);
      end if;

      if Batch_File /= null then
         Execute_Batch (Batch_File.all, As_File => True);
      end if;

      --  Take into account the preferences loaded when creating the kernel.
      --  This needs to be done after all the graphical elements have been
      --  created, to be sure they are realized and will take the preferences
      --  into acocunt.

      Preferences_Changed (GPS.Kernel);

      Started := True;

      --  Set the title of the GPS window.
      Child_Selected (Get_MDI (GPS.Kernel), GPS.Kernel);

      return False;
   end Finish_Setup;

   -------------------
   -- Title_Changed --
   -------------------

   procedure Title_Changed
     (MDI    : access GObject_Record'Class;
      Child  : Gtk_Args;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (MDI);
      C : constant MDI_Child := MDI_Child (To_Object (Child, 1));

   begin
      if Started then
         --  Set the title of the main window.
         Reset_Title (Glide_Window (Get_Main_Window (Kernel)),
                      Get_Short_Title (C));
      end if;
   end Title_Changed;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
     (MDI : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Child   : constant MDI_Child := Get_Focus_Child (MDI_Window (MDI));
   begin
      --  Set the title of the main window.

      if Started then
         if Child = null then
            Reset_Title
              (Glide_Window (Get_Main_Window (Kernel)));
         else
            Reset_Title
              (Glide_Window (Get_Main_Window (Kernel)),
               Get_Short_Title (Child));
         end if;
      end if;

      if Context /= null then
         Context_Changed (Kernel, Context);
      end if;
   end Child_Selected;

   ---------------------
   -- Main_Processing --
   ---------------------

   procedure Main_Processing is
      Log_File : aliased String := Get_Home_Dir (GPS.Kernel) & "log";
      Pid_File : aliased String := Log_File & "." & Pid_Image;
      Str      : String_Access;

   begin
      Gtk.Main.Main;
   exception
      when E : others =>
         Unexpected_Exception := True;
         Trace (Me, "Unhandled exception: " & Exception_Information (E));

         if Is_Regular_File (Log_File & "." & Pid_Image) then
            Str := Pid_File'Unchecked_Access;
         else
            Str := Log_File'Unchecked_Access;
         end if;

         Button := Message_Dialog
           ("Unexpected fatal error, GPS is in an inconsistent state" &
            ASCII.LF & "Please report with contents of " & Str.all &
            ASCII.LF & ASCII.LF &
            "You will be asked to save modified files before GPS exits",
            Error, Button_OK,
            Title         => -"Fatal Error",
            Justification => Justify_Left);
         Result := Save_MDI_Children (GPS.Kernel, Force => False);
   end Main_Processing;

   -----------------
   -- Do_Cleanups --
   -----------------

   procedure Do_Cleanups is
      Kernel   : constant Kernel_Handle := GPS.Kernel;
      Log_File : constant String := Get_Home_Dir (Kernel) & "log";
      Success  : Boolean;

   begin
      if not Cleanup_Needed then
         return;
      end if;

      Cleanup_Needed := False;

      if Get_Pref (Kernel, Save_Desktop_On_Exit) then
         Save_Desktop (Kernel);
      end if;

      Save_Accel_Map (File_Utils.Name_As_Directory (Dir.all) & "custom_key");

      Free_Modules (Kernel);

      --  Call Handlers_Destroy after Free_Modules and Destroy (GPS),
      --  since some handlers are already disconnected by these functions, and
      --  only Handlers_Destroy know what handlers are still left and need to
      --  be disconnected.

      Handlers_Destroy (Kernel);

      --  Since the call to destroy below will free the animation at some
      --  point, we no longer want to access/update it past this point.

      GPS.Animation_Image := null;
      Destroy (GPS);
      Destroy (Kernel);

      Projects.Registry.Finalize;
      Traces.Finalize;

      Remote_Connections.Close_All_Connections;

      --  In case of a normal exit, rename log.<pid> as log to avoid
      --  generating a new log file for each session; this way we still
      --  keep the log file in case of post mortem analysis.
      --  In case of unexpected exit, keep the log file under its original
      --  name, so that it does not get erased by the next session and can
      --  be reported.

      if not Unexpected_Exception
        and then Is_Regular_File (Log_File & "." & Pid_Image)
      then
         Rename_File (Log_File & "." & Pid_Image, Log_File, Success);
      end if;

      Free (Home);
      Free (Dir);
      Free (Prefix);
      Free (Startup_Dir);
   end Do_Cleanups;

begin
   Init_Settings;
   Main_Processing;
   Do_Cleanups;

exception
   when E : others =>
      Trace (Me, "Unexpected exception: " & Exception_Information (E));
end GPS;
