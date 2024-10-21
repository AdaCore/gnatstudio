------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Text_IO;                      use Ada.Text_IO;
with Config;                           use Config;
with Interfaces.C;                     use Interfaces.C;
with Glib;                             use Glib;
with Glib.Application;                 use Glib.Application;
with Glib.Error;                       use Glib.Error;
with Glib.Messages;                    use Glib.Messages;
with Glib.Option;
with Glib.Utils;
with GNAT.Command_Line;                use GNAT.Command_Line;
with GNAT.Directory_Operations;        use GNAT.Directory_Operations;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with GNATCOLL.Memory;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS.VSS_Utils;           use GNATCOLL.VFS.VSS_Utils;
with GPS.Callbacks;                    use GPS.Callbacks;
with GPS.Globals;                      use GPS.Globals;
with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Traces;
with Gtk;
with Gtk_Utils;                        use Gtk_Utils;
with Gtkada.Dialogs;                   use Gtkada.Dialogs;
with Gtkada.Intl;
with Gtkada.Types;                     use Gtkada.Types;
with GUI_Utils;                        use GUI_Utils;
with Src_Editor_Box;                   use Src_Editor_Box;
with String_Utils;
with VSS.Standard_Paths;
with VSS.Strings;
with VSS.Strings.Conversions;

package body GPS.Initialization is

   procedure Initialize_Environment_Variables;
   --  Sanitize, sets and take into account various environment variables, and
   --  initialize GNAT Studio according to them.

   procedure Initialize_Low_Level (Status_Code : out Glib.Gint);
   --  Initializes the low-level gtk, python, traces layers
   --  This needs to be performed after environment variable initialisation.

   procedure Handle_X_Switch (Val : String);
   --  Handles the -X command line switch

   procedure Build_Command_Line;
   --  Initialize the variable GPS_Command_Line.

   --------------------------------------
   -- Initialize_Environment_Variables --
   --------------------------------------

   procedure Initialize_Environment_Variables is

      function Get_Cwd return String;
      --  proxies for the services in the command line, usable even when no
      --  command line is passed

      procedure Each_Environment_Variable (Name, Value : String);
      --  If Name is a special environment variable, then store its preserved
      --  values into Env object.

      procedure Reset_Environment_Variable (Name : String);
      --  Reset environment variable of the current process called Name to the
      --  value of the environment variable with prefix "GPS_STARTUP_" when it
      --  exists.

      -------------
      -- Get_Cwd --
      -------------

      function Get_Cwd return String is
      begin
         return Get_Current_Dir.Display_Full_Name;
      end Get_Cwd;

      -------------------------------
      -- Each_Environment_Variable --
      -------------------------------

      procedure Each_Environment_Variable (Name, Value : String) is
         Prefix : constant String := "GPS_STARTUP_";
      begin
         if Starts_With (Name, Prefix) then
            declare
               Unprefixed_Name : constant String :=
                 Name (Name'First + Prefix'Length .. Name'Last);

            begin
               if Value /= "_ABSENT_VARIABLE_" then
                  Env.Insert (Unprefixed_Name, Value);
               elsif Env.Contains (Unprefixed_Name) then
                  Env.Remove (Unprefixed_Name);
               end if;

               Env.Remove (Name);
            end;
         end if;
      end Each_Environment_Variable;

      --------------------------------
      -- Reset_Environment_Variable --
      --------------------------------

      procedure Reset_Environment_Variable (Name : String) is
         Backup_Name : constant String := "GPS_STARTUP_" & Name;

      begin
         if Env.Contains (Backup_Name) then
            Setenv (Name, Env.Value (Backup_Name));
         end if;
      end Reset_Environment_Variable;

   begin
      --  Reset the environment that was set before GNAT Studio was started
      --  (since starting GNAT Studio will generally imply a change in
      --  LD_LIBRARY_PATH to point to the right libraries

      Reset_Environment_Variable ("LD_LIBRARY_PATH");
      Reset_Environment_Variable ("DYLD_LIBRARY_PATH");
      Reset_Environment_Variable ("DYLD_FALLBACK_LIBRARY_PATH");

      declare
         Charset : constant String := Env.Value ("CHARSET", "");
      begin
         if Charset = "" then
            --  Gtk+ does not like if CHARSET is not defined.
            --  Need to set CHARSET *before* calling Gtk.Main.Init, so cannot
            --  use Get_Pref here.

            Setenv ("CHARSET", Config.Default_Charset);
         end if;
      end;

      declare
         Overlay_Behavior : constant String :=
           Env.Value ("GTK_OVERLAY_SCROLLING", "");
      begin
         if Overlay_Behavior = "" then
            --  See U630-025: with its GTK patch GTK_OVERLAY_SCROLLING is
            --  setting the default value for the "overlay_scrolling" property.
            Setenv ("GTK_OVERLAY_SCROLLING", "0");
         end if;
      end;

      Startup_Dir := new String'(Get_Cwd);

      --  Set the TERM variable to a dummy value, since we only know how to
      --  handle simple terminals

      Setenv ("TERM", "dumb");

      declare
         Home : constant VSS.Strings.Virtual_String :=
           Getenv_With_Fallback ("GNATSTUDIO_HOME", "GPS_HOME");

      begin
         if not Home.Is_Empty then
            Home_Dir := Create (Home);
            Home_Dir.Make_Dir;

         else
            Home_Dir :=
              Create
                (VSS.Standard_Paths.Writable_Location
                   (VSS.Standard_Paths.Home_Location));
         end if;

         --  Under Windows, when the user directory contains international
         --  characters, the value contained in the environment might not
         --  be encoded in the same way as the filesystem. Add a safety check
         --  for this.

         if not Home_Dir.Is_Directory then
            declare
               As_UTF8 : constant Glib.UTF8_String :=
                 Glib.Utils.Get_Home_Dir;
               Tmp     : constant Virtual_File := Create (+(As_UTF8));
            begin
               if Tmp.Is_Directory then
                  --  $HOME/$USERPROFILE does not exist but its UTF8
                  --  representation exists; this is a safer bet for home
                  --  directory: use it.
                  Home_Dir := Tmp;
               end if;
            end;
         end if;
      exception
         when others =>
            Report_Error
              ("Could not create/access the GNAT Studio home in"
               & ASCII.LF & "'" & Home_Dir.Display_Full_Name & "'");
      end;

      GNATStudio_Home_Dir := Create_From_Dir (Home_Dir, ".gnatstudio");
      GPS_Log_Dir := Create_From_Dir (GNATStudio_Home_Dir, "log");

      Ensure_Directory (GNATStudio_Home_Dir);

      declare
         Prefix : constant String := Env.Value ("GPS_ROOT", "");
      begin
         if Prefix /= "" then
            Prefix_Dir := Create (+Prefix);
         end if;
      end;

      if Prefix_Dir = No_File then
         declare
            Prefix : constant String := Executable_Location;
         begin
            --  Check whether we are running the installed GNAT Studio, or
            --  locally from the development environment.

            if Prefix'Length < 4
              or else Prefix (Prefix'Last - 3 .. Prefix'Last - 1) /= "obj"
            then
               Prefix_Dir := Create (+Prefix);
            else
               Prefix_Dir := Create (+Config.Prefix);
            end if;

         end;
      end if;

      --  Load EDITION.txt file to add to version information

      declare
         Edition_File : constant Virtual_File :=
           Create_From_Dir (Prefix_Dir, "share/gnatstudio/EDITION.txt");
         Content      : GPS.Globals.String_Access;

      begin
         if Edition_File.Is_Readable then
            Content := Edition_File.Read_File;
            Config.Version := To_Unbounded_String (Content.all);

            --  In Community, GCov should be enabled
            if Starts_With (Content.all, "Community") then
               Setenv ("ENABLE_GCOV", "1");
            end if;
            Free (Content);
         end if;

      exception
         when others =>
            Put_Line ("Exception occurred when setting the edition");
      end;

      declare
         Tmp    : constant String := Env.Value ("PATH", "");
         Prefix : constant String := Prefix_Dir.Display_Full_Name;
         Bin    : constant String :=
           (if Prefix (Prefix'Last) = Directory_Separator
            then Prefix else Prefix & Directory_Separator) &
           "bin";

      begin
         if Tmp = "" then
            Setenv ("PATH", Bin);
         else
            Setenv ("PATH", Tmp & Path_Separator & Bin);
         end if;
      end;

      --  Python startup path

      declare
         Python_Path : constant String := Env.Value ("PYTHONPATH", "");
         New_Val     : GPS.Globals.String_Access;

      begin
         if Python_Path = "" then
            New_Val := new String'
              (+Create_From_Dir
                 (Prefix_Dir, "share/gnatstudio/python").Full_Name);
         else
            New_Val := new String'
              (+To_Path
                 (From_Path (+Python_Path) &
                  (1 => Create_From_Dir
                       (Prefix_Dir, "share/gnatstudio/python"))));
         end if;

         Setenv ("PYTHONPATH", New_Val.all);
         Trace (Main_Trace, "PYTHONPATH=" & New_Val.all);
         Free (New_Val);
      end;

      --  Temporary for DAP testing, to set gdb 13.x version
      --  Will be removed when gdb v. 13.x is default
      declare
         DAP_GDB_Path : constant String := Env.Value ("DAP_GDB", "");
      begin
         if DAP_GDB_Path /= "" then
            DAP_GDB_Adapter := VSS.Strings.Conversions.To_Virtual_String
              (DAP_GDB_Path);
         end if;
      end;

      for J of Env.Keys loop
         Each_Environment_Variable (J, Env.Value (J));
      end loop;
   end Initialize_Environment_Variables;

   --------------------------
   -- Initialize_Low_Level --
   --------------------------

   procedure Initialize_Low_Level (Status_Code : out Glib.Gint) is
      Ignored     : Log_Handler_Id;
      pragma Unreferenced (Ignored);

      procedure Create_ALS_Traces_File_If_Needed (Language : String);
      --  Create the ALS traces file for the given language ('ada' or 'gpr').

      --------------------------------------
      -- Create_ALS_Traces_File_If_Needed --
      --------------------------------------

      procedure Create_ALS_Traces_File_If_Needed (Language : String) is
         Filename   : constant String := Language & "_ls_traces.cfg";
         ALS_Traces : constant Virtual_File :=
           Create_From_Dir (GNATStudio_Home_Dir, +Filename);
         File       : Writable_File;
      begin
         if not ALS_Traces.Is_Regular_File then
            File := ALS_Traces.Write_File;
            if Active (Testsuite_Handle) then
               --  In testsuite mode, create the ALS log traces with
               --  the full contents by default.
               Write
                 (File,
                  ">log/"
                  & Language
                  & "_ls_log.$T.txt:buffer_size=0:buffer_size=0"
                  & "ALS.MAIN=yes" & ASCII.LF
                  & "ALS.IN=yes" & ASCII.LF
                  & "ALS.OUT=yes" & ASCII.LF);
            else
               Write
                 (File,
                  ">log/"
                  & Language
                  & "_ls_log.$T.txt:buffer_size=0:buffer_size=0"
                  & ASCII.LF
                  & "DEBUG.ABSOLUTE_TIME=yes"
                  & ASCII.LF
                  & "ALS.MAIN=yes" & ASCII.LF
                  & ASCII.LF
                  & "## uncomment the following 2 lines"
                  & " to activate full traces" & ASCII.LF
                  & "#ALS.IN=yes" & ASCII.LF
                  & "#ALS.OUT=yes" & ASCII.LF);
            end if;

            Close (File);
         end if;
      end Create_ALS_Traces_File_If_Needed;

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
           Create_From_Dir (GNATStudio_Home_Dir, "plug-ins");
         Themes             : constant Virtual_File :=
           Create_From_Dir (GNATStudio_Home_Dir, "themes");
         Gnatinspect_Traces : constant Virtual_File :=
           Create_From_Dir (GNATStudio_Home_Dir, "gnatinspect_traces.cfg");
         File               : Writable_File;
      begin
            --  If the GNAT Studio home dir is not found, create it, and make
            --  sure to display the preferences assistant at the end.

         if not Is_Directory (GNATStudio_Home_Dir) then
            Show_Preferences_Assistant := True;
            Make_Dir (GNATStudio_Home_Dir);
         end if;

         declare
            Success : Boolean;
         begin
            if not Is_Directory (GPS_Log_Dir) then
               --  A safety check: a previous version of GNAT Studio could
               --  have left a regular file $HOME/.gnatstudio/log.
               if Is_Regular_File (GPS_Log_Dir) then
                  GNATCOLL.VFS.Delete (GPS_Log_Dir, Success);
                  --  Another safety: on bad filesystems, deletion isn't always
                  --  instantaneous - it's a very rare case and should not
                  --  occur regularly, so use a delay here.
                  delay 1.0;
               end if;
               Make_Dir (GPS_Log_Dir);
            end if;
         exception
            when VFS_Directory_Error =>
               Report_Error ((-"Cannot create logs directory ") &
                               GPS_Log_Dir.Display_Full_Name & ASCII.LF);
               Status_Code := 1;
               return;
         end;

         --  Setup the GNAT Studio traces configuration
         GPS.Traces.Setup_Traces_Config
           (GNATStudio_Home_Dir => GNATStudio_Home_Dir);

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

         if not Is_Directory (Themes) then
            Make_Dir (Themes);
         end if;

      exception
         when VFS_Directory_Error =>
            Report_Error
              ((-"Cannot create config directory ") &
                 GNATStudio_Home_Dir.Display_Full_Name & ASCII.LF);
            Status_Code := 1;
            return;
      end;

      declare
         Tmp : constant Virtual_File := Get_Tmp_Directory;
      begin
         if not Is_Directory (Tmp) then
            Report_Error
              ((-"Cannot access temporary directory ") &
                 Tmp.Display_Full_Name);
            Status_Code := 1;
            return;
         end if;
      end;

      --  Initialize the traces

      declare
         File : constant Virtual_File :=
                  Create_From_Dir (GNATStudio_Home_Dir, "traces.cfg");
      begin
         GNATCOLL.Traces.Parse_Config_File
           (Filename     => No_File,
            Default      => File,
            On_Exception => GNATCOLL.Traces.Deactivate);
      exception
         when others =>
            Report_Error ((-"Cannot parse file ") & File.Display_Full_Name);
            Status_Code := 1;
            return;
      end;

      --  Create the traces files for the Ada Language Server. Do this
      --  after initializing the GNAT Studio traces, since the contents
      --  depends on the testsuite trace.
      Create_ALS_Traces_File_If_Needed ("ada");
      Create_ALS_Traces_File_If_Needed ("gpr");

      --  Check whether we should enable memory monitor. We do not use a
      --  constant for the trace_handle, since we must create it only after
      --  the call to Add_Trace_Decorators.
      Memory_Monitor := Active (Create ("DEBUG.ADA_MEMORY", Off));
      GNATCOLL.Memory.Configure
        (Activate_Monitor  => Memory_Monitor,
         Stack_Trace_Depth => Memory_Stack_Depth,
         Disable_Free      => False);

      Trace (Main_Trace, "GNAT Studio " & To_String (Config.Version) & " ("
             & Config.Source_Date & ") hosted on " & Config.Target);
      Trace (Main_Trace, "Gtk+ static version: "
             & String_Utils.Image (Integer (Gtk.Major_Version)) & '.'
             & String_Utils.Image (Integer (Gtk.Minor_Version)) & '.'
             & String_Utils.Image (Integer (Gtk.Micro_Version)));
      Trace (Main_Trace, "Gtk+ dynamic version: "
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
         Setenv
           (Name  => Val (Val'First .. Idx - 1),
            Value => Val (Idx + 1 .. Val'Last));
      else
         Report_Error ("Invalid value for -X, should be VAR=VALUE");
         GPS_Command_Line.Do_Exit := True;
      end if;
   end Handle_X_Switch;

   ------------------------
   -- Build_Command_Line --
   ------------------------

   procedure Build_Command_Line is

      use type Glib.Option.GOption_Flags;
      function To_Gchar (C : Character) return Glib.Gchar;
      function To_Gchar (C : Character) return Glib.Gchar is
      begin
         return Glib.Gchar (Interfaces.C.char'(Interfaces.C.To_C (C)));
      end To_Gchar;

      Opt_Project  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("project"),
                        Short_Name      => To_Gchar ('P'),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Load project file project or project.gpr"),
                        Arg_Description => New_String ("project"));
      Opt_Scenario  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("scenario"),
                        Short_Name      => To_Gchar ('X'),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Set the value of a scenario variable"),
                        Arg_Description => New_String ("var=value"));
      Opt_Help     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("help"),
                        Short_Name      => To_Gchar ('h'),
                        Flags           => Glib.Option.G_Option_Flag_No_Arg,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Show this help message and exit"),
                        Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Help_All : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("help-all"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           =>
                          Glib.Option.G_Option_Flag_No_Arg
                        + Glib.Option.G_Option_Flag_Hidden,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Show this help message with all options"),
                        Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Version  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("version"),
                        Short_Name      => To_Gchar ('v'),
                        Flags           => Glib.Option.G_Option_Flag_No_Arg,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Show the GNAT Studio version and exit"),
                        Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Debug    : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("debug"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           =>
                          Glib.Option.G_Option_Flag_Optional_Arg
                        + Glib.Option.G_Option_Flag_Filename,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Start a debug session"),
                        Arg_Description => New_String ("[program]"));
      Opt_Hide     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("hide"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => Glib.Option.G_Option_Flag_No_Arg,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Hide GNAT Studio main window"),
                        Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Host     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("host"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Use tools_host to launch tools (e.g. gdb)"),
                        Arg_Description => New_String ("tools_host"));
      Opt_Target   : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("target"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Load program on machine TARG using " &
                           "protocol PRO"),
                        Arg_Description => New_String ("TARG:PRO"));
      Opt_Load     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("load"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Execute an external file written in " &
                           "the language lang"),
                        Arg_Description => New_String ("lang:file"));
      Opt_Eval     : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("eval"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Execute a command written in the language " &
                           "lang (before --load)"),
                        Arg_Description => New_String ("lang:cmd"));
      Opt_Readonly : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("readonly"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => Glib.Option.G_Option_Flag_No_Arg,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Open all files in read-only mode"),
                        Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Server   : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("server"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Start GNAT Studio in server mode, opening a " &
                           "socket on the given port"),
                        Arg_Description => New_String ("port"));
      Opt_Traceon  : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("traceon"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Activate traces for a specific debug stream"),
                        Arg_Description => New_String ("stream"));
      Opt_Traceoff : constant Glib.Option.GOption_Entry :=
                       (Long_Name       => New_String ("traceoff"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => 0,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Disable traces for a specific debug stream"),
                        Arg_Description => New_String ("stream"));
      Opt_Tracefile : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("tracefile"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Load traces configuration from file"),
                         Arg_Description => New_String ("file"));
      Opt_Tracelist : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("tracelist"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_No_Arg,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("List all available debug streams"),
                         Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Pwd       : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("pwd"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Initial current directory"),
                         Arg_Description => New_String ("PWD"));
      Opt_Path      : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("path"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Prepend to PATH environment variable"),
                         Arg_Description => New_String ("PATH"));

      --  Config files

      Opt_Config : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("config"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Specify the configuration file (.cgpr) to load"),
                         Arg_Description => New_String ("file"));
      Opt_Autoconf : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("autoconf"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_No_Arg,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Generate .cgpr automatically if needed"),
                         Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Configdb : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("configdb"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Extra directories for gprconfig"),
                         Arg_Description => New_String ("dir"));
      Opt_Relocate : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("relocate-build-tree"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Relocate build directories for the "
                            & "current project"),
                         Arg_Description => New_String ("dir"));
      Opt_Rootdir  : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String ("root-dir"),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_Switch'Address,
                         Description     => New_String
                           ("Root directory for the current project: "
                            & "must be used with relocate-build-tree"),
                         Arg_Description => New_String ("dir"));
      Opt_Ignore   : constant Glib.Option.GOption_Entry :=
                       (Long_Name       =>
                          New_String ("ignore-saved-scenario-values"),
                        Short_Name      => To_Gchar (ASCII.NUL),
                        Flags           => Glib.Option.G_Option_Flag_No_Arg,
                        Arg             => Glib.Option.G_Option_Arg_Callback,
                        Arg_Data        => On_Switch'Address,
                        Description     => New_String
                          ("Ignore the scenario values saved in .gnatstudio"),
                        Arg_Description => Gtkada.Types.Null_Ptr);

      --  Option for remaining arguments
      Opt_Remaining : constant Glib.Option.GOption_Entry :=
                        (Long_Name       => New_String (""),
                         Short_Name      => To_Gchar (ASCII.NUL),
                         Flags           => Glib.Option.G_Option_Flag_Filename,
                         Arg             => Glib.Option.G_Option_Arg_Callback,
                         Arg_Data        => On_File_Switch'Address,
                         Description     => Gtkada.Types.Null_Ptr,
                         Arg_Description => Gtkada.Types.Null_Ptr);
      Opt_Entries   : constant Glib.Option.GOption_Entry_Array :=
                        (Opt_Project,
                         Opt_Scenario,
                         Opt_Help,
                         Opt_Help_All,
                         Opt_Version,
                         Opt_Debug,
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
                         Opt_Config,
                         Opt_Autoconf,
                         Opt_Relocate,
                         Opt_Rootdir,
                         Opt_Ignore,
                         Opt_Configdb,
                         Opt_Remaining,
                         Opt_Pwd,
                         Opt_Path,
                         Glib.Option.Null_GOption_Entry);

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

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : String) is
      Ignored : Message_Dialog_Buttons;
   begin
      if Config.Host = Windows then
         declare
            Tmp_Log_File          : constant Virtual_File :=
                                      Create_From_Dir
                                        (Get_Tmp_Directory,
                                         "gnatstudio_error_log.txt");
            Writable_Tmp_Log_File : Writable_File := Tmp_Log_File.Write_File
              (Append => False);
         begin
            Write (Writable_Tmp_Log_File, Str => Message);
            Close (Writable_Tmp_Log_File);
         end;

      else
         Put_Line (Standard_Error, Message);
      end if;
   end Report_Error;

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
      --  Make sure that we don't display the preferences assistant when
      --  GNAT Studio is invoked with some switches: it might bother advanced
      --  users.

      Show_Preferences_Assistant := False;

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
              "GNAT Studio " & To_String (Config.Version) & " ("
              & Config.Source_Date & ") hosted on "
              & Config.Target & ASCII.LF & ASCII.LF
              & GPS_Command_Line.Context.Get_Help
              (Switch /= "--help-all", null);
         begin
            Put_Line (Help);
         end;

         GPS_Command_Line.Do_Exit := True;

      elsif Switch = "-X" then
         Handle_X_Switch (ICS.Value (Value));

      elsif Switch = "--debug" then
         Free (Program_Args);

         if Value /= ICS.Null_Ptr then
            Program_Args := new String'(ICS.Value (Value));
         else
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

            Free (GPS.Globals.Target);
            Free (Protocol);
            GPS.Globals.Target   :=
              new String '(Param (Param'First .. Column - 1));
            Protocol :=
              new String '(Param (Column + 1 .. Param'Last));
         end;

      elsif Switch = "--config" then
         Config_Files.Config_File := Create_From_Base
            (+ICS.Value (Value), Get_Current_Dir.Full_Name.all);

      elsif Switch = "--configdb" then
         Append
            (Config_Files.DB_Dirs,
             Create_From_Base
               (+ICS.Value (Value), Get_Current_Dir.Full_Name.all));

      elsif Switch = "--relocate-build-tree" then
         GPS.Globals.Build_Tree_Dir := Create_From_Base
           (+ICS.Value (Value), Get_Current_Dir.Full_Name.all);
         Normalize_Path (GPS.Globals.Build_Tree_Dir);

      elsif Switch = "--root-dir" then
         GPS.Globals.Root_Dir := Create_From_Base
           (+ICS.Value (Value), Get_Current_Dir.Full_Name.all);
         Normalize_Path (GPS.Globals.Root_Dir);

      elsif Switch = "--autoconf" then
         Config_Files.Autoconf := True;

      elsif Switch = "--traceon" then
         if not GNATCOLL.Traces.Exists (ICS.Value (Value)) then
            Put_Line ("Unknown trace for traceon: " & ICS.Value (Value));
         end if;
         GNATCOLL.Traces.Set_Active (Create (ICS.Value (Value)), True);

      elsif Switch = "--traceoff" then
         if not GNATCOLL.Traces.Exists (ICS.Value (Value)) then
            Put_Line ("Unknown trace for traceoff: " & ICS.Value (Value));
         end if;
         GNATCOLL.Traces.Set_Active (Create (ICS.Value (Value)), False);

      elsif Switch = "--tracefile" then
         GNATCOLL.Traces.Parse_Config_File
           (Filename => Create_From_Base (+ICS.Value (Value)));

      elsif Switch = "--tracelist" then
         GNATCOLL.Traces.Show_Configuration
           (Ada.Text_IO.Put_Line'Access);
         GPS_Command_Line.Do_Exit := True;

      elsif Switch = "--pwd" then
         GNAT.Directory_Operations.Change_Dir (ICS.Value (Value));

      elsif Switch = "--ignore-saved-scenario-values" then
         --  The kernel is not defined yet so store the value in a local var
         Ignore_Saved_Values := True;

      elsif Switch = "--path" then
         declare
            Current : GPS.Globals.String_Access := Getenv ("PATH");
         begin
            Setenv ("PATH", ICS.Value (Value) & Path_Separator & Current.all);
            Free (Current);
         end;
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
      if FName = "" then
         --  Ignore empty switch and succeed
         return 1;
      elsif FName (FName'First) = '-' then
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
      Exit_Status.all := 0;

      --  Sanitize the environment variables, and perform various init from
      --  them
      Initialize_Environment_Variables;

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
         --  Do not return 1 here. This is documented as exiting the
         --  application, but results in invalid memory access on OSX.
         --  Instead, we test the same flag again in Command_Line_Callback
         --  and exit at that point.
         null;
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

end GPS.Initialization;
