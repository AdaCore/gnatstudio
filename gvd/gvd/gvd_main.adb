-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Gdk.Types;                 use Gdk.Types;
with Gtk.Main;                  use Gtk.Main;
with Gtk;                       use Gtk;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.List;                  use Gtk.List;
with Gtk.List_Item;             use Gtk.List_Item;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Rc;                    use Gtk.Rc;
with Gtkada.Intl;               use Gtkada.Intl;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Toolbar;            use Gtkada.Toolbar;

with GUI_Utils;                 use GUI_Utils;

with GVD.Main_Window;           use GVD.Main_Window;
with GVD.Menu.Standalone;       use GVD.Menu.Standalone;
with Debugger;
with Process_Proxies;
with OS_Utils;                  use OS_Utils;
with GVD.Process;               use GVD.Process;
with GVD.Process.Standalone;    use GVD.Process.Standalone;
with GVD.Trace;                 use GVD.Trace;
with GVD.Types;                 use GVD.Types;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Window_Settings;       use GVD.Window_Settings;
with GVD.Code_Editors;          use GVD.Code_Editors;
with GVD.Toolbar;               use GVD.Toolbar;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.GVD;     use Language_Handlers.GVD;
with Language.Ada;              use Language.Ada;
with Language.C;                use Language.C;
with Language.Cpp;              use Language.Cpp;
with Default_Preferences;       use Default_Preferences;

with GVD.Open_Program_Dialog;   use GVD.Open_Program_Dialog;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Expect;               use GNAT.Expect;

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.IO;                   use GNAT.IO;

procedure GVD_Main is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   Directory_Separator : constant Character := GNAT.OS_Lib.Directory_Separator;
   Process             : Visual_Debugger;
   Debugger_List       : Argument_List (1 .. Argument_Count);
   Program_Args        : String_Access := new String'("");
   Debugger_Index      : Natural := 0;
   Main_Debug_Window   : GVD_Main_Window;
   Id                  : Glib.Gint;
   Level               : Integer;
   Debug_Type          : Debugger_Type := Gdb_Type;
   Button              : Message_Dialog_Buttons;
   Prefix              : String_Access;
   Home                : String_Access;
   Dir                 : String_Access;
   Remote_Host         : String_Access := new String'("");
   Debugger_Name       : String_Access := new String'("");
   Target              : String_Access := new String'("");
   Protocol            : String_Access := new String'("");
   Tmp_String          : String_Access;
   Debuggee_Name       : String_Access;
   Item                : Gtk_List_Item;
   Handler             : GVD_Language_Handler;

   procedure Init;
   --  Set up environment for GVD.

   procedure Bug_Dialog
     (Win : GVD_Main_Window; E : Exception_Occurrence);
   --  Display a bug box on the screen with as much information as possible.

   procedure Help;
   --  Display help on the standard output.

   function Clean_Parameter return String;
   --  Return a clean version of the parameter for command line switches, ie
   --  return the same thing as GNAT.Command_Line.Parameter, but strips the
   --  leading '=' if any, so that users can say '--log-level=4' for instance.

   procedure Ctrl_C_Handler;
   --  Handler for Ctrl-C events.

   ----------
   -- Init --
   ----------

   procedure Init is
      Dir_Created : Boolean := False;
   begin
      --  Set the TERM variable to a dummy value, since we only know how to
      --  handle simple terminals

      Setenv ("TERM", "dumb");

      Home := Getenv ("GVD_HOME");

      if Home.all = "" then
         Free (Home);
         Home := Getenv ("HOME");
      end if;

      Prefix := Getenv ("GVD_ROOT");

      if Prefix.all = "" then
         Free (Prefix);

         Prefix := Getenv ("GNAT_ROOT");

         if Prefix.all = "" then
            Free (Prefix);
            Prefix := new String'(Executable_Location);

            if Prefix.all = "" then
               Free (Prefix);
               Prefix := new String'(GVD.Prefix);
            end if;
         end if;
      end if;

      Bind_Text_Domain
        ("gvd", Prefix.all & GNAT.OS_Lib.Directory_Separator & "share" &
         Directory_Separator & "locale");

      if Home.all /= "" then
         if Is_Directory_Separator (Home (Home'Last)) then
            Dir := new String'(Home (Home'First .. Home'Last - 1) &
              Directory_Separator & ".gvd");
         else
            Dir := new String'(Home.all & Directory_Separator & ".gvd");
         end if;

      else
         --  Default to /
         Dir := new String'(Directory_Separator & ".gvd");
      end if;

      begin
         if not Is_Directory (Dir.all) then
            Make_Dir (Dir.all);
            Button := Message_Dialog
              ((-"Created config directory ") & Dir.all,
               Information, Button_OK, Justification => Justify_Left);
            Dir_Created := True;
         end if;

         if not
           Is_Directory (Dir.all & Directory_Separator & "sessions")
         then
            Make_Dir (Dir.all & Directory_Separator & "sessions");
            if not Dir_Created then
               Button := Message_Dialog
                 ((-"Created config directory ")
                  & Dir.all & Directory_Separator & "sessions",
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

      --  Load the preferences, or set the default values

      Load_Preferences
        (GVD_Prefs, Dir.all & Directory_Separator & "preferences");
   end Init;

   ----------------
   -- Bug_Dialog --
   ----------------

   procedure Bug_Dialog
     (Win : GVD_Main_Window; E : Exception_Occurrence) is
   begin
      Output_Line  (Win, "# Bug detected in GVD");
      Output_Line  (Win, "# Version: " & GVD.Version);
      Output_Line  (Win, "# Date: " & GVD.Source_Date);
      Output_Line  (Win, "# Host: " & GVD.Target);
      Output_Line  (Win, Exception_Information (E));
      Output_Error (Win, "Unexpected internal error, please report");
   end Bug_Dialog;

   ----------
   -- Help --
   ----------

   procedure Help is
      use ASCII;
   begin
      if GVD.Can_Output then
         Put_Line ("GVD " & GVD.Version & ", the GNU Visual Debugger.");
         Put_Line (-"Usage:");
         Put_Line (-"   gvd [options...] executable-file");
         Put_Line (
           -"       [--dargs [debugger options]] [--pargs [program options]]");
         Put_Line (-"Options:");
         Put_Line
           (-"   --debugger DEBUG    use DEBUG as the underlying debugger.");
         Put_Line (-"   --jdb               assume a java debugger.");
         Put_Line (-"   --host HOST         Run inferior debugger on HOST.");
         Put_Line ((-"   --target=TARG:PRO   ") &
                   (-"Load program on machine TARG using protocol PRO."));
         Put_Line
           (-"   --log-level [0-4]   Set level of logging (Default is 3).");
         Put_Line (-"   --editor-window=xid Use xid as the editor X window.");
         Put_Line
           (-("   --tty               Use controlling tty as additional " &
              "debugger console."));
         Put_Line (-"   --version           Show the GVD version and exit.");

      else
         Button := Message_Dialog
           ("GVD " & GVD.Version & LF &
            (-"Usage:") & LF &
            (-"   gvd [options...] executable-file") &
            (-" [--dargs [debugger options]] [--pargs [program options]]") &
            LF & (-"Options:") & LF &
            (-"   --debugger DEBUG    use DEBUG as the underlying debugger.")
            & LF & (-"   --jdb               assume a java debugger.") & LF &
            (-"   --host HOST         Run inferior debugger on HOST.") & LF &
            (-"   --target=TARG:PRO   ") &
            (-"Load program on machine TARG using protocol PRO.") & LF &
            (-"   --log-level [0-4]   Set level of logging (Default is 3).") &
            LF & (-"   --version           Show the GVD version and exit."),
            Information, Button_OK,
            Title => -"Help",
            Justification => Justify_Left);
      end if;
   end Help;

   ---------------------
   -- Clean_Parameter --
   ---------------------

   function Clean_Parameter return String is
      P : constant String := Parameter;
   begin
      if P (P'First) = '=' then
         return P (P'First + 1 .. P'Last);
      else
         return P;
      end if;
   end Clean_Parameter;

   --------------------
   -- Ctrl_C_Handler --
   --------------------

   procedure Ctrl_C_Handler is
   begin
      --  Ignore Ctrl-C events

      null;
   end Ctrl_C_Handler;

begin
   OS_Utils.Install_Ctrl_C_Handler (Ctrl_C_Handler'Unrestricted_Access);

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   GVD_Prefs := new GVD_Preferences_Manager;
   Register_Default_Preferences (GVD_Prefs, Page_Prefix => -"General:");
   Init;

   Gtk_New (Main_Debug_Window, "<gvd>", GVD_Menu_Items.all);

   Gtk_New (Handler);
   Main_Debug_Window.Lang_Handler := Language_Handler (Handler);
   Register_Language (Handler, "ada", Ada_Lang);
   Add_File_Extensions
     (Handler, "ada", Get_Pref (GVD_Prefs, Ada_Extensions));

   Register_Language (Handler, "c", C_Lang);
   Add_File_Extensions
     (Handler, "c", Get_Pref (GVD_Prefs, C_Extensions));

   Register_Language (Handler, "c++", Cpp_Lang);
   Add_File_Extensions
     (Handler, "c++", Get_Pref (GVD_Prefs, Cpp_Extensions));

   Set_Toolbar
     (Main_Debug_Window,
      Get_Handle_Box (Create_Toolbar (Main_Debug_Window)));

   --  Load the window_settings, if any.

   if Is_Regular_File (Dir.all & Directory_Separator & "window_settings") then
      Load_Window_Settings
        (Dir.all & Directory_Separator & "window_settings",
         Gtk_Widget (Main_Debug_Window));
   end if;

   Main_Debug_Window.Home_Dir := Dir;
   Main_Debug_Window.Prefix_Directory := Prefix;

   declare
      Rc : constant String := Prefix.all & Directory_Separator & "bin" &
        Directory_Separator & "gtkrc";
   begin
      if Is_Regular_File (Rc) then
         Gtk.Rc.Parse (Rc);
      end if;
   end;

   --  ??? Should have a cleaner way of initializing Log_File

   declare
      Log : aliased constant String :=
        Dir.all & Directory_Separator & "log" & ASCII.NUL;
   begin
      Main_Debug_Window.Debug_Mode := True;
      Main_Debug_Window.Log_Level  := GVD.Types.Hidden;
      Main_Debug_Window.Log_File   := Create_File (Log'Address, Fmode => Text);
   end;

   Initialize_Option_Scan (Section_Delimiters => "-dargs -pargs");

   loop
      case Getopt ("-debugger: -jdb -editor-window: -tty fullname " &
        "-version -help -host: -log-level: -target:")
      is
         -- long option names --
         when '-' =>

            case Full_Switch (Full_Switch'First + 1) is
               -- --debugger --
               when 'd' =>
                  Free (Debugger_Name);
                  Debugger_Name := new String'(Clean_Parameter);

                  if Main_Debug_Window.Open_Program = null then
                     Gtk_New (Main_Debug_Window.Open_Program);
                  end if;

                  Gtk_New (Item, Clean_Parameter);
                  Add
                    (Get_List (Main_Debug_Window.Open_Program.Debugger_Combo),
                     Item);
                  Set_Text
                    (Get_Entry (Main_Debug_Window.Open_Program.Debugger_Combo),
                     Clean_Parameter);

               -- --editor-window --
               when 'e' =>
                  Main_Debug_Window.External_XID :=
                    Guint32'Value (Clean_Parameter);

               -- -fullname --
               when 'f' =>
                  --  supported for backward compatibility only, and
                  --  compatibility with Emacs' gdb mode
                  null;

               -- --jdb --
               when 'j' => Debug_Type := Jdb_Type;

               -- --log-level --
               when 'l' =>
                  begin
                     Level := Integer'Value (Clean_Parameter);
                  exception
                     when Constraint_Error =>
                        if GVD.Can_Output then
                           Put_Line ("Invalid parameter to --log-level");
                        end if;

                        Help;
                        OS_Exit (-1);
                  end;

                  --  Level 0 is no logging, 1 is minimal logging (only
                  --  user commands), 4 is max logging (all commands)

                  if Level = 0 then
                     Main_Debug_Window.Debug_Mode := False;
                  elsif
                    Level in 1 .. Command_Type'Pos (Command_Type'Last) + 1
                  then
                     Main_Debug_Window.Debug_Mode := True;
                     Main_Debug_Window.Log_Level :=
                       Command_Type'Val
                         (Command_Type'Pos (Command_Type'Last) + 1 - Level);

                  else
                     if GVD.Can_Output then
                        Put_Line ("Invalid value for --log-level");
                     end if;

                     Help;
                     OS_Exit (-1);
                  end if;

               when 't' =>
                  -- --tty --
                  if Full_Switch = "-tty" then
                     Main_Debug_Window.TTY_Mode := True;
                     Id := Standard_Input_Package.Add
                       (0, Input_Read, Input_Available'Access,
                        Main_Debug_Window.all'Access);

                  -- --target --
                  elsif Full_Switch = "-target" then
                     declare
                        Param  : constant String := Clean_Parameter;
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

                        if Main_Debug_Window.Open_Program = null then
                           Gtk_New (Main_Debug_Window.Open_Program);
                        end if;

                        Set_Text
                          (Get_Entry
                            (Main_Debug_Window.Open_Program.Protocol_Combo),
                           Protocol.all);

                        Gtk_New (Item, Target.all);
                        Add (Get_List
                               (Main_Debug_Window.
                                  Open_Program.Program_Host_Combo),
                             Item);
                        Set_Text
                          (Main_Debug_Window.Open_Program.Target_Entry,
                           Target.all);
                     end;
                  end if;

               -- --version --
               when 'v' =>
                  if GVD.Can_Output then
                     Put_Line ("GVD version " & GVD.Version &
                       " (" & GVD.Source_Date & ") hosted on " & GVD.Target);
                  else
                     Button := Message_Dialog
                       ("GVD version " & GVD.Version &
                        " (" & GVD.Source_Date & ") hosted on " & GVD.Target,
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

                  -- --host --
                  elsif Full_Switch = "-host" then
                     Free (Remote_Host);
                     Remote_Host := new String'(Clean_Parameter);

                     if Main_Debug_Window.Open_Program = null then
                        Gtk_New (Main_Debug_Window.Open_Program);
                     end if;

                     Gtk_New (Item, Clean_Parameter);
                     Add (Get_List (Main_Debug_Window.Open_Program.Host_Combo),
                          Item);
                     Set_Text
                       (Get_Entry (Main_Debug_Window.Open_Program.Host_Combo),
                        Clean_Parameter);
                  end if;

               when others =>
                  null;
            end case;

         when ASCII.NUL =>
            exit;

         when others =>
            null;
      end case;
   end loop;

   --  Do we have an executable on the command line (this is the first
   --  non-switch argument found on the command line)
   Debuggee_Name := new String'(GNAT.Command_Line.Get_Argument);

   if Debuggee_Name.all /= "" then
      if Main_Debug_Window.Open_Program = null then
         Gtk_New (Main_Debug_Window.Open_Program);
      end if;

      declare
         Item : Gtk_List_Item;
      begin
         Gtk_New (Item, Debuggee_Name.all);
         Add (Get_List (Main_Debug_Window.Open_Program.Program_Combo), Item);
         Set_Text
           (Get_Entry (Main_Debug_Window.Open_Program.Program_Combo),
            Debuggee_Name.all);
      end;
   end if;

   --  Debugger args
   Goto_Section ("-dargs");

   loop
      exit when Getopt ("*") = ASCII.NUL;

      Debugger_Index := Debugger_Index + 1;
      Debugger_List (Debugger_Index) := new String'(Full_Switch);
   end loop;

   --  Program args
   Goto_Section ("-pargs");

   loop
      exit when Getopt ("*") = ASCII.NUL;

      Tmp_String := Program_Args;
      Program_Args := new String'(Program_Args.all & ' ' & Full_Switch);
      Free (Tmp_String);
   end loop;

   --  Show the main window ASAP, and then set the cursor to busy while we
   --  call Create_Debugger. Unset the busy cursor right after the call.

   Show_All (Main_Debug_Window);
   Process := Create_Debugger
     (Window          => Main_Debug_Window,
      Kind            => Debug_Type,
      Executable      => Debuggee_Name.all,
      Debugger_Args   => Debugger_List (1 .. Debugger_Index),
      Executable_Args => Program_Args.all,
      Remote_Host     => Remote_Host.all,
      Remote_Target   => Target.all,
      Remote_Protocol => Protocol.all,
      Debugger_Name   => Debugger_Name.all);

   Free (Program_Args);

   for J in Debugger_List'Range loop
      Free (Debugger_List (J));
   end loop;

   if Dir /= null
     and then Is_Directory (Dir.all & Directory_Separator & "sessions")
   then
      Main_Debug_Window.Sessions_Dir := new String'
        (Dir.all & Directory_Separator & "sessions");
   else
      Button := Message_Dialog
        ((-"Cannot find sessions directory ") & Dir.all &
         Directory_Separator & "sessions" & ASCII.LF & (-"Exiting..."),
         Error, Button_OK,
         Justification => Justify_Left);
      OS_Exit (1);
   end if;

   Show_All (Main_Debug_Window);

   --  Scroll the explorer window to the currently displayed file.
   Display_Selection (Process.Editor_Text);

   loop
      begin
         Gtk.Main.Main;
         exit;
      exception
         when E : others =>
            declare
               List : Debugger_List_Link := Main_Debug_Window.First_Debugger;

               use type Debugger.Debugger_Access;

            begin
               while List /= null loop
                  Process := Visual_Debugger (List.Debugger);

                  if Process.Timeout_Id /= 0 then
                     Timeout_Remove (Process.Timeout_Id);
                     Process.Timeout_Id := 0;
                  end if;

                  if Process.Debugger /= null then
                     Process_Proxies.Set_Command_In_Process
                       (Debugger.Get_Process (Process.Debugger), False);
                     Set_Busy (Process, False);
                     Unregister_Dialog (Process);
                     Free (Process.Current_Command);
                  end if;

                  List := List.Next;
               end loop;
            end;

            Bug_Dialog (Main_Debug_Window, E);
      end;
   end loop;

   Save_Preferences
     (GVD_Prefs,
      Main_Debug_Window.Home_Dir.all & Directory_Separator & "preferences");
   Destroy (Main_Debug_Window);
   Destroy (Default_Preferences.Preferences_Manager (GVD_Prefs));

exception
   when Process_Died =>
      if GVD.Can_Output then
         Put_Line ("The underlying debugger died prematurely. Exiting...");
      end if;

   when Invalid_Switch | Invalid_Parameter =>
      if GVD.Can_Output then
         Put_Line ("Invalid command line");
      end if;

      Help;

   when E : others =>
      Bug_Dialog (Main_Debug_Window, E);
      Destroy (Main_Debug_Window);
end GVD_Main;
