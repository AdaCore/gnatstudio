-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Glib;                  use Glib;
with Gdk.Input;
with Gdk.Types;             use Gdk.Types;
with Gtk.Main;
with Gtk;                   use Gtk;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Notebook;          use Gtk.Notebook;
with Gtkada.Intl;           use Gtkada.Intl;
with Gtkada.Dialogs;        use Gtkada.Dialogs;

with GVD.Main_Window;       use GVD.Main_Window;
with GVD.Menu;              use GVD.Menu;
with Debugger;
with Process_Proxies;
with OS_Utils;              use OS_Utils;
with GVD.Process;           use GVD.Process;
with GVD.Trace;             use GVD.Trace;
with GVD.Types;             use GVD.Types;
with GVD.Preferences;       use GVD.Preferences;
with GVD.Window_Settings;   use GVD.Window_Settings;

with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Command_Line; use GNAT.Command_Line;
pragma Warnings (Off);
with GNAT.Expect; use GNAT.Expect;
pragma Warnings (On);

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions;    use Ada.Exceptions;
with GNAT.IO;           use GNAT.IO;

procedure GVD_Main is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   Process           : Debugger_Process_Tab;
   Debugger_List     : Argument_List (1 .. Argument_Count);
   Program_Args      : String_Access := new String' ("");
   Debugger_Index    : Natural := 0;
   Main_Debug_Window : GVD_Main_Window;
   Id                : Glib.Gint;
   Level             : Integer;
   Debug_Type        : Debugger_Type := Gdb_Type;
   Button            : Message_Dialog_Buttons;
   Editor            : String_Access;
   Prefix            : String_Access;
   Home              : String_Access;
   Dir               : String_Access;
   Remote_Host       : String_Access := new String' ("");
   Debugger_Name     : String_Access := new String' ("");
   Target            : String_Access := new String' ("");
   Protocol          : String_Access := new String' ("");
   Tmp_String        : String_Access;
   Debuggee_Name     : String_Access;

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

   procedure Init is
      Dir_Created : Boolean := False;
   begin
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
            Prefix := new String' (Executable_Location);

            if Prefix.all = "" then
               Free (Prefix);
               Prefix := new String' (GVD.Prefix);
            end if;
         end if;
      end if;

      Bind_Text_Domain ("gvd", Prefix.all & Directory_Separator & "share" &
        Directory_Separator & "locale");

      if Home.all /= "" then
         if Is_Directory_Separator (Home (Home'Last)) then
            Dir := new String' (Home (Home'First .. Home'Last - 1) &
              Directory_Separator & ".gvd");
         else
            Dir := new String' (Home.all & Directory_Separator & ".gvd");
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

      if Is_Regular_File (Dir.all & Directory_Separator & "preferences") then
         Load_Preferences (Dir.all & Directory_Separator & "preferences");
      else
         Set_Default_Preferences;
      end if;
   end Init;

   procedure Bug_Dialog
     (Win : GVD_Main_Window; E : Exception_Occurrence) is
   begin
      Output_Line (Win, "# Bug detected in GVD");
      Output_Line (Win, "# Version: " & GVD.Version);
      Output_Line (Win, "# Date: " & GVD.Source_Date);
      Output_Line (Win, "# Target: " & GVD.Target);
      Output_Line (Win, Exception_Information (E));

      Button := Message_Dialog
        ((-"Please report [see GVD Manual under Reporting Bugs]") & ASCII.LF &
         (-"with the contents of the file ") &
         Dir.all & Directory_Separator & "log" & ASCII.LF &
         (-"and a description as complete as possible (including sources)") &
         ASCII.LF & (-"to reproduce the bug"),
         Error, Button_OK,
         Title => -"Bug detected in GVD",
         Justification => Justify_Left);
   end Bug_Dialog;

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

   procedure G_Type_Init (Debug_Flags : Gint);
   pragma Import (C, G_Type_Init, "g_type_init");
begin
   Gtk.Main.Set_Locale;
   --  G_Type_Init (1 + 2);
   Gtk.Main.Init;
   Init;
   Gtk_New (Main_Debug_Window, "<gvd>", GVD_Menu_Items.all);

   --  Load the window_settings, if any.

   if Is_Regular_File (Dir.all & Directory_Separator & "window_settings") then
      Load_Window_Settings
        (Dir.all & Directory_Separator & "window_settings",
         Gtk_Widget (Main_Debug_Window));
   end if;

   Main_Debug_Window.Gvd_Home_Dir := Dir;
   Main_Debug_Window.Prefix_Directory := Prefix;

   --  ??? Should have a cleaner way of initializing Log_File

   declare
      Log : aliased constant String :=
        Dir.all & Directory_Separator & "log" & ASCII.NUL;
   begin
      Main_Debug_Window.Debug_Mode := True;
      Main_Debug_Window.Log_Level  := GVD.Types.Hidden;
      Main_Debug_Window.Log_File   := Create_File (Log'Address, Fmode => Text);
   end;

   Editor := Getenv ("GVD_EDITOR");

   if Editor.all /= "" then
      Main_Debug_Window.External_Editor := Editor;
   else
      Free (Editor);
      Main_Debug_Window.External_Editor :=
        new String' (Get_Pref (Default_External_Editor));
   end if;

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
                  Debugger_Name := new String' (Clean_Parameter);

               when 'e' =>
                  -- --editor-window --
                  Main_Debug_Window.External_XID :=
                    Guint32'Value (Clean_Parameter);

               -- -fullname --
               when 'f' => null;
                  --  supported for backward compatibility only, and
                  --  compatibility with Emacs' gdb mode

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
                     end;
                  end if;

               -- --version --
               when 'v' =>
                  if GVD.Can_Output then
                     Put_Line ("GVD version " & GVD.Version &
                       " (" & GVD.Source_Date & ") for " & GVD.Target);
                  else
                     Button := Message_Dialog
                       ("GVD version " & GVD.Version &
                        " (" & GVD.Source_Date & ") for " & GVD.Target,
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
                     Remote_Host := new String' (Clean_Parameter);
                  end if;

               when others =>
                  null;
            end case;

         when ASCII.Nul =>
            exit;

         when others =>
            null;
      end case;
   end loop;

   --  Do we have an executable on the command line (this is the first
   --  non-switch argument found on the command line)
   Debuggee_Name := new String' (GNAT.Command_Line.Get_Argument);

   --  Debugger args
   Goto_Section ("-dargs");

   loop
      exit when Getopt ("*") = ASCII.NUL;

      Debugger_Index := Debugger_Index + 1;
      Debugger_List (Debugger_Index) := new String' (Full_Switch);
   end loop;

   --  Program args
   Goto_Section ("-pargs");

   loop
      exit when Getopt ("*") = ASCII.NUL;

      Tmp_String := Program_Args;
      Program_Args := new String' (Program_Args.all & ' ' & Full_Switch);
      Free (Tmp_String);
   end loop;

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

   loop
      begin
         Gtk.Main.Main;
         exit;
      exception
         when E : others =>
            declare
               Page      : Gtk_Widget;
               Num_Pages : constant Gint :=
                 Gint (Page_List.Length
                   (Get_Children (Main_Debug_Window.Process_Notebook)));

            begin
               for Page_Num in 0 .. Num_Pages - 1 loop
                  Page := Get_Nth_Page
                    (Main_Debug_Window.Process_Notebook, Page_Num);

                  if Page /= null then
                     Process := Process_User_Data.Get (Page);

                     if Process.Input_Id /= 0 then
                        Gdk.Input.Remove (Process.Input_Id);
                        Process.Input_Id := 0;
                     end if;

                     Process_Proxies.Set_Command_In_Process
                       (Debugger.Get_Process (Process.Debugger), False);
                     Set_Busy_Cursor (Process, False);
                     Unregister_Dialog (Process);
                     Free (Process.Current_Command);
                  end if;
               end loop;
            end;

            Bug_Dialog (Main_Debug_Window, E);
      end;
   end loop;

   Destroy (Main_Debug_Window);

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
