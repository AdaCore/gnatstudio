-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Debugger;
with Process_Proxies;
with GVD.Process;           use GVD.Process;
with GVD.Types;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GVD.Preferences;       use GVD.Preferences;

pragma Warnings (Off);
with GNAT.Expect; use GNAT.Expect;
pragma Warnings (On);

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

with Language.Debugger.Ada; use Language.Debugger.Ada;
with Language.Debugger.C;   use Language.Debugger.C;
with Language;              use Language;
with Display_Items;         use Display_Items;
with GVD.Strings;           use GVD.Strings;

procedure GVD_Main is
   Process           : Debugger_Process_Tab;
   List              : Argument_List (1 .. Argument_Count);
   Main_Debug_Window : Main_Debug_Window_Access;
   Id                : Glib.Gint;
   Index             : Natural := 0;
   Level             : Integer;
   Debug_Type        : GVD.Types.Debugger_Type := GVD.Types.Gdb_Type;
   Button            : Message_Dialog_Buttons;
   Editor            : String_Access;
   Root              : String_Access;
   Home              : String_Access;
   Dir               : String_Access;
   Remote_Host       : String_Access := new String' ("");
   Debugger_Name     : String_Access := new String' ("");
   Target            : String_Access := new String' ("");
   Protocol          : String_Access := new String' ("");
   Debuggee_Name     : String_Access;

   procedure Init;
   --  Set up environment for GVD.

   procedure Bug_Dialog (E : Exception_Occurrence);
   --  Display a bug box on the screen with as much information as possible.

   function Format (Str : String; Columns : Positive) return String;
   --  Cut Str in lines of no more than Columns columns by replacing spaces
   --  by ASCII.LF characters at the most appropriate place.

   procedure Help;
   --  Display help on the standard output.

   function Clean_Parameter return String;
   --  Return a clean version of the parameter for command line switches, ie
   --  return the same thing as GNAT.Command_Line.Parameter, but strips the
   --  leading '=' if any, so that users can say '--log-level=4' for instance.

   procedure Init is
      Dir_Created : Boolean := False;
   begin
      Root := Getenv ("GVD_ROOT");
      Home := Getenv ("GVD_HOME");

      if Home.all = "" then
         Home := Getenv ("HOME");
      end if;

      if Root.all /= "" then
         Bind_Text_Domain ("GtkAda", Root.all & Directory_Separator & "share" &
           Directory_Separator & "locale");
         Bind_Text_Domain ("gvd", Root.all & Directory_Separator & "share" &
           Directory_Separator & "locale");
      end if;

      if Home.all /= "" then
         Dir := new String' (Home.all & Directory_Separator & ".gvd");
      else
         Dir := new String'(Directory_Separator & ".gvd"); -- ??? Is this right
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

      --  ??? This should be moved in a future "preferences" package, so as to
      --  accomodate user's specific extensions
      Add_File_Extension (new Ada_Language, "\.adb$");
      Add_File_Extension (new Ada_Language, "\.ads$");
      Add_File_Extension (new C_Language, "\.c$");
      Add_File_Extension (new C_Language, "\.h$");
   end Init;

   function Format (Str : String; Columns : Positive) return String is
      S     : String (Str'Range);
      Blank : Natural := 0;
      Count : Natural := 0;

   begin
      for J in Str'Range loop
         S (J) := Str (J);

         if Str (J) = ASCII.LF then
            Count := 0;
         else
            Count := Count + 1;

            if Str (J) = ' ' then
               Blank := J;
            end if;

            if Count = Columns and Blank /= 0 then
               S (Blank) := ASCII.LF;
               Count := 0;
               Blank := 0;
            end if;
         end if;
      end loop;

      return Strip_Control_M (S);
   end Format;

   procedure Bug_Dialog (E : Exception_Occurrence) is
   begin
      Put_Line (Standard_Error, -"Bug detected in GVD");
      Put_Line (Standard_Error,
                (-"Please report with the contents of the file ") &
                Dir.all & Directory_Separator & "log");
      Put_Line (Standard_Error, -"and the following information:");
      Put_Line (Standard_Error, (-"Version: ") & GVD.Version);
      Put_Line (Standard_Error, Exception_Information (E));
      Button := Message_Dialog
        ((-"Please report with the contents of the file ") &
         Dir.all & Directory_Separator & "log" & ASCII.LF &
         (-"and the following information:") & ASCII.LF &
         (-"Version: ") & GVD.Version & ASCII.LF &
         Format (Exception_Information (E), Columns => 80),
         Error, Button_OK,
         Title => -"Bug detected in GVD",
         Justification => Justify_Left);
   end Bug_Dialog;

   procedure Help is
   begin
      Put_Line ("GVD " & GVD.Version & ", the GNU Visual Debugger.");
      Put_Line (-"Usage:");
      Put_Line (-"   gvd [options...] executable-file");
      Put_Line (-"Options:");
      Put_Line (-"   --debugger DEBUG  use DEBUG as the underlying debugger.");
      Put_Line (-"   --jdb             assume a java debugger.");
      Put_Line (-"   --host HOST       Run inferior debugger on HOST.");
      Put_Line (-"   --no-explorer     Do not display explorer window.");
      Put_Line (-("   --target TARG:PRO " &
                  "Load program on machine TARG using protocol PRO."));
      Put_Line (-"   --log-level [0-4] Set level of logging (Default is 3).");
      Put_Line (-("   --tty             Use controlling tty as additional " &
                  "debugger console."));
      Put_Line (-"   --version         Show the GVD version and exit.");
      New_Line;
      Put_Line (-"Other arguments are passed to the underlying debugger.");
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

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Init;
   Gtk_New (Main_Debug_Window);

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
        Current_Preferences.Default_External_Editor;
   end if;

   loop
      case Getopt ("-debugger: -jdb -tty fullname -version -help " &
        "-host: -log-level: -no-explorer -target:")
      is
         -- long option names --
         when '-' =>

            case Full_Switch (Full_Switch'First + 1) is
               -- --debugger --
               when 'd' =>
                  Free (Debugger_Name);
                  Debugger_Name := new String' (Clean_Parameter);

               -- -fullname --
               when 'f' => null;
                  --  supported for backward compatibility only, and
                  --  compatibility with Emacs' gdb mode

               -- --jdb --
               when 'j' => Debug_Type := GVD.Types.Jdb_Type;

               -- --log-level --
               when 'l' =>
                  begin
                     Level := Integer'Value (Clean_Parameter);
                  exception
                     when Constraint_Error =>
                        Put_Line ("Invalid parameter to --log-level");
                        Help;
                        OS_Exit (-1);
                  end;

                  --  Level 0 is no logging, 1 is minimal logging (only
                  --  user commands), 4 is max logging (all commands)

                  if Level = 0 then
                     Main_Debug_Window.Debug_Mode := False;
                  else
                     Main_Debug_Window.Debug_Mode := True;
                     Main_Debug_Window.Log_Level :=
                       GVD.Types.Command_Type'Val
                         (GVD.Types.Command_Type'Pos
                           (GVD.Types.Command_Type'Last) + 1 - Level);
                  end if;

               -- --no-explorer --
               when 'n' =>
                  Current_Preferences.Display_Explorer := False;

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
                          Ada.Strings.Fixed.Index (Param, ":");

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
                  Put_Line ("GVD Version " & GVD.Version);
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

   Process := Create_Debugger
     (Window          => Main_Debug_Window,
      Kind            => Debug_Type,
      Executable      => Debuggee_Name.all,
      Params          => List (1 .. Index),
      Remote_Host     => Remote_Host.all,
      Remote_Target   => Target.all,
      Remote_Protocol => Protocol.all,
      Debugger_Name   => Debugger_Name.all);

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
   Init_Graphics (Get_Window (Main_Debug_Window));

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

            Bug_Dialog (E);
      end;
   end loop;

   Destroy (Main_Debug_Window);

exception
   when Process_Died =>
      Put_Line ("The underlying debugger died prematurely. Exiting...");

   when Invalid_Switch =>
      Put_Line ("Invalid command line");
      Help;

   when E : others =>
      Bug_Dialog (E);
      Destroy (Main_Debug_Window);
end GVD_Main;
