-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Glib;
with Gdk.Types; use Gdk.Types;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Enums; use Gtk.Enums;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Gtkada.Intl; use Gtkada.Intl;
with Gtkada.Dialogs; use Gtkada.Dialogs;
with Odd.Process; use Odd.Process;
with Odd.Types;
with Debugger;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect; use GNAT.Expect;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Language.Debugger.Ada; use Language.Debugger.Ada;
with Language.Debugger.C;   use Language.Debugger.C;
with Language;              use Language;
with Display_Items;         use Display_Items;
with Odd.Strings;           use Odd.Strings;

procedure Odd_Main is
   Process_Tab       : Debugger_Process_Tab;
   List              : Argument_List (1 .. Argument_Count);
   Main_Debug_Window : Main_Debug_Window_Access;
   Id                : Glib.Gint;
   Index             : Natural := 0;
   Level             : Integer;
   Debug_Type        : Debugger.Debugger_Type := Debugger.Gdb_Type;
   Button            : Message_Dialog_Buttons;
   Root              : String_Access;
   Home              : String_Access;
   Dir               : String_Access;
   Remote_Host       : String_Access := new String' ("");
   Debuggee_Name     : String_Access;

   procedure Init;
   --  Set up environment for Odd.

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
         Bind_Text_Domain ("Gvd", Root.all & Directory_Separator & "share" &
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
                "Please report with the contents of the file " &
                Dir.all & Directory_Separator & "log");
      Put_Line (Standard_Error, "and the following information:");
      Put_Line (Standard_Error, "Version: " & Odd.Version);
      Put_Line (Standard_Error, Exception_Information (E));
      Button := Message_Dialog
        ("Please report with the contents of the file " &
         Dir.all & Directory_Separator & "log" & ASCII.LF &
         "and the following information:" & ASCII.LF &
         "Version: " & Odd.Version & ASCII.LF &
         Format (Exception_Information (E), Columns => 80),
         Error, Button_OK,
         Title => -"Bug detected in GVD",
         Justification => Justify_Left);
   end Bug_Dialog;

   procedure Help is
   begin
      Put_Line ("GVD " & Odd.Version);
      Put_Line ("This is a beta release of GVD, the GNU Visual Debugger.");
      Put_Line ("Please do not redistribute.");
      Put_Line ("Usage:");
      Put_Line ("   gvd [options...] executable-file");
      Put_Line ("Options:");
      Put_Line ("   --log-level [0-4] Set level of logging (Default is 3).");
      Put_Line ("   --jdb             Invoke JDB as inferior debugger.");
      Put_Line ("   --host HOST       Run inferior debugger on HOST.");
      Put_Line ("   --tty             Use controlling tty as additional " &
                "debugger console.");
      Put_Line ("   --version         Show the GVD version and exit.");
      New_Line;
      Put_Line ("Other arguments are passed to the underlying debugger.");
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
      Main_Debug_Window.Log_Level  := Odd.Types.Hidden;
      Main_Debug_Window.Log_File   := Create_File (Log'Address, Fmode => Text);
   end;

   loop
      case Getopt ("-tty fullname -jdb -version -help -host: -log-level:") is
         -- long option names --
         when '-' =>

            case Full_Switch (Full_Switch'First + 1) is
               -- --tty mode --
               when 't' =>
                  --  Install input handler to receive commands from an
                  --  external IDE while handling GtkAda events.

                  Main_Debug_Window.TTY_Mode := True;
                  Id := Standard_Input_Package.Add
                    (0, Input_Read, Input_Available'Access,
                     Main_Debug_Window.all'Access);

               -- -fullname --
               when 'f' => null;
                  --  supported for backward compatibility only, and
                  --  compatibility with Emacs' gdb mode

               -- --jdb --
               when 'j' => Debug_Type := Debugger.Jdb_Type;

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
                       Odd.Types.Command_Type'Val
                         (Odd.Types.Command_Type'Pos
                           (Odd.Types.Command_Type'Last) + 1 - Level);
                  end if;

               -- --version --
               when 'v' =>
                  Put_Line ("GVD Version " & Odd.Version);
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

   --  ??? Should set the executable here, so that we can use Set_Executable
   --  and get initialization for free.

   Process_Tab := Create_Debugger
     (Main_Debug_Window,
      Debug_Type,
      Debuggee_Name.all,
      List (1 .. Index),
      Remote_Host => Remote_Host.all);

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
end Odd_Main;
