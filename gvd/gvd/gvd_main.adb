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
with Debugger;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Odd_Main is
   Process_Tab       : Debugger_Process_Tab;
   List              : Argument_List (1 .. Argument_Count);
   Main_Debug_Window : Main_Debug_Window_Access;
   Id                : Glib.Gint;
   Index             : Natural := 0;
   Debug_Type        : Debugger.Debugger_Type := Debugger.Gdb_Type;
   Button            : Message_Dialog_Buttons;

   procedure Init;
   --  Set up environment for Odd.

   procedure Bug_Dialog (E : Exception_Occurrence);
   --  Display a bug box on the screen with as much information as possible.

   function Format (Str : String; Columns : Positive) return String;
   --  Cut Str in lines of no more than Columns columns by replacing spaces
   --  by ASCII.LF characters at the most appropriate place.

   procedure Init is
      Root : String_Access;
      Home : String_Access;

   begin
      Root := Getenv ("GVD_ROOT");
      Home := Getenv ("HOME");

      if Root /= null then
         Bind_Text_Domain ("GtkAda", Root.all & Directory_Separator & "share" &
           Directory_Separator & "locale");
         Bind_Text_Domain ("Gvd", Root.all & Directory_Separator & "share" &
           Directory_Separator & "locale");
         Free (Root);
      end if;

      if Home /= null then
         declare
            Dir : constant String := Home.all & Directory_Separator & ".gvd";
         begin
            if not Is_Directory (Dir) then
               Make_Dir (Home.all & Directory_Separator & ".gvd");
               Button := Message_Dialog
                 ((-"Created config directory ") & Dir,
                  Information, Button_OK,
                  Justification => Justify_Left);
            end if;
         exception
            when Directory_Error =>
               Button := Message_Dialog
                 ((-"Cannot create config directory ") & Dir & ASCII.LF &
                    (-"Exiting..."),
                  Error, Button_OK,
                  Justification => Justify_Left);
         end;

         Free (Home);
      end if;
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

      return S;
   end Format;

   procedure Bug_Dialog (E : Exception_Occurrence) is
   begin
      Button := Message_Dialog
        ((-"Please report with the following information:") & ASCII.LF &
         Format (Exception_Information (E), Columns => 80),
         Error, Button_OK,
         Title => -"Bug detected in odd",
         Justification => Justify_Left);
      Put_Line (Standard_Error, -"Bug detected in odd");
      Put_Line (Standard_Error,
        -"Please report with the following information:");
      Put_Line (Standard_Error, Exception_Information (E));
   end Bug_Dialog;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Init;
   Gtk_New (Main_Debug_Window);

   for J in 1 .. Argument_Count loop
      if Argument (J) = "--tty" then
         --  Install input handler to receive commands from an external
         --  IDE while handling GtkAda events.

         Main_Debug_Window.TTY_Mode := True;
         Id := Standard_Input_Package.Add
           (0, Input_Read, Input_Available'Access,
            Main_Debug_Window.all'Access);

      elsif Argument (J) = "--debug" then
         Main_Debug_Window.Debug_Mode := True;

      elsif Argument (J) = "--jdb" then
         Debug_Type := Debugger.Jdb_Type;
      else
         Index := Index + 1;
         List (Index) := new String' (Argument (J));
      end if;
   end loop;

   --  ??? Should set the executable here, so that we can use Set_Executable
   --  and get initialization for free.
   Process_Tab := Create_Debugger
     (Main_Debug_Window, Debug_Type, "", List (1 .. Index));
   Show_All (Main_Debug_Window);

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
   when E : others =>
      Bug_Dialog (E);
      Destroy (Main_Debug_Window);
end Odd_Main;
