-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;                         use Glib;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Main;                     use Gtk.Main;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.Dialogs;               use Gtkada.Dialogs;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.File_Selector.Filters; use Gtkada.File_Selector.Filters;

with Glide_Intl;              use Glide_Intl;

with GVD.Menu;                use GVD.Menu;
with GVD.Types;               use GVD.Types;
with GVD.Status_Bar;          use GVD.Status_Bar;
with GVD.Process;             use GVD.Process;
with Debugger;                use Debugger;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Help;       use Glide_Kernel.Help;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Editor;     use Glide_Kernel.Editor;
with Glide_Kernel.Project;    use Glide_Kernel.Project;

with Glide_Main_Window;       use Glide_Main_Window;
with Glide_Page;              use Glide_Page;

with Vdiff_Pkg;               use Vdiff_Pkg;
with Vdiff_Utils;             use Vdiff_Utils;
with Diff_Utils;              use Diff_Utils;

with OS_Utils;                use OS_Utils;
with GVD.Dialogs;             use GVD.Dialogs;

with GNAT.Expect;               use GNAT.Expect;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Factory_Data;            use Factory_Data;

with Ada.Exceptions;          use Ada.Exceptions;
with Traces;                  use Traces;

package body Glide_Menu is

   Me : Debug_Handle := Create ("Menu");

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Open_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Open menu

   procedure On_New_View
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->New View menu

   procedure On_New_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->New menu

   procedure On_Save
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save menu

   procedure On_Save_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save As menu

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Close menu

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save Desktop menu

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Exit menu

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Undo menu

   procedure On_Redo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Redo menu

   procedure On_Cut
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Cut menu

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Copy menu

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Paste menu

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Select All menu

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Preferences menu

   procedure On_Goto_Declaration_Or_Body
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Navigate->Goto Declaration<->Body

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->Open menu

   procedure On_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Build->Make menu

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Build->Run menu

   procedure On_Stop_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Build->Stop Build menu

   procedure On_Debug_Executable
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Debug->Another Executable menu

   procedure On_Step
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Step menu

   procedure On_Step_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Step Instruction menu

   procedure On_Next
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Next menu

   procedure On_Next_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Next Instruction menu

   procedure On_Finish
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Finish menu

   procedure On_Continue
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Debug->Continue menu

   procedure On_Generate_Body
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Tools->Generate Body menu

   procedure On_Compare_Two_Files
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Tools->Compare->Two Files menu

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Help->Manual menu

   procedure On_About_Glide
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Help->About menu

   -----------------------------
   -- Misc internal functions --
   -----------------------------

   procedure Refresh;
   --  Handle pending graphical events.

   -------------
   -- Refresh --
   -------------

   procedure Refresh is
      Dead : Boolean;
   begin
      while Gtk.Main.Events_Pending loop
         Dead := Main_Iteration;
      end loop;
   end Refresh;

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      declare
         Filename : constant String := Select_File (Title => -"Open File");
      begin
         if Filename = "" then
            return;
         end if;

         Open_Or_Create (Glide_Window (Object).Kernel, Filename);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_File;

   ---------------------
   -- On_Open_Project --
   ---------------------

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      File_Selector : File_Selector_Window_Access;
   begin
      Gtk_New (File_Selector, "/", Get_Current_Dir, -"Open Project");
      Register_Filter (File_Selector, Prj_File_Filter);

      declare
         Filename : constant String := Select_File (File_Selector);
      begin
         if Filename /= "" then
            Load_Project (Glide_Window (Object).Kernel, Filename);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Project;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      New_Editor (Glide_Window (Object).Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_File;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Success : Boolean;
   begin
      Save_To_File (Glide_Window (Object).Kernel, Success => Success);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Success : Boolean;
   begin
      declare
         Name : constant String := Select_File (-"Save File As");
      begin
         Save_To_File (Glide_Window (Object).Kernel, Name, Success);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_As;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Close;

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Button : constant Message_Dialog_Buttons :=
        Message_Dialog
          (Msg            => -"Are you sure you want to quit ?",
           Dialog_Type    => Confirmation,
           Buttons        => Button_Yes or Button_No,
           Default_Button => Button_No,
           Parent         => Gtk_Window (Object));
   begin
      if Button = Button_Yes then
         Main_Quit;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Exit;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
   begin
      Save_Desktop (Top.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_Desktop;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Undo;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Redo;

   ------------
   -- On_Cut --
   ------------

   procedure On_Cut
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      Cut_Clipboard (Glide_Window (Object).Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      Copy_Clipboard (Glide_Window (Object).Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      Paste_Clipboard (Glide_Window (Object).Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      Select_All (Glide_Window (Object).Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Select_All;

   -----------------
   -- On_New_View --
   -----------------

   procedure On_New_View
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant Glide_Window := Glide_Window (Object);
   begin
      New_View (Top.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_New_View;

   --------------------
   -- On_Preferences --
   --------------------

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Preferences;

   ---------------------------------
   -- On_Goto_Declaration_Or_Body --
   ---------------------------------

   procedure On_Goto_Declaration_Or_Body
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
   begin
      if Get_Editor_Filename (Top.Kernel) = "" then
         --  Nothing to do, since no saved editor has the focus
         return;
      end if;

      Goto_Declaration_Or_Body (Top.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Goto_Declaration_Or_Body;

   --------------
   -- On_Build --
   --------------

   procedure On_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top       : constant Glide_Window := Glide_Window (Object);
      Fd        : Process_Descriptor;
      Matched   : Match_Array (0 .. 0);
      Result    : Expect_Match;
      Args      : Argument_List_Access;
      Matcher   : constant Pattern_Matcher := Compile
        ("completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);
      Title     : constant String := Get_Editor_Filename (Top.Kernel);
      Project   : constant String := Get_Project_File_Name (Top.Kernel);
      Cmd       : constant String :=
        "gnatmake -P" & Project & " "
        & Scenario_Variables_Cmd_Line (Top.Kernel)
        & " " & Title;

   begin
      if Title = "" then
         return;
      end if;

      if Project = "" then
         --  This is the default internal project

         Args := Argument_String_To_List ("gnatmake -d " & Title);
         Console.Insert (Top.Kernel, "gnatmake " & Title, False);

      else
         Args := Argument_String_To_List (Cmd & " -d");
         Console.Insert (Top.Kernel, Cmd, False);
      end if;

      Top.Interrupted := False;
      Non_Blocking_Spawn
        (Fd, Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
         Err_To_Out  => True);

      loop
         Refresh;

         if Top.Interrupted then
            Interrupt (Fd);
            Console.Insert (Top.Kernel, "<^C>");
         end if;

         Expect (Fd, Result, ".+", Timeout => 50);

         declare
            S : constant String := Expect_Out (Fd);
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               Console.Insert (Top.Kernel, S, Add_LF => False);
            else
               Print_Message
                 (Top.Statusbar, GVD.Status_Bar.Help,
                  S (S'First + 1 .. S'Last));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Console.Insert (Top.Kernel, Expect_Out (Fd), Add_LF => False);
         --  ??? Check returned status.

         if Top.Interrupted then
            Top.Interrupted := False;
            Print_Message
              (Top.Statusbar, GVD.Status_Bar.Help,
               -"build interrupted.");
         else
            Print_Message
              (Top.Statusbar, GVD.Status_Bar.Help,
               -"build completed.");
         end if;

         Close (Fd);

      when E : others =>
         Close (Fd);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Build;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Arguments : constant String := Simple_Entry_Dialog
        (Parent  => Object,
         Title   => -"Arguments Selection",
         Message => -"Enter the arguments to your application:",
         Key     => "glide_run_arguments");

   begin
      if Arguments = ""
        or else Arguments (Arguments'First) /= ASCII.NUL
      then
         null;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Run;

   -------------------
   -- On_Stop_Build --
   -------------------

   procedure On_Stop_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant Glide_Window := Glide_Window (Object);
   begin
      Top.Interrupted := True;
   end On_Stop_Build;

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Debug_Executable
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
      end if;

      GVD.Menu.On_Open_Program (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Executable;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Step (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Step_Instruction (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Next (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Next_Instruction (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Finish (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Continue (Object, Action, Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Continue;

   ----------------------
   -- On_Generate_Body --
   ----------------------

   procedure On_Generate_Body
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top     : constant Glide_Window := Glide_Window (Object);
      Success : Boolean;
      Title   : constant String := Get_Editor_Filename (Top.Kernel);

      function Body_Name (Name : String) return String;
      --  Return the name of the body corresponding to a spec file.

      procedure Gnatstub (Name : String; Success : out Boolean);
      --  Launch gnatstub process and wait for it.

      function Body_Name (Name : String) return String is
      begin
         return Name (Name'First .. Name'Last - 1) & 'b';
      end Body_Name;

      procedure Gnatstub (Name : String; Success : out Boolean) is
         Args : Argument_List (1 .. 1);
         Exec : String_Access := Locate_Exec_On_Path ("gnatstub");

      begin
         if Exec = null then
            Success := False;
            return;
         end if;

         Args (1) := new String' (Name);
         Spawn (Exec.all, Args, Refresh'Access, Success);
         Free (Exec);
         Free (Args (1));
      end Gnatstub;

   begin
      if Title /= "" then
         Gnatstub (Title, Success);

         if Success then
            Open_File (Top.Kernel, Body_Name (Title), Success);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Generate_Body;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top    : constant Glide_Window := Glide_Window (Object);
      Vdiff  : Vdiff_Access;
      Result : Diff_Occurrence_Link;
      File1  : constant String := Select_File (-"Select First File");
      Child  : MDI_Child;

   begin
      if File1 = "" then
         return;
      end if;

      declare
         File2 : constant String := Select_File (-"Select Second File");

      begin
         if File2 = "" then
            return;
         end if;

         Result := Diff (File1, File2);
         Gtk_New (Vdiff);
         Set_Text (Vdiff.File_Label1, File1);
         Set_Text (Vdiff.File_Label2, File2);
         Fill_Diff_Lists (Vdiff.Clist1, Vdiff.Clist2, File1, File2, Result);
         Show_All (Vdiff.Main_Box);
         Child := Put (Get_MDI (Top.Kernel), Vdiff);

         --  ??? Connect to destroy signal so that we can free result:
         --  Free (Result);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   ---------------
   -- On_Manual --
   ---------------

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant Glide_Window := Glide_Window (Object);
   begin
      --  ??? Should use Top.Prefix
      Display_Help
        (Top.Kernel, "/opt/gtk-1.3/doc/GtkAda/gtkada_rm/gtkada_rm_toc.html");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Manual;

   --------------------
   -- On_About_Glide --
   --------------------

   procedure On_About_Glide
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        (-"Glide II" & ASCII.LF & ASCII.LF & "(c) 2001 ACT-Europe",
         Help_Msg =>
           (-"This is the About information box.") & ASCII.LF & ASCII.LF &
           (-"Click on the OK button to close this window."),
         Title => -"About...");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_About_Glide;

   ----------------------
   -- Glide_Menu_Items --
   ----------------------

   function Glide_Menu_Items return Gtk_Item_Factory_Entry_Access is
      File        : constant String := "/_" & (-"File")     & '/';
      Edit        : constant String := "/_" & (-"Edit")     & '/';
      Gotom       : constant String := "/_" & (-"Navigate") & '/';
      Project     : constant String := "/_" & (-"Project")  & '/';
      Build       : constant String := "/_" & (-"Build")    & '/';
      Debug_Sub   : constant String := (-"Debug")          & '/';
      Debug       : constant String := "/_" & Debug_Sub;
      Data_Sub    : constant String := (-"Data")           & '/';
      Session_Sub : constant String := (-"Session")        & '/';
      Tools       : constant String := "/_" & (-"Tools")    & '/';
      Compare_Sub : constant String :=        (-"Compare")  & '/';
      Window      : constant String := "/_" & (-"Window");
      Help        : constant String := "/_" & (-"Help")     & '/';

   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (File & (-"New"), "", Stock_New, On_New_File'Access),
         Gtk_New (File & (-"New View"), "", On_New_View'Access),
         Gtk_New (File & "sep1", Item_Type => Separator),
         Gtk_New (File & (-"Open..."), "F3", Stock_Open, On_Open_File'Access),
         Gtk_New (File & (-"Reopen"), Item_Type => Branch),
         Gtk_New (File & "sep2", Item_Type => Separator),
         Gtk_New (File & (-"Save"), "", Stock_Save, On_Save'Access),
         Gtk_New (File & (-"Save As..."), "",
                  Stock_Save_As, On_Save_As'Access),
         Gtk_New (File & (-"Close"), "", Stock_Close, On_Close'Access),
         Gtk_New (File & (-"Close All"), "", null),
         Gtk_New (File & (-"Save Desktop"), "", On_Save_Desktop'Access),
         Gtk_New (File & "sep3", Item_Type => Separator),
         Gtk_New (File & (-"Print"), "", Stock_Print, null),
         Gtk_New (File & "sep4", Item_Type => Separator),
         Gtk_New (File & (-"Exit"), "<control>Q",
                  Stock_Quit, On_Exit'Access),

         Gtk_New (Edit & (-"Undo"), "", Stock_Undo, On_Undo'Access),
         Gtk_New (Edit & (-"Redo"), "", Stock_Redo, On_Redo'Access),
         Gtk_New (Edit & "sep1", Item_Type => Separator),
         Gtk_New (Edit & (-"Cut"), "<shift>DEL", Stock_Cut, On_Cut'Access),
         Gtk_New
           (Edit & (-"Copy"), "<control>INS", Stock_Copy, On_Copy'Access),
         Gtk_New
           (Edit & (-"Paste"), "<shift>INS", Stock_Paste, On_Paste'Access),
         Gtk_New
           (Edit & (-"Select All"), "<control>A", On_Select_All'Access),
         Gtk_New (Edit & "sep2", Item_Type => Separator),
         Gtk_New (Edit & (-"Preferences"), "",
                  Stock_Preferences, On_Preferences'Access),

         Gtk_New (Gotom & (-"Goto Line..."), "", Stock_Jump_To, null),
         Gtk_New (Gotom & (-"Goto Declaration<->Body"), "", Stock_Home,
                  On_Goto_Declaration_Or_Body'Access),
         Gtk_New (Gotom & (-"Goto Body"), "", "", null),
         Gtk_New (Gotom & (-"Goto File Spec<->Body"), "", Stock_Convert, null),
         Gtk_New (Gotom & (-"Goto Previous Reference"), "", Stock_Undo, null),
         Gtk_New (Gotom & (-"Goto Parent Unit"), "", Stock_Go_Up, null),
         Gtk_New (Gotom & (-"List References"), "", Stock_Index, null),
         Gtk_New (Gotom & "sep1", Item_Type => Separator),
         Gtk_New (Gotom & (-"Start Of Statement"), "", Stock_Go_Up, null),
         Gtk_New (Gotom & (-"End Of Statement"), "", Stock_Go_Down, null),
         Gtk_New (Gotom & (-"Next Procedure"), "", Stock_Go_Forward, null),
         Gtk_New (Gotom & (-"Previous Procedure"), "", Stock_Go_Back, null),

         Gtk_New (Project & (-"Open..."), "", Stock_Open,
                  On_Open_Project'Access),
         Gtk_New (Project & "sep1", Item_Type => Separator),
         Gtk_New (Project & (-"Generate API doc"), "", Stock_Execute, null),
         Gtk_New (Project & "sep2", Item_Type => Separator),
         Gtk_New (Project & (-"Task Manager"), "", null),

         Gtk_New (Build & (-"Check File"), "", null),
         Gtk_New (Build & (-"Compile File"), "", Stock_Convert, null),
         Gtk_New (Build & (-"Make"), "", Stock_Refresh, On_Build'Access),
         Gtk_New (Build & (-"Build Library"), "", null),
         Gtk_New (Build & "sep1", Item_Type => Separator),
         Gtk_New (Build & (-"Execute..."), "", Stock_Execute, On_Run'Access),
         Gtk_New (Build & "sep2", Item_Type => Separator),
         Gtk_New
           (Build & (-"Stop Build"), "", Stock_Stop, On_Stop_Build'Access),

         Gtk_New (Debug & (-"Start"), "", On_Run'Access),
         Gtk_New (Debug & Debug_Sub & (-"Another Executable..."), "",
                  On_Debug_Executable'Access),
         Gtk_New (Debug & Debug_Sub & (-"Running Process..."), "", null),
         Gtk_New (Debug & Debug_Sub & (-"Core File..."), "", null),
         Gtk_New (Debug & Session_Sub & (-"Open..."), "", Stock_Open, null),
         Gtk_New (Debug & Session_Sub & (-"Save As..."), "",
                  Stock_Save_As, null),
         Gtk_New (Debug & Session_Sub & (-"Command History"), "",
                  Stock_Index, null),
         Gtk_New (Debug & Data_Sub & (-"Call Stack"), "", null, Check_Item),
         Gtk_New (Debug & Data_Sub & (-"Threads"), "", null),
         Gtk_New (Debug & Data_Sub & (-"Tasks"), "", null),
         Gtk_New (Debug & Data_Sub & "sep1", Item_Type => Separator),
         Gtk_New (Debug & Data_Sub & (-"Edit Breakpoints"), "", null),
         Gtk_New (Debug & Data_Sub & (-"Examine Memory"), "", null),
         Gtk_New (Debug & Data_Sub & "sep2", Item_Type => Separator),
         Gtk_New (Debug & Data_Sub & (-"Display Local Variables"),
                  "<alt>L", null),
         Gtk_New (Debug & Data_Sub & (-"Display Arguments"), "<alt>U", null),
         Gtk_New (Debug & Data_Sub & (-"Display Registers"), "", null),
         Gtk_New (Debug & Data_Sub & (-"Display Any Expression..."), "", null),
         Gtk_New (Debug & Data_Sub & "sep3", Item_Type => Separator),
         Gtk_New (Debug & Data_Sub & (-"Refresh"), "<control>L",
                  Stock_Refresh, null),
         Gtk_New (Debug & Data_Sub & (-"Show"), "", null),
         Gtk_New (Debug & "sep2", Item_Type => Separator),
         Gtk_New (Debug & (-"Step"), "F5", On_Step'Access),
         Gtk_New (Debug & (-"Step Instruction"), "<shift>F5",
                  On_Step_Instruction'Access),
         Gtk_New (Debug & (-"Next"), "F6", On_Next'Access),
         Gtk_New (Debug & (-"Next Instruction"), "<shift>F6",
                  On_Next_Instruction'Access),
         Gtk_New (Debug & (-"Finish"), "F7", On_Finish'Access),
         Gtk_New (Debug & (-"Continue"), "F8", On_Continue'Access),
         Gtk_New (Debug & (-"Interrupt"), "ESC", Stock_Stop, null),
         Gtk_New (Debug & (-"Detach Process"), "", null),

         Gtk_New (Tools & (-"Pretty Print"), "", null),
         Gtk_New (Tools & (-"Generate Body"), "", On_Generate_Body'Access),
         Gtk_New (Tools & (-"Call Graph"), "", null),
         Gtk_New (Tools & (-"Code Fixing"), "", null),
         Gtk_New (Tools & (-"Profile"), "", null),
         Gtk_New (Tools & (-"Memory Analyzer"), "", null),
         Gtk_New (Tools & Compare_Sub & (-"Two Files..."), "",
                  On_Compare_Two_Files'Access),
         Gtk_New (Tools & Compare_Sub & (-"Three Files..."), "", null),

         Gtk_New (Window),

         Gtk_New (Help & (-"Using the GNU Visual Debugger"), "",
                  On_Manual'Access),
         Gtk_New (Help & (-"GNAT User's Guide"), "", null),
         Gtk_New (Help & (-"GNAT Reference Manual"), "", null),
         Gtk_New (Help & (-"Ada 95 Reference Manual"), "", null),
         Gtk_New (Help & (-"Using the GNU Debugger"), "", null),
         Gtk_New (Help & (-"Using GCC"), "", null),
         Gtk_New (Help & (-"About Glide"), "", On_About_Glide'Access));
   end Glide_Menu_Items;

end Glide_Menu;
