-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                             ACT-Europe                            --
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

with Glib;                    use Glib;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Stock;               use Gtk.Stock;
with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;   use Gtkada.File_Selection;

with Creation_Wizard;         use Creation_Wizard;
with Glide_Intl;              use Glide_Intl;

with GVD.Menu;                use GVD.Menu;
with GVD.Status_Bar;          use GVD.Status_Bar;
with GVD.Process;             use GVD.Process;
with GVD.Types;               use GVD.Types;
with Debugger;                use Debugger;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Editor;     use Glide_Kernel.Editor;
with Glide_Kernel.Project;    use Glide_Kernel.Project;

with Glide_Main_Window;       use Glide_Main_Window;
with Glide_Page;              use Glide_Page;

with Hyper_Grep;              use Hyper_Grep;
with Vdiff_Pkg;               use Vdiff_Pkg;
with Vdiff_Utils;             use Vdiff_Utils;
with Diff_Utils;              use Diff_Utils;

with Prj_Editor_Window;       use Prj_Editor_Window;

with GVD.Dialogs;             use GVD.Dialogs;

with GNAT.Expect;             use GNAT.Expect;
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Factory_Data;            use Factory_Data;

--  AUnit components
with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Make_Test_Window_Pkg;    use Make_Test_Window_Pkg;
with Make_Suite_Window_Pkg;   use Make_Suite_Window_Pkg;

package body Glide_Menu is

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

   procedure On_Search_Files
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Search in Files menu

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->Open menu

   procedure On_New_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->New menu

   procedure On_Edit_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->Edit menu

   procedure On_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Build->Build menu

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Build->Run menu

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

   procedure On_New_Test_Case
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Tools->Unit Testing->New Test Case menu

   procedure On_New_Test_Suite
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Tools->Unit Testing->New Test Suite menu

   procedure On_New_Test_Harness
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Tools->Unit Testing->New Test Harness menu

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

   ------------------
   -- On_Open_File --
   ------------------

   procedure On_Open_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Filename : constant String :=
        File_Selection_Dialog (Title => "Open File", Must_Exist => True);

   begin
      if Filename = "" then
         return;
      end if;

      Open_File (Glide_Window (Object).Kernel, Filename);
   end On_Open_File;

   ---------------------
   -- On_Open_Project --
   ---------------------

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Filename : constant String :=
        File_Selection_Dialog (Title => "Open Project", Must_Exist => True);

   begin
      if Filename /= "" then
         Load_Project (Glide_Window (Object).Kernel, Filename);
      end if;
   end On_Open_Project;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Wiz : Creation_Wizard.Prj_Wizard;
   begin
      Gtk_New (Wiz);
      Set_Current_Page (Wiz, 1);
      Show_All (Wiz);
      Main;
   end On_New_Project;

   -----------------
   -- On_New_File --
   -----------------

   procedure On_New_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      New_Editor (Glide_Window (Object).Kernel);
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
         Name : constant String :=
           File_Selection_Dialog (Title => "Save File As...");

      begin
         Save_To_File (Glide_Window (Object).Kernel, Name, Success);
      end;
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
          (Msg            => "Are you sure you want to quit ?",
           Dialog_Type    => Confirmation,
           Buttons        => Button_Yes or Button_No,
           Default_Button => Button_No);
   begin
      if Button = Button_Yes then
         Main_Quit;
      end if;
   end On_Exit;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
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
   end On_Preferences;

   ---------------------
   -- On_Search_Files --
   ---------------------

   procedure On_Search_Files
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Grep : Hyper_Grep_Access;
      --  Id   : Message_Id;

   begin
      Gtk_New (Grep, Object.all'Access);
      Show_All (Grep);
      Main;
      --  Id := Push (Top.Statusbar, 1, "end of search.");
      Print_Message (Top.Statusbar, Help, "end of search.");
   end On_Search_Files;

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
        (ASCII.SUB & "completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);
      Dead      : Boolean;
      Cmd       : constant String :=
        "gnatmake -P" & Get_Project_File_Name (Top.Kernel) & " ";
      Title     : constant String := Get_Focus_Title (Top.Kernel);

   begin
      if not Focus_Is_Editor (Top.Kernel) then
         return;
      end if;

      Args := Argument_String_To_List (Cmd & Title);
      Console.Insert (Top.Kernel, Cmd & Title & ASCII.LF, False);
      Non_Blocking_Spawn
        (Fd, Args (Args'First).all, Args (Args'First + 1 .. Args'Last),
         Err_To_Out  => True);

      loop
         while Gtk.Main.Events_Pending loop
            Dead := Main_Iteration;
         end loop;

         --  ??? if Top.Terminated then
         --     Interrupt (Fd);
         --     return;
         --  end if;

         Expect (Fd, Result, ".+", Timeout => 50);

         declare
            S : constant String := Expect_Out (Fd);
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               Console.Insert (Top.Kernel, S);
            else
               Print_Message (Top.Statusbar, Help, S (S'First + 1 .. S'Last));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Console.Insert (Top.Kernel, Expect_Out (Fd));
         Print_Message (Top.Statusbar, Help, -"completed.");
         Close (Fd);
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
   end On_Run;

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Edit_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant Glide_Window := Glide_Window (Object);
      Win  : Project_Editor;

   begin
      Gtk_New (Win, Top.Kernel);
      Show_All (Win);
   end On_Edit_Project;

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

      function Body_Name (Name : String) return String;

      procedure Gnatstub (Name : String; Success : out Boolean);

      function Body_Name (Name : String) return String is
      begin
         return Name (Name'First .. Name'Last - 1) & 'b';
      end Body_Name;

      procedure Gnatstub (Name : String; Success : out Boolean) is
         Args : Argument_List (1 .. 2);
         Exec : String_Access := Locate_Exec_On_Path ("gnatstub");

      begin
         Args (1) := new String' ("-f");
         Args (2) := new String' (Name);
         Spawn (Exec.all, Args, Success);
         Free (Exec);
         Free (Args (1));
         Free (Args (2));
      end Gnatstub;

   begin
      if Focus_Is_Editor (Top.Kernel) then
         declare
            Title : constant String := Get_Focus_Title (Top.Kernel);
         begin
            Gnatstub (Title, Success);

            if Success then
               Open_File (Top.Kernel, Body_Name (Title));
            end if;
         end;
      end if;
   end On_Generate_Body;

   ----------------------
   -- On_New_Test_Case --
   ----------------------

   procedure On_New_Test_Case
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Test_Window : Make_Test_Window_Access;
      Top              : constant Glide_Window := Glide_Window (Object);

   begin
      Gtk_New (Make_Test_Window);
      Show_All (Make_Test_Window);
      Gtk.Main.Main;

      if Make_Test_Window.Name /= null then
         declare
            File : constant String := Make_Test_Window.Name.all;
         begin
            Open_File (Top.Kernel, File & ".ads");
            Open_File (Top.Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Test_Window.Name);
      Destroy (Make_Test_Window);
   end On_New_Test_Case;

   -----------------------
   -- On_New_Test_Suite --
   -----------------------

   procedure On_New_Test_Suite
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Suite_Window : Make_Suite_Window_Access;
      Top               : constant Glide_Window := Glide_Window (Object);

   begin
      Gtk_New (Make_Suite_Window);
      Show_All (Make_Suite_Window);
      Gtk.Main.Main;

      if Make_Suite_Window.Name /= null then
         declare
            File : constant String := Make_Suite_Window.Name.all;
         begin
            Open_File (Top.Kernel, File & ".ads");
            Open_File (Top.Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Suite_Window.Name);
      Destroy (Make_Suite_Window);
   end On_New_Test_Suite;

   -------------------------
   -- On_New_Test_Harness --
   -------------------------

   procedure On_New_Test_Harness
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Harness_Window : Make_Harness_Window_Access;
      Top                 : constant Glide_Window := Glide_Window (Object);

   begin
      Gtk_New (Make_Harness_Window);
      Show_All (Make_Harness_Window);
      Gtk.Main.Main;

      if Make_Harness_Window.Procedure_Name /= null then
         declare
            File : constant String := Make_Harness_Window.Procedure_Name.all;
         begin
            Open_File (Top.Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Harness_Window.Procedure_Name);
      Destroy (Make_Harness_Window);
   end On_New_Test_Harness;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Vdiff  : Vdiff_Access;
      Result : Diff_Occurrence_Link;
      File1  : constant String := File_Selection_Dialog ("Select First File");
      File2  : constant String := File_Selection_Dialog ("Select Second File");

   begin
      Result := Diff (File1, File2);
      Gtk_New (Vdiff);
      Set_Text (Vdiff.File_Label1, File1);
      Set_Text (Vdiff.File_Label2, File2);
      Fill_Diff_Lists (Vdiff.Clist1, Vdiff.Clist2, File1, File2, Result);
      Show_All (Vdiff);
      --  ??? Free (Result);
   end On_Compare_Two_Files;

   ---------------
   -- On_Manual --
   ---------------

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
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
   end On_About_Glide;

   ----------------------
   -- Glide_Menu_Items --
   ----------------------

   function Glide_Menu_Items return Gtk_Item_Factory_Entry_Access is
   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (-"/_File", Item_Type => Branch),
         Gtk_New (-"/_File/New", "", Stock_New, On_New_File'Access),
         Gtk_New (-"/_File/New View", "", On_New_View'Access),
         Gtk_New (-"/_File/sep1", Item_Type => Separator),
         Gtk_New (-"/_File/Open...", "F3", Stock_Open, On_Open_File'Access),
         Gtk_New (-"/_File/Reopen", Item_Type => Branch),
         Gtk_New (-"/_File/sep2", Item_Type => Separator),
         Gtk_New (-"/_File/Save", "", Stock_Save, On_Save'Access),
         Gtk_New (-"/_File/Save As...", "", Stock_Save_As, On_Save_As'Access),
         Gtk_New (-"/_File/Close", "", Stock_Close, On_Close'Access),
         Gtk_New (-"/_File/Close All", "", null),
         Gtk_New (-"/_File/sep3", Item_Type => Separator),
         Gtk_New (-"/_File/Print", "", Stock_Print, null),
         Gtk_New (-"/_File/sep4", Item_Type => Separator),
         Gtk_New (-"/_File/Exit", "<control>Q", Stock_Quit, On_Exit'Access),

         Gtk_New (-"/_Edit", Item_Type => Branch),
         Gtk_New (-"/_Edit/Undo", "", Stock_Undo, On_Undo'Access),
         Gtk_New (-"/_Edit/Redo", "", Stock_Redo, On_Redo'Access),
         Gtk_New (-"/_Edit/sep1", Item_Type => Separator),
         Gtk_New (-"/_Edit/Cut", "<shift>DEL", Stock_Cut, On_Cut'Access),
         Gtk_New (-"/_Edit/Copy", "<control>INS", Stock_Copy, On_Copy'Access),
         Gtk_New (-"/_Edit/Paste", "<shift>INS", Stock_Paste, On_Paste'Access),
         Gtk_New (-"/_Edit/Select All", "<control>A", On_Select_All'Access),
         Gtk_New (-"/_Edit/sep2", Item_Type => Separator),
         Gtk_New (-"/_Edit/Preferences...", "",
                  Stock_Preferences, On_Preferences'Access),

         Gtk_New (-"/_Search", Item_Type => Branch),
         Gtk_New (-"/_Search/Search...", "", Stock_Find, null),
         Gtk_New (-"/_Search/Search Next", "", Stock_Go_Forward, null),
         Gtk_New (-"/_Search/Search Previous", "", Stock_Go_Back, null),
         Gtk_New (-"/_Search/Search And Replace...", "",
                  Stock_Find_And_Replace, null),
         Gtk_New (-"/_Search/sep1", Item_Type => Separator),
         Gtk_New (-"/_Search/Goto Line...", "", Stock_Jump_To, null),
         Gtk_New (-"/_Search/Goto Declaration<->Body", "", Stock_Home, null),
         Gtk_New (-"/_Search/Goto File Spec<->Body", "", Stock_Convert, null),
         Gtk_New (-"/_Search/Goto Previous Reference", "", Stock_Undo, null),
         Gtk_New (-"/_Search/Goto Parent Unit", "", Stock_Go_Up, null),
         Gtk_New (-"/_Search/List References", "", Stock_Index, null),
         Gtk_New (-"/_Search/Syntax", Item_Type => Branch),
         Gtk_New (-"/_Search/Syntax/Start Of Statement", "",
                  Stock_Go_Up, null),
         Gtk_New (-"/_Search/Syntax/End Of Statement", "",
                  Stock_Go_Down, null),
         Gtk_New (-"/_Search/Syntax/Next Procedure", "",
                  Stock_Go_Forward, null),
         Gtk_New (-"/_Search/Syntax/Previous Procedure", "",
                  Stock_Go_Back, null),
         Gtk_New (-"/_Search/sep2", Item_Type => Separator),
         Gtk_New (-"/_Search/Search in Files...", "", Stock_Find,
                  On_Search_Files'Access),

         Gtk_New (-"/_VCS", Item_Type => Branch),
         Gtk_New (-"/_VCS/Check In", "", Stock_Goto_Last, null),
         Gtk_New (-"/_VCS/Check Out", "", Stock_Goto_First, null),
         Gtk_New (-"/_VCS/Edit Revision History", "", Stock_New, null),
         Gtk_New (-"/_VCS/Revert to Last Version", "",
                  Stock_Revert_To_Saved, null),
         Gtk_New (-"/_VCS/Undo Last Check-In", "", Stock_Undelete, null),
         Gtk_New (-"/_VCS/sep1", Item_Type => Separator),
         Gtk_New (-"/_VCS/Compare with Last Version", "",
                  Stock_Sort_Ascending, null),
         Gtk_New (-"/_VCS/Annotate", "", Stock_Preferences, null),

         Gtk_New (-"/_Project", Item_Type => Branch),
         Gtk_New (-"/_Project/New...", "", Stock_New, On_New_Project'Access),
         Gtk_New (-"/_Project/Open...", "", Stock_Open,
                  On_Open_Project'Access),
         Gtk_New (-"/_Project/Edit...", "",
                  Stock_Properties, On_Edit_Project'Access),
         Gtk_New (-"/_Project/sep1", Item_Type => Separator),
         Gtk_New (-"/_Project/Generate API doc", "", Stock_Execute, null),
         Gtk_New (-"/_Project/sep2", Item_Type => Separator),
         Gtk_New (-"/_Project/Task Manager", "", null),

         Gtk_New (-"/_Build", Item_Type => Branch),
         Gtk_New (-"/_Build/Check File", "", null),
         Gtk_New (-"/_Build/Compile File", "", Stock_Convert, null),
         Gtk_New (-"/_Build/Make", "", Stock_Refresh, On_Build'Access),
         Gtk_New (-"/_Build/Build Library", "", null),
         Gtk_New (-"/_Build/sep1", Item_Type => Separator),
         Gtk_New (-"/_Build/Execute...", "", Stock_Execute, On_Run'Access),
         Gtk_New (-"/_Build/sep2", Item_Type => Separator),
         Gtk_New (-"/_Build/Stop Build", "", Stock_Stop, null),

         Gtk_New (-"/_Debug", Item_Type => Branch),
         Gtk_New (-"/_Debug/Start", "", On_Run'Access),
         Gtk_New (-"/_Debug/Debug", Item_Type => Branch),
         Gtk_New (-"/_Debug/Debug/Another Executable...", "",
                  On_Debug_Executable'Access),
         Gtk_New (-"/_Debug/Debug/Running Process...", "", null),
         Gtk_New (-"/_Debug/Debug/Core File...", "", null),
         Gtk_New (-"/_Debug/Session", Item_Type => Branch),
         Gtk_New (-"/_Debug/Session/Open...", "", Stock_Open, null),
         Gtk_New (-"/_Debug/Session/Save As...", "", Stock_Save_As, null),
         Gtk_New (-"/_Debug/Session/Command History...", "",
                  Stock_Index, null),
         Gtk_New (-"/_Debug/Data", Item_Type => Branch),
         Gtk_New (-"/_Debug/Data/Call Stack", "", null, Check_Item),
         Gtk_New (-"/_Debug/Data/Threads", "", null),
         Gtk_New (-"/_Debug/Data/Tasks", "", null),
         Gtk_New (-"/_Debug/Data/sep1", Item_Type => Separator),
         Gtk_New (-"/_Debug/Data/Edit Breakpoints...", "", null),
         Gtk_New (-"/_Debug/Data/Examine Memory...", "", null),
         Gtk_New (-"/_Debug/Data/sep2", Item_Type => Separator),
         Gtk_New (-"/_Debug/Data/Display Local Variables", "<alt>L", null),
         Gtk_New (-"/_Debug/Data/Display Arguments", "<alt>U",
                  null),
         Gtk_New (-"/_Debug/Data/Display Registers", "", null),
         Gtk_New (-"/_Debug/Data/Display Any Expression...", "", null),
         Gtk_New (-"/_Debug/Data/sep3", Item_Type => Separator),
         Gtk_New (-"/_Debug/Data/Refresh", "<control>L", Stock_Refresh, null),
         Gtk_New (-"/_Debug/Data/Show", "", null),
         Gtk_New (-"/_Debug/sep2", Item_Type => Separator),
         Gtk_New (-"/_Debug/Step", "F5", On_Step'Access),
         Gtk_New (-"/_Debug/Step Instruction", "<shift>F5",
                  On_Step_Instruction'Access),
         Gtk_New (-"/_Debug/Next", "F6", On_Next'Access),
         Gtk_New (-"/_Debug/Next Instruction", "<shift>F6",
                  On_Next_Instruction'Access),
         Gtk_New (-"/_Debug/Finish", "F7", On_Finish'Access),
         Gtk_New (-"/_Debug/Continue", "F8", On_Continue'Access),
         Gtk_New (-"/_Debug/Interrupt", "ESC", Stock_Stop, null),
         Gtk_New (-"/_Debug/Detach Process", "", null),

         Gtk_New (-"/_Tools", Item_Type => Branch),
         Gtk_New (-"/_Tools/Pretty Print", "", null),
         Gtk_New (-"/_Tools/Generate Body", "", On_Generate_Body'Access),
         Gtk_New (-"/_Tools/Class Browser...", "", null),
         Gtk_New (-"/_Tools/Dependency Browser...", "", null),
         Gtk_New (-"/_Tools/Call Graph...", "", null),
         Gtk_New (-"/_Tools/Metrics...", "", null),
         Gtk_New (-"/_Tools/Code Fixing...", "", null),
         Gtk_New (-"/_Tools/Profile", "", null),
         Gtk_New (-"/_Tools/Memory Analyzer", "", null),
         Gtk_New (-"/_Tools/Unit Testing", Item_Type => Branch),
         Gtk_New (-"/_Tools/Unit Testing/New Test Case", "",
                  On_New_Test_Case'Access),
         Gtk_New (-"/_Tools/Unit Testing/Add Routine", "", null),
         Gtk_New (-"/_Tools/Unit Testing/New Test Suite", "",
                  On_New_Test_Suite'Access),
         Gtk_New (-"/_Tools/Unit Testing/New Test Harness", "",
                  On_New_Test_Harness'Access),
         Gtk_New (-"/_Tools/Compare", Item_Type => Branch),
         Gtk_New (-"/_Tools/Compare/Two Files...", "",
                  On_Compare_Two_Files'Access),
         Gtk_New (-"/_Tools/Compare/Three Files...", "", null),

         Gtk_New (-"/_Window"),

         Gtk_New (-"/_Help", Item_Type => Branch),
         Gtk_New (-"/_Help/Glide Manual...", "F1",
                  Stock_Help, On_Manual'Access),
         Gtk_New (-"/_Help/About Glide...", "", On_About_Glide'Access));
   end Glide_Menu_Items;

end Glide_Menu;
