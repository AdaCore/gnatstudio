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

with Glib;                  use Glib;
with Gdk.Color;             use Gdk.Color;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;              use Gtk.Main;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Status_Bar;        use Gtk.Status_Bar;
with Gtk.Text;              use Gtk.Text;
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtkada.File_Selection; use Gtkada.File_Selection;

with Creation_Wizard;       use Creation_Wizard;
with Glide_Intl;            use Glide_Intl;

--  with Glide_Pkg;             use Glide_Pkg;
with GVD.Main_Window;       use GVD.Main_Window;
with GVD.Status_Bar;        use GVD.Status_Bar;
with GVD.Process;           use GVD.Process;
with Glide_Page;

with Hyper_Grep;            use Hyper_Grep;
with Vdiff_Pkg;             use Vdiff_Pkg;
with Vdiff_Utils;           use Vdiff_Utils;
with Diff_Utils;            use Diff_Utils;

with GVD.Dialogs;           use GVD.Dialogs;

with GNAT.Expect;           use GNAT.Expect;
with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with Factory_Data;          use Factory_Data;

--  AUnit components
with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Make_Test_Window_Pkg; use Make_Test_Window_Pkg;
with Make_Suite_Window_Pkg; use Make_Suite_Window_Pkg;

package body Glide_Menu is

   Highlight_File : constant String := "#FF0000000000";

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Open_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Open File menu

   procedure On_New_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->New Project menu

   procedure On_New_File
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->New File menu

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

   procedure On_Build
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->Build menu

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Run->Run menu

   procedure On_Debug
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Run->Debug menu

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
      Widget : Limited_Widget) is
   begin
      null;
   end On_Open_File;

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
      Set_Page (Wiz, 1);
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
      null;
   end On_New_File;

   -------------
   -- On_Save --
   -------------

   procedure On_Save
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
   end On_Save;

   ----------------
   -- On_Save_As --
   ----------------

   procedure On_Save_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
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
      Widget : Limited_Widget) is
   begin
      Main_Quit;
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
      null;
   end On_Cut;

   -------------
   -- On_Copy --
   -------------

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
   end On_Copy;

   --------------
   -- On_Paste --
   --------------

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
   end On_Paste;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
   end On_Select_All;

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
      Top  : constant GVD_Main_Window := GVD_Main_Window (Object);
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
      Top       : constant GVD_Main_Window := GVD_Main_Window (Object);
      --  Top       : constant Glide_Access := Glide_Access (Object);

      Cmd       : constant String := "./build_gvd";  --  ???
      Fd        : Process_Descriptor;
      Matched   : Match_Array (0 .. 3);
      Result    : Expect_Match;
      Args      : constant Argument_List_Access :=
        Argument_String_To_List (Cmd);
      Matcher   : constant Pattern_Matcher := Compile
        (ASCII.SUB & "completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.$",
         Multiple_Lines);
      File      : constant Pattern_Matcher :=
        Compile ("([^:]*):(\d+):(\d+:)?");
      Dead      : Boolean;
      --  Id        : Message_Id;
      Last      : Natural;
      Highlight : Gdk_Color;
      Console   : constant Gtk_Text :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Console;
      --  Top.Console;

   begin
      Highlight := Parse (Highlight_File);
      Alloc (Get_Default_Colormap, Highlight);
      Insert (Console, Chars => Cmd & ASCII.LF);
      Non_Blocking_Spawn
        (Fd, Args (Args'First).all, Args.all,
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
               Match (File, S, Matched);

               if Matched (0) /= No_Match then
                  Insert
                    (Console,
                     Chars => S (S'First .. Matched (1).First - 1));

                  if Matched (3) = No_Match then
                     Last := Matched (2).Last;
                  else
                     Last := Matched (3).Last - 1;
                  end if;

                  Insert
                    (Console,
                     Fore => Highlight,
                     Chars => S (Matched (1).First .. Last));
                  Insert (Console, Chars => S (Last + 1 .. S'Last));

               else
                  Insert (Console, Chars => S);
               end if;
            else
               --  Id := Push (Top.Statusbar, 1, S (S'First + 1 .. S'Last));
               Print_Message (Top.Statusbar, Help, S (S'First + 1 .. S'Last));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Insert (Console, Chars => Expect_Out (Fd));
         --  Id := Push (Top.Statusbar, 1, "completed.");
         Print_Message (Top.Statusbar, Help, "completed.");
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

   --------------
   -- On_Debug --
   --------------

   procedure On_Debug
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      --  GVD.Menu.On_Run (null, Action, Widget);
      null;
   end On_Debug;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_New_Test_Case
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Test_Window : Make_Test_Window_Access;
   begin
      Gtk_New (Make_Test_Window);
      Show_All (Make_Test_Window);
      Gtk.Main.Main;
   end On_New_Test_Case;

   procedure On_New_Test_Suite
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Suite_Window : Make_Suite_Window_Access;
   begin
      Gtk_New (Make_Suite_Window);
      Show_All (Make_Suite_Window);
      Gtk.Main.Main;
   end On_New_Test_Suite;

   procedure On_New_Test_Harness
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Make_Harness_Window : Make_Harness_Window_Access;
   begin
      Gtk_New (Make_Harness_Window);
      Show_All (Make_Harness_Window);
      Gtk.Main.Main;
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
         Gtk_New (-"/_File/New", "", On_New_File'Access),
         Gtk_New (-"/_File/New Project...", "", On_New_Project'Access),
         Gtk_New (-"/_File/sep1", Item_Type => Separator),
         Gtk_New (-"/_File/Open...", "F3", On_Open_File'Access),
         Gtk_New (-"/_File/Open From Path...", "", On_Open_File'Access),
         Gtk_New (-"/_File/Open Project...", "F3", null),
         Gtk_New (-"/_File/Reopen", Item_Type => Branch),
         Gtk_New (-"/_File/sep2", Item_Type => Separator),
         Gtk_New (-"/_File/Save", "", On_Save'Access),
         Gtk_New (-"/_File/Save As...", "", On_Save_As'Access),
         Gtk_New (-"/_File/Close", "", On_Close'Access),
         Gtk_New (-"/_File/Close All", "", null),
         Gtk_New (-"/_File/sep3", Item_Type => Separator),
         Gtk_New (-"/_File/Print", "", null),
         Gtk_New (-"/_File/sep4", Item_Type => Separator),
         Gtk_New (-"/_File/Exit", "<control>Q", On_Exit'Access),

         Gtk_New (-"/_Edit", Item_Type => Branch),
         Gtk_New (-"/_Edit/Undo", "", On_Undo'Access),
         Gtk_New (-"/_Edit/Redo", "", On_Redo'Access),
         Gtk_New (-"/_Edit/sep1", Item_Type => Separator),
         Gtk_New (-"/_Edit/Cut", "<shift>DEL", On_Cut'Access),
         Gtk_New (-"/_Edit/Copy", "<control>INS", On_Copy'Access),
         Gtk_New (-"/_Edit/Paste", "<shift>INS", On_Paste'Access),
         Gtk_New (-"/_Edit/Select All", "<control>A", On_Select_All'Access),
         Gtk_New (-"/_Edit/sep2", Item_Type => Separator),
         Gtk_New (-"/_Edit/Preferences...", "", On_Preferences'Access),

         Gtk_New (-"/_Search", Item_Type => Branch),
         Gtk_New (-"/_Search/Search...", "", null),
         Gtk_New (-"/_Search/Search Next", "", null),
         Gtk_New (-"/_Search/Search Previous", "", null),
         Gtk_New (-"/_Search/Search And Replace...", "", null),
         Gtk_New (-"/_Search/sep1", Item_Type => Separator),
         Gtk_New (-"/_Search/Goto Line...", "", null),
         Gtk_New (-"/_Search/Goto Declaration<->Body", "", null),
         Gtk_New (-"/_Search/Goto File Spec<->Body", "", null),
         Gtk_New (-"/_Search/Goto Previous Reference", "", null),
         Gtk_New (-"/_Search/Goto Parent Unit", "", null),
         Gtk_New (-"/_Search/List References", "", null),
         Gtk_New (-"/_Search/Syntax", Item_Type => Branch),
         Gtk_New (-"/_Search/Syntax/Start Of Statement", "", null),
         Gtk_New (-"/_Search/Syntax/End Of Statement", "", null),
         Gtk_New (-"/_Search/Syntax/Next Procedure", "", null),
         Gtk_New (-"/_Search/Syntax/Previous Procedure", "", null),
         Gtk_New (-"/_Search/sep2", Item_Type => Separator),
         Gtk_New (-"/_Search/Search in Files...", "",
                  On_Search_Files'Access),

         Gtk_New (-"/_VCS", Item_Type => Branch),
         Gtk_New (-"/_VCS/Check In", "", null),
         Gtk_New (-"/_VCS/Check Out", "", null),
         Gtk_New (-"/_VCS/Edit Revision History", "", null),
         Gtk_New (-"/_VCS/Revert to Last Version", "", null),
         Gtk_New (-"/_VCS/Undo Last Check-In", "", null),
         Gtk_New (-"/_VCS/sep1", Item_Type => Separator),
         Gtk_New (-"/_VCS/Compare with Last Version", "", null),
         Gtk_New (-"/_VCS/Annotate", "", null),

         Gtk_New (-"/_Project", Item_Type => Branch),
         Gtk_New (-"/_Project/Edit Project...", "", null),
         Gtk_New (-"/_Project/Task Manager", "", null),
         Gtk_New (-"/_Project/sep1", Item_Type => Separator),
         Gtk_New (-"/_Project/Check File", "", null),
         Gtk_New (-"/_Project/Compile File", "", null),
         Gtk_New (-"/_Project/Build", "", On_Build'Access),
         Gtk_New (-"/_Project/Build Library", "", On_Build'Access),
         Gtk_New (-"/_Project/sep2", Item_Type => Separator),
         Gtk_New (-"/_Project/Run...", "", On_Run'Access),

         Gtk_New (-"/_Debug", Item_Type => Branch),
         Gtk_New (-"/_Debug/Start", "", On_Run'Access),
         Gtk_New (-"/_Debug/Debug", Item_Type => Branch),
         Gtk_New (-"/_Debug/Debug/Another Executable...", "", null),
         Gtk_New (-"/_Debug/Debug/Running Process...", "", null),
         Gtk_New (-"/_Debug/Debug/Core File...", "", null),
         Gtk_New (-"/_Debug/Session", Item_Type => Branch),
         Gtk_New (-"/_Debug/Session/Open...", "", null),
         Gtk_New (-"/_Debug/Session/Save As...", "", null),
         Gtk_New (-"/_Debug/Session/Command History...", "", null),
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
         Gtk_New (-"/_Debug/Data/Refresh", "<control>L", null),
         Gtk_New (-"/_Debug/Data/Show", "", null),
         Gtk_New (-"/_Debug/sep2", Item_Type => Separator),
         Gtk_New (-"/_Debug/Step", "F5", null),
         Gtk_New (-"/_Debug/Step Instruction", "<shift>F5",
                  null),
         Gtk_New (-"/_Debug/Next", "F6", null),
         Gtk_New (-"/_Debug/Next Instruction", "<shift>F6",
                  null),
         Gtk_New (-"/_Debug/Finish", "F7", null),
         Gtk_New (-"/_Debug/Continue", "F8", null),
         Gtk_New (-"/_Debug/Interrupt", "esc", null),
         Gtk_New (-"/_Debug/Detach Process", "", null),
         Gtk_New (-"/_Debug/sep3", Item_Type => Separator),
         Gtk_New (-"/_Debug/Profile", "", null),
         Gtk_New (-"/_Debug/Memory Analyzer", "", null),

         Gtk_New (-"/_Tools", Item_Type => Branch),
         Gtk_New (-"/_Tools/Pretty Print", "", null),
         Gtk_New (-"/_Tools/Generate Body", "", null),
         Gtk_New (-"/_Tools/Generate HTML...", "", null),
         Gtk_New (-"/_Tools/Class Browser...", "", null),
         Gtk_New (-"/_Tools/Dependency Browser...", "", null),
         Gtk_New (-"/_Tools/Call Graph...", "", null),
         Gtk_New (-"/_Tools/Metrics...", "", null),
         Gtk_New (-"/_Tools/Code Fixing...", "", null),
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
         Gtk_New (-"/_Help/Glide Manual...", "F1", On_Manual'Access),
         Gtk_New (-"/_Help/About Glide...", "", On_About_Glide'Access));
   end Glide_Menu_Items;

end Glide_Menu;
