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

with Gdk.Window;          use Gdk.Window;
with Gtk.Container;       use Gtk.Container;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Text;            use Gtk.Text;
with General_Preferences_Pkg; use General_Preferences_Pkg;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Odd_Intl;            use Odd_Intl;
with GVD;                 use GVD;
with GVD.Process;         use GVD.Process;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
pragma Warnings (Off);
with GNAT.Expect;         use GNAT.Expect;
pragma Warnings (On);
with Glib;                use Glib;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Breakpoints_Pkg;     use Breakpoints_Pkg;
with GVD.Process;         use GVD.Process;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Display_Items;       use Display_Items;
with Gtkada.Canvas;       use Gtkada.Canvas;
with GVD.Canvas;          use GVD.Canvas;
with GVD.Dialogs;         use GVD.Dialogs;
with Gtkada.Types;        use Gtkada.Types;
with GVD.Types;           use GVD.Types;
with GVD.Strings;         use GVD.Strings;
with GVD.Code_Editors;    use GVD.Code_Editors;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Window_Settings; use GVD.Window_Settings;
with GVD.Memory_View;     use GVD.Memory_View;
with Unchecked_Deallocation;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with List_Select_Pkg;     use List_Select_Pkg;

package body Main_Debug_Window_Pkg.Callbacks is

   use GVD;
   use Gtk.Arguments;

   procedure Free is new Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access);
   --  Close all the debuggers associated with a given main debug window
   --  by looking at all the pages of the main notebook.
   -----------------------
   -- Cleanup_Debuggers --
   -----------------------

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access) is
      Tab      : Debugger_Process_Tab;
      Page     : Gtk_Widget;

   begin
      --  First switch to the last page (to prevent automatic page
      --  switching when the other pages are deleted, which would fail)
      Set_Page (Top.Process_Notebook, -1);

      loop
         Page := Get_Nth_Page (Top.Process_Notebook, 0);
         exit when Page = null;

         Tab := Process_User_Data.Get (Page);
         Tab.Exiting := True;

         begin
            Close (Tab.Debugger);
         exception
            when others =>
               --  ??? Would be nice to handle more specific errors, but
               --  since we are exiting, ignore any exception instead of
               --  generating unfriendly bug boxes
               null;
         end;

         Remove_Page (Top.Process_Notebook, 0);
      end loop;

      Free (Top.Command_History);
   end Cleanup_Debuggers;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      --  Ref the object since we will destroy it in the main procedure.

      Ref (Object);
      Save_Window_Settings
        (Main_Debug_Window_Access (Object).Gvd_Home_Dir.all &
         Directory_Separator & "window_settings", Gtk_Widget (Object));
      Cleanup_Debuggers (Main_Debug_Window_Access (Object));
      Main_Quit;

      return False;
   end On_Main_Debug_Window_Delete_Event;

   -------------------------------
   -- On_Open_Program1_Activate --
   -------------------------------

   procedure On_Open_Program1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         S : constant String := To_Unix_Pathname (File_Selection_Dialog);
      begin
         if S = "" then
            return;
         end if;

         if Tab.Descriptor.Remote_Host'Length /= 0
           or else Is_Regular_File (S)
         then
            Set_Executable (Tab.Debugger, S, Mode => Hidden);
         else
            Print_Message
              (Main_Debug_Window_Access (Get_Toplevel (Object)).Statusbar1,
               Error,
               (-" Could not find file: ") & S);
         end if;

      exception
         when Executable_Not_Found =>
            Print_Message
              (Main_Debug_Window_Access (Get_Toplevel (Object)).Statusbar1,
               Error,
               (-" Could not find file: ") & S);
      end;
   end On_Open_Program1_Activate;

   --------------------------------
   -- On_Open_Debugger1_Activate --
   --------------------------------

   procedure On_Open_Debugger1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Program : Program_Descriptor;
      List    : Argument_List (1 .. 0);
      Process : Debugger_Process_Tab;
      Top     : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);

   begin
      Open_Program (Top.Open_Program, Program);

      if Program.Launch = New_Debugger then
         Process :=
           Create_Debugger
             (Main_Debug_Window_Access (Object),
              Program.Debugger,
              Program.Program.all,
              List, "",
              Program.Remote_Host.all,
              Program.Remote_Target.all,
              Program.Protocol.all,
              Program.Debugger_Name.all);
      end if;
   end On_Open_Debugger1_Activate;

   ---------------------------------
   -- On_Open_Core_Dump1_Activate --
   ---------------------------------

   procedure On_Open_Core_Dump1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         S : constant String :=
           To_Unix_Pathname (File_Selection_Dialog ("Select Core File"));
      begin
         if Tab.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File (Tab.Debugger, S, Mode => Hidden);
         else
            Print_Message
              (Main_Debug_Window_Access (Get_Toplevel (Object)).Statusbar1,
               Error, -(" Could not find core file: ") & S);
         end if;
      end;
   end On_Open_Core_Dump1_Activate;

   ------------------------------
   -- On_Edit_Source1_Activate --
   ------------------------------

   procedure On_Edit_Source1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      function Substitute
        (Name : String; File : String; Line : Natural) return String;
      --  Substitute %f and %l in Name by the file name and the line number.

      ----------------
      -- Substitute --
      ----------------

      function Substitute
        (Name : String; File : String; Line : Natural) return String
      is
         Index : Natural := Name'First;
      begin
         while Index < Name'Last loop
            if Name (Index) = '%' and then Name (Index + 1) = 'f' then
               return Name (Name'First .. Index - 1) &
                 File & Substitute (Name (Index + 2 .. Name'Last), File, Line);

            elsif Name (Index) = '%' and then Name (Index + 1) = 'l' then
               declare
                  Img : constant String := Natural'Image (Line);
               begin
                  return Name (Name'First .. Index - 1) &
                    Img (Img'First + 1 .. Img'Last) &
                    Substitute (Name (Index + 2 .. Name'Last), File, Line);
               end;
            end if;

            Index := Index + 1;
         end loop;

         return Name;
      end Substitute;

      Tab       : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);
      Host_File : constant String :=
        To_Host_Pathname (Get_Current_File (Tab.Editor_Text));
      Editor : constant String := Substitute
        (Main_Debug_Window_Access (Tab.Window).External_Editor.all,
         Host_File, Get_Line (Tab.Editor_Text));
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      Print_Message
        (Main_Debug_Window_Access (Tab.Window).Statusbar1, Help, Editor);

      Args := Argument_String_To_List (Editor);
      Prog := Locate_Exec_On_Path (Args (Args'First).all);

      if Prog /= null then
         Pid := GNAT.OS_Lib.Non_Blocking_Spawn
           (Prog.all, Args (Args'First + 1 .. Args'Last));
         Free (Prog);
      end if;

      if Args /= null then
         for J in Args'Range loop
            Free (Args (J));
         end loop;

         Free (Args);
      end if;
   end On_Edit_Source1_Activate;

   ------------------------------
   -- On_Open_Source1_Activate --
   ------------------------------

   procedure On_Open_Source1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      File_Name : constant String :=
        File_Selection_Dialog (Title => -"Source name", Must_Exist => True);
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      Load_File (Tab.Editor_Text, File_Name, Set_Current => False);
      Set_Line (Tab.Editor_Text, 1, Set_Current => False);
   end On_Open_Source1_Activate;

   -------------------------------
   -- On_Open_Session1_Activate --
   -------------------------------

   procedure On_Open_Session1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      Open_Session (Top, Top.Open_Session, Top.Sessions_Dir.all);
   end On_Open_Session1_Activate;

   ----------------------------------
   -- On_Save_Session_As1_Activate --
   ----------------------------------

   procedure On_Save_Session_As1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      Save_Session (Top, Top.Open_Session, Top.Sessions_Dir.all);
   end On_Save_Session_As1_Activate;

   ------------------------------------
   -- On_Attach_To_Process1_Activate --
   ------------------------------------

   procedure On_Attach_To_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Process_List  : List_Select_Access;
      Command_Index : Integer := Exec_Command'First;
      Args          : Argument_List_Access;
      P             : Process_Descriptor;
      Match         : Expect_Match := 0;

   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Gtk_New (Process_List, Title => -"Process Selection");
      Skip_To_Char (Exec_Command, Command_Index, ' ');
      Args := Argument_String_To_List
        (Exec_Command (Command_Index + 1 .. Exec_Command'Last));

      declare
         New_Args : Argument_List (Args'First .. Args'Last + 1);
      begin
         New_Args (Args'First .. Args'Last) := Args.all;
         New_Args (New_Args'Last) := new String' (Get_Pref (List_Processes));

         GNAT.Expect.Non_Blocking_Spawn
           (P,
            Exec_Command (Exec_Command'First .. Command_Index - 1),
            New_Args);
         Expect (P, Match, "\n");

         for J in New_Args'Range loop
            Free (New_Args (J));
         end loop;
      end;

      Free (Args);

      --  Skip the first line in the output.
      Expect (P, Match, "\n");

      while Match = 1 loop
         declare
            S     : constant String := Expect_Out (P);
            Index : Integer := S'First;
         begin
            Skip_Blanks (S, Index);
            Skip_To_Char (S, Index, ' ');
            Add_Item
              (Process_List, S (S'First .. Index), S (Index + 1 .. S'Last));
         end;

         Expect (P, Match, "\n");
      end loop;

      Close (P);

      declare
         Arguments : constant String := Show (Process_List);
      begin
         if Arguments /= "" then
            Attach_Process
              (Tab.Debugger, Arguments, Mode => GVD.Types.Visible);
         end if;
      end;
   end On_Attach_To_Process1_Activate;

   ---------------------------------
   -- On_Detach_Process1_Activate --
   ---------------------------------

   procedure On_Detach_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Detach_Process (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Detach_Process1_Activate;

   -----------------------------------
   -- On_Change_Directory1_Activate --
   -----------------------------------

   procedure On_Change_Directory1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         Dir : constant String := To_Unix_Pathname (File_Selection_Dialog
           (Title       => "Directory Selection",
            Dir_Only    => True,
            Must_Exist  => True));

      begin
         if Dir /= "" then
            Change_Directory (Tab.Debugger, Dir, Mode => GVD.Types.Visible);
         end if;
      end;
   end On_Change_Directory1_Activate;

   ------------------------
   -- On_Close1_Activate --
   ------------------------

   procedure On_Close1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Close_Debugger (Tab);
      end if;
   end On_Close1_Activate;

   -----------------------
   -- On_Exit1_Activate --
   -----------------------

   procedure On_Exit1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Save_Window_Settings (Main_Debug_Window_Access (Object).Gvd_Home_Dir.all
                            & Directory_Separator & "window_settings",
                            Gtk_Widget (Object));
      Cleanup_Debuggers (Main_Debug_Window_Access (Object));
      Main_Quit;
   end On_Exit1_Activate;

   -----------------------
   -- On_Undo1_Activate --
   -----------------------

   procedure On_Undo1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Undo1_Activate;

   -----------------------
   -- On_Redo1_Activate --
   -----------------------

   procedure On_Redo1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Redo1_Activate;

   ----------------------
   -- On_Cut1_Activate --
   ----------------------

   procedure On_Cut1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Cut1_Activate;

   -----------------------
   -- On_Copy1_Activate --
   -----------------------

   procedure On_Copy1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Copy1_Activate;

   ------------------------
   -- On_Paste1_Activate --
   ------------------------

   procedure On_Paste1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Paste1_Activate;

   -----------------------------
   -- On_Select_All1_Activate --
   -----------------------------

   procedure On_Select_All1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Select_All1_Activate;

   -------------------------
   -- On_Search1_Activate --
   -------------------------

   procedure On_Search1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Search1_Activate;

   ------------------------------
   -- On_Preferences1_Activate --
   ------------------------------

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      if Top.GVD_Preferences = null then
         Gtk_New (Top.GVD_Preferences, Top);
      end if;

      --  First do a show_all, so that Fill_Dialog can choose to
      --  hide or deactivate widgets.
      Show_All (Top.GVD_Preferences);
      GVD.Preferences.Fill_Dialog (Top.GVD_Preferences);
   end On_Preferences1_Activate;

   -------------------------------
   -- On_Gdb_Settings1_Activate --
   -------------------------------

   procedure On_Gdb_Settings1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Gdb_Settings1_Activate;

   ----------------------
   -- On_Run1_Activate --
   ----------------------

   procedure On_Run1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         Is_Start  : aliased Boolean;
         Arguments : constant String := Display_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Arguments Selection",
            Message => -"Enter the arguments to your application:",
            Key     => "gvd_run_arguments",
            Check_Msg => -"Stop at beginning of main subprogram",
            Button_Active => Is_Start'Access);
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            if Is_Start then
               Start (Tab.Debugger, Arguments, Mode => GVD.Types.Visible);
            else
               Run (Tab.Debugger, Arguments, Mode => GVD.Types.Visible);
            end if;
         end if;
      end;
   end On_Run1_Activate;

   -----------------------
   -- On_Step1_Activate --
   -----------------------

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step1_Activate;

   -----------------------------------
   -- On_Step_Instruction1_Activate --
   -----------------------------------

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step_Instruction1_Activate;

   -----------------------
   -- On_Next1_Activate --
   -----------------------

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next1_Activate;

   -----------------------------------
   -- On_Next_Instruction1_Activate --
   -----------------------------------

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next_Instruction1_Activate;

   -------------------------
   -- On_Finish1_Activate --
   -------------------------

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Finish (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Continue (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Continue1_Activate;

   ------------------------------------------
   -- On_Continue_Without_Signal1_Activate --
   ------------------------------------------

   procedure On_Continue_Without_Signal1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Continue_Without_Signal1_Activate;

   -----------------------
   -- On_Kill1_Activate --
   -----------------------

   procedure On_Kill1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Kill1_Activate;

   ----------------------------
   -- On_Interrupt1_Activate --
   ----------------------------

   procedure On_Interrupt1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Tab = null then
         return;
      end if;

      --  Give some visual feedback to the user
      Output_Text (Tab, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Tab);

      Interrupt
        (Tab.Debugger,
         Wait_For_Prompt => False);

      if not Command_In_Process (Get_Process (Tab.Debugger)) then
         Display_Prompt (Tab.Debugger);
      end if;

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.
   end On_Interrupt1_Activate;

   ------------------------
   -- On_Abort1_Activate --
   ------------------------

   procedure On_Abort1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Abort1_Activate;

   ----------------------------------
   -- On_Command_History1_Activate --
   ----------------------------------

   procedure On_Command_History1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top  : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Get_Toplevel (Object));
      Page : Gtk_Widget :=
        Get_Nth_Page
          (Top.Process_Notebook, Get_Current_Page (Top.Process_Notebook));
      Tab  : constant Debugger_Process_Tab := Process_User_Data.Get (Page);

      use String_History;

   begin
      Show_All (Top.History_Dialog);
      Gdk_Raise (Get_Window (Top.History_Dialog));
      Update (Top.History_Dialog, Tab);
   end On_Command_History1_Activate;

   -------------------------------
   -- On_Clear_Window1_Activate --
   -------------------------------

   procedure On_Clear_Window1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Get_Toplevel (Object));
      Page : constant Gtk_Widget :=
        Get_Nth_Page
          (Top.Process_Notebook, Get_Current_Page (Top.Process_Notebook));
      Tab : constant Debugger_Process_Tab := Process_User_Data.Get (Page);

   begin
      Handler_Block (Tab.Debugger_Text, Tab.Delete_Text_Handler_Id);
      Delete_Text (Tab.Debugger_Text);
      Handler_Unblock (Tab.Debugger_Text, Tab.Delete_Text_Handler_Id);
      Display_Prompt (Tab.Debugger);
   end On_Clear_Window1_Activate;

   ---------------------------------
   -- On_Define_Command1_Activate --
   ---------------------------------

   procedure On_Define_Command1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Define_Command1_Activate;

   -------------------------------
   -- On_Edit_Buttons1_Activate --
   -------------------------------

   procedure On_Edit_Buttons1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Edit_Buttons1_Activate;

   ----------------------------
   -- On_Call_Stack_Activate --
   ----------------------------

   procedure On_Call_Stack_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top       : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Process   : Debugger_Process_Tab;
      Page      : Gtk_Widget;
      Num_Pages : constant Gint :=
        Gint (Page_List.Length (Get_Children (Top.Process_Notebook)));
      Parent    : Gtk_Container;

   begin
      --  ??? Is there a memory leak here ? Data_Paned might be ref'd, but
      --  not actually in a parent, and this means that it isn't destroyed
      --  when the process_tab is destroyed.

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         if Page /= null then
            Process := Process_User_Data.Get (Page);

            if Get_Active (Top.Call_Stack) then
               --  Put back the canvas into the data/editor paned.
               Parent :=
                 Gtk_Container (Get_Parent (Process.Data_Scrolledwindow));
               Reparent (Process.Data_Scrolledwindow, Process.Data_Paned);
               Add (Parent, Process.Data_Paned);
               Unref (Process.Data_Paned);
               Show_All (Parent);
            else
               --  Ref the widget so that it is not destroyed.
               Ref (Process.Data_Paned);
               Parent := Gtk_Container (Get_Parent (Process.Data_Paned));
               Remove (Parent, Process.Data_Paned);
               Reparent (Process.Data_Scrolledwindow, Parent);
            end if;
         end if;
      end loop;
   end On_Call_Stack_Activate;

   --------------------------
   -- On_Threads1_Activate --
   --------------------------

   procedure On_Threads1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Tab      : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Tab /= null then
         Show_All (Top.Thread_Dialog);
         Gdk_Raise (Get_Window (Top.Thread_Dialog));
         Update (Top.Thread_Dialog, Tab);
      end if;
   end On_Threads1_Activate;

   ------------------------
   -- On_Tasks1_Activate --
   ------------------------

   procedure On_Tasks1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Tab      : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Tab /= null then
         Show_All (Top.Task_Dialog);
         Gdk_Raise (Get_Window (Top.Task_Dialog));
         Update (Top.Task_Dialog, Tab);
      end if;
   end On_Tasks1_Activate;

   --------------------------
   -- On_Signals1_Activate --
   --------------------------

   procedure On_Signals1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Signals1_Activate;

   -----------------------------------
   -- On_Edit_Breakpoints1_Activate --
   -----------------------------------

   procedure On_Edit_Breakpoints1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null then
         Breakpoint_Editor
           (Breakpoints_Access (Top.Breakpoints_Editor), Process);
      end if;
   end On_Edit_Breakpoints1_Activate;

   --------------------------------
   -- On_Edit_Displays1_Activate --
   --------------------------------

   procedure On_Edit_Displays1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit_Displays1_Activate;

   ---------------------------------
   -- On_Examine_Memory1_Activate --
   ---------------------------------

   procedure On_Examine_Memory1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
   begin
      Show_All (Top.Memory_View);
      Gdk_Raise (Get_Window (Top.Memory_View));
   end On_Examine_Memory1_Activate;

   ------------------------------------------
   -- On_Display_Local_Variables1_Activate --
   ------------------------------------------

   procedure On_Display_Local_Variables1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null then
         Process_User_Command
           (Process,
            "graph display `" & Info_Locals (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Local_Variables1_Activate;

   ------------------------------------
   -- On_Display_Arguments1_Activate --
   ------------------------------------

   procedure On_Display_Arguments1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null then
         Process_User_Command
           (Process,
            "graph display `" & Info_Args (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Arguments1_Activate;

   ------------------------------------
   -- On_Display_Registers1_Activate --
   ------------------------------------

   procedure On_Display_Registers1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null
        and then not Command_In_Process (Get_Process (Process.Debugger))
      then
         Process_User_Command
           (Process,
            "graph display `" & Info_Registers (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Registers1_Activate;

   -------------------------------------
   -- On_Display_Expression1_Activate --
   -------------------------------------

   procedure On_Display_Expression1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         Is_Func : aliased Boolean;
         Expression : constant String := Display_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Expression Selection",
            Message => -"Enter an expression to display:",
            Key     => "gvd_display_expression_dialog",
            Check_Msg => -"Expression is a subprogram call",
            Button_Active => Is_Func'Access);
      begin
         if Expression /= ""
           and then Expression (Expression'First) /= ASCII.NUL
         then
            if Is_Func then
               Process_User_Command
                 (Tab,
                  "graph print `" & Expression & '`',
                  Output_Command => True);
            else
               Process_User_Command
                 (Tab,
                  "graph display " & Expression,
                  Output_Command => True);
            end if;
         end if;
      end;
   end On_Display_Expression1_Activate;

   --------------------------------------
   -- On_More_Status_Display1_Activate --
   --------------------------------------

   procedure On_More_Status_Display1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_More_Status_Display1_Activate;

   --------------------------
   -- On_Refresh1_Activate --
   --------------------------

   procedure On_Refresh1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      function Internal_Update_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class)
        return Boolean;
      --  Update the value for a specific item

      --------------------------
      -- Internal_Update_Item --
      --------------------------

      function Internal_Update_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class)
        return Boolean
      is
      begin
         Display_Items.Update
           (GVD_Canvas (Canvas),
            Display_Item (Item),
            Redisplay_Canvas => False);
         return True;
      end Internal_Update_Item;

      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         For_Each_Item
           (Tab.Data_Canvas, Internal_Update_Item'Unrestricted_Access);
         Refresh_Canvas (Tab.Data_Canvas);
      end if;
   end On_Refresh1_Activate;

   -----------------------
   -- On_Show1_Activate --
   -----------------------

   procedure On_Show1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Process : Debugger_Process_Tab;
   begin
      if Get_Pref (Separate_Data) then
         Process := Get_Current_Process (Object);
         Show (Process);
         Gdk_Raise (Get_Window (Process));
      end if;
   end On_Show1_Activate;

   ------------------------
   -- On_Manual_Activate --
   ------------------------

   procedure On_Manual_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Browse : constant String :=
        Get_Pref (HTML_Browser) & " " &
          Main_Debug_Window_Access (Tab.Window).Prefix_Directory.all &
          Directory_Separator & "doc" & Directory_Separator & "gvd" &
          Directory_Separator & "gvd.html";
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      Print_Message
        (Main_Debug_Window_Access (Tab.Window).Statusbar1, Help, Browse);

      Args := Argument_String_To_List (Browse);
      Prog := Locate_Exec_On_Path (Args (Args'First).all);

      if Prog /= null then
         Pid := GNAT.OS_Lib.Non_Blocking_Spawn
           (Prog.all, Args (Args'First + 1 .. Args'Last));
         Free (Prog);
      end if;

      if Args /= null then
         for J in Args'Range loop
            Free (Args (J));
         end loop;

         Free (Args);
      end if;
   end On_Manual_Activate;

   ---------------------------
   -- On_About_Gvd_Activate --
   ---------------------------

   procedure On_About_Gvd_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        ("GVD " & Version & (-" built for ") & GVD.Target & ASCII.LF &
         (-"The GNU Visual Debugger") & ASCII.LF & ASCII.LF &
         (-"by Emmanuel Briot & Arnaud Charlet") & ASCII.LF & ASCII.LF &
           "(c) 2000, 2001 ACT-Europe",
         Help_Msg =>
           (-"This is the About information box.") & ASCII.LF & ASCII.LF &
           (-"Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_Gvd_Activate;

   ------------------------------
   -- On_Run1_Toolbar_Activate --
   ------------------------------

   procedure On_Run1_Toolbar_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Run (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Run1_Toolbar_Activate;

   ------------------------
   -- On_Start1_Activate --
   ------------------------

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Start (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Start1_Activate;

   ---------------------
   -- On_Up1_Activate --
   ---------------------

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Stack_Up (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Up1_Activate;

   -----------------------
   -- On_Down1_Activate --
   -----------------------

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Stack_Down (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Down1_Activate;

   -------------------------------------
   -- On_Process_Notebook_Switch_Page --
   -------------------------------------

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Arg1 : Address := To_Address (Params, 1);
      Arg2 : Guint := To_Guint (Params, 2);
      --  Number of the page that will be displayed

      Page : Gtk_Widget := Get_Nth_Page
        (Main_Debug_Window_Access (Object).Process_Notebook, Gint (Arg2));
      Process : Debugger_Process_Tab;
   begin
      Process :=
        Debugger_Process_Tab (Process_User_Data.Get (Page));
      Update_External_Dialogs
        (Main_Debug_Window_Access (Object), Gtk_Widget (Process));

      if Main_Debug_Window_Access (Object).Breakpoints_Editor /= null then
         Set_Process
           (Breakpoints_Access
            (Main_Debug_Window_Access (Object).Breakpoints_Editor), Process);
      end if;

   exception
      --  The page wasn't associated with a debugger yet
      when Gtkada.Types.Data_Error =>
         null;
   end On_Process_Notebook_Switch_Page;

end Main_Debug_Window_Pkg.Callbacks;
