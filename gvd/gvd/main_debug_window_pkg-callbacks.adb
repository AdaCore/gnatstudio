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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Text;            use Gtk.Text;
with Odd_Preferences_Pkg; use Odd_Preferences_Pkg;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Odd_Intl;            use Odd_Intl;
with Odd.Process;         use Odd.Process;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Glib;                use Glib;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Breakpoints_Pkg;     use Breakpoints_Pkg;
with Odd.Process;         use Odd.Process;
with GNAT.Expect;         use GNAT.Expect;
with Ada.Text_IO;         use Ada.Text_IO;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Display_Items;       use Display_Items;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Odd.Canvas;          use Odd.Canvas;
with Odd.Dialogs;         use Odd.Dialogs;
with Gtkada.Types;        use Gtkada.Types;
with Odd.Types;           use Odd.Types;
with Odd.Strings;         use Odd.Strings;
with Odd.Preferences;
with Odd.Code_Editors;    use Odd.Code_Editors;
with Unchecked_Deallocation;

package body Main_Debug_Window_Pkg.Callbacks is

   use Odd;
   use Gtk.Arguments;

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access);
   --  Close all the debuggers associated with a given main debug window
   --  by looking at all the pages of the main notebook.

   -----------------------
   -- Cleanup_Debuggers --
   -----------------------

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access) is
      Tab      : Debugger_Process_Tab;
      Page_Num : Gint := 0;
      Page     : Gtk_Widget;

   begin
      Free (Top.Command_History);

      loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         exit when Page = null;

         Page_Num := Page_Num + 1;
         Tab := Process_User_Data.Get (Page);
         Close (Tab.Debugger);
      end loop;
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
         if S /= "" then
            Set_Executable (Tab.Debugger, S, Mode => Hidden);
         end if;
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
              List,
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
   begin
      null;
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
            if Name (Index) = '%'
              and then Name (Index + 1) = 'f'
            then
               return Name (Name'First .. Index - 1)
                 & File
                 & Substitute (Name (Index + 2 .. Name'Last), File, Line);

            elsif Name (Index) = '%'
              and then Name (Index + 1) = 'l'
            then
               declare
                  Img : constant String := Natural'Image (Line);
               begin
                  return Name (Name'First .. Index - 1)
                    & Img (Img'First + 1 .. Img'Last)
                    & Substitute (Name (Index + 2 .. Name'Last), File, Line);
               end;
            end if;

            Index := Index + 1;
         end loop;

         return Name;
      end Substitute;

      procedure Free is new Unchecked_Deallocation
        (Argument_List, Argument_List_Access);

      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Editor : constant String := Substitute
        (Odd.Preferences.External_Editor,
         Get_Current_File (Tab.Editor_Text),
         Get_Line (Tab.Editor_Text));
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

   --------------------------------
   -- On_Reload_Source1_Activate --
   --------------------------------

   procedure On_Reload_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Reload_Source1_Activate;

   -------------------------------
   -- On_Open_Session1_Activate --
   -------------------------------

   procedure On_Open_Session1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top         : constant Main_Debug_Window_Access :=
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
      Top         : constant Main_Debug_Window_Access :=
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
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         Arguments : constant String := Simple_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Process Selection",
            Message => -"Enter the process id to debug:",
            Key     => "odd_process_id");
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            Set_Busy_Cursor (Tab, True);
            Attach_Process
              (Tab.Debugger, Arguments, Mode => Odd.Types.Visible);
            Set_Busy_Cursor (Tab, False);
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
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Detach_Process (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Detach_Process1_Activate;

   -----------------------------------
   -- On_Change_Directory1_Activate --
   -----------------------------------

   procedure On_Change_Directory1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null then
         return;
      end if;

      declare
         Dir : constant String := To_Unix_Pathname (File_Selection_Dialog
           (Title       => "Directory Selection",
            Dir_Only    => True,
            Must_Exist  => True));

      begin
         if Dir /= "" then
            Set_Busy_Cursor (Tab, True);
            Change_Directory (Tab.Debugger, Dir, Mode => Odd.Types.Visible);
            Set_Busy_Cursor (Tab, False);
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
      if Tab = null then
         return;
      end if;

      Close_Debugger (Tab);
   end On_Close1_Activate;

   -----------------------
   -- On_Exit1_Activate --
   -----------------------

   procedure On_Exit1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Cleanup_Debuggers (Main_Debug_Window_Access (Object));
      Main_Quit;
   end On_Exit1_Activate;

   -----------------------
   -- On_Undo3_Activate --
   -----------------------

   procedure On_Undo3_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Undo3_Activate;

   -----------------------
   -- On_Redo1_Activate --
   -----------------------

   procedure On_Redo1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Redo1_Activate;

   ----------------------
   -- On_Cut1_Activate --
   ----------------------

   procedure On_Cut1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Cut1_Activate;

   -----------------------
   -- On_Copy1_Activate --
   -----------------------

   procedure On_Copy1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Copy1_Activate;

   ------------------------
   -- On_Paste1_Activate --
   ------------------------

   procedure On_Paste1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Paste1_Activate;

   -----------------------------
   -- On_Select_All1_Activate --
   -----------------------------

   procedure On_Select_All1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
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
      if Top.Odd_Preferences = null then
         Gtk_New (Top.Odd_Preferences);
      end if;

      Show_All (Top.Odd_Preferences);
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
            Key     => "odd_run_arguments",
            Check_Msg => -"Stop at beginning of main subprogram",
            Button_Active => Is_Start'Access);
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            Set_Busy_Cursor (Tab, True);
            if Is_Start then
               Start (Tab.Debugger, Arguments, Mode => Odd.Types.Visible);
            else
               Run (Tab.Debugger, Arguments, Mode => Odd.Types.Visible);
            end if;
            Set_Busy_Cursor (Tab, False);
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
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Step_Into (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Step1_Activate;

   -----------------------------------
   -- On_Step_Instruction1_Activate --
   -----------------------------------

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Step_Into_Instruction (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Step_Instruction1_Activate;

   -----------------------
   -- On_Next1_Activate --
   -----------------------

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Step_Over (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Next1_Activate;

   -----------------------------------
   -- On_Next_Instruction1_Activate --
   -----------------------------------

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Step_Over_Instruction (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Next_Instruction1_Activate;

   -------------------------
   -- On_Finish1_Activate --
   -------------------------

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Finish (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Continue (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
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
      Tmp : Boolean;

   begin
      if Tab = null then
         return;
      end if;

      --  Give some visual feedback to the user
      Text_Output_Handler (Tab, "<^C>" & ASCII.LF, Is_Command => True);

      --  Process the events so as to show the text.
      while Gtk.Main.Events_Pending loop
         Tmp := Gtk.Main.Main_Iteration;
      end loop;

      --  Empty all the buffers to avoid waiting for a long time that all
      --  the output is processed.
      Interrupt (Tab.Debugger);
      Flush (Get_Descriptor (Get_Process (Tab.Debugger)).all, Timeout => 100);

      --  Make sure a final prompt is displayed for the user.
      --  If we are already processing a command (such as "run"), that command
      --  is already waiting for the prompt, and we don't need to do it.
      --  Otherwise, we need to do it ourselves, so that the new prompt
      --  appears as well.
      Display_Prompt
        (Tab.Debugger,
         Wait_For_Prompt =>
           not Command_In_Process (Get_Process (Tab.Debugger)));
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
   -- On_Backtrace1_Activate --
   ----------------------------

   procedure On_Backtrace1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : constant Main_Debug_Window_Access :=
        Main_Debug_Window_Access (Object);
      Tab      : constant Debugger_Process_Tab := Get_Current_Process (Top);

   begin
      if Tab = null then
         return;
      end if;

      Show_All (Top.Backtrace_Dialog);
      Update (Top.Backtrace_Dialog, Tab);
   end On_Backtrace1_Activate;

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
      if Tab = null then
         return;
      end if;

      Show_All (Top.Task_Dialog);
      Update (Top.Task_Dialog, Tab);
   end On_Threads1_Activate;

   ----------------------------
   -- On_Processes1_Activate --
   ----------------------------

   procedure On_Processes1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Processes1_Activate;

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
   begin
      null;
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
            Key     => "odd_display_expression_dialog",
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
           (Odd_Canvas (Canvas),
            Display_Item (Item),
            Redisplay_Canvas => False);
         return True;
      end Internal_Update_Item;

      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Odd.Canvas.For_Each_Item
           (Tab.Data_Canvas, Internal_Update_Item'Unrestricted_Access);
         Refresh_Canvas (Tab.Data_Canvas);
      end if;
   end On_Refresh1_Activate;

   ---------------------------
   -- On_Overview1_Activate --
   ---------------------------

   procedure On_Overview1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Overview1_Activate;

   --------------------------
   -- On_On_Item1_Activate --
   --------------------------

   procedure On_On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_On_Item1_Activate;

   ----------------------------
   -- On_What_Now_1_Activate --
   ----------------------------

   procedure On_What_Now_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_What_Now_1_Activate;

   ---------------------------------
   -- On_Tip_Of_The_Day1_Activate --
   ---------------------------------

   procedure On_Tip_Of_The_Day1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Tip_Of_The_Day1_Activate;

   ----------------------------
   -- On_About_Odd1_Activate --
   ----------------------------

   procedure On_About_Odd1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        ("GVD " & Version &
         (-(": The GNU Visual Debugger" & ASCII.LF & ASCII.LF &
            "by Emmanuel Briot & Arnaud Charlet")),
         Help_Msg =>
           -("This is the About information box." & ASCII.LF & ASCII.LF &
             "Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_Odd1_Activate;

   ------------------------------
   -- On_Run1_Toolbar_Activate --
   ------------------------------

   procedure On_Run1_Toolbar_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      Set_Busy_Cursor (Tab, True);
      Run (Tab.Debugger, Mode => Odd.Types.Visible);
      Set_Busy_Cursor (Tab, False);
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
      if Tab /= null
        and then not Command_In_Process (Get_Process (Tab.Debugger))
      then
         Set_Busy_Cursor (Tab, True);
         Start (Tab.Debugger, Mode => Odd.Types.Visible);
         Set_Busy_Cursor (Tab, False);
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
      if Tab /= null
        and then not Command_In_Process (Get_Process (Tab.Debugger))
      then
         Set_Busy_Cursor (Tab, True);
         Stack_Up (Tab.Debugger, Mode => Odd.Types.Visible);
         Set_Busy_Cursor (Tab, False);
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
      if Tab /= null
        and then not Command_In_Process (Get_Process (Tab.Debugger))
      then
         Set_Busy_Cursor (Tab, True);
         Stack_Down (Tab.Debugger, Mode => Odd.Types.Visible);
         Set_Busy_Cursor (Tab, False);
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
