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

with Glib;                use Glib;
with Gdk.Window;          use Gdk.Window;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text;            use Gtk.Text;
with Gtk.Window;          use Gtk.Window;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Gtkada.Canvas;       use Gtkada.Canvas;

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Odd_Intl;            use Odd_Intl;
with GVD;                 use GVD;
with GVD.Process;         use GVD.Process;
with GVD.Proc_Utils;      use GVD.Proc_Utils;
with GVD.Canvas;          use GVD.Canvas;
with GVD.Dialogs;         use GVD.Dialogs;
with GVD.Process;         use GVD.Process;
with GVD.Trace;           use GVD.Trace;
with GVD.Types;           use GVD.Types;
with String_Utils;        use String_Utils;
with GVD.Code_Editors;    use GVD.Code_Editors;
with GVD.Files;           use GVD.Files;
with GVD.Preferences;     use GVD.Preferences;
with GVD.Window_Settings; use GVD.Window_Settings;
with GVD.Main_Window;     use GVD.Main_Window;
with GVD.Memory_View;     use GVD.Memory_View;
with GVD.Preferences_Dialog;  use GVD.Preferences_Dialog;
with GVD.Open_Program_Dialog; use GVD.Open_Program_Dialog;
with GVD.Session_Dialog;  use GVD.Session_Dialog;
with Basic_Types;         use Basic_Types;
with List_Select_Pkg;     use List_Select_Pkg;
with Dock_Paned;          use Dock_Paned;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Breakpoints_Editor;  use Breakpoints_Editor;
with Display_Items;       use Display_Items;

package body GVD.Menu is

   use GVD;

   procedure Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   ---------------------
   -- On_Open_Program --
   ---------------------

   procedure On_Open_Program
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      --  ??? Should be able to remove this test at some point
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
            Output_Error
              (GVD_Main_Window (Get_Toplevel (Object)),
               (-" Could not find file: ") & S);
         end if;

      exception
         when Executable_Not_Found =>
            Output_Error
              (GVD_Main_Window (Get_Toplevel (Object)),
               (-" Could not find file: ") & S);
      end;
   end On_Open_Program;

   ----------------------
   -- On_Open_Debugger --
   ----------------------

   procedure On_Open_Debugger
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Program : Program_Descriptor;
      List    : Argument_List (1 .. 0);
      Process : Debugger_Process_Tab;
      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab     : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      Open_Program (Top.Open_Program, Program);

      case Program.Launch is
         when None =>
            return;

         when Current_Debugger =>
            if Tab /= null then
               Close_Debugger (Tab);
            end if;

         when New_Debugger =>
            null;
      end case;

      Process :=
        Create_Debugger
          (Top,
           Program.Debugger,
           Program.Program.all,
           List, "",
           Program.Remote_Host.all,
           Program.Remote_Target.all,
           Program.Protocol.all,
           Program.Debugger_Name.all);
   end On_Open_Debugger;

   -----------------------
   -- On_Open_Core_Dump --
   -----------------------

   procedure On_Open_Core_Dump
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      --  ??? Should be able to remove this test at some point
      if Tab = null
        or else Command_In_Process (Get_Process (Tab.Debugger))
      then
         return;
      end if;

      declare
         S : constant String :=
           To_Unix_Pathname (File_Selection_Dialog (-"Select Core File"));
         --  ??? To_Unix_Pathname should only be called for debuggers that
         --  expect unix pathname like gdb.

      begin
         if Tab.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File (Tab.Debugger, S, Mode => GVD.Types.Visible);
         else
            Output_Error
              (GVD_Main_Window (Get_Toplevel (Object)),
               -(" Could not find core file: ") & S);
         end if;
      end;
   end On_Open_Core_Dump;

   --------------------
   -- On_Edit_Source --
   --------------------

   procedure On_Edit_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
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
        (GVD_Main_Window (Tab.Window).External_Editor.all,
         Host_File, Get_Line (Tab.Editor_Text));
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      Output_Info (GVD_Main_Window (Tab.Window), Editor);

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
   end On_Edit_Source;

   --------------------
   -- On_Open_Source --
   --------------------

   procedure On_Open_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      File_Name : constant String :=
        File_Selection_Dialog (Title => -"Source name", Must_Exist => True);
      Tab       : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);

   begin
      Load_File (Tab.Editor_Text, File_Name, Set_Current => False);
      Set_Line (Tab.Editor_Text, 1, Set_Current => False);
   end On_Open_Source;

   -----------------------
   -- On_Reload_Sources --
   -----------------------

   procedure On_Reload_Sources
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top    : constant GVD_Main_Window := GVD_Main_Window (Object);
      Editor : constant Code_Editor :=
        Get_Current_Process (Object).Editor_Text;

   begin
      GVD.Files.Clear_Cache (Top, Force => True);
      Load_File (Editor, Get_Current_File (Editor), Force => True);
   end On_Reload_Sources;

   ---------------------
   -- On_Open_Session --
   ---------------------

   procedure On_Open_Session
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
   begin
      Open_Session (Top, Top.Open_Session, Top.Sessions_Dir.all);
   end On_Open_Session;

   ------------------------
   -- On_Save_Session_As --
   ------------------------

   procedure On_Save_Session_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
   begin
      Save_Session (Top, Top.Open_Session, Top.Sessions_Dir.all);
   end On_Save_Session_As;

   --------------------------
   -- On_Attach_To_Process --
   --------------------------

   procedure On_Attach_To_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab           : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);
      Process_List  : List_Select_Access;
      Success       : Boolean;
      Info          : Process_Info;

   begin
      if Tab = null then
         return;
      end if;

      Gtk_New (Process_List, Title => -"Process Selection");

      Open_Processes (Tab.Debugger);

      loop
         Next_Process (Tab.Debugger, Info, Success);

         exit when not Success;

         Add_Item (Process_List, Info.Id, Info.Info);
      end loop;

      Close_Processes (Tab.Debugger);

      declare
         Argument : constant String := Show (Process_List);
      begin
         if Argument /= "" then
            Attach_Process (Tab.Debugger, Argument, Mode => GVD.Types.Visible);
         end if;
      end;
   end On_Attach_To_Process;

   -----------------------
   -- On_Detach_Process --
   -----------------------

   procedure On_Detach_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Detach_Process (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Detach_Process;

   -------------------------
   -- On_Change_Directory --
   -------------------------

   procedure On_Change_Directory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null then
         return;
      end if;

      declare
         Dir : constant String := To_Unix_Pathname (File_Selection_Dialog
           (Title       => -"Directory Selection",
            Dir_Only    => True,
            Must_Exist  => True));

      begin
         if Dir /= "" then
            Change_Directory (Tab.Debugger, Dir, Mode => GVD.Types.Visible);
         end if;
      end;
   end On_Change_Directory;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Close_Debugger (Tab);
      end if;
   end On_Close;

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      Save_Window_Settings
        (GVD_Main_Window (Object).Gvd_Home_Dir.all
         & Directory_Separator & "window_settings",
         Gtk_Widget (Object));
      Cleanup_Debuggers (GVD_Main_Window (Object));
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
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
   begin
      if Top.GVD_Preferences = null then
         Gtk_New (Top.GVD_Preferences, Top);
      end if;

      --  First do a show_all, so that Fill_Dialog can choose to
      --  hide or deactivate widgets.
      Show_All (Top.GVD_Preferences);
      GVD.Preferences.Fill_Dialog (Top.GVD_Preferences);
   end On_Preferences;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null then
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
   end On_Run;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Finish (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Continue (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Continue;

   -------------
   -- On_Kill --
   -------------

   procedure On_Kill
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      null;
   end On_Kill;

   ------------------
   -- On_Interrupt --
   ------------------

   procedure On_Interrupt
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null then
         return;
      end if;

      --  Give some visual feedback to the user
      Output_Text (Tab, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Tab);

      --  Need to flush the queue of commands
      Clear_Queue (Tab.Debugger);

      Interrupt (Tab.Debugger);

      if not Command_In_Process (Get_Process (Tab.Debugger)) then
         Display_Prompt (Tab.Debugger);
      end if;

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.
   end On_Interrupt;

   ------------------------
   -- On_Command_History --
   ------------------------

   procedure On_Command_History
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (Get_Toplevel (Object));
      Page : Gtk_Widget :=
        Get_Nth_Page
          (Top.Process_Notebook, Get_Current_Page (Top.Process_Notebook));
      Tab  : constant Debugger_Process_Tab := Process_User_Data.Get (Page);

      use String_History;

   begin
      Show_All (Top.History_Dialog);
      Gdk_Raise (Get_Window (Top.History_Dialog));
      Update (Top.History_Dialog, Tab);
   end On_Command_History;

   ---------------------
   -- On_Clear_Window --
   ---------------------

   procedure On_Clear_Window
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window :=
        GVD_Main_Window (Get_Toplevel (Object));
      Page : constant Gtk_Widget :=
        Get_Nth_Page
          (Top.Process_Notebook, Get_Current_Page (Top.Process_Notebook));
      Tab : constant Debugger_Process_Tab := Process_User_Data.Get (Page);

   begin
      Handler_Block (Tab.Debugger_Text, Tab.Delete_Text_Handler_Id);
      Delete_Text (Tab.Debugger_Text);
      Handler_Unblock (Tab.Debugger_Text, Tab.Delete_Text_Handler_Id);
      Display_Prompt (Tab.Debugger);
   end On_Clear_Window;

   -------------------
   -- On_Call_Stack --
   -------------------

   procedure On_Call_Stack
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top       : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process   : Debugger_Process_Tab;
      Page      : Gtk_Widget;
      Num_Pages : constant Gint :=
        Gint (Page_List.Length (Get_Children (Top.Process_Notebook)));

   begin
      --  ??? Is there a memory leak here ? Data_Paned might be ref'd, but
      --  not actually in a parent, and this means that it isn't destroyed
      --  when the process_tab is destroyed.

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         if Page /= null then
            Process := Process_User_Data.Get (Page);

            if Get_Active (Gtk_Check_Menu_Item (Get_Item
              (Top.Factory, -"/Data/Call Stack")))
            then
               Add1 (Process.Data_Paned, Process.Stack_Scrolledwindow);
               Unref (Process.Stack_Scrolledwindow);
               Update_Call_Stack (Process);
            else
               Ref (Process.Stack_Scrolledwindow);
               Dock_Remove (Process.Data_Paned, Process.Stack_Scrolledwindow);
            end if;
         end if;
      end loop;
   end On_Call_Stack;

   ----------------
   -- On_Threads --
   ----------------

   procedure On_Threads
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Tab /= null then
         Show_All (Top.Thread_Dialog);
         Gdk_Raise (Get_Window (Top.Thread_Dialog));
         Update (Top.Thread_Dialog, Tab);
      end if;
   end On_Threads;

   --------------
   -- On_Tasks --
   --------------

   procedure On_Tasks
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Tab /= null then
         Show_All (Top.Task_Dialog);
         Gdk_Raise (Get_Window (Top.Task_Dialog));
         Update (Top.Task_Dialog, Tab);
      end if;
   end On_Tasks;

   -------------------------
   -- On_Edit_Breakpoints --
   -------------------------

   procedure On_Edit_Breakpoints
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);

   begin
      if Process /= null then
         Breakpoint_Editor
           (Breakpoint_Editor_Access (Top.Breakpoints_Editor), Process);
      end if;
   end On_Edit_Breakpoints;

   -----------------------
   -- On_Examine_Memory --
   -----------------------

   procedure On_Examine_Memory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top : constant GVD_Main_Window := GVD_Main_Window (Object);
   begin
      Show_All (Top.Memory_View);
      Gdk_Raise (Get_Window (Top.Memory_View));
   end On_Examine_Memory;

   --------------------------------
   -- On_Display_Local_Variables --
   --------------------------------

   procedure On_Display_Local_Variables
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null then
         Process_User_Command
           (Process,
            "graph display `" & Info_Locals (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Local_Variables;

   --------------------------
   -- On_Display_Arguments --
   --------------------------

   procedure On_Display_Arguments
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Process /= null then
         Process_User_Command
           (Process,
            "graph display `" & Info_Args (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Arguments;

   --------------------------
   -- On_Display_Registers --
   --------------------------

   procedure On_Display_Registers
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      --  ??? Should be able to remove this test at some point
      if Process /= null
        and then not Command_In_Process (Get_Process (Process.Debugger))
      then
         Process_User_Command
           (Process,
            "graph display `" & Info_Registers (Process.Debugger) & '`',
            Output_Command => True);
      end if;
   end On_Display_Registers;

   ---------------------------
   -- On_Display_Expression --
   ---------------------------

   procedure On_Display_Expression
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget) is
   begin
      On_Display_Expression (Object);
   end On_Display_Expression;

   procedure On_Display_Expression (Object : Data_Type_Access) is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      --  ??? Should be able to remove this test at some point
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
   end On_Display_Expression;

   ----------------
   -- On_Refresh --
   ----------------

   procedure On_Refresh
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      function Internal_Update_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Update the value for a specific item

      --------------------------
      -- Internal_Update_Item --
      --------------------------

      function Internal_Update_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
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
   end On_Refresh;

   -------------
   -- On_Show --
   -------------

   procedure On_Show
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Process : Debugger_Process_Tab;
   begin
      if Get_Pref (Separate_Data) then
         Process := Get_Current_Process (Object);
         Show (Process);
         Gdk_Raise (Get_Window (Process));
      end if;
   end On_Show;

   ---------------
   -- On_Manual --
   ---------------

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Browse : constant String :=
        Get_Pref (HTML_Browser) & " " &
          GVD_Main_Window (Object).Prefix_Directory.all &
          Directory_Separator & "doc" & Directory_Separator & "gvd" &
          Directory_Separator & "gvd.html";
      Args   : Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;

   begin
      Output_Info (GVD_Main_Window (Object), Browse);

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
   end On_Manual;

   ------------------
   -- On_About_GVD --
   ------------------

   procedure On_About_GVD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        ("GVD " & Version & " (" & Source_Date & ")" &
         (-" built for ") & GVD.Target & ASCII.LF &
         (-"The GNU Visual Debugger") & ASCII.LF & ASCII.LF &
         (-"by Emmanuel Briot, Arnaud Charlet & Nicolas Setton") &
           ASCII.LF & ASCII.LF & "(c) 2000, 2001 ACT-Europe",
         Help_Msg =>
           (-"This is the About information box.") & ASCII.LF & ASCII.LF &
           (-"Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_GVD;

   --------------------
   -- GVD_Menu_Items --
   --------------------

   function GVD_Menu_Items return Gtk_Item_Factory_Entry_Access is
   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (-"/_File", Item_Type => Branch),
         Gtk_New (-"/_File/Open Program...", "F3", On_Open_Program'Access),
         Gtk_New (-"/_File/New Debugger...", "", On_Open_Debugger'Access),
         Gtk_New (-"/_File/Open Core Dump...", "", On_Open_Core_Dump'Access),
         Gtk_New (-"/_File/sep1", Item_Type => Separator),
         Gtk_New (-"/_File/Edit Current Source", "<control>E",
                  On_Edit_Source'Access),
         Gtk_New (-"/_File/Open Source...", "", On_Open_Source'Access),
         Gtk_New (-"/_File/Reload Sources", "", On_Reload_Sources'Access),
         Gtk_New (-"/_File/sep2", Item_Type => Separator),
         Gtk_New (-"/_File/Open Session...", "<control>N",
                  On_Open_Session'Access),
         Gtk_New (-"/_File/Save Session As...", "<control>S",
                  On_Save_Session_As'Access),
         Gtk_New (-"/_File/sep3", Item_Type => Separator),
         Gtk_New (-"/_File/Attach To Process...", "",
                  On_Attach_To_Process'Access),
         Gtk_New (-"/_File/Detach Process", "", On_Detach_Process'Access),
         Gtk_New (-"/_File/sep4", Item_Type => Separator),
         Gtk_New (-"/_File/Change Directory...", "",
                  On_Change_Directory'Access),
         Gtk_New (-"/_File/sep5", Item_Type => Separator),
         Gtk_New (-"/_File/Close", "", On_Close'Access),
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

         Gtk_New (-"/_Program", Item_Type => Branch),
         Gtk_New (-"/_Program/Run-Start...", "F2", On_Run'Access),
         Gtk_New (-"/_Program/sep1", Item_Type => Separator),
         Gtk_New (-"/_Program/Step", "F5", On_Step'Access),
         Gtk_New (-"/_Program/Step Instruction", "<shift>F5",
                  On_Step_Instruction'Access),
         Gtk_New (-"/_Program/Next", "F6", On_Next'Access),
         Gtk_New (-"/_Program/Next Instruction", "<shift>F6",
                  On_Next_Instruction'Access),
         Gtk_New (-"/_Program/Finish", "F7", On_Finish'Access),
         Gtk_New (-"/_Program/sep2", Item_Type => Separator),
         Gtk_New (-"/_Program/Continue", "F8", On_Continue'Access),
         Gtk_New (-"/_Program/sep3", Item_Type => Separator),
         Gtk_New (-"/_Program/Kill", "", On_Kill'Access),
         Gtk_New (-"/_Program/Interrupt", "ESC", On_Interrupt'Access),

         Gtk_New (-"/_Command", Item_Type => Branch),
         Gtk_New (-"/_Command/Command History...", "",
                  On_Command_History'Access),
         Gtk_New (-"/_Command/Clear Window", "", On_Clear_Window'Access),

         Gtk_New (-"/_Data", Item_Type => Branch),
         Gtk_New (-"/_Data/Call Stack", "", On_Call_Stack'Access, Check_Item),
         Gtk_New (-"/_Data/Threads", "", On_Threads'Access),
         Gtk_New (-"/_Data/Tasks", "", On_Tasks'Access),
         Gtk_New (-"/_Data/sep1", Item_Type => Separator),
         Gtk_New (-"/_Data/Edit Breakpoints...", "",
                  On_Edit_Breakpoints'Access),
         Gtk_New (-"/_Data/Examine Memory...", "", On_Examine_Memory'Access),
         Gtk_New (-"/_Data/sep2", Item_Type => Separator),
         Gtk_New (-"/_Data/Display Local Variables", "<alt>L",
                  On_Display_Local_Variables'Access),
         Gtk_New (-"/_Data/Display Arguments", "<alt>U",
                  On_Display_Arguments'Access),
         Gtk_New (-"/_Data/Display Registers", "",
                  On_Display_Registers'Access),
         Gtk_New (-"/_Data/Display Any Expression...", "",
                  On_Display_Expression'Access),
         Gtk_New (-"/_Data/sep3", Item_Type => Separator),
         Gtk_New (-"/_Data/Refresh", "<control>L", On_Refresh'Access),
         Gtk_New (-"/_Data/Show", "", On_Show'Access),

         Gtk_New (-"/_Window"),

         Gtk_New (-"/_Help", Item_Type => Branch),
         Gtk_New (-"/_Help/GVD Manual...", "F1", On_Manual'Access),
         Gtk_New (-"/_Help/About GVD...", "", On_About_GVD'Access));
   end GVD_Menu_Items;

end GVD.Menu;
