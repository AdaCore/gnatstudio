-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Item_Factory;    use Gtk.Item_Factory;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text;            use Gtk.Text;
with Gtk.Window;          use Gtk.Window;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Gtkada.Canvas;       use Gtkada.Canvas;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Odd_Intl;            use Odd_Intl;
with GVD;                 use GVD;
with GVD.Process;         use GVD.Process;
with GVD.Proc_Utils;      use GVD.Proc_Utils;
with GVD.Canvas;          use GVD.Canvas;
with GVD.Dialogs;         use GVD.Dialogs;
with GVD.Trace;           use GVD.Trace;
with GVD.Types;           use GVD.Types;
with String_Utils;        use String_Utils;
with GVD.Main_Window;     use GVD.Main_Window;
with GVD.Memory_View;     use GVD.Memory_View;
with Basic_Types;         use Basic_Types;
with List_Select_Pkg;     use List_Select_Pkg;
with Dock_Paned;          use Dock_Paned;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Breakpoints_Editor;  use Breakpoints_Editor;
with Display_Items;       use Display_Items;

package body GVD.Menu is

   use GVD;

   ---------------------
   -- On_Open_Program --
   ---------------------

   procedure On_Open_Program
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

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
            Change_Dir (Dir_Name (S));

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

   -----------------------
   -- On_Open_Core_Dump --
   -----------------------

   procedure On_Open_Core_Dump
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

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
   -- On_Add_Symbols --
   --------------------

   procedure On_Add_Symbols
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

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
           To_Unix_Pathname (File_Selection_Dialog (-"Select Module"));

      begin
         if S = "" then
            return;
         end if;

         if Tab.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Add_Symbols (Tab.Debugger, S, Mode => GVD.Types.Visible);
         else
            Output_Error
              (GVD_Main_Window (Get_Toplevel (Object)),
               -(" Could not find file: ") & S);
         end if;
      end;
   end On_Add_Symbols;

   --------------------------
   -- On_Attach_To_Process --
   --------------------------

   procedure On_Attach_To_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Tab           : constant Debugger_Process_Tab :=
        Get_Current_Process (Object);
      Process_List  : List_Select_Access;
      Success       : Boolean;
      Info          : Process_Info;
      Button        : Message_Dialog_Buttons;

   begin
      if Tab = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Tab.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
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
      pragma Unreferenced (Action, Widget);

      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;

   begin
      if Tab /= null then
         if Command_In_Process (Get_Process (Tab.Debugger)) then
            Button := Message_Dialog
              ((-"Cannot detach the task/process while the") & ASCII.LF &
               (-"underlying debugger is busy.") & ASCII.LF &
               (-"Interrupt the debugger or wait for its availability."),
              Dialog_Type => Warning,
              Buttons => Button_OK);

         else
            Detach_Process (Tab.Debugger, Mode => GVD.Types.Visible);
         end if;
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
      pragma Unreferenced (Action, Widget);

      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;

   begin
      if Tab = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Tab.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot change directory while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
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
            Change_Dir (Dir);
         end if;
      end;
   end On_Change_Directory;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;

   begin
      if Tab = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Tab.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot rerun while the underlying debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      declare
         Is_Start  : aliased Boolean;
         Arguments : constant String := Display_Entry_Dialog
           (Parent  => Tab.Window,
            Title   => -"Run/Start",
            Message => -"Run arguments:",
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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Object, Action, Widget);
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
      pragma Unreferenced (Action, Widget);

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

      Set_Busy (Tab, False);

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.

      --  Note that doing anything at this point is very unsafe, since we got
      --  called while handling a command, and this command has not been fully
      --  handled yet, so we cannot reliably send new commands to the debugger
      --  without creating a synchronization problem. Also, we should be able
      --  to clean up properly the current command, which is particularly
      --  tricky when handling an internal command.
   end On_Interrupt;

   ------------------------
   -- On_Command_History --
   ------------------------

   procedure On_Command_History
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (Get_Toplevel (Object));
      Page : constant Gtk_Widget := Get_Nth_Page
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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

      Top       : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process   : Debugger_Process_Tab;
      Page      : Gtk_Widget;
      Num_Pages : constant Gint :=
        Gint (Page_List.Length (Get_Children (Top.Process_Notebook)));
      Button    : Message_Dialog_Buttons;
      Item      : Gtk_Widget;

   begin
      --  ??? Is there a memory leak here ? Data_Paned might be ref'd, but
      --  not actually in a parent, and this means that it isn't destroyed
      --  when the process_tab is destroyed.

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Top.Process_Notebook, Page_Num);

         if Page /= null then
            Process := Process_User_Data.Get (Page);

            Item := Get_Item (Top.Factory, -"/Data/Call Stack");

            if Item = null then
               Item := Get_Item (Top.Factory, -"/Debug/Data/Call Stack");
            end if;

            if Get_Active (Gtk_Check_Menu_Item (Item)) then
               Add1 (Process.Data_Paned, Process.Stack_Scrolledwindow);
               Unref (Process.Stack_Scrolledwindow);

               if Command_In_Process (Get_Process (Process.Debugger)) then
                  Button := Message_Dialog
                    ((-"Cannot show call stack while the debugger is busy.") &
                     ASCII.LF &
                     (-"Interrupt the debugger or wait for its availability."),
                    Dialog_Type => Warning,
                    Buttons => Button_OK);

               else
                  Update_Call_Stack (Process);
               end if;

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
      pragma Unreferenced (Action, Widget);

      Top    : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;

   begin
      if Tab /= null then
         if Command_In_Process (Get_Process (Tab.Debugger)) then
            Button := Message_Dialog
              ((-"Cannot display threads list while the debugger is busy.") &
               ASCII.LF &
               (-"Interrupt the debugger or wait for its availability."),
              Dialog_Type => Warning,
              Buttons => Button_OK);
            return;
         end if;

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
      pragma Unreferenced (Action, Widget);

      Top    : constant GVD_Main_Window := GVD_Main_Window (Object);
      Tab    : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;

   begin
      if Tab /= null then
         if Command_In_Process (Get_Process (Tab.Debugger)) then
            Button := Message_Dialog
              ((-"Cannot display tasks list while the debugger is busy.") &
               ASCII.LF &
               (-"Interrupt the debugger or wait for its availability."),
              Dialog_Type => Warning,
              Buttons => Button_OK);
            return;
         end if;

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
      pragma Unreferenced (Action, Widget);

      Top     : constant GVD_Main_Window := GVD_Main_Window (Object);
      Process : constant Debugger_Process_Tab := Get_Current_Process (Object);
      Button  : Message_Dialog_Buttons;

   begin
      if Process /= null then
         if Command_In_Process (Get_Process (Process.Debugger)) then
            Button := Message_Dialog
              ((-"Cannot edit breakpoints while the debugger is busy.") &
               ASCII.LF &
               (-"Interrupt the debugger or wait for its availability."),
              Dialog_Type => Warning,
              Buttons => Button_OK);
            return;
         end if;

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

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
      pragma Unreferenced (Action, Widget);

      Process : Debugger_Process_Tab;
   begin
      Process := Get_Current_Process (Object);

      if Process.Data_Paned = null then
         Setup_Data_Window (Process);
      end if;
   end On_Show;

end GVD.Menu;
