-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                 use Glib;
with Gdk.Window;           use Gdk.Window;
with Gtk.Window;           use Gtk.Window;
with Gtkada.Dialogs;       use Gtkada.Dialogs;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.MDI;           use Gtkada.MDI;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Widget;           use Gtk.Widget;

with GPS.Intl;             use GPS.Intl;
with GVD;                  use GVD;
with GVD.Process;          use GVD.Process;
with GVD.Proc_Utils;       use GVD.Proc_Utils;
with GVD.Call_Stack;       use GVD.Call_Stack;
with GVD.Canvas;           use GVD.Canvas;
with GVD.Dialogs;          use GVD.Dialogs;
with GVD.Types;            use GVD.Types;
with GPS.Main_Window;      use GPS.Main_Window;
with GVD.Memory_View;      use GVD.Memory_View;
with Basic_Types;          use Basic_Types;
with Std_Dialogs;          use Std_Dialogs;
with Histories;            use Histories;
with List_Select_Pkg;      use List_Select_Pkg;
with Debugger;             use Debugger;
with Process_Proxies;      use Process_Proxies;
with Breakpoints_Editor;   use Breakpoints_Editor;
with Display_Items;        use Display_Items;
with Interactive_Consoles; use Interactive_Consoles;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Main_Window;      use GPS.Main_Window;

package body GVD.Menu is

   use GVD;

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with builder_module.adb, since we want
   --  to have the same history for the run command in GPS.

   --------------------------
   -- On_Attach_To_Process --
   --------------------------

   procedure On_Attach_To_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Tab           : constant Visual_Debugger :=
        Get_Current_Process (Object);
      Process_List  : List_Select_Access;
      Success       : Boolean;
      Info          : Process_Info;
      Button        : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

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
      Tab    : constant Visual_Debugger := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);

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

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Tab         : constant Visual_Debugger :=
        Get_Current_Process (Object);
      Button      : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);

      Button2     : Boolean_Access := null;
      Multitasks  : aliased Boolean := False;
      Multi_Msg   : aliased String := -"Enable VxWorks multi-tasks mode";
      No_Msg      : aliased String := "";
      Msg         : Basic_Types.String_Access := No_Msg'Unchecked_Access;
      WTX_Version : Natural;

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

      --  If we are debugging against VxWorks enable the multi-tasks
      --  mode checkbox

      Info_WTX (Tab.Debugger, WTX_Version);

      if WTX_Version = 2 then
         Button2 := Multitasks'Unchecked_Access;
         Msg := Multi_Msg'Unchecked_Access;
      end if;

      declare
         Is_Start  : aliased Boolean;
         Arguments : constant String := Display_Entry_Dialog
           (Parent         => Tab.Window,
            Title          => -"Run/Start",
            Message        => -"Run arguments:",
            Key            => Cst_Run_Arguments_History,
            History        => Get_History (GPS_Window (Tab.Window).Kernel),
            Check_Msg      => -"Stop at beginning of main subprogram",
            Check_Msg2     => Msg.all,
            Button_Active  => Is_Start'Access,
            Button2_Active => Button2,
            Key_Check      => "stop_beginning_debugger",
            Key_Check2     => "multitask_mode_debugger");
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            if Button2 /= null then
               if Multitasks then
                  Process_User_Command
                    (Tab,
                     "set multi-tasks-mode on",
                     Output_Command => False);
               else
                  Process_User_Command
                    (Tab,
                     "set multi-tasks-mode off",
                     Output_Command => False);
               end if;
            end if;

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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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
      pragma Unreferenced (Action, Widget);

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Kill_Process (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
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

      Tab : constant Visual_Debugger := Get_Current_Process (Object);
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

      Top  : constant GPS_Window :=
        GPS_Window (Get_Toplevel (Object));
      Tab  : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      Show_All (Top.History_Dialog);
      Gdk_Raise (Get_Window (Top.History_Dialog));
      Update (History_Dialog_Access (Top.History_Dialog), Tab);
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

      Tab : constant Visual_Debugger :=
        Get_Current_Process (Get_Toplevel (Object));

   begin
      Clear (Tab.Debugger_Text);
   end On_Clear_Window;

   -------------------
   -- On_Call_Stack --
   -------------------

   procedure On_Call_Stack
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top       : constant GPS_Window := GPS_Window (Object);
      Process   : Visual_Debugger;
      Child     : MDI_Child;
      Button    : Message_Dialog_Buttons;
      List      : Debugger_List_Link := Top.First_Debugger;
      pragma Unreferenced (Action, Widget, Button);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            if Process.Stack = null then
               Create_Call_Stack (Process);

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
               Child := Find_MDI_Child (Top.Process_Mdi, Process.Stack);

               if Child /= null then
                  Raise_Child (Child);
               else
                  --  Something really bad happened: the stack window is not
                  --  part of the MDI, reset it.

                  Destroy (Process.Stack);
                  Process.Stack := null;
                  Create_Call_Stack (Process);
               end if;
            end if;
         end if;

         List := List.Next;
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
      Top    : constant GPS_Window := GPS_Window (Object);
      Tab    : constant Visual_Debugger := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);
      Dialog : Thread_Dialog_Access;

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

         if Top.Thread_Dialog = null then
            Gtk_New (Dialog, Gtk_Window (Top));
            Top.Thread_Dialog := Gtk_Dialog (Dialog);
         end if;

         Show_All (Top.Thread_Dialog);
         Gdk_Raise (Get_Window (Top.Thread_Dialog));
         Update (Dialog, Tab);
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
      Top    : constant GPS_Window := GPS_Window (Object);
      Tab    : constant Visual_Debugger := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);
      Dialog : Task_Dialog_Access;

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

         if Top.Task_Dialog = null then
            Gtk_New (Dialog, Gtk_Window (Top));
            Top.Task_Dialog := Gtk_Dialog (Dialog);
         end if;

         Show_All (Top.Task_Dialog);
         Gdk_Raise (Get_Window (Top.Task_Dialog));
         Update (Dialog, Tab);
      end if;
   end On_Tasks;

   -----------
   -- On_PD --
   -----------

   procedure On_PD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top    : constant GPS_Window := GPS_Window (Object);
      Tab    : constant Visual_Debugger := Get_Current_Process (Object);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);
      Dialog : PD_Dialog_Access;

   begin
      if Tab /= null then
         if Command_In_Process (Get_Process (Tab.Debugger)) then
            Button := Message_Dialog
              ((-"Cannot display protection domain list while the " &
                "debugger is busy.") &
               ASCII.LF &
               (-"Interrupt the debugger or wait for its availability."),
              Dialog_Type => Warning,
              Buttons => Button_OK);
            return;
         end if;

         if Top.PD_Dialog = null then
            Gtk_New (Dialog, Gtk_Window (Top));
            Top.PD_Dialog := Gtk_Dialog (Dialog);
         end if;

         Show_All (Top.PD_Dialog);
         Gdk_Raise (Get_Window (Top.PD_Dialog));
         Update (Dialog, Tab);
      end if;
   end On_PD;

   -------------------------
   -- On_Edit_Breakpoints --
   -------------------------

   procedure On_Edit_Breakpoints
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      Top     : constant GPS_Window := GPS_Window (Object);
      Process : constant Visual_Debugger := Get_Current_Process (Object);
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Action, Widget, Button);

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

      Top         : constant GPS_Window := GPS_Window (Object);
      Memory_View : GVD_Memory_View;
   begin
      if Top.Memory_View = null then
         Gtk_New (Memory_View, Gtk_Widget (Top));
         Top.Memory_View := Gtk_Window (Memory_View);
      end if;

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

      Process : constant Visual_Debugger := Get_Current_Process (Object);
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

      Process : constant Visual_Debugger := Get_Current_Process (Object);
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

      Process : constant Visual_Debugger := Get_Current_Process (Object);
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

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Debugger : Visual_Debugger) is
   begin
      --  ??? Should be able to remove this test at some point
      if Debugger = null
        or else Debugger.Debugger = null
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return;
      end if;

      declare
         Is_Func : aliased Boolean;
         Expression : constant String := Display_Entry_Dialog
           (Parent  => Debugger.Window,
            Title   => -"Expression Selection",
            Message => -"Enter an expression to display:",
            Key     => "gvd_display_expression_dialog",
            Check_Msg => -"Expression is a subprogram call",
            History   => Get_History (GPS_Window (Debugger.Window).Kernel),
            Key_Check => "expression_subprogram_debugger",
            Button_Active => Is_Func'Access);
      begin
         if Expression /= ""
           and then Expression (Expression'First) /= ASCII.NUL
         then
            if Is_Func then
               Process_User_Command
                 (Debugger,
                  "graph print `" & Expression & '`',
                  Output_Command => True);
            else
               Process_User_Command
                 (Debugger,
                  "graph display " & Expression,
                  Output_Command => True);
            end if;
         end if;
      end;
   end Display_Expression;

   ---------------------------
   -- On_Display_Expression --
   ---------------------------

   procedure On_Display_Expression
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
   begin
      Display_Expression (Get_Current_Process (Object));
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
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
      Iter : Item_Iterator;
      Item : Canvas_Item;
   begin
      if Tab /= null then
         Iter := Start (Tab.Data_Canvas);
         loop
            Item := Get (Iter);
            exit when Item = null;

            Display_Items.Update
              (GVD_Canvas (Tab.Data_Canvas),
               Display_Item (Item),
               Redisplay_Canvas => False);

            Next (Iter);
         end loop;
         Refresh_Canvas (Tab.Data_Canvas);
      end if;
   end On_Refresh;

end GVD.Menu;
