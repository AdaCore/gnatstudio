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
with Odd_Preferences_Pkg; use Odd_Preferences_Pkg;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Odd_Intl;            use Odd_Intl;
with Odd.Process;         use Odd.Process;
with Odd.Dialogs.Callbacks; use Odd.Dialogs.Callbacks;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Glib;                use Glib;
with Debugger;            use Debugger;
with Process_Proxies;     use Process_Proxies;
with Language;            use Language;
with Odd.Types;

package body Main_Debug_Window_Pkg.Callbacks is

   use Odd;
   use Gtk.Arguments;

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access);
   --  Close all the debuggers associated with a given main debug window
   --  by looking at all the pages of the main notebook.

   procedure Cleanup_Debuggers (Top : Main_Debug_Window_Access) is
      Tab  : Debugger_Process_Tab;
      Page_Num : Gint := 0;
      Page : Gtk_Widget;

   begin
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
      Program : Program_Descriptor;
      List    : Argument_List (1 .. 0);
      Process : Debugger_Process_Tab;
      Top     : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab     : Debugger_Process_Tab;

   begin
      Open_Program (Top.Open_Program, Program);

      case Program.Launch is
         when Current_Debugger =>
            Tab := Process_User_Data.Get
              (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
            Set_Executable (Tab.Debugger, Program.Program.all);

         when New_Debugger =>
            Process  :=
              Create_Debugger
                (Main_Debug_Window_Access (Object),
                 Program.Debugger,
                 Program.Program.all,
                 List,
                 Program.Remote_Host.all,
                 Program.Remote_Target.all,
                 Program.Protocol.all);

         when None =>
            null;
      end case;

      Free (Program);
   end On_Open_Program1_Activate;

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
   -- On_Open_Source1_Activate --
   ------------------------------

   procedure On_Open_Source1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top  : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab  : Debugger_Process_Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      List : Odd.Types.String_Array := Source_Files_List (Tab.Debugger);
   begin
      Odd.Types.Free (List);
   end On_Open_Source1_Activate;

   -------------------------------
   -- On_Open_Session1_Activate --
   -------------------------------

   procedure On_Open_Session1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Open_Session1_Activate;

   ----------------------------------
   -- On_Save_Session_As1_Activate --
   ----------------------------------

   procedure On_Save_Session_As1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Save_Session_As1_Activate;

   ------------------------------------
   -- On_Attach_To_Process1_Activate --
   ------------------------------------

   procedure On_Attach_To_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Attach_To_Process1_Activate;

   ---------------------------------
   -- On_Detach_Process1_Activate --
   ---------------------------------

   procedure On_Detach_Process1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Detach_Process1_Activate;

   ------------------------------
   -- On_Print_Graph1_Activate --
   ------------------------------

   procedure On_Print_Graph1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Print_Graph1_Activate;

   -----------------------------------
   -- On_Change_Directory1_Activate --
   -----------------------------------

   procedure On_Change_Directory1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Change_Directory1_Activate;

   ------------------------
   -- On_Close1_Activate --
   ------------------------

   procedure On_Close1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Close1_Activate;

   --------------------------
   -- On_Restart1_Activate --
   --------------------------

   procedure On_Restart1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Restart1_Activate;

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

   ------------------------
   -- On_Clear1_Activate --
   ------------------------

   procedure On_Clear1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear1_Activate;

   -------------------------
   -- On_Delete1_Activate --
   -------------------------

   procedure On_Delete1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Delete1_Activate;

   -----------------------------
   -- On_Select_All1_Activate --
   -----------------------------

   procedure On_Select_All1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Select_All1_Activate;

   ------------------------------
   -- On_Preferences1_Activate --
   ------------------------------

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
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

   -------------------------------
   -- On_Save_Options1_Activate --
   -------------------------------

   procedure On_Save_Options1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Save_Options1_Activate;

   -----------------------------------
   -- On_Execution_Window1_Activate --
   -----------------------------------

   procedure On_Execution_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Execution_Window1_Activate;

   ------------------------------
   -- On_Gdb_Console1_Activate --
   ------------------------------

   procedure On_Gdb_Console1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Gdb_Console1_Activate;

   --------------------------------
   -- On_Source_Window1_Activate --
   --------------------------------

   procedure On_Source_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Source_Window1_Activate;

   ------------------------------
   -- On_Data_Window1_Activate --
   ------------------------------

   procedure On_Data_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Data_Window1_Activate;

   --------------------------------------
   -- On_Machine_Code_Window1_Activate --
   --------------------------------------

   procedure On_Machine_Code_Window1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Machine_Code_Window1_Activate;

   ----------------------
   -- On_Run1_Activate --
   ----------------------

   procedure On_Run1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));

      if Command_In_Process (Get_Process (Tab.Debugger)) then
         return;
      end if;

      Run (Tab.Debugger, True);
   end On_Run1_Activate;

   ----------------------------
   -- On_Run_Again1_Activate --
   ----------------------------

   procedure On_Run_Again1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Run_Again1_Activate;

   ------------------------
   -- On_Start1_Activate --
   ------------------------

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Start (Tab.Debugger, True);
   end On_Start1_Activate;

   ------------------------------------------
   -- On_Run_In_Execution_Window1_Activate --
   ------------------------------------------

   procedure On_Run_In_Execution_Window1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Run_In_Execution_Window1_Activate;

   -----------------------
   -- On_Step1_Activate --
   -----------------------

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Step_Into (Tab.Debugger, True);
   end On_Step1_Activate;

   -----------------------------------
   -- On_Step_Instruction1_Activate --
   -----------------------------------

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Step_Instruction1_Activate;

   -----------------------
   -- On_Next1_Activate --
   -----------------------

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Step_Over (Tab.Debugger, True);
   end On_Next1_Activate;

   -----------------------------------
   -- On_Next_Instruction1_Activate --
   -----------------------------------

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Next_Instruction1_Activate;

   ------------------------
   -- On_Until1_Activate --
   ------------------------

   procedure On_Until1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Until1_Activate;

   -------------------------
   -- On_Finish1_Activate --
   -------------------------

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Finish (Tab.Debugger, True);
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Continue (Tab.Debugger, True);
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
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Interrupt (Tab.Debugger);
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
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Command_History1_Activate;

   ---------------------------
   -- On_Previous1_Activate --
   ---------------------------

   procedure On_Previous1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Previous1_Activate;

   -----------------------
   -- On_Next2_Activate --
   -----------------------

   procedure On_Next2_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Next2_Activate;

   --------------------------------
   -- On_Find_Backward1_Activate --
   --------------------------------

   procedure On_Find_Backward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Backward1_Activate;

   -------------------------------
   -- On_Find_Forward1_Activate --
   -------------------------------

   procedure On_Find_Forward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Forward1_Activate;

   ------------------------------
   -- On_Quit_Search1_Activate --
   ------------------------------

   procedure On_Quit_Search1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Quit_Search1_Activate;

   ---------------------------
   -- On_Complete1_Activate --
   ---------------------------

   procedure On_Complete1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Complete1_Activate;

   ------------------------
   -- On_Apply1_Activate --
   ------------------------

   procedure On_Apply1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Apply1_Activate;

   -----------------------------
   -- On_Clear_Line1_Activate --
   -----------------------------

   procedure On_Clear_Line1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear_Line1_Activate;

   -------------------------------
   -- On_Clear_Window1_Activate --
   -------------------------------

   procedure On_Clear_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Clear_Window1_Activate;

   ---------------------------------
   -- On_Define_Command1_Activate --
   ---------------------------------

   procedure On_Define_Command1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Define_Command1_Activate;

   -------------------------------
   -- On_Edit_Buttons1_Activate --
   -------------------------------

   procedure On_Edit_Buttons1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
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
      Top      : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab      : Debugger_Process_Tab;
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Internal : Boolean;

   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));

      Internal := Is_Internal_Command (Get_Process (Tab.Debugger));
      Push_Internal_Command_Status (Get_Process (Tab.Debugger), True);
      Backtrace (Tab.Debugger, Bt, Len);
      Pop_Internal_Command_Status (Get_Process (Tab.Debugger));

      if Top.Backtrace_Dialog = null then
         Gtk_New (Top.Backtrace_Dialog, Gtk_Window (Object), Bt (1 .. Len));
         Widget_Callback.Connect
           (Gtk_Widget (Tab), "context_changed",
            On_Backtrace_Process_Stopped'Access);
      else
         Update (Top.Backtrace_Dialog, Bt (1 .. Len));
      end if;

      Free (Bt (1 .. Len));
      Show_All (Top.Backtrace_Dialog);
   end On_Backtrace1_Activate;

   ----------------------------
   -- On_Registers1_Activate --
   ----------------------------

   procedure On_Registers1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Registers1_Activate;

   --------------------------
   -- On_Threads1_Activate --
   --------------------------

   procedure On_Threads1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Top      : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab      : Debugger_Process_Tab;
      Internal : Boolean;

   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));

      Internal := Is_Internal_Command (Get_Process (Tab.Debugger));
      Push_Internal_Command_Status (Get_Process (Tab.Debugger), True);

      declare
         Info : Thread_Information_Array := Info_Threads (Tab.Debugger);
      begin
         if Top.Task_Dialog = null then
            Gtk_New (Top.Task_Dialog, Gtk_Window (Object), Info);
            Widget_Callback.Connect
              (Gtk_Widget (Tab), "process_stopped",
               On_Task_Process_Stopped'Access);
         else
            Update (Top.Task_Dialog, Info);
         end if;

         Free (Info);
      end;

      Pop_Internal_Command_Status (Get_Process (Tab.Debugger));
      Show_All (Top.Task_Dialog);
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

   --------------------------
   -- On_Lookup_1_Activate --
   --------------------------

   procedure On_Lookup_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Lookup_1_Activate;

   ------------------------
   -- On_Find_1_Activate --
   ------------------------

   procedure On_Find_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_1_Activate;

   ------------------------
   -- On_Find_2_Activate --
   ------------------------

   procedure On_Find_2_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_2_Activate;

   ----------------------------------
   -- On_Find_Words_Only1_Activate --
   ----------------------------------

   procedure On_Find_Words_Only1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Words_Only1_Activate;

   --------------------------------------
   -- On_Find_Case_Sensitive1_Activate --
   --------------------------------------

   procedure On_Find_Case_Sensitive1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Find_Case_Sensitive1_Activate;

   ---------------------------------------
   -- On_Display_Line_Numbers1_Activate --
   ---------------------------------------

   procedure On_Display_Line_Numbers1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_Line_Numbers1_Activate;

   ---------------------------------------
   -- On_Display_Machine_Code1_Activate --
   ---------------------------------------

   procedure On_Display_Machine_Code1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_Machine_Code1_Activate;

   ------------------------------
   -- On_Edit_Source1_Activate --
   ------------------------------

   procedure On_Edit_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
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

   -----------------------------------
   -- On_Edit_Breakpoints1_Activate --
   -----------------------------------

   procedure On_Edit_Breakpoints1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Descriptor : Breakpoint_Descriptor;
   begin
      Breakpoint_Editor
        (Main_Debug_Window_Access (Object).Breakpoints, Descriptor);
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

   -----------------------------------
   -- On_Edit_Watchpoints1_Activate --
   -----------------------------------

   procedure On_Edit_Watchpoints1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit_Watchpoints1_Activate;

   ---------------------------------
   -- On_Examine_Memory1_Activate --
   ---------------------------------

   procedure On_Examine_Memory1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Examine_Memory1_Activate;

   -------------------------
   -- On_Print_1_Activate --
   -------------------------

   procedure On_Print_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Print_1_Activate;

   ---------------------------
   -- On_Display_1_Activate --
   ---------------------------

   procedure On_Display_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_1_Activate;

   ---------------------------------
   -- On_Detect_Aliases1_Activate --
   ---------------------------------

   procedure On_Detect_Aliases1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Detect_Aliases1_Activate;

   ------------------------------------------
   -- On_Display_Local_Variables1_Activate --
   ------------------------------------------

   procedure On_Display_Local_Variables1_Activate
     (Object : access Gtk_Check_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_Local_Variables1_Activate;

   ------------------------------------
   -- On_Display_Arguments1_Activate --
   ------------------------------------

   procedure On_Display_Arguments1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_Arguments1_Activate;

   --------------------------------------
   -- On_More_Status_Display1_Activate --
   --------------------------------------

   procedure On_More_Status_Display1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_More_Status_Display1_Activate;

   --------------------------------
   -- On_Align_On_Grid1_Activate --
   --------------------------------

   procedure On_Align_On_Grid1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Align_On_Grid1_Activate;

   -------------------------------
   -- On_Rotate_Graph1_Activate --
   -------------------------------

   procedure On_Rotate_Graph1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Rotate_Graph1_Activate;

   -------------------------------
   -- On_Layout_Graph1_Activate --
   -------------------------------

   procedure On_Layout_Graph1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Layout_Graph1_Activate;

   --------------------------
   -- On_Refresh1_Activate --
   --------------------------

   procedure On_Refresh1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
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
   -- On_On_Window1_Activate --
   ----------------------------

   procedure On_On_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_On_Window1_Activate;

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

   --------------------------------
   -- On_Odd_Reference1_Activate --
   --------------------------------

   procedure On_Odd_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_Reference1_Activate;

   ---------------------------
   -- On_Odd_News1_Activate --
   ---------------------------

   procedure On_Odd_News1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_News1_Activate;

   --------------------------------
   -- On_Gdb_Reference1_Activate --
   --------------------------------

   procedure On_Gdb_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Gdb_Reference1_Activate;

   ------------------------------
   -- On_Odd_License1_Activate --
   ------------------------------

   procedure On_Odd_License1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_License1_Activate;

   -------------------------------
   -- On_Odd_Www_Page1_Activate --
   -------------------------------

   procedure On_Odd_Www_Page1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Odd_Www_Page1_Activate;

   ----------------------------
   -- On_About_Odd1_Activate --
   ----------------------------

   procedure On_About_Odd1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        (-("ODD: The Other Display Debugger" & ASCII.LF & ASCII.LF &
           "(c) 2000 by Emmanuel Briot & Arnaud Charlet"),
         Help_Msg =>
           -("This is the About information box." & ASCII.LF & ASCII.LF &
             "Click on the OK button to close this window."),
         Title => -"About...");
   end On_About_Odd1_Activate;

   ------------------------
   -- On_Print1_Activate --
   ------------------------

   procedure On_Print1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;

      use type Odd.Types.String_Access;

   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));

      if Top.Print_Dialog = null then
         Gtk_New (Top.Print_Dialog);
      end if;

      Show_All (Top.Print_Dialog);
      Main;
      Hide (Top.Print_Dialog);

      if Top.Print_Dialog.Variable /= null then
         Process_User_Command
           (Tab, "graph print " & Top.Print_Dialog.Variable.all);
      end if;
   end On_Print1_Activate;

   ---------------------
   -- On_Up1_Activate --
   ---------------------

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Stack_Up (Tab.Debugger, True);
   end On_Up1_Activate;

   -----------------------
   -- On_Down1_Activate --
   -----------------------

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Top : Main_Debug_Window_Access := Main_Debug_Window_Access (Object);
      Tab : Debugger_Process_Tab;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Top.Process_Notebook)));
      Stack_Down (Tab.Debugger, True);
   end On_Down1_Activate;

end Main_Debug_Window_Pkg.Callbacks;
