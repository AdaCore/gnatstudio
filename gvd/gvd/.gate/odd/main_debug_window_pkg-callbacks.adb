with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Main_Debug_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Main_Debug_Window_Delete_Event;

   -------------------------------
   -- On_Open_Program1_Activate --
   -------------------------------

   procedure On_Open_Program1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
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
      null;
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

   ------------------------------
   -- On_Preferences1_Activate --
   ------------------------------

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
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
   begin
      null;
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
   begin
      null;
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
   begin
      null;
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
   begin
      null;
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
   begin
      null;
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
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
   begin
      null;
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
   begin
      null;
   end On_Backtrace1_Activate;

   --------------------------
   -- On_Threads1_Activate --
   --------------------------

   procedure On_Threads1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
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
   begin
      null;
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

   ------------------------
   -- On_Print1_Activate --
   ------------------------

   procedure On_Print1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Print1_Activate;

   --------------------------
   -- On_Display1_Activate --
   --------------------------

   procedure On_Display1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Display1_Activate;

   ------------------------------------------
   -- On_Display_Local_Variables1_Activate --
   ------------------------------------------

   procedure On_Display_Local_Variables1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Display_Local_Variables1_Activate;

   ------------------------------------
   -- On_Display_Arguments1_Activate --
   ------------------------------------

   procedure On_Display_Arguments1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Display_Arguments1_Activate;

   ------------------------------------
   -- On_Display_Registers1_Activate --
   ------------------------------------

   procedure On_Display_Registers1_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Display_Registers1_Activate;

   ---------------------------------------
   -- On_Display_Machine_Code1_Activate --
   ---------------------------------------

   procedure On_Display_Machine_Code1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Display_Machine_Code1_Activate;

   --------------------------------------
   -- On_More_Status_Display1_Activate --
   --------------------------------------

   procedure On_More_Status_Display1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_More_Status_Display1_Activate;

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
   begin
      null;
   end On_About_Odd1_Activate;

   -----------------------
   -- On_Print1_Clicked --
   -----------------------

   procedure On_Print1_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Print1_Clicked;

   -------------------------
   -- On_Display1_Clicked --
   -------------------------

   procedure On_Display1_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Display1_Clicked;

   ---------------------
   -- On_Up1_Activate --
   ---------------------

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Up1_Activate;

   -----------------------
   -- On_Down1_Activate --
   -----------------------

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Down1_Activate;

end Main_Debug_Window_Pkg.Callbacks;
