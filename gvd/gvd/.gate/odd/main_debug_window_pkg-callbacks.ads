with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Main_Debug_Window_Pkg.Callbacks is
   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Open_Program1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Open_Debugger1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Open_Core_Dump1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Edit_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Reload_Source1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Open_Session1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Save_Session_As1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Attach_To_Process1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Detach_Process1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Change_Directory1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Restart1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Exit1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Undo3_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Redo1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Search1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cut1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Copy1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Paste1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Clear1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Delete1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Select_All1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Gdb_Settings1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Run1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Run_Again1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Run_In_Execution_Window1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Until1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Continue_Without_Signal1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Kill1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Interrupt1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Abort1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Command_History1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Find_Backward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Find_Forward1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Clear_Line1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Clear_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Define_Command1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Edit_Buttons1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Backtrace1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Threads1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Processes1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Signals1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Edit_Breakpoints1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Edit_Displays1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Examine_Memory1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Display_Local_Variables1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Display_Arguments1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Display_Registers1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Display_Expression1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_More_Status_Display1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Refresh1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Overview1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_On_Item1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_On_Window1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_What_Now_1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Tip_Of_The_Day1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Odd_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Odd_News1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Gdb_Reference1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Odd_License1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Odd_Www_Page1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_About_Odd1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Print1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Display1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

end Main_Debug_Window_Pkg.Callbacks;
