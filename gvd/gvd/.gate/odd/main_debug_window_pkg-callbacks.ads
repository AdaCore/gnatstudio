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
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Open_Source1_Activate
     (Object : access Gtk_Widget_Record'Class);

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

   procedure On_Close1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Exit1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Undo1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Redo1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cut1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Copy1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Paste1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Select_All1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Search1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Preferences1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Gdb_Settings1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Run1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next_Instruction1_Activate
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
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Clear_Window1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Define_Command1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Edit_Buttons1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Call_Stack_Activate
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
     (Object : access Gtk_Widget_Record'Class);

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
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Show1_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Manual_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_About_Gvd_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Run1_Toolbar_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

end Main_Debug_Window_Pkg.Callbacks;
