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

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;  use Gtk.Enums;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Odd_Tools.Process; use Odd_Tools.Process;
with Odd.Pixmaps; use Odd.Pixmaps;

package body Main_Debug_Window_Pkg is

procedure Gtk_New (Main_Debug_Window : out Main_Debug_Window_Access) is
begin
   Main_Debug_Window := new Main_Debug_Window_Record;
   Main_Debug_Window_Pkg.Initialize (Main_Debug_Window);
end Gtk_New;

procedure Initialize (Main_Debug_Window : access Main_Debug_Window_Record'Class) is
   The_Accel_Group : Gtk_Accel_Group;
   Combo1_Items : String_List.Glist;
   Process : Process_Tab_Access;
   List : Argument_List (1 .. 0);

begin
   Gtk.Window.Initialize (Main_Debug_Window, Window_Toplevel);
   Window_Callback.Connect
     (Main_Debug_Window, "delete_event", On_Main_Debug_Window_Delete_Event'Access);
   Set_Title (Main_Debug_Window, -"The Other Display Debugger");
   Set_Policy (Main_Debug_Window, False, True, False);
   Set_Position (Main_Debug_Window, Win_Pos_None);
   Set_Modal (Main_Debug_Window, False);
   Set_Default_Size (Main_Debug_Window, 750, 600);

   Gtk_New_Vbox (Main_Debug_Window.Vbox1, False, 0);
   Add (Main_Debug_Window, Main_Debug_Window.Vbox1);

   Gtk_New (Main_Debug_Window.Menubar1);
   Pack_Start (Main_Debug_Window.Vbox1, Main_Debug_Window.Menubar1, False, False, 0);
   Set_Shadow_Type (Main_Debug_Window.Menubar1, Shadow_Out);

   Gtk_New (Main_Debug_Window.File1, -"File");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.File1);
   Set_Right_Justify (Main_Debug_Window.File1, False);

   Gtk_New (Main_Debug_Window.File1_Menu);
   Set_Submenu (Main_Debug_Window.File1, Main_Debug_Window.File1_Menu);

   Gtk_New (Main_Debug_Window.Open_Program1, -"Open Program...");
   Gtk_New (The_Accel_Group);
   Add_Accel_Group (Main_Debug_Window, The_Accel_Group);
   Add_Accelerator (Main_Debug_Window.Open_Program1, "activate",
     The_Accel_Group, GDK_O, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Open_Program1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Open_Program1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Open_Program1);
   Set_Right_Justify (Main_Debug_Window.Open_Program1, False);

   Gtk_New (Main_Debug_Window.Open_Recent1, -"Open Recent");
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Open_Recent1);
   Set_Right_Justify (Main_Debug_Window.Open_Recent1, False);

   Gtk_New (Main_Debug_Window.Open_Core_Dump1, -"Open Core Dump...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Open_Core_Dump1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Open_Core_Dump1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Open_Core_Dump1);
   Set_Right_Justify (Main_Debug_Window.Open_Core_Dump1, False);

   Gtk_New (Main_Debug_Window.Open_Source1, -"Open Source...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Open_Source1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Open_Source1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Open_Source1);
   Set_Right_Justify (Main_Debug_Window.Open_Source1, False);

   Gtk_New (Main_Debug_Window.Separator1);
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Separator1);
   Set_Right_Justify (Main_Debug_Window.Separator1, False);

   Gtk_New (Main_Debug_Window.Open_Session1, -"Open Session...");
   Add_Accelerator (Main_Debug_Window.Open_Session1, "activate",
     The_Accel_Group, GDK_N, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Open_Session1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Open_Session1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Open_Session1);
   Set_Right_Justify (Main_Debug_Window.Open_Session1, False);

   Gtk_New (Main_Debug_Window.Save_Session_As1, -"Save Session As...");
   Add_Accelerator (Main_Debug_Window.Save_Session_As1, "activate",
     The_Accel_Group, GDK_S, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Save_Session_As1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Save_Session_As1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Save_Session_As1);
   Set_Right_Justify (Main_Debug_Window.Save_Session_As1, False);

   Gtk_New (Main_Debug_Window.Separator2);
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Separator2);
   Set_Right_Justify (Main_Debug_Window.Separator2, False);

   Gtk_New (Main_Debug_Window.Attach_To_Process1, -"Attach To Process...");
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Attach_To_Process1, "activate",
      Widget_Callback.To_Marshaller (On_Attach_To_Process1_Activate'Access), Main_Debug_Window);
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Attach_To_Process1);
   Set_Right_Justify (Main_Debug_Window.Attach_To_Process1, False);

   Gtk_New (Main_Debug_Window.Detach_Process1, -"Detach Process");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Detach_Process1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Detach_Process1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Detach_Process1);
   Set_Right_Justify (Main_Debug_Window.Detach_Process1, False);

   Gtk_New (Main_Debug_Window.Separator3);
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Separator3);
   Set_Right_Justify (Main_Debug_Window.Separator3, False);

   Gtk_New (Main_Debug_Window.Print_Graph1, -"Print Graph...");
   Add_Accelerator (Main_Debug_Window.Print_Graph1, "activate",
     The_Accel_Group, GDK_P, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Print_Graph1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Print_Graph1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Print_Graph1);
   Set_Right_Justify (Main_Debug_Window.Print_Graph1, False);

   Gtk_New (Main_Debug_Window.Change_Directory1, -"Change Directory...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Change_Directory1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Change_Directory1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Change_Directory1);
   Set_Right_Justify (Main_Debug_Window.Change_Directory1, False);

   Gtk_New (Main_Debug_Window.Make1, -"Make...");
   Add_Accelerator (Main_Debug_Window.Make1, "activate",
     The_Accel_Group, GDK_M, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Make1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Make1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Make1);
   Set_Right_Justify (Main_Debug_Window.Make1, False);

   Gtk_New (Main_Debug_Window.Separator4);
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Separator4);
   Set_Right_Justify (Main_Debug_Window.Separator4, False);

   Gtk_New (Main_Debug_Window.Close1, -"Close");
   Add_Accelerator (Main_Debug_Window.Close1, "activate",
     The_Accel_Group, GDK_W, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Close1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Close1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Close1);
   Set_Right_Justify (Main_Debug_Window.Close1, False);

   Gtk_New (Main_Debug_Window.Restart1, -"Restart");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Restart1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Restart1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Restart1);
   Set_Right_Justify (Main_Debug_Window.Restart1, False);

   Gtk_New (Main_Debug_Window.Exit1, -"Exit");
   Add_Accelerator (Main_Debug_Window.Exit1, "activate",
     The_Accel_Group, GDK_Q, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Exit1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Exit1_Activate'Access));
   Add (Main_Debug_Window.File1_Menu, Main_Debug_Window.Exit1);
   Set_Right_Justify (Main_Debug_Window.Exit1, False);

   Gtk_New (Main_Debug_Window.Edit2, -"Edit");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Edit2);
   Set_Right_Justify (Main_Debug_Window.Edit2, False);

   Gtk_New (Main_Debug_Window.Edit2_Menu);
   Set_Submenu (Main_Debug_Window.Edit2, Main_Debug_Window.Edit2_Menu);

   Gtk_New (Main_Debug_Window.Undo3, -"Undo");
   Add_Accelerator (Main_Debug_Window.Undo3, "activate",
     The_Accel_Group, GDK_z, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Undo3, "activate",
      Menu_Item_Callback.To_Marshaller (On_Undo3_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Undo3);
   Set_Right_Justify (Main_Debug_Window.Undo3, False);

   Gtk_New (Main_Debug_Window.Redo1, -"Redo");
   Add_Accelerator (Main_Debug_Window.Redo1, "activate",
     The_Accel_Group, GDK_y, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Redo1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Redo1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Redo1);
   Set_Right_Justify (Main_Debug_Window.Redo1, False);

   Gtk_New (Main_Debug_Window.Separator5);
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Separator5);
   Set_Right_Justify (Main_Debug_Window.Separator5, False);

   Gtk_New (Main_Debug_Window.Cut1, -"Cut");
   Add_Accelerator (Main_Debug_Window.Cut1, "activate",
     The_Accel_Group, GDK_Delete, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Cut1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Cut1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Cut1);
   Set_Right_Justify (Main_Debug_Window.Cut1, False);

   Gtk_New (Main_Debug_Window.Copy1, -"Copy");
   Add_Accelerator (Main_Debug_Window.Copy1, "activate",
     The_Accel_Group, GDK_Insert, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Copy1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Copy1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Copy1);
   Set_Right_Justify (Main_Debug_Window.Copy1, False);

   Gtk_New (Main_Debug_Window.Paste1, -"Paste");
   Add_Accelerator (Main_Debug_Window.Paste1, "activate",
     The_Accel_Group, GDK_Insert, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Paste1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Paste1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Paste1);
   Set_Right_Justify (Main_Debug_Window.Paste1, False);

   Gtk_New (Main_Debug_Window.Clear1, -"Clear");
   Add_Accelerator (Main_Debug_Window.Clear1, "activate",
     The_Accel_Group, GDK_U, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Clear1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Clear1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Clear1);
   Set_Right_Justify (Main_Debug_Window.Clear1, False);

   Gtk_New (Main_Debug_Window.Delete1, -"Delete");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Delete1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Delete1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Delete1);
   Set_Right_Justify (Main_Debug_Window.Delete1, False);

   Gtk_New (Main_Debug_Window.Separator6);
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Separator6);
   Set_Right_Justify (Main_Debug_Window.Separator6, False);

   Gtk_New (Main_Debug_Window.Select_All1, -"Select All");
   Add_Accelerator (Main_Debug_Window.Select_All1, "activate",
     The_Accel_Group, GDK_A, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Select_All1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Select_All1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Select_All1);
   Set_Right_Justify (Main_Debug_Window.Select_All1, False);

   Gtk_New (Main_Debug_Window.Separator7);
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Separator7);
   Set_Right_Justify (Main_Debug_Window.Separator7, False);

   Gtk_New (Main_Debug_Window.Preferences1, -"Preferences...");
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Preferences1, "activate",
      Widget_Callback.To_Marshaller (On_Preferences1_Activate'Access), Main_Debug_Window);
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Preferences1);
   Set_Right_Justify (Main_Debug_Window.Preferences1, False);

   Gtk_New (Main_Debug_Window.Gdb_Settings1, -"GDB Settings...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Gdb_Settings1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Gdb_Settings1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Gdb_Settings1);
   Set_Right_Justify (Main_Debug_Window.Gdb_Settings1, False);

   Gtk_New (Main_Debug_Window.Separator8);
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Separator8);
   Set_Right_Justify (Main_Debug_Window.Separator8, False);

   Gtk_New (Main_Debug_Window.Save_Options1, -"Save Options");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Save_Options1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Save_Options1_Activate'Access));
   Add (Main_Debug_Window.Edit2_Menu, Main_Debug_Window.Save_Options1);
   Set_Right_Justify (Main_Debug_Window.Save_Options1, False);

   Gtk_New (Main_Debug_Window.View1, -"View");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.View1);
   Set_Right_Justify (Main_Debug_Window.View1, False);

   Gtk_New (Main_Debug_Window.View1_Menu);
   Set_Submenu (Main_Debug_Window.View1, Main_Debug_Window.View1_Menu);

   Gtk_New (Main_Debug_Window.Command_Tool1, -"Command Tool...");
   Add_Accelerator (Main_Debug_Window.Command_Tool1, "activate",
     The_Accel_Group, GDK_8, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Command_Tool1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Command_Tool1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Command_Tool1);
   Set_Right_Justify (Main_Debug_Window.Command_Tool1, False);

   Gtk_New (Main_Debug_Window.Execution_Window1, -"Execution Window...");
   Add_Accelerator (Main_Debug_Window.Execution_Window1, "activate",
     The_Accel_Group, GDK_9, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Execution_Window1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Execution_Window1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Execution_Window1);
   Set_Right_Justify (Main_Debug_Window.Execution_Window1, False);

   Gtk_New (Main_Debug_Window.Separator9);
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Separator9);
   Set_Right_Justify (Main_Debug_Window.Separator9, False);

   Gtk_New (Main_Debug_Window.Gdb_Console1, -"GDB Console");
   Add_Accelerator (Main_Debug_Window.Gdb_Console1, "activate",
     The_Accel_Group, GDK_1, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Gdb_Console1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Gdb_Console1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Gdb_Console1);
   Set_Active (Main_Debug_Window.Gdb_Console1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Gdb_Console1, False);

   Gtk_New (Main_Debug_Window.Source_Window1, -"Source Window");
   Add_Accelerator (Main_Debug_Window.Source_Window1, "activate",
     The_Accel_Group, GDK_2, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Source_Window1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Source_Window1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Source_Window1);
   Set_Active (Main_Debug_Window.Source_Window1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Source_Window1, False);

   Gtk_New (Main_Debug_Window.Data_Window1, -"Data Window");
   Add_Accelerator (Main_Debug_Window.Data_Window1, "activate",
     The_Accel_Group, GDK_3, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Data_Window1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Data_Window1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Data_Window1);
   Set_Active (Main_Debug_Window.Data_Window1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Data_Window1, False);

   Gtk_New (Main_Debug_Window.Machine_Code_Window1, -"Machine Code Window");
   Add_Accelerator (Main_Debug_Window.Machine_Code_Window1, "activate",
     The_Accel_Group, GDK_4, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Machine_Code_Window1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Machine_Code_Window1_Activate'Access));
   Add (Main_Debug_Window.View1_Menu, Main_Debug_Window.Machine_Code_Window1);
   Set_Active (Main_Debug_Window.Machine_Code_Window1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Machine_Code_Window1, False);

   Gtk_New (Main_Debug_Window.Program1, -"Program");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Program1);
   Set_Right_Justify (Main_Debug_Window.Program1, False);

   Gtk_New (Main_Debug_Window.Program1_Menu);
   Set_Submenu (Main_Debug_Window.Program1, Main_Debug_Window.Program1_Menu);

   Gtk_New (Main_Debug_Window.Run1, -"Run...");
   Add_Accelerator (Main_Debug_Window.Run1, "activate",
     The_Accel_Group, GDK_F2, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Run1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Run1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Run1);
   Set_Right_Justify (Main_Debug_Window.Run1, False);

   Gtk_New (Main_Debug_Window.Run_Again1, -"Run Again");
   Add_Accelerator (Main_Debug_Window.Run_Again1, "activate",
     The_Accel_Group, GDK_F3, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Run_Again1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Run_Again1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Run_Again1);
   Set_Right_Justify (Main_Debug_Window.Run_Again1, False);

   Gtk_New (Main_Debug_Window.Start1, -"Start");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Start1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Start1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Start1);
   Set_Right_Justify (Main_Debug_Window.Start1, False);

   Gtk_New (Main_Debug_Window.Separator10);
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Separator10);
   Set_Right_Justify (Main_Debug_Window.Separator10, False);

   Gtk_New (Main_Debug_Window.Run_In_Execution_Window1, -"Run in Execution Window");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Run_In_Execution_Window1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Run_In_Execution_Window1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Run_In_Execution_Window1);
   Set_Right_Justify (Main_Debug_Window.Run_In_Execution_Window1, False);

   Gtk_New (Main_Debug_Window.Separator11);
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Separator11);
   Set_Right_Justify (Main_Debug_Window.Separator11, False);

   Gtk_New (Main_Debug_Window.Step1, -"Step");
   Add_Accelerator (Main_Debug_Window.Step1, "activate",
     The_Accel_Group, GDK_F5, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Step1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Step1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Step1);
   Set_Right_Justify (Main_Debug_Window.Step1, False);

   Gtk_New (Main_Debug_Window.Step_Instruction1, -"Step Instruction");
   Add_Accelerator (Main_Debug_Window.Step_Instruction1, "activate",
     The_Accel_Group, GDK_F5, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Step_Instruction1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Step_Instruction1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Step_Instruction1);
   Set_Right_Justify (Main_Debug_Window.Step_Instruction1, False);

   Gtk_New (Main_Debug_Window.Next1, -"Next");
   Add_Accelerator (Main_Debug_Window.Next1, "activate",
     The_Accel_Group, GDK_F6, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Next1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Next1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Next1);
   Set_Right_Justify (Main_Debug_Window.Next1, False);

   Gtk_New (Main_Debug_Window.Next_Instruction1, -"Next Instruction");
   Add_Accelerator (Main_Debug_Window.Next_Instruction1, "activate",
     The_Accel_Group, GDK_F6, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Next_Instruction1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Next_Instruction1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Next_Instruction1);
   Set_Right_Justify (Main_Debug_Window.Next_Instruction1, False);

   Gtk_New (Main_Debug_Window.Until1, -"Until");
   Add_Accelerator (Main_Debug_Window.Until1, "activate",
     The_Accel_Group, GDK_F7, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Until1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Until1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Until1);
   Set_Right_Justify (Main_Debug_Window.Until1, False);

   Gtk_New (Main_Debug_Window.Finish1, -"Finish");
   Add_Accelerator (Main_Debug_Window.Finish1, "activate",
     The_Accel_Group, GDK_F8, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Finish1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Finish1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Finish1);
   Set_Right_Justify (Main_Debug_Window.Finish1, False);

   Gtk_New (Main_Debug_Window.Separator12);
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Separator12);
   Set_Right_Justify (Main_Debug_Window.Separator12, False);

   Gtk_New (Main_Debug_Window.Continue1, -"Continue");
   Add_Accelerator (Main_Debug_Window.Continue1, "activate",
     The_Accel_Group, GDK_F9, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Continue1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Continue1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Continue1);
   Set_Right_Justify (Main_Debug_Window.Continue1, False);

   Gtk_New (Main_Debug_Window.Continue_Without_Signal1, -"Continue without Signal");
   Add_Accelerator (Main_Debug_Window.Continue_Without_Signal1, "activate",
     The_Accel_Group, GDK_F9, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Continue_Without_Signal1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Continue_Without_Signal1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Continue_Without_Signal1);
   Set_Right_Justify (Main_Debug_Window.Continue_Without_Signal1, False);

   Gtk_New (Main_Debug_Window.Separator13);
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Separator13);
   Set_Right_Justify (Main_Debug_Window.Separator13, False);

   Gtk_New (Main_Debug_Window.Kill1, -"Kill");
   Add_Accelerator (Main_Debug_Window.Kill1, "activate",
     The_Accel_Group, GDK_F4, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Kill1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Kill1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Kill1);
   Set_Right_Justify (Main_Debug_Window.Kill1, False);

   Gtk_New (Main_Debug_Window.Interrupt1, -"Interrupt");
   Add_Accelerator (Main_Debug_Window.Interrupt1, "activate",
     The_Accel_Group, GDK_Escape, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Interrupt1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Interrupt1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Interrupt1);
   Set_Right_Justify (Main_Debug_Window.Interrupt1, False);

   Gtk_New (Main_Debug_Window.Abort1, -"Abort");
   Add_Accelerator (Main_Debug_Window.Abort1, "activate",
     The_Accel_Group, GDK_backslash, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Abort1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Abort1_Activate'Access));
   Add (Main_Debug_Window.Program1_Menu, Main_Debug_Window.Abort1);
   Set_Right_Justify (Main_Debug_Window.Abort1, False);

   Gtk_New (Main_Debug_Window.Commands1, -"Commands");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Commands1);
   Set_Right_Justify (Main_Debug_Window.Commands1, False);

   Gtk_New (Main_Debug_Window.Commands1_Menu);
   Set_Submenu (Main_Debug_Window.Commands1, Main_Debug_Window.Commands1_Menu);

   Gtk_New (Main_Debug_Window.Command_History1, -"Command History...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Command_History1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Command_History1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Command_History1);
   Set_Right_Justify (Main_Debug_Window.Command_History1, False);

   Gtk_New (Main_Debug_Window.Separator14);
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Separator14);
   Set_Right_Justify (Main_Debug_Window.Separator14, False);

   Gtk_New (Main_Debug_Window.Previous1, -"Previous");
   Add_Accelerator (Main_Debug_Window.Previous1, "activate",
     The_Accel_Group, GDK_Up, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Previous1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Previous1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Previous1);
   Set_Right_Justify (Main_Debug_Window.Previous1, False);

   Gtk_New (Main_Debug_Window.Next2, -"Next");
   Add_Accelerator (Main_Debug_Window.Next2, "activate",
     The_Accel_Group, GDK_Down, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Next2, "activate",
      Menu_Item_Callback.To_Marshaller (On_Next2_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Next2);
   Set_Right_Justify (Main_Debug_Window.Next2, False);

   Gtk_New (Main_Debug_Window.Separator15);
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Separator15);
   Set_Right_Justify (Main_Debug_Window.Separator15, False);

   Gtk_New (Main_Debug_Window.Find_Backward1, -"Find Backward");
   Add_Accelerator (Main_Debug_Window.Find_Backward1, "activate",
     The_Accel_Group, GDK_B, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_Backward1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_Backward1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Find_Backward1);
   Set_Right_Justify (Main_Debug_Window.Find_Backward1, False);

   Gtk_New (Main_Debug_Window.Find_Forward1, -"Find Forward");
   Add_Accelerator (Main_Debug_Window.Find_Forward1, "activate",
     The_Accel_Group, GDK_S, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_Forward1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_Forward1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Find_Forward1);
   Set_Right_Justify (Main_Debug_Window.Find_Forward1, False);

   Gtk_New (Main_Debug_Window.Quit_Search1, -"Quit Search");
   Add_Accelerator (Main_Debug_Window.Quit_Search1, "activate",
     The_Accel_Group, GDK_Escape, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Quit_Search1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Quit_Search1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Quit_Search1);
   Set_Right_Justify (Main_Debug_Window.Quit_Search1, False);

   Gtk_New (Main_Debug_Window.Separator16);
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Separator16);
   Set_Right_Justify (Main_Debug_Window.Separator16, False);

   Gtk_New (Main_Debug_Window.Complete1, -"Complete");
   Add_Accelerator (Main_Debug_Window.Complete1, "activate",
     The_Accel_Group, GDK_Tab, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Complete1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Complete1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Complete1);
   Set_Right_Justify (Main_Debug_Window.Complete1, False);

   Gtk_New (Main_Debug_Window.Apply1, -"Apply");
   Add_Accelerator (Main_Debug_Window.Apply1, "activate",
     The_Accel_Group, GDK_Return, 0, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Apply1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Apply1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Apply1);
   Set_Right_Justify (Main_Debug_Window.Apply1, False);

   Gtk_New (Main_Debug_Window.Separator17);
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Separator17);
   Set_Right_Justify (Main_Debug_Window.Separator17, False);

   Gtk_New (Main_Debug_Window.Clear_Line1, -"Clear Line");
   Add_Accelerator (Main_Debug_Window.Clear_Line1, "activate",
     The_Accel_Group, GDK_u, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Clear_Line1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Clear_Line1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Clear_Line1);
   Set_Right_Justify (Main_Debug_Window.Clear_Line1, False);

   Gtk_New (Main_Debug_Window.Clear_Window1, -"Clear Window");
   Add_Accelerator (Main_Debug_Window.Clear_Window1, "activate",
     The_Accel_Group, GDK_u, Gdk.Types.Control_Mask or Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Clear_Window1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Clear_Window1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Clear_Window1);
   Set_Right_Justify (Main_Debug_Window.Clear_Window1, False);

   Gtk_New (Main_Debug_Window.Separator18);
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Separator18);
   Set_Right_Justify (Main_Debug_Window.Separator18, False);

   Gtk_New (Main_Debug_Window.Define_Command1, -"Define Command...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Define_Command1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Define_Command1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Define_Command1);
   Set_Right_Justify (Main_Debug_Window.Define_Command1, False);

   Gtk_New (Main_Debug_Window.Edit_Buttons1, -"Edit Buttons...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Edit_Buttons1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit_Buttons1_Activate'Access));
   Add (Main_Debug_Window.Commands1_Menu, Main_Debug_Window.Edit_Buttons1);
   Set_Right_Justify (Main_Debug_Window.Edit_Buttons1, False);

   Gtk_New (Main_Debug_Window.Status1, -"Status");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Status1);
   Set_Right_Justify (Main_Debug_Window.Status1, False);

   Gtk_New (Main_Debug_Window.Status1_Menu);
   Set_Submenu (Main_Debug_Window.Status1, Main_Debug_Window.Status1_Menu);

   Gtk_New (Main_Debug_Window.Backtrace1, -"Backtrace...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Backtrace1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Backtrace1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Backtrace1);
   Set_Right_Justify (Main_Debug_Window.Backtrace1, False);

   Gtk_New (Main_Debug_Window.Registers1, -"Registers...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Registers1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Registers1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Registers1);
   Set_Right_Justify (Main_Debug_Window.Registers1, False);

   Gtk_New (Main_Debug_Window.Threads1, -"Threads...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Threads1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Threads1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Threads1);
   Set_Right_Justify (Main_Debug_Window.Threads1, False);

   Gtk_New (Main_Debug_Window.Tasks1, -"Tasks...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Tasks1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Tasks1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Tasks1);
   Set_Right_Justify (Main_Debug_Window.Tasks1, False);

   Gtk_New (Main_Debug_Window.Processus1, -"Processus...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Processus1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Processus1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Processus1);
   Set_Right_Justify (Main_Debug_Window.Processus1, False);

   Gtk_New (Main_Debug_Window.Signals1, -"Signals...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Signals1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Signals1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Signals1);
   Set_Right_Justify (Main_Debug_Window.Signals1, False);

   Gtk_New (Main_Debug_Window.Separator19);
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Separator19);
   Set_Right_Justify (Main_Debug_Window.Separator19, False);

   Gtk_New (Main_Debug_Window.Up1, -"Up");
   Add_Accelerator (Main_Debug_Window.Up1, "activate",
     The_Accel_Group, GDK_Up, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Up1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Up1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Up1);
   Set_Right_Justify (Main_Debug_Window.Up1, False);

   Gtk_New (Main_Debug_Window.Down1, -"Down");
   Add_Accelerator (Main_Debug_Window.Down1, "activate",
     The_Accel_Group, GDK_Down, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Down1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Down1_Activate'Access));
   Add (Main_Debug_Window.Status1_Menu, Main_Debug_Window.Down1);
   Set_Right_Justify (Main_Debug_Window.Down1, False);

   Gtk_New (Main_Debug_Window.Source1, -"Source");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Source1);
   Set_Right_Justify (Main_Debug_Window.Source1, False);

   Gtk_New (Main_Debug_Window.Source1_Menu);
   Set_Submenu (Main_Debug_Window.Source1, Main_Debug_Window.Source1_Menu);

   Gtk_New (Main_Debug_Window.Edit_Breakpoints1, -"Edit Breakpoints");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Edit_Breakpoints1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit_Breakpoints1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Edit_Breakpoints1);
   Set_Right_Justify (Main_Debug_Window.Edit_Breakpoints1, False);

   Gtk_New (Main_Debug_Window.Separator20);
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Separator20);
   Set_Right_Justify (Main_Debug_Window.Separator20, False);

   Gtk_New (Main_Debug_Window.Lookup_1, -"Lookup ()");
   Add_Accelerator (Main_Debug_Window.Lookup_1, "activate",
     The_Accel_Group, GDK_Slash, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Lookup_1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Lookup_1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Lookup_1);
   Set_Right_Justify (Main_Debug_Window.Lookup_1, False);

   Gtk_New (Main_Debug_Window.Find_1, -"Find>> ()");
   Add_Accelerator (Main_Debug_Window.Find_1, "activate",
     The_Accel_Group, GDK_decimalpoint, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Find_1);
   Set_Right_Justify (Main_Debug_Window.Find_1, False);

   Gtk_New (Main_Debug_Window.Find_2, -"Find<< ()");
   Add_Accelerator (Main_Debug_Window.Find_2, "activate",
     The_Accel_Group, GDK_comma, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_2, "activate",
      Menu_Item_Callback.To_Marshaller (On_Find_2_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Find_2);
   Set_Right_Justify (Main_Debug_Window.Find_2, False);

   Gtk_New (Main_Debug_Window.Separator21);
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Separator21);
   Set_Right_Justify (Main_Debug_Window.Separator21, False);

   Gtk_New (Main_Debug_Window.Find_Words_Only1, -"Find Words Only");
   Add_Accelerator (Main_Debug_Window.Find_Words_Only1, "activate",
     The_Accel_Group, GDK_W, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_Words_Only1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Find_Words_Only1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Find_Words_Only1);
   Set_Active (Main_Debug_Window.Find_Words_Only1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Find_Words_Only1, False);

   Gtk_New (Main_Debug_Window.Find_Case_Sensitive1, -"Find Case Sensitive");
   Add_Accelerator (Main_Debug_Window.Find_Case_Sensitive1, "activate",
     The_Accel_Group, GDK_I, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Find_Case_Sensitive1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Find_Case_Sensitive1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Find_Case_Sensitive1);
   Set_Active (Main_Debug_Window.Find_Case_Sensitive1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Find_Case_Sensitive1, False);

   Gtk_New (Main_Debug_Window.Separator22);
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Separator22);
   Set_Right_Justify (Main_Debug_Window.Separator22, False);

   Gtk_New (Main_Debug_Window.Display_Line_Numbers1, -"Display Line Numbers");
   Add_Accelerator (Main_Debug_Window.Display_Line_Numbers1, "activate",
     The_Accel_Group, GDK_N, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Display_Line_Numbers1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Display_Line_Numbers1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Display_Line_Numbers1);
   Set_Active (Main_Debug_Window.Display_Line_Numbers1, True);
   Set_Always_Show_Toggle (Main_Debug_Window.Display_Line_Numbers1, False);

   Gtk_New (Main_Debug_Window.Display_Machine_Code1, -"Display Machine Code");
   Add_Accelerator (Main_Debug_Window.Display_Machine_Code1, "activate",
     The_Accel_Group, GDK_4, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Display_Machine_Code1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Display_Machine_Code1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Display_Machine_Code1);
   Set_Right_Justify (Main_Debug_Window.Display_Machine_Code1, False);

   Gtk_New (Main_Debug_Window.Separator23);
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Separator23);
   Set_Right_Justify (Main_Debug_Window.Separator23, False);

   Gtk_New (Main_Debug_Window.Edit_Source1, -"Edit Source...");
   Add_Accelerator (Main_Debug_Window.Edit_Source1, "activate",
     The_Accel_Group, GDK_v, Gdk.Types.Control_Mask or Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Edit_Source1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit_Source1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Edit_Source1);
   Set_Right_Justify (Main_Debug_Window.Edit_Source1, False);

   Gtk_New (Main_Debug_Window.Reload_Source1, -"Reload Source");
   Add_Accelerator (Main_Debug_Window.Reload_Source1, "activate",
     The_Accel_Group, GDK_l, Gdk.Types.Control_Mask or Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Reload_Source1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Reload_Source1_Activate'Access));
   Add (Main_Debug_Window.Source1_Menu, Main_Debug_Window.Reload_Source1);
   Set_Right_Justify (Main_Debug_Window.Reload_Source1, False);

   Gtk_New (Main_Debug_Window.Data1, -"Data");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Data1);
   Set_Right_Justify (Main_Debug_Window.Data1, False);

   Gtk_New (Main_Debug_Window.Data1_Menu);
   Set_Submenu (Main_Debug_Window.Data1, Main_Debug_Window.Data1_Menu);

   Gtk_New (Main_Debug_Window.Edit_Displays1, -"Edit Displays...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Edit_Displays1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit_Displays1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Edit_Displays1);
   Set_Right_Justify (Main_Debug_Window.Edit_Displays1, False);

   Gtk_New (Main_Debug_Window.Edit_Watchpoints1, -"Edit Watchpoints...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Edit_Watchpoints1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit_Watchpoints1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Edit_Watchpoints1);
   Set_Right_Justify (Main_Debug_Window.Edit_Watchpoints1, False);

   Gtk_New (Main_Debug_Window.Examine_Memory1, -"Examine Memory...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Examine_Memory1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Examine_Memory1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Examine_Memory1);
   Set_Right_Justify (Main_Debug_Window.Examine_Memory1, False);

   Gtk_New (Main_Debug_Window.Separator24);
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Separator24);
   Set_Right_Justify (Main_Debug_Window.Separator24, False);

   Gtk_New (Main_Debug_Window.Print_1, -"Print ()");
   Add_Accelerator (Main_Debug_Window.Print_1, "activate",
     The_Accel_Group, GDK_equal, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Print_1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Print_1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Print_1);
   Set_Right_Justify (Main_Debug_Window.Print_1, False);

   Gtk_New (Main_Debug_Window.Display_1, -"Display ()");
   Add_Accelerator (Main_Debug_Window.Display_1, "activate",
     The_Accel_Group, GDK_minus, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Display_1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Display_1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Display_1);
   Set_Right_Justify (Main_Debug_Window.Display_1, False);

   Gtk_New (Main_Debug_Window.Separator25);
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Separator25);
   Set_Right_Justify (Main_Debug_Window.Separator25, False);

   Gtk_New (Main_Debug_Window.Detect_Aliases1, -"Detect Aliases");
   Add_Accelerator (Main_Debug_Window.Detect_Aliases1, "activate",
     The_Accel_Group, GDK_A, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Detect_Aliases1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Detect_Aliases1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Detect_Aliases1);
   Set_Active (Main_Debug_Window.Detect_Aliases1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Detect_Aliases1, False);

   Gtk_New (Main_Debug_Window.Separator26);
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Separator26);
   Set_Right_Justify (Main_Debug_Window.Separator26, False);

   Gtk_New (Main_Debug_Window.Display_Local_Variables1, -"Display Local Variables");
   Add_Accelerator (Main_Debug_Window.Display_Local_Variables1, "activate",
     The_Accel_Group, GDK_L, Gdk.Types.Mod1_Mask, Accel_Visible);
   Check_Menu_Item_Callback.Connect
     (Main_Debug_Window.Display_Local_Variables1, "activate",
      Check_Menu_Item_Callback.To_Marshaller (On_Display_Local_Variables1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Display_Local_Variables1);
   Set_Active (Main_Debug_Window.Display_Local_Variables1, False);
   Set_Always_Show_Toggle (Main_Debug_Window.Display_Local_Variables1, False);

   Gtk_New (Main_Debug_Window.Display_Arguments1, -"Display Arguments");
   Add_Accelerator (Main_Debug_Window.Display_Arguments1, "activate",
     The_Accel_Group, GDK_U, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Display_Arguments1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Display_Arguments1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Display_Arguments1);
   Set_Right_Justify (Main_Debug_Window.Display_Arguments1, False);

   Gtk_New (Main_Debug_Window.More_Status_Display1, -"More Status Display...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.More_Status_Display1, "activate",
      Menu_Item_Callback.To_Marshaller (On_More_Status_Display1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.More_Status_Display1);
   Set_Right_Justify (Main_Debug_Window.More_Status_Display1, False);

   Gtk_New (Main_Debug_Window.Separator27);
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Separator27);
   Set_Right_Justify (Main_Debug_Window.Separator27, False);

   Gtk_New (Main_Debug_Window.Align_On_Grid1, -"Align on Grid");
   Add_Accelerator (Main_Debug_Window.Align_On_Grid1, "activate",
     The_Accel_Group, GDK_G, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Align_On_Grid1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Align_On_Grid1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Align_On_Grid1);
   Set_Right_Justify (Main_Debug_Window.Align_On_Grid1, False);

   Gtk_New (Main_Debug_Window.Rotate_Graph1, -"Rotate Graph");
   Add_Accelerator (Main_Debug_Window.Rotate_Graph1, "activate",
     The_Accel_Group, GDK_R, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Rotate_Graph1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Rotate_Graph1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Rotate_Graph1);
   Set_Right_Justify (Main_Debug_Window.Rotate_Graph1, False);

   Gtk_New (Main_Debug_Window.Layout_Graph1, -"Layout Graph");
   Add_Accelerator (Main_Debug_Window.Layout_Graph1, "activate",
     The_Accel_Group, GDK_Y, Gdk.Types.Mod1_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Layout_Graph1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Layout_Graph1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Layout_Graph1);
   Set_Right_Justify (Main_Debug_Window.Layout_Graph1, False);

   Gtk_New (Main_Debug_Window.Separator28);
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Separator28);
   Set_Right_Justify (Main_Debug_Window.Separator28, False);

   Gtk_New (Main_Debug_Window.Refresh1, -"Refresh");
   Add_Accelerator (Main_Debug_Window.Refresh1, "activate",
     The_Accel_Group, GDK_L, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Refresh1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Refresh1_Activate'Access));
   Add (Main_Debug_Window.Data1_Menu, Main_Debug_Window.Refresh1);
   Set_Right_Justify (Main_Debug_Window.Refresh1, False);

   Gtk_New (Main_Debug_Window.Help1, -"Help");
   Add (Main_Debug_Window.Menubar1, Main_Debug_Window.Help1);
   Set_Right_Justify (Main_Debug_Window.Help1, True);

   Gtk_New (Main_Debug_Window.Help1_Menu);
   Set_Submenu (Main_Debug_Window.Help1, Main_Debug_Window.Help1_Menu);

   Gtk_New (Main_Debug_Window.Overview1, -"Overview...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Overview1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Overview1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Overview1);
   Set_Right_Justify (Main_Debug_Window.Overview1, False);

   Gtk_New (Main_Debug_Window.On_Item1, -"On Item...");
   Add_Accelerator (Main_Debug_Window.On_Item1, "activate",
     The_Accel_Group, GDK_F1, Gdk.Types.Shift_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.On_Item1, "activate",
      Menu_Item_Callback.To_Marshaller (On_On_Item1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.On_Item1);
   Set_Right_Justify (Main_Debug_Window.On_Item1, False);

   Gtk_New (Main_Debug_Window.On_Window1, -"On Window...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.On_Window1, "activate",
      Menu_Item_Callback.To_Marshaller (On_On_Window1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.On_Window1);
   Set_Right_Justify (Main_Debug_Window.On_Window1, False);

   Gtk_New (Main_Debug_Window.Separator29);
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Separator29);
   Set_Right_Justify (Main_Debug_Window.Separator29, False);

   Gtk_New (Main_Debug_Window.What_Now_1, -"What Now ... ?");
   Add_Accelerator (Main_Debug_Window.What_Now_1, "activate",
     The_Accel_Group, GDK_F1, Gdk.Types.Control_Mask, Accel_Visible);
   Menu_Item_Callback.Connect
     (Main_Debug_Window.What_Now_1, "activate",
      Menu_Item_Callback.To_Marshaller (On_What_Now_1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.What_Now_1);
   Set_Right_Justify (Main_Debug_Window.What_Now_1, False);

   Gtk_New (Main_Debug_Window.Tip_Of_The_Day1, -"Tip Of The Day...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Tip_Of_The_Day1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Tip_Of_The_Day1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Tip_Of_The_Day1);
   Set_Right_Justify (Main_Debug_Window.Tip_Of_The_Day1, False);

   Gtk_New (Main_Debug_Window.Separator30);
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Separator30);
   Set_Right_Justify (Main_Debug_Window.Separator30, False);

   Gtk_New (Main_Debug_Window.Odd_Reference1, -"ODD Reference...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Odd_Reference1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Odd_Reference1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Odd_Reference1);
   Set_Right_Justify (Main_Debug_Window.Odd_Reference1, False);

   Gtk_New (Main_Debug_Window.Odd_News1, -"ODD News...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Odd_News1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Odd_News1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Odd_News1);
   Set_Right_Justify (Main_Debug_Window.Odd_News1, False);

   Gtk_New (Main_Debug_Window.Gdb_Reference1, -"GDB Reference...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Gdb_Reference1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Gdb_Reference1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Gdb_Reference1);
   Set_Right_Justify (Main_Debug_Window.Gdb_Reference1, False);

   Gtk_New (Main_Debug_Window.Separator31);
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Separator31);
   Set_Right_Justify (Main_Debug_Window.Separator31, False);

   Gtk_New (Main_Debug_Window.Odd_License1, -"ODD License...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Odd_License1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Odd_License1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Odd_License1);
   Set_Right_Justify (Main_Debug_Window.Odd_License1, False);

   Gtk_New (Main_Debug_Window.Odd_Www_Page1, -"ODD WWW Page...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.Odd_Www_Page1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Odd_Www_Page1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Odd_Www_Page1);
   Set_Right_Justify (Main_Debug_Window.Odd_Www_Page1, False);

   Gtk_New (Main_Debug_Window.Separator32);
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.Separator32);
   Set_Right_Justify (Main_Debug_Window.Separator32, False);

   Gtk_New (Main_Debug_Window.About_Odd1, -"About ODD...");
   Menu_Item_Callback.Connect
     (Main_Debug_Window.About_Odd1, "activate",
      Menu_Item_Callback.To_Marshaller (On_About_Odd1_Activate'Access));
   Add (Main_Debug_Window.Help1_Menu, Main_Debug_Window.About_Odd1);
   Set_Right_Justify (Main_Debug_Window.About_Odd1, False);

   Gtk_New (Main_Debug_Window.Handlebox1);
   Pack_Start (Main_Debug_Window.Vbox1, Main_Debug_Window.Handlebox1, False, False, 0);
   Set_Shadow_Type (Main_Debug_Window.Handlebox1, Shadow_Out);
   Set_Handle_Position (Main_Debug_Window.Handlebox1, Pos_Left);
   Set_Snap_Edge (Main_Debug_Window.Handlebox1, Pos_Top);

   Gtk_New (Main_Debug_Window.Toolbar1, Orientation_Horizontal, Toolbar_Icons);
   Add (Main_Debug_Window.Handlebox1, Main_Debug_Window.Toolbar1);
   Set_Space_Size (Main_Debug_Window.Toolbar1, 5);
   Set_Space_Style (Main_Debug_Window.Toolbar1, Toolbar_Space_Empty);
   Set_Tooltips (Main_Debug_Window.Toolbar1, True);
   Set_Button_Relief (Main_Debug_Window.Toolbar1, Relief_None);

   Gtk_New (Main_Debug_Window.Label1, -("( ): "));
   Append_Widget (Main_Debug_Window.Toolbar1, Main_Debug_Window.Label1);
   Set_Alignment (Main_Debug_Window.Label1, 0.5, 0.5);
   Set_Padding (Main_Debug_Window.Label1, 0, 0);
   Set_Justify (Main_Debug_Window.Label1, Justify_Center);
   Set_Line_Wrap (Main_Debug_Window.Label1, False);

   Gtk_New (Main_Debug_Window.Combo1);
   Append_Widget (Main_Debug_Window.Toolbar1, Main_Debug_Window.Combo1);
   Set_Case_Sensitive (Main_Debug_Window.Combo1, False);
   Set_Use_Arrows (Main_Debug_Window.Combo1, True);
   Set_Use_Arrows_Always (Main_Debug_Window.Combo1, False);
   String_List.Append (Combo1_Items, -"");
   Combo.Set_Popdown_Strings (Main_Debug_Window.Combo1, Combo1_Items);
   Free_String_List (Combo1_Items);

   Main_Debug_Window.Button12 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Lookup () in the source",
      Icon => Gtk_Widget (Create_Pixmap (lookup_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button13 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Icon => Gtk_Widget (Create_Pixmap (findfwd_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button14 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Set/Delete breakpoint at ()",
      Icon => Gtk_Widget (Create_Pixmap (breakat_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button15 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Stop whenever () changes",
      Icon => Gtk_Widget (Create_Pixmap (watch_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button16 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Print ()",
      Icon => Gtk_Widget (Create_Pixmap (print_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button17 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Display ()",
      Icon => Gtk_Widget (Create_Pixmap (display_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button18 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Plot ()",
      Icon => Gtk_Widget (Create_Pixmap (plot_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button19 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Icon => Gtk_Widget (Create_Pixmap (show_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button20 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Icon => Gtk_Widget (Create_Pixmap (rotate_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button21 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Set the value of ()",
      Icon => Gtk_Widget (Create_Pixmap (set_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button22 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar1,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Icon => Gtk_Widget (Create_Pixmap (undisplay_xpm, Main_Debug_Window)));

   Main_Debug_Window.Entry1 := Get_Entry (Main_Debug_Window.Combo1);
   Set_Editable (Main_Debug_Window.Entry1, True);
   Set_Max_Length (Main_Debug_Window.Entry1, 0);
   Set_Text (Main_Debug_Window.Entry1, -"");
   Set_Visibility (Main_Debug_Window.Entry1, True);

   Gtk_New (Main_Debug_Window.Handlebox2);
   Pack_Start (Main_Debug_Window.Vbox1, Main_Debug_Window.Handlebox2, False, False, 0);
   Set_Shadow_Type (Main_Debug_Window.Handlebox2, Shadow_Out);
   Set_Handle_Position (Main_Debug_Window.Handlebox2, Pos_Left);
   Set_Snap_Edge (Main_Debug_Window.Handlebox2, Pos_Top);

   Gtk_New (Main_Debug_Window.Toolbar2, Orientation_Horizontal, Toolbar_Icons);
   Add (Main_Debug_Window.Handlebox2, Main_Debug_Window.Toolbar2);
   Set_Space_Size (Main_Debug_Window.Toolbar2, 5);
   Set_Space_Style (Main_Debug_Window.Toolbar2, Toolbar_Space_Empty);
   Set_Tooltips (Main_Debug_Window.Toolbar2, True);
   Set_Button_Relief (Main_Debug_Window.Toolbar2, Relief_None);
   Main_Debug_Window.Button23 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Start the debugged program",
      Icon => Gtk_Widget (Create_Pixmap (run_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button24 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Start the debugged program, stopping at the beginning of the main procedure",
      Icon => Gtk_Widget (Create_Pixmap (start_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button25 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Interrupt debugged program",
      Icon => Gtk_Widget (Create_Pixmap (interrupt_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button26 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step program until it reaches a different source line",
      Icon => Gtk_Widget (Create_Pixmap (step_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button27 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step one instruction exactly",
      Icon => Gtk_Widget (Create_Pixmap (stepi_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button28 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step program, proceeding through subroutine calls",
      Icon => Gtk_Widget (Create_Pixmap (next_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button29 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step one instruction, but proceed through subroutine calls",
      Icon => Gtk_Widget (Create_Pixmap (nexti_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button30 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Execute until the program reaches a source line greater than the current",
      Icon => Gtk_Widget (Create_Pixmap (until_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button31 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Execute until selected stack frame returns",
      Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button32 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Continue program being debugged, after signal or breakpoint",
      Icon => Gtk_Widget (Create_Pixmap (cont_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button33 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Kill execution of program being debugged",
      Icon => Gtk_Widget (Create_Pixmap (kill_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button34 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Select and print stack frame that called this one",
      Icon => Gtk_Widget (Create_Pixmap (up_xpm, Main_Debug_Window)));
   Main_Debug_Window.Button35 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar2,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Select and print stack frame called by this one",
      Icon => Gtk_Widget (Create_Pixmap (down_xpm, Main_Debug_Window)));

   Gtk_New (Main_Debug_Window.Process_Notebook);
   Pack_Start (Main_Debug_Window.Vbox1, Main_Debug_Window.Process_Notebook, True, True, 0);
   Set_Scrollable (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Border (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Tabs (Main_Debug_Window.Process_Notebook, True);
   Set_Tab_Hborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Vborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Pos (Main_Debug_Window.Process_Notebook, Pos_Top);

   Gtk_New (Process);
   Add (Main_Debug_Window.Process_Notebook, Process.Process_Paned);
   Create_Debugger (Process, List);

   Gtk_New (Main_Debug_Window.Label49, -("Processus 1"));
   Set_Alignment (Main_Debug_Window.Label49, 0.5, 0.5);
   Set_Padding (Main_Debug_Window.Label49, 0, 0);
   Set_Justify (Main_Debug_Window.Label49, Justify_Center);
   Set_Line_Wrap (Main_Debug_Window.Label49, False);
   Set_Tab (Main_Debug_Window.Process_Notebook, 0, Main_Debug_Window.Label49);

   Gtk_New_Hbox (Main_Debug_Window.Hbox1, False, 0);
   Pack_Start (Main_Debug_Window.Vbox1, Main_Debug_Window.Hbox1, False, False, 0);

   Gtk_New (Main_Debug_Window.Button1);
   Pack_Start (Main_Debug_Window.Hbox1, Main_Debug_Window.Button1, False, False, 0);

   Gtk_New (Main_Debug_Window.Arrow1, Arrow_Up, Shadow_Out);
   Add (Main_Debug_Window.Button1, Main_Debug_Window.Arrow1);
   Set_Alignment (Main_Debug_Window.Arrow1, 0.5, 0.5);
   Set_Padding (Main_Debug_Window.Arrow1, 0, 0);

   Gtk_New (Main_Debug_Window.Statusbar1);
   Pack_Start (Main_Debug_Window.Hbox1, Main_Debug_Window.Statusbar1, True, True, 0);

   Gtk_New (Main_Debug_Window.Eventbox1);
   Pack_Start (Main_Debug_Window.Hbox1, Main_Debug_Window.Eventbox1, False, False, 0);

   Gtk_New (Main_Debug_Window.Checkbutton1);
   Set_Sensitive (Main_Debug_Window.Checkbutton1, False);
   Add (Main_Debug_Window.Eventbox1, Main_Debug_Window.Checkbutton1);
   Set_Active (Main_Debug_Window.Checkbutton1, True);

end Initialize;

end Main_Debug_Window_Pkg;
