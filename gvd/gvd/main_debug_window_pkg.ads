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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Button; use Gtk.Button;
with Gtk.Arrow; use Gtk.Arrow;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Odd_Preferences_Pkg; use Odd_Preferences_Pkg;
with Open_Program_Pkg; use Open_Program_Pkg;
with Breakpoints_Pkg; use Breakpoints_Pkg;
with Odd.Dialogs; use Odd.Dialogs;
with Print_Dialog_Pkg; use Print_Dialog_Pkg;
with Gtkada.Toolbar; use Gtkada.Toolbar;

package Main_Debug_Window_Pkg is

   type Main_Debug_Window_Record is new Gtk_Window_Record with record
      -----------------------
      -- Additional fields --
      -----------------------

      Odd_Preferences     : Odd_Preferences_Access;
      Open_Program        : Open_Program_Access;
      Breakpoints         : Breakpoints_Access;
      Task_Dialog         : Task_Dialog_Access;
      Backtrace_Dialog    : Backtrace_Dialog_Access;
      Print_Dialog        : Print_Dialog_Access;
      TTY_Mode            : Boolean := False;
      Debug_Mode          : Boolean := False;

      -------------------------

      Vbox1 : Gtk_Vbox;
      Menubar1 : Gtk_Menu_Bar;
      File1 : Gtk_Menu_Item;
      File1_Menu : Gtk_Menu;
      Open_Program1 : Gtk_Menu_Item;
      Open_Recent1 : Gtk_Menu_Item;
      Open_Core_Dump1 : Gtk_Menu_Item;
      Open_Source1 : Gtk_Menu_Item;
      Separator1 : Gtk_Menu_Item;
      Open_Session1 : Gtk_Menu_Item;
      Save_Session_As1 : Gtk_Menu_Item;
      Separator2 : Gtk_Menu_Item;
      Attach_To_Process1 : Gtk_Menu_Item;
      Detach_Process1 : Gtk_Menu_Item;
      Separator3 : Gtk_Menu_Item;
      Print_Graph1 : Gtk_Menu_Item;
      Change_Directory1 : Gtk_Menu_Item;
      Separator4 : Gtk_Menu_Item;
      Close1 : Gtk_Menu_Item;
      Restart1 : Gtk_Menu_Item;
      Exit1 : Gtk_Menu_Item;
      Edit2 : Gtk_Menu_Item;
      Edit2_Menu : Gtk_Menu;
      Undo3 : Gtk_Menu_Item;
      Redo1 : Gtk_Menu_Item;
      Separator5 : Gtk_Menu_Item;
      Cut1 : Gtk_Menu_Item;
      Copy1 : Gtk_Menu_Item;
      Paste1 : Gtk_Menu_Item;
      Clear1 : Gtk_Menu_Item;
      Delete1 : Gtk_Menu_Item;
      Separator6 : Gtk_Menu_Item;
      Select_All1 : Gtk_Menu_Item;
      Separator7 : Gtk_Menu_Item;
      Preferences1 : Gtk_Menu_Item;
      Gdb_Settings1 : Gtk_Menu_Item;
      Separator8 : Gtk_Menu_Item;
      Save_Options1 : Gtk_Menu_Item;
      View1 : Gtk_Menu_Item;
      View1_Menu : Gtk_Menu;
      Execution_Window1 : Gtk_Menu_Item;
      Separator9 : Gtk_Menu_Item;
      Gdb_Console1 : Gtk_Check_Menu_Item;
      Source_Window1 : Gtk_Check_Menu_Item;
      Data_Window1 : Gtk_Check_Menu_Item;
      Machine_Code_Window1 : Gtk_Check_Menu_Item;
      Program1 : Gtk_Menu_Item;
      Program1_Menu : Gtk_Menu;
      Run1 : Gtk_Menu_Item;
      Run_Again1 : Gtk_Menu_Item;
      Start1 : Gtk_Menu_Item;
      Separator10 : Gtk_Menu_Item;
      Run_In_Execution_Window1 : Gtk_Menu_Item;
      Separator11 : Gtk_Menu_Item;
      Step1 : Gtk_Menu_Item;
      Step_Instruction1 : Gtk_Menu_Item;
      Next1 : Gtk_Menu_Item;
      Next_Instruction1 : Gtk_Menu_Item;
      Until1 : Gtk_Menu_Item;
      Finish1 : Gtk_Menu_Item;
      Separator12 : Gtk_Menu_Item;
      Continue1 : Gtk_Menu_Item;
      Continue_Without_Signal1 : Gtk_Menu_Item;
      Separator13 : Gtk_Menu_Item;
      Kill1 : Gtk_Menu_Item;
      Interrupt1 : Gtk_Menu_Item;
      Abort1 : Gtk_Menu_Item;
      Commands1 : Gtk_Menu_Item;
      Commands1_Menu : Gtk_Menu;
      Command_History1 : Gtk_Menu_Item;
      Separator14 : Gtk_Menu_Item;
      Previous1 : Gtk_Menu_Item;
      Next2 : Gtk_Menu_Item;
      Separator15 : Gtk_Menu_Item;
      Find_Backward1 : Gtk_Menu_Item;
      Find_Forward1 : Gtk_Menu_Item;
      Quit_Search1 : Gtk_Menu_Item;
      Separator16 : Gtk_Menu_Item;
      Complete1 : Gtk_Menu_Item;
      Apply1 : Gtk_Menu_Item;
      Separator17 : Gtk_Menu_Item;
      Clear_Line1 : Gtk_Menu_Item;
      Clear_Window1 : Gtk_Menu_Item;
      Separator18 : Gtk_Menu_Item;
      Define_Command1 : Gtk_Menu_Item;
      Edit_Buttons1 : Gtk_Menu_Item;
      Status1 : Gtk_Menu_Item;
      Status1_Menu : Gtk_Menu;
      Backtrace1 : Gtk_Menu_Item;
      Registers1 : Gtk_Menu_Item;
      Threads1 : Gtk_Menu_Item;
      Processes1 : Gtk_Menu_Item;
      Signals1 : Gtk_Menu_Item;
      Source1 : Gtk_Menu_Item;
      Source1_Menu : Gtk_Menu;
      Lookup_1 : Gtk_Menu_Item;
      Find_1 : Gtk_Menu_Item;
      Find_2 : Gtk_Menu_Item;
      Separator21 : Gtk_Menu_Item;
      Find_Words_Only1 : Gtk_Check_Menu_Item;
      Find_Case_Sensitive1 : Gtk_Check_Menu_Item;
      Separator22 : Gtk_Menu_Item;
      Display_Line_Numbers1 : Gtk_Check_Menu_Item;
      Display_Machine_Code1 : Gtk_Menu_Item;
      Separator23 : Gtk_Menu_Item;
      Edit_Source1 : Gtk_Menu_Item;
      Reload_Source1 : Gtk_Menu_Item;
      Data1 : Gtk_Menu_Item;
      Data1_Menu : Gtk_Menu;
      Edit_Breakpoints1 : Gtk_Menu_Item;
      Edit_Displays1 : Gtk_Menu_Item;
      Edit_Watchpoints1 : Gtk_Menu_Item;
      Examine_Memory1 : Gtk_Menu_Item;
      Separator24 : Gtk_Menu_Item;
      Print_1 : Gtk_Menu_Item;
      Display_1 : Gtk_Menu_Item;
      Separator25 : Gtk_Menu_Item;
      Detect_Aliases1 : Gtk_Check_Menu_Item;
      Separator26 : Gtk_Menu_Item;
      Display_Local_Variables1 : Gtk_Check_Menu_Item;
      Display_Arguments1 : Gtk_Menu_Item;
      More_Status_Display1 : Gtk_Menu_Item;
      Separator27 : Gtk_Menu_Item;
      Align_On_Grid1 : Gtk_Menu_Item;
      Rotate_Graph1 : Gtk_Menu_Item;
      Layout_Graph1 : Gtk_Menu_Item;
      Separator28 : Gtk_Menu_Item;
      Refresh1 : Gtk_Menu_Item;
      Help1 : Gtk_Menu_Item;
      Help1_Menu : Gtk_Menu;
      Overview1 : Gtk_Menu_Item;
      On_Item1 : Gtk_Menu_Item;
      On_Window1 : Gtk_Menu_Item;
      Separator29 : Gtk_Menu_Item;
      What_Now_1 : Gtk_Menu_Item;
      Tip_Of_The_Day1 : Gtk_Menu_Item;
      Separator30 : Gtk_Menu_Item;
      Odd_Reference1 : Gtk_Menu_Item;
      Odd_News1 : Gtk_Menu_Item;
      Gdb_Reference1 : Gtk_Menu_Item;
      Separator31 : Gtk_Menu_Item;
      Odd_License1 : Gtk_Menu_Item;
      Odd_Www_Page1 : Gtk_Menu_Item;
      Separator32 : Gtk_Menu_Item;
      About_Odd1 : Gtk_Menu_Item;
      Toolbar1 : Gtk_Toolbar;
      Label53 : Gtk_Label;
      Combo6 : Gtk_Combo;
      Entry15 : Gtk_Entry;
      Button62 : Gtk_Widget;
      Button63 : Gtk_Widget;
      Button64 : Gtk_Widget;
      Button65 : Gtk_Widget;
      Button66 : Gtk_Widget;
      Button67 : Gtk_Widget;
      Button68 : Gtk_Widget;
      Button71 : Gtk_Widget;
      Toolbar2 : Gtkada_Toolbar;
      Button49 : Gtk_Widget;
      Button50 : Gtk_Widget;
      Button52 : Gtk_Widget;
      Button53 : Gtk_Widget;
      Button54 : Gtk_Widget;
      Button55 : Gtk_Widget;
      Button56 : Gtk_Widget;
      Button58 : Gtk_Widget;
      Button60 : Gtk_Widget;
      Button61 : Gtk_Widget;
      Button57 : Gtk_Widget;
      Button51 : Gtk_Widget;
      Frame7 : Gtk_Frame;
      Process_Notebook : Gtk_Notebook;
      Hbox1 : Gtk_Hbox;
      Button1 : Gtk_Button;
      Arrow1 : Gtk_Arrow;
      Statusbar1 : Gtk_Statusbar;
      Eventbox1 : Gtk_Event_Box;
      Checkbutton1 : Gtk_Check_Button;
   end record;
   type Main_Debug_Window_Access is access all Main_Debug_Window_Record'Class;

   procedure Gtk_New (Main_Debug_Window : out Main_Debug_Window_Access);
   procedure Initialize
     (Main_Debug_Window : access Main_Debug_Window_Record'Class);

end Main_Debug_Window_Pkg;
