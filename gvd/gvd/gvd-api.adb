-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                              ACT-Europe                           --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk; use Gtk;
with Gtk.Arguments; use Gtk.Arguments;
with GVD.Types;
with GVD.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;
with Ada.Unchecked_Conversion;

package body GVD.API is

   function "+" is new
     Ada.Unchecked_Conversion (Main_Debug_Window, Main_Debug_Window_Access);

   function "-" is new
     Ada.Unchecked_Conversion (Main_Debug_Window_Access, Main_Debug_Window);

   function "+" is new
     Ada.Unchecked_Conversion
       (Debugger_Process_Tab, GVD.Process.Debugger_Process_Tab);

   function "-" is new
     Ada.Unchecked_Conversion
       (GVD.Process.Debugger_Process_Tab, Debugger_Process_Tab);

   ---------------
   -- About_GVD --
   ---------------

   procedure About_GVD (Window : Main_Debug_Window) is
   begin
      --  On_About_Odd1_Activate (+Window);
      null;
   end About_GVD;

   -----------------------
   -- Attach_To_Process --
   -----------------------

   procedure Attach_To_Process (Window : Main_Debug_Window) is
   begin
      On_Attach_To_Process1_Activate (+Window);
   end Attach_To_Process;

   ----------------
   -- Call_Stack --
   ----------------

   procedure Call_Stack (Window : Main_Debug_Window) is
   begin
      On_Call_Stack_Activate (+Window);
   end Call_Stack;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory (Window : Main_Debug_Window) is
   begin
      On_Change_Directory1_Activate (+Window);
   end Change_Directory;

   ----------------------
   -- Change_Line_Nums --
   ----------------------

   procedure Change_Line_Nums (Editor : Source_Editor; Toggle : Integer) is
   begin
      Set_Show_Line_Nums (+Editor, Boolean'Val (Toggle));
   end Change_Line_Nums;

   ----------------------------
   -- Change_Lines_With_Code --
   ----------------------------

   procedure Change_Lines_With_Code
     (Editor : Source_Editor;
      Toggle : Integer) is
   begin
      Set_Show_Lines_With_Code (+Editor, Boolean'Val (Toggle));
   end Change_Lines_With_Code;

   -----------------
   -- Change_Mode --
   -----------------

   procedure Change_Mode (Editor : Code_Editor; Mode : View_Mode) is
   begin
      null;
   end Change_Mode;

   ------------------
   -- Clear_Window --
   ------------------

   procedure Clear_Window (Window : Main_Debug_Window) is
   begin
      On_Clear_Window1_Activate (+Window);
   end Clear_Window;

   -----------
   -- Close --
   -----------

   procedure Close (Window : Main_Debug_Window) is
   begin
      On_Close1_Activate (+Window);
   end Close;

   --------------------
   -- Close_Debugger --
   --------------------

   procedure Close_Debugger (Debugger : Debugger_Process_Tab) is
   begin
      GVD.Process.Close_Debugger (+Debugger);
   end Close_Debugger;

   ---------------------
   -- Command_History --
   ---------------------

   procedure Command_History (Window : Main_Debug_Window) is
   begin
      On_Command_History1_Activate (+Window);
   end Command_History;

   --------------
   -- Continue --
   --------------

   procedure Continue (Window : Main_Debug_Window) is
   begin
      On_Continue1_Activate (+Window);
   end Continue;

   -----------------------------
   -- Continue_Without_Signal --
   -----------------------------

   procedure Continue_Without_Signal (Window : Main_Debug_Window) is
   begin
      On_Continue_Without_Signal1_Activate (+Window);
   end Continue_Without_Signal;

   ----------
   -- Copy --
   ----------

   procedure Copy (Window : Main_Debug_Window) is
   begin
      On_Copy1_Activate (+Window);
   end Copy;

   ---------------------
   -- Create_Debugger --
   ---------------------

   function Create_Debugger
     (Window          : Main_Debug_Window;
      Kind            : Debugger_Type;
      Executable      : chars_ptr;
      Params          : System.Address;
      N_Params        : Integer;
      Remote_Host     : chars_ptr;
      Remote_Target   : chars_ptr;
      Remote_Protocol : chars_ptr;
      Debugger_Name   : chars_ptr)
      return Debugger_Process_Tab
   is
      use Interfaces.C;

      Debugger     : Debugger_Process_Tab;
      Local_Params : Argument_List (1 .. N_Params);
      C_Params     : chars_ptr_array (1 .. size_t (N_Params));
      for C_Params'Address use Params;
      pragma Import (Ada, C_Params);

      function Str (S : chars_ptr) return String;

      function Str (S : chars_ptr) return String is
      begin
         return Value (S);
      exception
         when Dereference_Error =>
            return "";
      end Str;

   begin
      for J in Local_Params'Range loop
         Local_Params (J) := new String' (Value (C_Params (size_t (J))));
      end loop;

      Debugger := -GVD.Process.Create_Debugger
        (+Window,
         GVD.Types.Debugger_Type'Val (Debugger_Type'Pos (Kind)),
         Str (Executable),
         Local_Params,
         Str (Remote_Host),
         Str (Remote_Target),
         Str (Remote_Protocol),
         Str (Debugger_Name));

      for J in Local_Params'Range loop
         Free (Local_Params (J));
      end loop;

      return Debugger;
   end Create_Debugger;

   ------------------------
   -- Create_Main_Window --
   ------------------------

   function Create_Main_Window return Main_Debug_Window is
      Window : Main_Debug_Window_Access;
   begin
      Gtk_New (Window);
      return -Window;
   end Create_Main_Window;

   ---------
   -- Cut --
   ---------

   procedure Cut (Window : Main_Debug_Window) is
   begin
      On_Cut1_Activate (+Window);
   end Cut;

   -----------------------------
   -- Debug_Window_Get_Widget --
   -----------------------------

   function Debug_Window_Get_Widget
     (Window : Main_Debug_Window) return System.Address is
   begin
      return Get_Object (+Window);
   end Debug_Window_Get_Widget;

   --------------------
   -- Define_Command --
   --------------------

   procedure Define_Command (Window : Main_Debug_Window) is
   begin
      On_Define_Command1_Activate (+Window);
   end Define_Command;

   --------------------
   -- Detach_Process --
   --------------------

   procedure Detach_Process (Window : Main_Debug_Window) is
   begin
      On_Detach_Process1_Activate (+Window);
   end Detach_Process;

   -----------------------
   -- Display_Arguments --
   -----------------------

   procedure Display_Arguments (Window : Main_Debug_Window) is
   begin
      On_Display_Arguments1_Activate (+Window);
   end Display_Arguments;

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Window : Main_Debug_Window) is
   begin
      On_Display_Expression1_Activate (+Window);
   end Display_Expression;

   -----------------------------
   -- Display_Local_Variables --
   -----------------------------

   procedure Display_Local_Variables (Window : Main_Debug_Window) is
   begin
      On_Display_Local_Variables1_Activate (+Window);
   end Display_Local_Variables;

   -----------------------
   -- Display_Registers --
   -----------------------

   procedure Display_Registers (Window : Main_Debug_Window) is
   begin
      On_Display_Registers1_Activate (+Window);
   end Display_Registers;

   ----------
   -- Down --
   ----------

   procedure Down (Window : Main_Debug_Window) is
   begin
      On_Down1_Activate (+Window, Make_Args (0, System.Null_Address));
   end Down;

   ----------------------
   -- Edit_Breakpoints --
   ----------------------

   procedure Edit_Breakpoints (Window : Main_Debug_Window) is
   begin
      On_Edit_Breakpoints1_Activate (+Window);
   end Edit_Breakpoints;

   ------------------
   -- Edit_Buttons --
   ------------------

   procedure Edit_Buttons (Window : Main_Debug_Window) is
   begin
      On_Edit_Buttons1_Activate (+Window);
   end Edit_Buttons;

   -------------------
   -- Edit_Displays --
   -------------------

   procedure Edit_Displays (Window : Main_Debug_Window) is
   begin
      --  On_Edit_Displays1_Activate (+Window);
      null;
   end Edit_Displays;

   -----------------
   -- Edit_Source --
   -----------------

   procedure Edit_Source (Window : Main_Debug_Window) is
   begin
      On_Edit_Source1_Activate (+Window);
   end Edit_Source;

   --------------------
   -- Examine_Memory --
   --------------------

   procedure Examine_Memory (Window : Main_Debug_Window) is
   begin
      On_Examine_Memory1_Activate (+Window);
   end Examine_Memory;

   ------------
   -- Finish --
   ------------

   procedure Finish (Window : Main_Debug_Window) is
   begin
      On_Finish1_Activate (+Window);
   end Finish;

   ------------------
   -- Gdb_Settings --
   ------------------

   procedure Gdb_Settings (Window : Main_Debug_Window) is
   begin
      On_Gdb_Settings1_Activate (+Window);
   end Gdb_Settings;

   -------------------------
   -- Get_Current_Process --
   -------------------------

   function Get_Current_Process
     (Main_Window : Main_Debug_Window) return Debugger_Process_Tab is
   begin
      return -GVD.Process.Get_Current_Process (+Main_Window);
   end Get_Current_Process;

   ---------------
   -- GVD_Abort --
   ---------------

   procedure GVD_Abort (Window : Main_Debug_Window) is
   begin
      On_Abort1_Activate (+Window);
   end GVD_Abort;

   --------------
   -- GVD_Exit --
   --------------

   procedure GVD_Exit (Window : Main_Debug_Window) is
   begin
      On_Exit1_Activate (+Window);
   end GVD_Exit;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Window : Main_Debug_Window) is
   begin
      On_Interrupt1_Activate (+Window);
   end Interrupt;

   ----------
   -- Kill --
   ----------

   procedure Kill (Window : Main_Debug_Window) is
   begin
      On_Kill1_Activate (+Window);
   end Kill;

   -------------------------
   -- More_Status_Display --
   -------------------------

   procedure More_Status_Display (Window : Main_Debug_Window) is
   begin
      --  On_More_Status_Display1_Activate (+Window);
      null;
   end More_Status_Display;

   ----------
   -- Next --
   ----------

   procedure Next (Window : Main_Debug_Window) is
   begin
      On_Next1_Activate (+Window);
   end Next;

   ----------------------
   -- Next_Instruction --
   ----------------------

   procedure Next_Instruction (Window : Main_Debug_Window) is
   begin
      On_Next_Instruction1_Activate (+Window);
   end Next_Instruction;

   -------------
   -- On_Item --
   -------------

   procedure On_Item (Window : Main_Debug_Window) is
   begin
      --  On_On_Item1_Activate (+Window);
      null;
   end On_Item;

   --------------------
   -- Open_Core_Dump --
   --------------------

   procedure Open_Core_Dump (Window : Main_Debug_Window) is
   begin
      On_Open_Core_Dump1_Activate (+Window);
   end Open_Core_Dump;

   -------------------
   -- Open_Debugger --
   -------------------

   procedure Open_Debugger (Window : Main_Debug_Window) is
   begin
      On_Open_Debugger1_Activate (+Window);
   end Open_Debugger;

   ------------------
   -- Open_Program --
   ------------------

   procedure Open_Program (Window : Main_Debug_Window) is
   begin
      On_Open_Program1_Activate (+Window);
   end Open_Program;

   ------------------
   -- Open_Session --
   ------------------

   procedure Open_Session (Window : Main_Debug_Window) is
   begin
      On_Open_Session1_Activate (+Window);
   end Open_Session;

   --------------
   -- Overview --
   --------------

   procedure Overview (Window : Main_Debug_Window) is
   begin
      --  On_Overview1_Activate (+Window);
      null;
   end Overview;

   -----------
   -- Paste --
   -----------

   procedure Paste (Window : Main_Debug_Window) is
   begin
      On_Paste1_Activate (+Window);
   end Paste;

   -----------------
   -- Preferences --
   -----------------

   procedure Preferences (Window : Main_Debug_Window) is
   begin
      On_Preferences1_Activate (+Window);
   end Preferences;

   ----------------------------
   -- Process_Tab_Get_Widget --
   ----------------------------

   function Process_Tab_Get_Widget
     (Window : Debugger_Process_Tab) return System.Address is
   begin
      return Get_Object (+Window);
   end Process_Tab_Get_Widget;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : Debugger_Process_Tab;
      Command        : chars_ptr;
      Output_Command : Integer;
      Mode           : Visible_Command) is
   begin
      GVD.Process.Process_User_Command
        (+Debugger,
         Value (Command),
         Boolean'Val (Output_Command),
         GVD.Types.Visible_Command'Val
           (Visible_Command'Pos (Mode) +
            GVD.Types.Visible_Command'Pos (GVD.Types.Visible_Command'First)));
   end Process_User_Command;

   ---------------
   -- Processes --
   ---------------

   procedure Processes (Window : Main_Debug_Window) is
   begin
      On_Processes1_Activate (+Window);
   end Processes;

   ----------
   -- Redo --
   ----------

   procedure Redo (Window : Main_Debug_Window) is
   begin
      On_Redo1_Activate (+Window);
   end Redo;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Window : Main_Debug_Window) is
   begin
      On_Refresh1_Activate (+Window);
   end Refresh;

   -------------------
   -- Reload_Source --
   -------------------

   procedure Reload_Source (Window : Main_Debug_Window) is
   begin
      On_Reload_Source1_Activate (+Window);
   end Reload_Source;

   ---------
   -- Run --
   ---------

   procedure Run (Window : Main_Debug_Window) is
   begin
      On_Run1_Activate (+Window);
   end Run;

   -----------------
   -- Run_Toolbar --
   -----------------

   procedure Run_Toolbar (Window : Main_Debug_Window) is
   begin
      On_Run1_Toolbar_Activate (+Window, Make_Args (0, System.Null_Address));
   end Run_Toolbar;

   ---------------------
   -- Save_Session_As --
   ---------------------

   procedure Save_Session_As (Window : Main_Debug_Window) is
   begin
      On_Save_Session_As1_Activate (+Window);
   end Save_Session_As;

   ------------
   -- Search --
   ------------

   procedure Search (Window : Main_Debug_Window) is
   begin
      On_Search1_Activate (+Window);
   end Search;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Window : Main_Debug_Window) is
   begin
      On_Select_All1_Activate (+Window);
   end Select_All;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Process : Debugger_Process_Tab;
      File    : chars_ptr;
      Line    : Integer)
   is
   begin
      null;
   end Set_Breakpoint;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu (Editor : Source_Editor) is
   begin
      null;
   end Show_Current_Line_Menu;

   -------------
   -- Signals --
   -------------

   procedure Signals (Window : Main_Debug_Window) is
   begin
      On_Signals1_Activate (+Window);
   end Signals;

   -----------
   -- Start --
   -----------

   procedure Start (Window : Main_Debug_Window) is
   begin
      On_Start1_Activate (+Window, Make_Args (0, System.Null_Address));
   end Start;

   ----------
   -- Step --
   ----------

   procedure Step (Window : Main_Debug_Window) is
   begin
      On_Step1_Activate (+Window);
   end Step;

   ----------------------
   -- Step_Instruction --
   ----------------------

   procedure Step_Instruction (Window : Main_Debug_Window) is
   begin
      On_Step_Instruction1_Activate (+Window);
   end Step_Instruction;

   -------------
   -- Threads --
   -------------

   procedure Threads (Window : Main_Debug_Window) is
   begin
      On_Threads1_Activate (+Window);
   end Threads;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Process : Debugger_Process_Tab;
      File    : chars_ptr;
      Line    : Integer)
   is
   begin
      null;
   end Till_Breakpoint;

   --------------------
   -- Tip_Of_The_Day --
   --------------------

   procedure Tip_Of_The_Day (Window : Main_Debug_Window) is
   begin
      --  On_Tip_Of_The_Day1_Activate (+Window);
      null;
   end Tip_Of_The_Day;

   ----------
   -- Undo --
   ----------

   procedure Undo (Window : Main_Debug_Window) is
   begin
      On_Undo1_Activate (+Window);
   end Undo;

   --------
   -- Up --
   --------

   procedure Up (Window : Main_Debug_Window) is
   begin
      On_Up1_Activate (+Window, Make_Args (0, System.Null_Address));
   end Up;

   --------------
   -- What_Now --
   --------------

   procedure What_Now (Window : Main_Debug_Window) is
   begin
      --  On_What_Now_1_Activate (+Window);
      null;
   end What_Now;

end GVD.API;

