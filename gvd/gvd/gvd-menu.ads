-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with Glib; use Glib;
with Factory_Data;

package GVD.Menu is
   use Factory_Data;

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Open_Program
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Open Program

   procedure On_Open_Core_Dump
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Open Core File

   procedure On_Add_Symbols
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Add Symbols

   procedure On_Attach_To_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Attach

   procedure On_Detach_Process
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Detach

   procedure On_Change_Directory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Change Directory

   procedure On_Run
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Run

   procedure On_Step
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Step

   procedure On_Step_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Step Instruction

   procedure On_Next
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Next

   procedure On_Next_Instruction
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Next Instruction

   procedure On_Finish
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Finish

   procedure On_Continue
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Continue

   procedure On_Kill
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Kill

   procedure On_Interrupt
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Program->Interrupt

   procedure On_Command_History
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Command->History

   procedure On_Clear_Window
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Command->Clear Window

   procedure On_Call_Stack
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Call Stack

   procedure On_Threads
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Threads

   procedure On_Tasks
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Tasks

   procedure On_PD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Protection Domains

   procedure On_Edit_Breakpoints
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Edit Breakpoints

   procedure On_Examine_Memory
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Examine Memory

   procedure On_Display_Local_Variables
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Display Local Variables

   procedure On_Display_Arguments
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Display Arguments

   procedure On_Display_Registers
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Display Registers

   procedure On_Display_Expression
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Display Expression

   procedure On_Refresh
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Data->Refresh

end GVD.Menu;
