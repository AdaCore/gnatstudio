-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2007, AdaCore               --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                   use Glib;

with GPS.Kernel;           use GPS.Kernel;
with Vdiff_Pkg;              use Vdiff_Pkg;
with Generic_List;

with Codefix;                use Codefix;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix_Module;

with Codefix_Window_Pkg;     use Codefix_Window_Pkg;

package Codefix.Graphics is

   Display_Lines_Before : constant Integer := 5;
   Display_Lines_After  : constant Integer := 5;

   procedure Free (This : in out Vdiff_Access);
   --  Free the memory associated to This.

   package Vdiff_Lists is new Generic_List (Vdiff_Access);
   use Vdiff_Lists;

   type Fix_Action is access procedure (Error : Error_Id);

   procedure No_Free (This : in out Error_Id);
   --  Do not free anything.

   package Error_Id_Lists is new Generic_List (Error_Id, No_Free);
   use Error_Id_Lists;

   type Status_Changed_Action is access procedure
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session      : access Codefix_Module.Codefix_Session_Record;
      Error        : Error_Id);
   --  Called when the status of an error has changed (was fixed or its fix
   --  was undone)

   type Graphic_Codefix_Record is new Codefix_Window_Record with record
      Session           : Codefix_Module.Codefix_Session;
      Successful_Update : Boolean;
      Nb_Tabs           : Integer := 0;
      Current_Error     : Error_Id := Null_Error_Id;
      Kernel            : Kernel_Handle;
      Vdiff_List        : Vdiff_Lists.List;
      Automatic_Skip    : State_List;
      Automatic_Fix     : State_List;
      Fixed_Cb          : Status_Changed_Action;
      Unfixed_Cb        : Status_Changed_Action;
      Fixes_List        : Error_Id_Lists.List;
   end record;

   type Graphic_Codefix_Access is access all Graphic_Codefix_Record;

   procedure Gtk_New
     (Graphic_Codefix : out Graphic_Codefix_Access;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session         : Codefix_Module.Codefix_Session;
      Fixed_Cb        : Status_Changed_Action := null;
      Unfixed_Cb      : Status_Changed_Action := null);
   --  Creates a new Graphic_Codefix. Fixed_Cb will be called after a fix, and
   --  Unfix_Cb after an Undo.

   procedure Initialize
     (Graphic_Codefix : access Graphic_Codefix_Record'Class;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Session         : Codefix_Module.Codefix_Session;
      Fixed_Cb        : Status_Changed_Action := null;
      Unfixed_Cb      : Status_Changed_Action := null);
   --  Display and load the first error of the Graphic_Codefix. If no error
   --  found, then only open a dialog.

   procedure Free (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Free the memory associated to a Graphic_Codefix.

   procedure Quit (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Terminate the window.

   procedure Next_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Display the next choice of solution for the current error.

   procedure Prev_Choice
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Display the previous choice of solution for the current error.

   procedure Load_Next_Error
     (Graphic_Codefix : access Graphic_Codefix_Record'Class;
      Warning_On_No_Error : Boolean := False);
   --  Load on the window the next error, and solutions associated to.

   procedure Load_Previous_Error
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Load on the window the previous fixable error.

   procedure Load_Error
     (Graphic_Codefix : access Graphic_Codefix_Record'Class;
      Success : out Boolean);
   --  Load or reload the current error.

   procedure Valid_Current_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Valid the current solution chosen by the user.

   function Get_Nth_Solution
     (Graphic_Codefix : access Graphic_Codefix_Record'Class) return Gint;
   --  Return the number of the current solution in the solution list.

   procedure Undo_Last_Fix
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Undo and reload the last error fixed in Graphic_Codefix.

   procedure Cancel_All_Fixes
     (Graphic_Codefix : access Graphic_Codefix_Record'Class);
   --  Cancel all the fixes that have been made in This.

end Codefix.Graphics;
