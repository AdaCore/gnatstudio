-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2003                        --
--                             ACT-Europe                            --
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

with Gtk.Menu;            use Gtk.Menu;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Debugger;            use Debugger;

package GVD.Call_Stack is

   type Call_Stack_Record is new Gtk_Scrolled_Window_Record with private;
   type Call_Stack is access all Call_Stack_Record'Class;

   type Stack_List_Mask is mod 2 ** 16;
   Frame_Num       : constant Stack_List_Mask := 2 ** 0;
   Program_Counter : constant Stack_List_Mask := 2 ** 1;
   Subprog_Name    : constant Stack_List_Mask := 2 ** 2;
   Params          : constant Stack_List_Mask := 2 ** 3;
   File_Location   : constant Stack_List_Mask := 2 ** 4;
   All_Info        : constant Stack_List_Mask := 2 ** 5 - 1;
   --  Lists the information to be displayed in the stack list window.

   procedure Gtk_New (Widget : out Call_Stack);
   --  Create a new call stack dialog.

   procedure Initialize (Widget : access Call_Stack_Record'Class);
   --  Internal initialization function.

   function Get_List_Mask (Stack : Call_Stack) return Stack_List_Mask;
   procedure Set_List_Mask (Stack : Call_Stack; Mask : Stack_List_Mask);
   --  Accessors for Stack.Backtrace_Mask.

   procedure Highlight_Frame (Stack : Call_Stack; Frame : Natural);
   --  Highlight frame number Frame.

   procedure Update (Stack : Call_Stack; Debugger : Debugger_Access);
   --  Update the call stack.

private

   type Call_Stack_Record is new Gtk_Scrolled_Window_Record with record
      Tree                       : Gtk_Tree_View;
      Model                      : Gtk_Tree_Store;
      Debugger                   : Debugger_Access;

      Call_Stack_Contextual_Menu : Gtk.Menu.Gtk_Menu;
      Block                      : Boolean := False;
      --  Whether to process selection events.

      Backtrace_Mask             : Stack_List_Mask := Subprog_Name;
      --  What columns to be displayed in the stack list window
   end record;

end GVD.Call_Stack;
