-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glide_Kernel;                use Glide_Kernel;
with Glide_Kernel.Scripts;        use Glide_Kernel.Scripts;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Kernel.Hooks;          use Glide_Kernel.Hooks;
with Glide_Intl;                  use Glide_Intl;

package body Src_Editor_Buffer.Hooks is

   Simple_Editor_Hook_Type : constant String := "simple_editor_hooks";

   procedure Simple_Editor_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles calls to run_hook from the shell for the simple editor hooks

   ------------------------------------
   -- Simple_Editor_Run_Hook_Handler --
   ------------------------------------

   procedure Simple_Editor_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Name   : constant String := Get_Hook_Name (Data, 1);
      Args   : File_Hooks_Args :=
        (Hooks_Data with
         Get_File (Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel)))));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args);
   end Simple_Editor_Run_Hook_Handler;

   --------------------
   -- Cursor_Stopped --
   --------------------

   procedure Cursor_Stopped (Buffer : Source_Buffer) is
      Data : constant File_Hooks_Args :=
        (Hooks_Data with File => Buffer.Filename);
   begin
      Run_Hook (Buffer.Kernel, Cursor_Stopped_Hook, Data, False);
   end Cursor_Stopped;

   ----------------
   -- Word_Added --
   ----------------

   procedure Word_Added (Buffer : Source_Buffer) is
      Data : constant File_Hooks_Args :=
        (Hooks_Data with File => Buffer.Filename);
   begin
      Run_Hook (Buffer.Kernel, Word_Added_Hook, Data, False);
   end Word_Added;

   ---------------------------
   -- Register_Editor_Hooks --
   ---------------------------

   procedure Register_Editor_Hooks
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Create_Hook_Type
        (Kernel, Simple_Editor_Hook_Type,
         -("Common type for all hooks related to source editors." & ASCII.LF
           & "Arguments are the following: (file)" & ASCII.LF),
         Hook_With_Args, Simple_Editor_Run_Hook_Handler'Access);

      Register_Hook
        (Kernel, Cursor_Stopped_Hook,
         -("Hook called when the cursor has stopped moving"),
         Type_Name => Simple_Editor_Hook_Type);

      Register_Hook
        (Kernel, Word_Added_Hook,
         -("Hook called when a word has been added in the editor"),
         Type_Name => Simple_Editor_Hook_Type);
   end Register_Editor_Hooks;

end Src_Editor_Buffer.Hooks;
