-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                        Copyright (C) 2004-2005                    --
--                            AdaCore                                --
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

with Entities;                  use Entities;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel;                use GPS.Kernel;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;

package body Src_Editor_Buffer.Hooks is

   Simple_Editor_Hook_Type : constant String := "simple_editor_hooks";

   procedure Simple_Editor_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles calls to run_hook from the shell for the simple editor hooks

   type Src_File_Location_Hooks_Args is new File_Location_Hooks_Args with
      record
         Parent_Entity : Entities.Entity_Information;
      end record;
   function Compute_Parent_Entity
     (Data : access Src_File_Location_Hooks_Args)
      return Entity_Information;
   procedure Destroy (Data : in out Src_File_Location_Hooks_Args'Class);
   --  See inherited documentation

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Src_File_Location_Hooks_Args'Class) is
   begin
      Unref (Data.Parent_Entity);
   end Destroy;

   ---------------------------
   -- Compute_Parent_Entity --
   ---------------------------

   function Compute_Parent_Entity
     (Data : access Src_File_Location_Hooks_Args)
      return Entity_Information
   is
      Box : Source_Editor_Box;
   begin
      if Data.Parent_Entity = null then
         Box := Get_Source_Box_From_MDI
           (Find_Editor (Data.Kernel, Data.File));
         if Box /= null then
            Data.Parent_Entity := Get_Subprogram
              (Box, Editable_Line_Type (Data.Line));
            Ref (Data.Parent_Entity);
         end if;
      end if;

      return Data.Parent_Entity;
   end Compute_Parent_Entity;

   ------------------------------------
   -- Simple_Editor_Run_Hook_Handler --
   ------------------------------------

   procedure Simple_Editor_Run_Hook_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Name   : constant String := Get_Hook_Name (Data, 1);
      Args   : aliased File_Hooks_Args :=
        (Kernel,
         Get_File (Get_Data (Nth_Arg (Data, 2, Get_File_Class (Kernel)))));
      pragma Unreferenced (Command);
   begin
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end Simple_Editor_Run_Hook_Handler;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed (Buffer : Source_Buffer) is
      Data : aliased Src_File_Location_Hooks_Args :=
        (File          => Buffer.Filename,
         Kernel        => Get_Kernel (Buffer),
         Line          => 0,
         Column        => 0,
         Parent_Entity => null);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI
        (Find_Editor (Get_Kernel (Buffer), Buffer.Filename));
   begin
      if Box /= null then
         Get_Cursor_Location (Box, Data.Line, Data.Column);
         Run_Hook (Buffer.Kernel, Location_Changed_Hook,
                   Data'Unchecked_Access, False);
         Destroy (Data);
      end if;
   end Location_Changed;

   ----------------
   -- Word_Added --
   ----------------

   procedure Word_Added (Buffer : Source_Buffer) is
      Data : aliased  File_Hooks_Args :=
        (Kernel => Get_Kernel (Buffer),
         File   => Buffer.Filename);
   begin
      Run_Hook (Buffer.Kernel, Word_Added_Hook, Data'Unchecked_Access, False);
   end Word_Added;

   ---------------------------
   -- Register_Editor_Hooks --
   ---------------------------

   procedure Register_Editor_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Hook : Src_File_Location_Hooks_Args;
      pragma Warnings (Off, Hook);
   begin
      Create_Hook_Type
        (Kernel, Simple_Editor_Hook_Type,
         Hook_With_Args, Simple_Editor_Run_Hook_Handler'Access);

      Register_Hook
        (Kernel, Location_Changed_Hook,
         Type_Name => Get_Name (Hook));

      Register_Hook
        (Kernel, Word_Added_Hook,
         Type_Name => Simple_Editor_Hook_Type);
   end Register_Editor_Hooks;

end Src_Editor_Buffer.Hooks;
