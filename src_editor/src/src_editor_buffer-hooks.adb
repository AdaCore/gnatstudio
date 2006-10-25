-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2004-2006                       --
--                             AdaCore                               --
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
with GPS.Kernel;                use GPS.Kernel;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;

package body Src_Editor_Buffer.Hooks is

   type Src_File_Location_Hooks_Args is new File_Location_Hooks_Args with
      record
         Parent_Entity : Entities.Entity_Information;
      end record;
   function Compute_Parent_Entity
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Src_File_Location_Hooks_Args)
      return Entity_Information;
   procedure Destroy (Data : in out Src_File_Location_Hooks_Args);
   --  See inherited documentation

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Src_File_Location_Hooks_Args) is
   begin
      Unref (Data.Parent_Entity);
   end Destroy;

   ---------------------------
   -- Compute_Parent_Entity --
   ---------------------------

   function Compute_Parent_Entity
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Src_File_Location_Hooks_Args)
      return Entity_Information
   is
      Box : Source_Editor_Box;
   begin
      if Data.Parent_Entity = null then
         Box := Get_Source_Box_From_MDI
           (Find_Editor (Kernel, Data.File));
         if Box /= null then
            Data.Parent_Entity := Get_Subprogram
              (Box, Editable_Line_Type (Data.Line));
            Ref (Data.Parent_Entity);
         end if;
      end if;

      return Data.Parent_Entity;
   end Compute_Parent_Entity;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed (Buffer : Source_Buffer) is
      Data : aliased Src_File_Location_Hooks_Args :=
               (Hooks_Data with
                File          => Buffer.Filename,
                Line          => 0,
                Column        => 0,
                Parent_Entity => null);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI
        (Find_Editor (Get_Kernel (Buffer), Buffer.Filename));
   begin
      if Box /= null then
         Get_Cursor_Position
           (Get_Buffer (Box), Editable_Line_Type (Data.Line),
            Character_Offset_Type (Data.Column));
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
               (Hooks_Data with File => Buffer.Filename);
   begin
      Run_Hook (Buffer.Kernel, Word_Added_Hook, Data'Unchecked_Access, False);
   end Word_Added;

   ---------------------
   -- Character_Added --
   ---------------------

   procedure Character_Added (Buffer : Source_Buffer; Character : Gunichar) is
      Data : aliased File_Edition_Hooks_Args :=
        (Hooks_Data with File => Buffer.Filename, Character => Character);
   begin
      Run_Hook
        (Buffer.Kernel, Character_Added_Hook, Data'Unchecked_Access, False);
   end Character_Added;

   ---------------------------
   -- Register_Editor_Hooks --
   ---------------------------

   procedure Register_Editor_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Hook_No_Return
        (Kernel, Location_Changed_Hook, File_Location_Hook_Type);
      Register_Hook_No_Return
        (Kernel, Word_Added_Hook, File_Location_Hook_Type);
      Register_Hook_No_Return
        (Kernel, Character_Added_Hook, File_Edition_Hook_Type);
   end Register_Editor_Hooks;

end Src_Editor_Buffer.Hooks;
