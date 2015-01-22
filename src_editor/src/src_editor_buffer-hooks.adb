------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Gtkada.MDI;                use Gtkada.MDI;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;
with GNATCOLL.Projects;         use GNATCOLL.Projects;

package body Src_Editor_Buffer.Hooks is

   type Src_File_Location_Hooks_Args is new File_Location_Hooks_Args with
     null record;
   overriding procedure Destroy (Data : in out Src_File_Location_Hooks_Args);
   --  See inherited documentation

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Data : in out Src_File_Location_Hooks_Args) is
   begin
      null;
   end Destroy;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed (Buffer : Source_Buffer) is
      --  Should let all views known, whichever project
      Project : constant Project_Type := No_Project;

      Data : aliased Src_File_Location_Hooks_Args :=
               (Hooks_Data with
                File          => Buffer.Filename,
                Project       => Project,
                Line          => 0,
                Column        => 0);
      Child : constant MDI_Child :=
        Find_Editor (Get_Kernel (Buffer), Buffer.Filename, Project);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
   begin
      if Box /= null then
         if Child = Get_MDI (Get_Kernel (Buffer)).Get_Focus_Child then
            Buffer.Kernel.Context_Changed
               (GPS_MDI_Child (Child).Build_Context);
         end if;

         Get_Cursor_Position
           (Get_Buffer (Box), Editable_Line_Type (Data.Line),
            Character_Offset_Type (Data.Column));
         Run_Hook (Buffer.Kernel, Location_Changed_Hook,
                   Data'Unchecked_Access);
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
      Run_Hook (Buffer.Kernel, Word_Added_Hook, Data'Unchecked_Access);
   end Word_Added;

   ---------------------
   -- Character_Added --
   ---------------------

   procedure Character_Added
     (Buffer      : Source_Buffer;
      Character   : Gunichar;
      Interactive : Boolean)
   is
      Data : aliased File_Edition_Hooks_Args :=
        (Hooks_Data
         with File   => Buffer.Filename,
         Character   => Character,
         Interactive => Interactive);
   begin
      Run_Hook
        (Buffer.Kernel, Character_Added_Hook, Data'Unchecked_Access);
   end Character_Added;

   ---------------------
   -- Buffer_Modified --
   ---------------------

   procedure Buffer_Modified (Buffer : Source_Buffer) is
      Data : aliased  File_Hooks_Args :=
               (Hooks_Data with File => Buffer.Filename);
   begin
      Run_Hook
        (Buffer.Kernel, Buffer_Modified_Hook, Data'Unchecked_Access);
   end Buffer_Modified;

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
