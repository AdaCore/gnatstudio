------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed (Buffer : Source_Buffer) is
      --  Should let all views known, whichever project
      Project : constant Project_Type := No_Project;
      Child : constant MDI_Child :=
        Find_Editor (Get_Kernel (Buffer), Buffer.Filename, Project);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
      Line : Editable_Line_Type;
      Column : Character_Offset_Type;
   begin
      if Box /= null then
         Get_Cursor_Position (Get_Buffer (Box), Line, Column);
         Location_Changed_Hook.Run
            (Kernel  => Buffer.Kernel,
             File    => Buffer.Filename,
             Line    => Integer (Line),
             Column  => Integer (Column),
             Project => Project);
      end if;
   end Location_Changed;

   ----------------
   -- Word_Added --
   ----------------

   procedure Word_Added (Buffer : Source_Buffer) is
   begin
      Word_Added_Hook.Run (Buffer.Kernel, File => Buffer.Filename);
   end Word_Added;

   ---------------------
   -- Character_Added --
   ---------------------

   procedure Character_Added
     (Buffer      : Source_Buffer;
      Character   : Gunichar;
      Interactive : Boolean) is
   begin
      Character_Added_Hook.Run
         (Buffer.Kernel, File => Buffer.Filename,
          Char => Character, Interactive => Interactive);
   end Character_Added;

   ---------------------
   -- Buffer_Modified --
   ---------------------

   procedure Buffer_Modified (Buffer : Source_Buffer) is
   begin
      Buffer_Edited_Hook.Run (Buffer.Kernel, Buffer.Filename);
   end Buffer_Modified;

end Src_Editor_Buffer.Hooks;
