-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                 Copyright (C) 2004-2008, AdaCore                  --
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

--  This package contains the hooks that are used by the editors

with GPS.Kernel; use GPS.Kernel;

package Src_Editor_Buffer.Hooks is

   Word_Added_Hook : constant Hook_Name := "word_added";

   ------------------------
   -- File_Edition_Hooks --
   ------------------------

   File_Edition_Hook_Type : constant Hook_Type := "file_edition_hooks";

   type File_Edition_Hooks_Args is new File_Hooks_Args with record
      Character : Gunichar;
   end record;
   type File_Edition_Hooks_Args_Access is access all
     File_Edition_Hooks_Args'Class;

   Character_Added_Hook : constant Hook_Name := "character_added";

   procedure Location_Changed (Buffer : Source_Buffer);
   --  Emit the hook Cursor_Stopped_Hook

   procedure Word_Added (Buffer : Source_Buffer);
   --  Emit the hook Word_Added_Hook

   procedure Character_Added (Buffer : Source_Buffer; Character : Gunichar);
   --  Emit the Character_Added_Hook. Character should be 8 (control-H) when a
   --  character was removed from the buffer.

   procedure Register_Editor_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the hooks related to the source editors

end Src_Editor_Buffer.Hooks;
