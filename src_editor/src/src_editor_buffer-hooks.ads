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

--  This package contains the hooks that are used by the editors.

package Src_Editor_Buffer.Hooks is

   Cursor_Stopped_Hook : constant String := "cursor_stopped";

   procedure Cursor_Stopped (Buffer : Source_Buffer);
   --  Emit the hook Cursor_Stopped_Hook.

   procedure Register_Editor_Hooks
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the hooks related to the source editors.

end Src_Editor_Buffer.Hooks;
