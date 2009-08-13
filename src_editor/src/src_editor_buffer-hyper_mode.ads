-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009, AdaCore                        --
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

--  This package describes the behavior of the Hyper Mode on source editor
--  buffers.

with Gtk.Text_Iter; use Gtk.Text_Iter;

package Src_Editor_Buffer.Hyper_Mode is

   procedure Hyper_Mode_Highlight_On
     (Buffer : Source_Buffer;
      Iter   : Gtk_Text_Iter);
   --  Tell the source buffer that the hyper mode is being requested at the
   --  coordinates given by Iter.

   procedure Remove_Highlight (Buffer : Source_Buffer);
   --  Remove highlighting left by the hyper mode, if any

   procedure Hyper_Mode_Click_On
     (Buffer    : Source_Buffer;
      Alternate : Boolean := False);
   --  React to a click on Iter while in hyper Mode.
   --  If Alternate is False, do the default behavior, otherwise do the
   --  alternate behavior.

   procedure Hyper_Mode_Enter (Buffer : Source_Buffer);
   --  Tell the source buffer that Hyper Mode is being activated

   procedure Hyper_Mode_Leave (Buffer : Source_Buffer);
   --  Tell the source buffer that Hyper Mode is being deactivated

end Src_Editor_Buffer.Hyper_Mode;
