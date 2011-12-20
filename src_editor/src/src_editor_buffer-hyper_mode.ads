------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
