-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides a number of utilities to share functions amongst
--  users of the completion information.

with Ada.Containers.Doubly_Linked_Lists;

with Gtk.Widget; use Gtk.Widget;
with Pango.Font; use Pango.Font;

with GPS.Kernel; use GPS.Kernel;
with Completion; use Completion;

package Completion_Utils is

   package Proposals_List is new Ada.Containers.Doubly_Linked_Lists
     (Completion_Proposal_Access);

   function Proposal_Widget
     (Kernel           : Kernel_Handle;
      Fixed_Width_Font : Pango_Font_Description;
      Proposals        : Proposals_List.List) return Gtk_Widget;
   --  Return a widget representing Proposal

end Completion_Utils;
