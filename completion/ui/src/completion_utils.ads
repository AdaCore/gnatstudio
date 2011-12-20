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

--  This package provides a number of utilities to share functions amongst
--  users of the completion information.

with Ada.Containers.Doubly_Linked_Lists;

with Gtk.Widget; use Gtk.Widget;
with Pango.Font; use Pango.Font;

with GPS.Kernel; use GPS.Kernel;
with Completion; use Completion;

with Engine_Wrappers; use Engine_Wrappers;

package Completion_Utils is

   package Proposals_List is new Ada.Containers.Doubly_Linked_Lists
     (Root_Proposal_Access);

   function Proposal_Widget
     (Kernel           : Kernel_Handle;
      Fixed_Width_Font : Pango_Font_Description;
      Proposals        : Proposals_List.List) return Gtk_Widget;
   --  Return a widget representing Proposal

end Completion_Utils;
