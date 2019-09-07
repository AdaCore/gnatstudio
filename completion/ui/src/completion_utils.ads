------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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
with Gtk.Box;    use Gtk.Box;
with Pango.Font; use Pango.Font;

with GPS.Kernel; use GPS.Kernel;
with Completion; use Completion;

with Engine_Wrappers; use Engine_Wrappers;

package Completion_Utils is

   package Proposals_List
   is new Ada.Containers.Doubly_Linked_Lists (Root_Proposal_Access);

   type Notes_Window_Info is record
      Notes_Box      : Gtk_Vbox;
      C              : Proposals_List.Cursor := Proposals_List.No_Element;
      Multiple_Items : Boolean;
   end record;
   --  Structure representing the state of the notes floating window
   --  Notes is the Gtk_Widget corresponding to the notes window
   --  C points to the next item to be computed
   --  Multiple_Items indicates wether the box shall contain multiple items or
   --  not

   procedure Add_Next_Item_Doc
     (Notes_Info       : in out Notes_Window_Info;
      Kernel           : Kernel_Handle;
      Fixed_Width_Font : Pango_Font_Description);
   --  This procedure adds one of the 1 .. N entries to the notes/documentation
   --  floating window.
   --  Loading is designed this way so as to enable async loading of
   --  documentation entries

end Completion_Utils;
