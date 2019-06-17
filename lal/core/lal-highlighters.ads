------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  This package provides and interface that allows to highlight code
--  via libadalang.

private with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Common;

package LAL.Highlighters is

   type Highlightable_Interface is interface;
   --  An interface used to provide highlighting capabilities via Libadalang.
   --
   --  Implementors of this interface only have to define how to display tokens
   --  associated with a given style depending on the actual objects behind the
   --  scene (e.g: using Gtk_Text_Tags for Gtk_Text_View objects or Pango
   --  Markup for Gtk_Label objects).

   procedure Highlight_Token
     (Self  : in out Highlightable_Interface;
      Token : Libadalang.Common.Token_Reference;
      Style : String) is abstract;
   --  Highlight the given token with the designated style.
   --  This subprohgram is called automatically by Highlight_Using_Tree when
   --  mapping a token to a given style.

   procedure Remove_Highlighting
     (Self  : in out Highlightable_Interface;
      Style : String;
      From  : Integer;
      To    : Integer) is abstract;
   --  Remove the given style highlighting between the given lines.
   --  This subrpogram is called automatically when needed in
   --  Highlight_Using_Tree.

   function Highlight_Using_Tree
     (Self : in out Highlightable_Interface'Class;
      Unit : Libadalang.Analysis.Analysis_Unit;
      From : Integer := -1;
      To   : Integer := -1) return Boolean;
   --  Highlight the given highlightable object, traversing the given unit
   --  to associate each token with a style.
   --  When From and To are specified, only the nodes between these lines get
   --  traversed. Otherwise, the whole unit gets traversed and highlighted.
   --  Return True when the unit could be highlighted, False otherwise.

end LAL.Highlighters;
