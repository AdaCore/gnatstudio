------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Glib.Graphs;
with Gtkada.Canvas;

package Layouts is

   procedure Layer_Layout
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Graph  : Glib.Graphs.Graph;
      Force  : Boolean := False;
      Vertical_Layout : Boolean := True);
   --  Layout the graph in layers

   procedure Simple_Layout
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Graph  : Glib.Graphs.Graph;
      Force  : Boolean := False;
      Vertical_Layout : Boolean := True);
   --  Simple layout, where items are put in the first possible column

end Layouts;
