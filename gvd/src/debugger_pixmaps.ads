------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Debugger_Pixmaps is

   Line_Has_Code_Pixbuf        : constant Unbounded_String :=
      To_Unbounded_String ("gps-emblem-debugger-line-has-code");
   Line_Might_Have_Code_Pixbuf : constant Unbounded_String :=
      To_Unbounded_String ("gps-emblem-debugger-line-might-have-code");
   Line_Has_Breakpoint_Pixbuf  : constant Unbounded_String :=
      To_Unbounded_String ("gps-emblem-debugger-breakpoint");
   Current_Line_Pixbuf         : constant Unbounded_String :=
      To_Unbounded_String ("gps-emblem-debugger-current");

end Debugger_Pixmaps;
