------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

--  This is the root package of the MI API.  This API provides tools to
--  interact with GDB through its MI (Machine Interface) mode, instead of using
--  the CLI (Command Line Interface) mode.  This MI mode has been specialy
--  designed for IDEs and their communication with GDB.  This protocol is
--  text-based, and provides a nice output for program (read "provides an
--  easy-to-parse output").  The MI output grammar is well defined and
--  described in the MI.Ast.Parser package.
--
--  Among other things, this API provides both lexer and parser to manipulate
--  the RAW output of GDB's MI mode and build an AST-like structure.  Several
--  visitors tagged type are also provided for common walk and manipulation of
--  this AST.  Specific visitors for dedicated tasks can be implemented just by
--  extending the base type (i.e. implementing the MI.Ast.Visitors.Visitor
--  interface).

package MI is

   ------------------------
   -- Empty root package --
   ------------------------

end MI;
