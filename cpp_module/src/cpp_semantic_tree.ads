------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

with Ada.Containers.Vectors;

with Basic_Types;     use Basic_Types;
with Glib;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with Language;        use Language;

package Cpp_Semantic_Tree is

   pragma Suppress (Container_Checks);
   package Token_List is
     new Ada.Containers.Vectors (Positive, Token_Record);

   type Parsed_Expression is record
      Tokens : Token_List.Vector;
   end record;

   Null_Parsed_Expression : constant Parsed_Expression;

   procedure Free (Expression : in out Parsed_Expression);
   --  Free memory associated with Expression

   function Parse_Expression_Backward
     (Buffer       : access constant Glib.UTF8_String;
      Start_Offset : String_Index_Type;
      End_Offset   : String_Index_Type := 0)
      return Parsed_Expression;
   --  Parse backwards the Buffer containing source C/C++ code. Start_Offset
   --  and End_Offset are offsets (in bytes) specifying the source to parse.
   --  Buffer must have a lifetime superior or equal to the resulting parser
   --  expression, as it gets referenced by this expression.
   --
   --  Example: If we have the following code: "A.Func (C).field" and
   --  Start_Offset points to "field", the returned parsed expression will
   --  contain 8 elements:
   --      Tok_Identifier + Tok_Dot + Tok_Identifier + Tok_Open_Parenthesis
   --      + Tok_Identifier + Tok_Close_Parenthesis + Tok_Dot + Tok_Identifdier
   --
   --  Note that Start_Offset must point on the d, or the last identifier
   --  returned will only contain part of the name.

private
   Test_Trace : constant Trace_Handle :=
     Create ("GPS.CPP.SEMANTIC_TREE_TEST", Off);

   Null_Parsed_Expression : constant Parsed_Expression :=
     (Tokens => Token_List.Empty_Vector);
end Cpp_Semantic_Tree;
