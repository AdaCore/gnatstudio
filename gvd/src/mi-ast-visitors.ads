------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

--  This package declares several general purpose implementations of this
--  interface.  To manipulate the AST or just walk through it, one should
--  extend MI.Ast.Visitor and implement its interface.  The all traversal can
--  then be done in a single call to this visitor on the top-level node of the
--  AST.

with MI.Ast; use MI.Ast;

package MI.Ast.Visitors is

   -------------------------------------
   -- Consistency visitor declaration --
   -------------------------------------

   type Consistency_Visitor is new Visitor with null record;
   --  One implementation of the Visitor interface which walks through the AST
   --  and ensure that there is no dangling nodes, i.e. pointer which are null,
   --  but shouldn't.

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Record_List);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_List_Value'Class);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_Pair);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_Record'Class);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Stream_Output_Record'Class);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : String_Value'Class);

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Value_List_Value'Class);

   -------------------------------------
   -- Dealloc visitor declaration --
   -------------------------------------

   type Dealloc_Visitor is new Mutable_Visitor with null record;
   --  One implementation of the Visitor interface which walks through the AST
   --  and Clear all elements

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Record_List);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_List_Value'Class);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_Pair);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_Record'Class);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Stream_Output_Record'Class);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out String_Value'Class);

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Value_List_Value'Class);

   -------------------------------------
   -- Record_List_Visitor declaration --
   -------------------------------------

   type Record_List_Visitor is new Visitor with null record;
   --  One implementation of the Visitor interface visiting only top level
   --  elements

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Record_List);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_List_Value'Class);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_Pair);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_Record'Class);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Stream_Output_Record'Class);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : String_Value'Class);

   overriding
   procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Value_List_Value'Class);

end MI.Ast.Visitors;
