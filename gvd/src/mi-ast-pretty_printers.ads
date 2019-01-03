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

--  The MI.Ast.Pretty_Printers package provides several implementation of the
--  MI.Ast.Visitor interface dedicated to the display of the AST produced by
--  this API's parser.

with MI.Ast; use MI.Ast;

package MI.Ast.Pretty_Printers is

   type Default_Pretty_Printer is new Visitor with private;
   --  The Default_Pretty_Printer is a Visitor interface implementation which
   --  simply print the MI output on the standard output stream in a more human
   --  readable way (i.e. with newlines!).

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Record_List);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_List_Value'Class);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_Pair);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_Record'Class);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Stream_Output_Record'Class);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : String_Value'Class);

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Value_List_Value'Class);

private

   procedure Increment_Indent (This : in out Default_Pretty_Printer);

   procedure Decrement_Indent (This : in out Default_Pretty_Printer);

   procedure Print_Indent (This : in out Default_Pretty_Printer);

   type Default_Pretty_Printer is new Visitor with record
      Indent_Shift   : Natural := 0;
      Is_First_Brace : Boolean := False;
   end record;
   --  Declaration of the Default_Pretty_Printer attributes.  The Indent_Shift
   --  value is used to print the correct number of spaces when indenting the
   --  output.  The Is_First_Brace is a workaround when printint the first
   --  opening brace which is one the same line as the previous output: in that
   --  case, we should not be printing the indentation spaces.  This is the
   --  role of that boolean attribute.

end MI.Ast.Pretty_Printers;
