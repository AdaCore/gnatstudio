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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Language; use Language;
with Language.Tree; use Language.Tree;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNAT.Regpat; use GNAT.Regpat;
with Language.Tree.Database; use Language.Tree.Database;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;

package body Codefix.SPARK_Parser is
   use Cursor_Lists;

   type Unexpected_Tilde_Or_Percent is new Error_Parser (4) with null record;

   overriding
   procedure Initialize (This : in out Unexpected_Tilde_Or_Percent);

   overriding
   procedure Fix
     (This         : Unexpected_Tilde_Or_Percent;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix Semantic Error 317: 'Tilde, in a function return annotation, may
   --    only be applied to an external variable of mode IN'
   --  Fix Semantic Error 318: 'Tilde or Percent may only be applied to
   --    variables'
   --  Fix Semantic Error 319: 'Tilde may only be applied to a variable which
   --    is both imported and exported'
   --  Fix Semantic Error 321: 'Tilde may not appear in pre-conditions'

   type Misplaced_Tilde_Or_Percent is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Misplaced_Tilde_Or_Percent);

   overriding
   procedure Fix
     (This         : Misplaced_Tilde_Or_Percent;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix Semantic Error 320: 'Tilde or Percent may only be applied to an
   --  entire variable'

   ---------------------------------
   -- Unexpected_Tilde_Or_Percent --
   ---------------------------------

   overriding procedure Initialize (This : in out Unexpected_Tilde_Or_Percent)
   is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
            (Compile ("Tilde, in a function return annotation, may " &
                      "only be applied to an external variable of mode IN")),
         2 => new Pattern_Matcher'
            (Compile ("Tilde or Percent may only be applied to variables")),
         3 => new Pattern_Matcher'
            (Compile ("Tilde may only be applied to a variable which " &
                      "is both imported and exported")),
         4 => new Pattern_Matcher'
            (Compile ("Tilde may not appear in pre-conditions")));
   end Initialize;

   overriding procedure Fix
     (This         : Unexpected_Tilde_Or_Percent;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Unexpected (Current_Text, Message, "(~|%)", Regular_Expression);
   end Fix;

   ---------------------------------
   -- Misplaced_Tilde_Or_Percent --
   ---------------------------------

   overriding procedure Initialize (This : in out Misplaced_Tilde_Or_Percent)
   is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
            (Compile ("Tilde or Percent may only be applied to an " &
                      "entire variable")));
   end Initialize;

   overriding procedure Fix
     (This         : Misplaced_Tilde_Or_Percent;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);
      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Move_Tilde_Or_Percent (Current_Text, Message);
   end Fix;

   ----------------------
   -- Register_Parsers --
   ----------------------

   procedure Register_Parsers (Processor : in out Fix_Processor) is
   begin
      Add_Parser (Processor, new Unexpected_Tilde_Or_Percent);
      Add_Parser (Processor, new Misplaced_Tilde_Or_Percent);
   end Register_Parsers;

end Codefix.SPARK_Parser;
