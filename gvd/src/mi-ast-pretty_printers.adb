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

with Ada.Text_IO; use Ada.Text_IO;

package body MI.Ast.Pretty_Printers is

   ----------------------
   -- Increment_Indent --
   ----------------------

   procedure Increment_Indent (This : in out Default_Pretty_Printer) is
   begin
      This.Indent_Shift := This.Indent_Shift + 2;
   end Increment_Indent;

   ----------------------
   -- Decrement_Indent --
   ----------------------

   procedure Decrement_Indent (This : in out Default_Pretty_Printer) is
   begin
      This.Indent_Shift := This.Indent_Shift - 2;
   end Decrement_Indent;

   ------------------
   -- Print_Indent --
   ------------------

   procedure Print_Indent (This : in out Default_Pretty_Printer) is
   begin
      Put ((1 .. This.Indent_Shift => ' '));
   end Print_Indent;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Record_List)
   is
      Cursor : Record_Lists.Cursor := Object.First;
   begin
      while Record_Lists.Has_Element (Cursor) loop
         Record_Lists.Element (Cursor).all.Accept_Visitor (This);
         New_Line;
         Cursor := Record_Lists.Next (Cursor);
      end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_List_Value'Class)
   is
      Cursor : Result_Pair_Lists.Cursor := Object.Value.First;
   begin
      if Object.Value.Is_Empty then
         if not This.Is_First_Brace then
            This.Print_Indent;
         end if;

         This.Is_First_Brace := False;
         Put ("{}");
         return;
      end if;

      --  The following if statement fixes a glitch in the output when printing
      --  a brace just after a colon.  In this case, we should not print the
      --  indentation spaces.

      if not This.Is_First_Brace then
         This.Print_Indent;
      end if;

      This.Is_First_Brace := False;
      Put_Line ("{");
      This.Increment_Indent;

      while Result_Pair_Lists.Has_Element (Cursor) loop
         This.Visit (Result_Pair_Lists.Element (Cursor));
         New_Line;
         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;

      This.Decrement_Indent;
      This.Print_Indent;
      Put ("}");
      This.Is_First_Brace := False;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_Pair) is
   begin
      This.Print_Indent;
      Put (Object.Variable.all & ": ");
      if Object.Value /= null then
         This.Is_First_Brace := True;
         Object.Value.all.Accept_Visitor (This);
      else
         Put ("[]");
      end if;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Result_Record'Class)
   is
      Cursor : Result_Pair_Lists.Cursor := Object.Results.First;
   begin
      case Object.R_Type is
         when Sync_Result =>
            Put ("(result");
         when Async_Exec =>
            Put ("(exec");
         when Async_Status =>
            Put ("(status");
         when Async_Notify =>
            Put ("(status");
      end case;

      Put (" <" & Object.Class.all & ">");

      if Object.Token /= -1 then
         Put (" [" & Integer'Image (Object.Token) & "]");
      end if;

      Put (")");

      if Object.Results.Is_Empty then
         return;
      end if;

      Put_Line (" {");
      This.Increment_Indent;

      while Result_Pair_Lists.Has_Element (Cursor) loop
         This.Visit (Result_Pair_Lists.Element (Cursor));
         New_Line;
         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;

      This.Decrement_Indent;
      This.Print_Indent;
      Put ("}");
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Stream_Output_Record'Class)
   is
      pragma Unreferenced (This);
   begin
      case Object.Output_Type is
         when Console =>
            Put ("(console) ");
         when Target =>
            Put ("(target) ");
         when Log =>
            Put ("(log) ");
      end case;

      Put (Object.Content.all);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : String_Value'Class)
   is
      pragma Unreferenced (This);
   begin
      Put (Object.Value.all);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Default_Pretty_Printer;
      Object : Value_List_Value'Class)
   is
      Cursor : Value_Lists.Cursor := Object.Value.First;
   begin
      if Object.Value.Is_Empty then
         if not This.Is_First_Brace then
            This.Print_Indent;
         end if;

         This.Is_First_Brace := False;
         Put ("{}");

         return;
      end if;

      --  The following if statement fixes a glitch in the output when printing
      --  a brace just after a colon.  In this case, we should not print the
      --  indentation spaces.

      if not This.Is_First_Brace then
         This.Print_Indent;
      end if;

      This.Is_First_Brace := False;
      Put_Line ("{");
      This.Increment_Indent;

      while Value_Lists.Has_Element (Cursor) loop
         Value_Lists.Element (Cursor).Accept_Visitor (This);
         New_Line;
         Cursor := Value_Lists.Next (Cursor);
      end loop;

      This.Decrement_Indent;
      This.Print_Indent;
      Put ("}");
   end Visit;

end MI.Ast.Pretty_Printers;
