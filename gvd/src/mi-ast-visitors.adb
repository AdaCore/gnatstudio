------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

package body MI.Ast.Visitors is

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Record_List)
   is
      Cursor : Record_Lists.Cursor := Object.First;
   begin
      while Record_Lists.Has_Element (Cursor) loop
         pragma Assert (Record_Lists.Element (Cursor) /= null);
         Record_Lists.Element (Cursor).all.Accept_Visitor (This);
         Cursor := Record_Lists.Next (Cursor);
      end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_List_Value'Class)
   is
      Cursor : Result_Pair_Lists.Cursor := Object.Value.First;
   begin
      while Result_Pair_Lists.Has_Element (Cursor) loop
         This.Visit (Result_Pair_Lists.Element (Cursor));
         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_Pair) is
   begin
      pragma Assert (Object.Variable /= null);

      if Object.Value /= null then
         Object.Value.all.Accept_Visitor (This);
      end if;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Result_Record'Class)
   is
      Cursor : Result_Pair_Lists.Cursor := Object.Results.First;
   begin
      pragma Assert (Object.Class /= null);

      while Result_Pair_Lists.Has_Element (Cursor) loop
         This.Visit (Result_Pair_Lists.Element (Cursor));
         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Stream_Output_Record'Class)
   is
      pragma Unreferenced (This);
   begin
      pragma Assert (Object.Content /= null);
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : String_Value'Class)
   is
      pragma Unreferenced (This);
   begin
      pragma Assert (Object.Value /= null);
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Consistency_Visitor;
      Object : Value_List_Value'Class)
   is
      Cursor : Value_Lists.Cursor := Object.Value.First;
   begin
      while Value_Lists.Has_Element (Cursor) loop
         pragma Assert (Value_Lists.Element (Cursor) /= null);
         Value_Lists.Element (Cursor).all.Accept_Visitor (This);
         Cursor := Value_Lists.Next (Cursor);
      end loop;
   end Visit;

   ------------------
   -- Declarations --
   ------------------

   procedure Free_String is
      new Ada.Unchecked_Deallocation (String, String_Access);
   --  Releases memory allocated for the given string.

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Record_List)
   is
      --  Cursor : Record_Lists.Cursor := Object.First;
      pragma Unreferenced (This);
   begin
      Clear_Record_List (Object);
      --  while Record_Lists.Has_Element (Cursor) loop
         --  pragma Assert (Record_Lists.Element (Cursor) /= null);
         --  Record_Lists.Element (Cursor).all.Accept_Visitor (This);
         --  Free_MI_Record (Record_Lists.Element (Cursor));
         --  Cursor := Record_Lists.Next (Cursor);
      --  end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_List_Value'Class)
   is
      pragma Unreferenced (This);
   begin
      Clear_Result_List_Value (Object);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_Pair)
   is
      pragma Unreferenced (This);
   begin
      Clear_Result_Pair (Object);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Result_Record'Class)
   is
      pragma Unreferenced (This);
   begin
      Clear_Result_Record (Object);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Stream_Output_Record'Class)
   is
      pragma Unreferenced (This);
   begin
      Free_String (Object.Content);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out String_Value'Class)
   is
      pragma Unreferenced (This);
   begin
      Clear_String_Value (Object);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding
   procedure Visit
     (This   : in out Dealloc_Visitor;
      Object : in out Value_List_Value'Class)
   is
      pragma Unreferenced (This);
   begin
      Clear_Value_List_Value (Object);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Record_List)
   is
      Cursor : Record_Lists.Cursor := Object.First;
   begin
      while Record_Lists.Has_Element (Cursor) loop
         Record_Lists.Element (Cursor).all.Accept_Visitor (This);
         Cursor := Record_Lists.Next (Cursor);
      end loop;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_List_Value'Class)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_Pair)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Result_Record'Class)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Stream_Output_Record'Class)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : String_Value'Class)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This   : in out Record_List_Visitor;
      Object : Value_List_Value'Class)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Object);
   begin
      null;
   end Visit;

end MI.Ast.Visitors;
