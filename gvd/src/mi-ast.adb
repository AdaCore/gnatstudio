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

with MI.Ast.Visitors; use MI.Ast.Visitors;

package body MI.Ast is

   ----------------------------------
   -- Function "=" for Result_Pair --
   ----------------------------------

   overriding function "=" (Left, Right : Result_Pair) return Boolean is
   begin
      return Left.Variable.all = Right.Variable.all
        and then Left.Value.all = Right.Value.all;
   end "=";

   ---------------------
   -- Clear_MI_Record --
   ---------------------

   procedure Clear_MI_Record (Rec : MI_Record_Access) is
      V : Dealloc_Visitor;
   begin
      Rec.all.Accept_Visitor (V);
   end Clear_MI_Record;

   -----------------------
   -- Clear_Record_List --
   -----------------------

   procedure Clear_Record_List (List : in out Record_List) is
      Cursor : Record_Lists.Cursor := Record_Lists.First (List);
      Rec    : MI_Record_Access;
   begin
      while Record_Lists.Has_Element (Cursor) loop
         Rec := Record_Lists.Element (Cursor);
         Clear_MI_Record (Rec);
         Free_MI_Record (Rec);
         Cursor := Record_Lists.Next (Cursor);
      end loop;

      List.Clear;
   end Clear_Record_List;

   --------------------
   -- Clear_MI_Value --
   --------------------

   procedure Clear_MI_Value (Value : MI_Value_Access) is
      V : Dealloc_Visitor;
   begin
      Value.all.Accept_Visitor (V);
   end Clear_MI_Value;

   ----------------------
   -- Clear_Value_List --
   ----------------------

   procedure Clear_Value_List (List : in out Value_List) is
      Cursor : Value_Lists.Cursor := Value_Lists.First (List);
      Value  : MI_Value_Access;
   begin
      while Value_Lists.Has_Element (Cursor) loop
         Value := Value_Lists.Element (Cursor);
         Clear_MI_Value (Value);
         Free_MI_Value (Value);
         Cursor := Value_Lists.Next (Cursor);
      end loop;

      List.Clear;
   end Clear_Value_List;

   -----------------------
   -- Clear_Result_Pair --
   -----------------------

   procedure Clear_Result_Pair (Pair : in out Result_Pair) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);
   begin
      if Pair.Variable /= null then
         Unchecked_Free (Pair.Variable);
         Pair.Variable := null;
      end if;

      if Pair.Value /= null then
         Clear_MI_Value (Pair.Value);
         Free_MI_Value (Pair.Value);
         Pair.Value := null;
      end if;
   end Clear_Result_Pair;

   ----------------------------
   -- Clear_Result_Pair_List --
   ----------------------------

   procedure Clear_Result_Pair_List (List : in out Result_Pair_List) is
      Cursor : Result_Pair_Lists.Cursor := Result_Pair_Lists.First (List);
      Pair   : Result_Pair;
   begin
      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Clear_Result_Pair (Pair);
         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;

      List.Clear;
   end Clear_Result_Pair_List;

   ------------------------
   -- Clear_String_Value --
   ------------------------

   procedure Clear_String_Value (Value : in out String_Value) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);
   begin
      if Value.Value /= null then
         Unchecked_Free (Value.Value);
         Value.Value := null;
      end if;
   end Clear_String_Value;

   -----------------------------
   -- Clear_Result_List_Value --
   -----------------------------

   procedure Clear_Result_List_Value (List : in out Result_List_Value) is
   begin
      Clear_Result_Pair_List (List.Value);
   end Clear_Result_List_Value;

   ----------------------------
   -- Clear_Value_List_Value --
   ----------------------------

   procedure Clear_Value_List_Value (List : in out Value_List_Value) is
   begin
      Clear_Value_List (List.Value);
   end Clear_Value_List_Value;

   -------------------------
   -- Clear_Result_Record --
   -------------------------

   procedure Clear_Result_Record (Rec : in out Result_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);
   begin
      if Rec.Class /= null then
         Unchecked_Free (Rec.Class);
         Rec.Class := null;
      end if;

      Clear_Result_Pair_List (Rec.Results);
   end Clear_Result_Record;

   --------------------------------
   -- Clear_Stream_Output_Record --
   --------------------------------

   procedure Clear_Stream_Output_Record (Rec : in out Stream_Output_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);
   begin
      Unchecked_Free (Rec.Content);
   end Clear_Stream_Output_Record;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Stream_Output_Record;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out String_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Result_List_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Value_List_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Result_Record;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Stream_Output_Record;
      V    : in out Mutable_Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out String_Value;
      V    : in out Mutable_Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Result_List_Value;
      V    : in out Mutable_Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Value_List_Value;
      V    : in out Mutable_Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   overriding procedure Accept_Visitor
     (This : in out Result_Record;
      V    : in out Mutable_Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

end MI.Ast;
