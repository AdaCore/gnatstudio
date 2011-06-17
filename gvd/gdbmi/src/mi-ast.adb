------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
--                                                                          --
-- GPS is free software;  you can  redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

package body MI.Ast is

   ----------------------------------
   -- Function "=" for Result_Pair --
   ----------------------------------

   function "=" (Left, Right : Result_Pair) return Boolean is
   begin
      return Left.Variable.all = Right.Variable.all
        and then Left.Value.all = Right.Value.all;
   end "=";

   --------------------
   -- Accept_Visitor --
   --------------------

   procedure Accept_Visitor
     (This : in out Stream_Output_Record;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   procedure Accept_Visitor
     (This : in out String_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   procedure Accept_Visitor
     (This : in out Result_List_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   procedure Accept_Visitor
     (This : in out Value_List_Value;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

   --------------------
   -- Accept_Visitor --
   --------------------

   procedure Accept_Visitor
     (This : in out Result_Record;
      V    : in out Visitor'Class) is
   begin
      V.Visit (This);
   end Accept_Visitor;

end MI.Ast;
