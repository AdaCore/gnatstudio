------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Language;               use Language;

package Docgen2.Comments is

   type Comment_Type is private;
   type Comment_Access is access all Comment_Type;

   No_Comment : constant Comment_Type;

   procedure Analyse_Comment
     (Comment     : in out Comment_Type;
      Docgen      : Docgen_Object;
      File        : GNATCOLL.VFS.Virtual_File;
      Entity_Name : String;
      Href        : String);
   --  Parse the comments list for user tags.

   procedure Free (Comment : in out Comment_Type);
   --  Free memory allocated by Comment

   procedure Insert_Before_First_Tag
     (Comment : in out Comment_Type;
      Text    : Ada.Strings.Unbounded.Unbounded_String);
   --  Add Text after the first tags of the comment. If the comment has no tags
   --  then Text is added at the end of the comment. No action performed under
   --  Docgen V2.

   function To_String (Comment : Comment_Type) return String;
   --  Return a formatted string representing Comment.

private

   type Node_Kind is (Root_Node, Text_Node, Element_Node);

   type Node (<>);
   type Node_Ptr is access all Node;
   type Node (Kind : Node_Kind) is record
      Next       : Node_Ptr;
      Parent     : Node_Ptr;
      Children   : Node_Ptr;
      Last_Child : Node_Ptr;

      case Kind is
         when Root_Node =>
            null;
         when Text_Node =>
            Value      : Ada.Strings.Unbounded.Unbounded_String;
         when Element_Node =>
            Tag        : Ada.Strings.Unbounded.Unbounded_String;
            Attributes : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   type Comment_Type is record
      Block      : Ada.Strings.Unbounded.Unbounded_String;
      Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Analysed   : Boolean := False;
      Filtered   : Boolean := False;
      Retrieved  : Boolean := False;
      --  Flags used to avoid analysing/filtering/retrieving a comment twice
   end record;

   No_Comment : constant Comment_Type :=
                  (Block      => Ada.Strings.Unbounded.Null_Unbounded_String,
                   Sloc_Start => (Line => 0, Column => 0, Index => 0),
                   Sloc_Stop  => (Line => 0, Column => 0, Index => 0),
                   Analysed   => False,
                   Filtered   => False,
                   Retrieved  => False);

end Docgen2.Comments;
