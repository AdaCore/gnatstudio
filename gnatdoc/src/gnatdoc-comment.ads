------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

--  This package provides support to management of structured comments. The
--  frontend retrieves blocks of comments from the sources, parses such blocks
--  and generates structured comments composed of tags and their attributes.

with Ada.Containers.Indefinite_Holders;

private package GNATdoc.Comment is

   type Structured_Comment is private;

   No_Structured_Comment : constant Structured_Comment;

   function No (Comment : Structured_Comment) return Boolean;
   --  Returns true if an structured comment is not available

   function Present (Comment : Structured_Comment) return Boolean;
   --  Returns true if an structured comment is available

   package Holder is new Ada.Containers.Indefinite_Holders
     (Root_Entity'Class);

   type Tag_Info is record
      Entity : Holder.Holder;
      Tag    : Ada.Strings.Unbounded.Unbounded_String;
      Attr   : Ada.Strings.Unbounded.Unbounded_String;
      Text   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Tag_Info_Ptr is access Tag_Info;

   ------------
   -- Cursor --
   ------------

   type Tag_Cursor is private;
   No_Cursor : constant Tag_Cursor;

   function  New_Cursor (Comment : Structured_Comment) return Tag_Cursor;
   procedure Next   (C : in out Tag_Cursor);
   function  Get    (C : Tag_Cursor) return Tag_Info_Ptr;
   function  At_End (C : Tag_Cursor) return Boolean;

   function First_Param (Comment : Structured_Comment) return Tag_Cursor;
   function Last_Param (Comment : Structured_Comment) return Tag_Cursor;

   function First_Field (Comment : Structured_Comment) return Tag_Cursor;
   function Last_Field (Comment : Structured_Comment) return Tag_Cursor;

   function Search_Param
     (Comment : Structured_Comment;
      Name    : String) return Tag_Cursor;
   --  Search in Comment for a @param tag whose parameter name is Name.
   --  No_Cursor is returned if Comment does not have such parameter.

   procedure Set_Text
     (C    : Tag_Cursor;
      Text : Unbounded_String);
   --  Raises Not_Empty if the tag already has some associated text

   ------------------------
   -- Structured_Comment --
   ------------------------

   function New_Structured_Comment return Structured_Comment;
   --  Constructor

   procedure Append_Field_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Field_Name : Unbounded_String;
      Text       : Unbounded_String);
   --  Append "@field Param_Name Text" to the comment. Entity is the entity
   --  associated with Field_Name.

   procedure Append_Param_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Param_Name : Unbounded_String;
      Text       : Unbounded_String);
   --  Append "@param Param_Name Text" to the comment. Entity is the entity
   --  associated with Param_Name.

   function Append_Tag
     (Comment   : Structured_Comment;
      Tag       : Unbounded_String;
      Entity    : Root_Entity'Class;
      Attribute : Unbounded_String;
      Text      : Unbounded_String := Null_Unbounded_String)
     return Tag_Cursor;
   --  Append "@Tag Attribute Text" to the comment. Entity is the entity
   --  associated with Attribute.

   procedure Append_Text
     (C    : Tag_Cursor;
      Text : String);
   --  Append Text to the tag associated with the cursor C

   procedure Free
     (Comment : in out Structured_Comment);
   --  Free the memory associated with Comment

   function To_Unbounded_String
     (Comment : Structured_Comment;
      Prefix  : String := "")
      return Unbounded_String;
   --  Convert Comment to an String. Prefix is used by print routines to
   --  format the output (if required).

   Not_Empty : exception;

private
   type Node;
   type Node_Ptr is access all Node;
   type Node is record
      Tag_Info : Tag_Info_Ptr;
      Next     : Node_Ptr;
   end record;

   type Tag_Cursor is new Node_Ptr;
   No_Cursor : constant Tag_Cursor := null;

   type Structured_Comment_Record is record
      Count       : Natural;
      First_Tag   : Node_Ptr;
      First_Param : Tag_Cursor;
      Last_Param  : Tag_Cursor;
      Last_Node   : Node_Ptr;
   end record;

   type Structured_Comment is access Structured_Comment_Record;

   No_Structured_Comment : constant Structured_Comment := null;

end GNATdoc.Comment;
