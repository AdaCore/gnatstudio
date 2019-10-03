------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

private package GNATdoc.Comment is

   type Structured_Comment is private;

   No_Structured_Comment : constant Structured_Comment;

   function No (Comment : Structured_Comment) return Boolean;
   --  Returns true if an structured comment is not available

   function Present (Comment : Structured_Comment) return Boolean;
   --  Returns true if an structured comment is available

   type Tag_Info is record
      Entity : Root_Entity_Ref;
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

   function First_Field (Comment : Structured_Comment) return Tag_Cursor;
   function Last_Field (Comment : Structured_Comment) return Tag_Cursor;

   function First_Param (Comment : Structured_Comment) return Tag_Cursor;
   function Last_Param (Comment : Structured_Comment) return Tag_Cursor;

   function First_Value (Comment : Structured_Comment) return Tag_Cursor;
   function Last_Value (Comment : Structured_Comment) return Tag_Cursor;

   function Search_Param
     (Comment : Structured_Comment;
      Name    : String) return Tag_Cursor;
   --  Search in Comment for a @param tag whose parameter name is Name.
   --  No_Cursor is returned if Comment does not have such parameter.

   procedure Set_Text
     (C    : Tag_Cursor;
      Text : Unbounded_String_Vectors.Vector);
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
      Text       : Unbounded_String_Vectors.Vector);
   --  Append "@field Field_Name Text" to the comment. Entity is the entity
   --  associated with Field_Name.

   procedure Append_Param_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Param_Name : Unbounded_String;
      Text       : Unbounded_String_Vectors.Vector);
   --  Append "@param Param_Name Text" to the comment. Entity is the entity
   --  associated with Param_Name.

   procedure Append_Value_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Value_Name : Unbounded_String;
      Text       : Unbounded_String_Vectors.Vector);
   --  Append "@value Value_Name Text" to the comment. Entity is the entity
   --  associated with Value_Name.

   function Append_Tag
     (Comment   : Structured_Comment;
      Tag       : Unbounded_String;
      Entity    : Root_Entity'Class;
      Attribute : Unbounded_String;
      Text      : Unbounded_String_Vectors.Vector :=
        Unbounded_String_Vectors.Empty_Vector)
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

   type String_Mode is (Single_Line_Mode, Plain_Text_Mode);
   --  Single_Line_Mode: The output associated with each attribute of the
   --    structured comment is generated in a single line of text aligned
   --    to the left. This mode is used to generate the output associated
   --    with the GNATdoc switch '--output=test'.
   --  Plain_Text_Mode: The output associated with each line of the structured
   --    comment is the plain unmodified text scanned by the parser, including
   --    line terminators. This mode is used to generate the output associated
   --    with the GNATdoc switch '--output=cm'.

   function To_Unbounded_String
     (Comment : Structured_Comment;
      Prefix  : String := "";
      Mode    : String_Mode := Single_Line_Mode)
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
