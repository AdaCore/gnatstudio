------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Entities;               use Entities;
with Language;               use Language;

package Docgen2.Tags is

   type Comment_Type is private;
   type Comment_Access is access all Comment_Type;

   No_Comment : constant Comment_Type;

   package Comments_List is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Comment_Access);

   procedure Add_Comment_Line
     (Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Comment    : String;
      Force_New  : Boolean;
      List       : in out Comments_List.Vector);
   --  Add a new comment line in the comments_list

   procedure Analyse_Comment
     (Comment     : in out Comment_Type;
      Docgen      : Docgen_Object;
      File        : Source_File;
      Entity_Name : String;
      Href        : String);
   --  Parse the comments list for user tags.

   procedure Free (List : in out Comments_List.Vector);
   --  Free memory allocated by List.

   procedure Free (Comment : in out Comment_Type);
   --  Free memory allocated by Comment

   function Find_Doc
     (Sloc_Start : Source_Location;
      Sloc_End   : Source_Location;
      Comments   : Comments_List.Vector;
      File_Doc   : Boolean := False) return Comment_Type;
   --  Find a comment placed just before Sloc_Start or just after Sloc_End.
   --  Comments is the list of comments constructed using Add_Comment_Line and
   --  Analyse_Comments.
   --  If File_Doc is set, then the first comment found before Sloc_Start is
   --  returned. Used when finding the global documentation for a file or
   --  package.
   --  The returned comment is a deep copy of the object in the list, so that
   --  the list can be freed while the returned values remain OK. This should
   --  then be freed manually.

   function To_String (Comment : Comment_Type) return String;
   --  Return a formatted string representing Comment.

   function Ignore
     (Loc      : Source_Location;
      Comments : Comments_List.Vector) return Boolean;
   --  Tell if the current file location should be ignored in the
   --  documentation (because of user tags <doc_ignore>)

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
   end record;

   No_Comment : constant Comment_Type :=
                  (Block      => Ada.Strings.Unbounded.Null_Unbounded_String,
                   Sloc_Start => (Line => 0, Column => 0, Index => 0),
                   Sloc_Stop  => (Line => 0, Column => 0, Index => 0),
                   Analysed   => False);

end Docgen2.Tags;
