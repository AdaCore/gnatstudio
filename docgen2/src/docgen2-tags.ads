-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Entities;               use Entities;
with GPS.Kernel;
with Language;               use Language;

with Docgen2_Backend;

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

   procedure Analyse_Comments
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Options : Docgen_Options;
      File    : Source_File;
      List    : in out Comments_List.Vector);
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

   function To_String
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend         : access Docgen2_Backend.Backend_Record'Class;
      Comment         : access Comment_Type;
      Entity_Name     : String;
      Href            : String;
      Keep_Formatting : Boolean) return String;
   --  Return a formatted string representing Comment.

   function Ignore
     (Loc      : Source_Location;
      Comments : Comments_List.Vector) return Boolean;
   --  Tell if the current file location should be ignored in the
   --  documentation (because of user tags <doc_ignore>)

private

   type Node;
   type Node_Ptr is access all Node;
   type Node is record
      Tag        : Ada.Strings.Unbounded.Unbounded_String;
      Value      : Ada.Strings.Unbounded.Unbounded_String;
      Attributes : Ada.Strings.Unbounded.Unbounded_String;
      Next       : Node_Ptr;
      Children   : Node_Ptr;
      Parent     : Node_Ptr;
   end record;

   type Comment_Type is record
      N          : Node_Ptr;
      Block      : Ada.Strings.Unbounded.Unbounded_String;
      Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Analysed   : Boolean;
   end record;

   No_Comment : constant Comment_Type :=
                  (N          => null,
                   Block      => Ada.Strings.Unbounded.Null_Unbounded_String,
                   Sloc_Start => (Line => 0, Column => 0, Index => 0),
                   Sloc_Stop  => (Line => 0, Column => 0, Index => 0),
                   Analysed   => False);

end Docgen2.Tags;
