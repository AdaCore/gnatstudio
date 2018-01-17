------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package defines the same XML api as Glib.XML, but does not depend
--  on Gtk/GtkAda.

with Interfaces.C;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package XML_Utils is

   subtype UTF8_String is String;
   type String_Ptr is access all String;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

   type Node;
   type Node_Ptr is access all Node;
   --  Pointer to a node of the XML tree

   type Node is record
      Tag   : String_Ptr;
      --  The name of this node. This is utf8-encoded

      Attributes   : String_Ptr;
      --  The attributes of this node. This is utf8-encoded

      Value : String_Ptr;
      --  The value, or null is not relevant. This is utf8-encoded

      Parent : Node_Ptr;
      --  The parent of this Node

      Child : Node_Ptr;
      --  The first Child of this Node. The next child is Child.Next

      Next  : Node_Ptr;
      --  Next sibling node

      Specific_Data : Interfaces.C.int;
      --  Use to store data specific to each implementation (e.g a boolean
      --  indicating whether this node has been accessed)
   end record;
   --  A node of the XML tree.
   --  Each time a tag is found in the XML file, a new node is created, that
   --  points to its parent, its children and its siblings (nodes at the same
   --  level in the tree and with the same parent).

   function Parse (File : Virtual_File) return Node_Ptr;
   --  Parse File and return the first node representing the XML file

   function Parse_Buffer (Buffer : UTF8_String) return Node_Ptr;
   --  Parse a given Buffer in memory and return the first node representing
   --  the XML contents.

   procedure Print (N : Node_Ptr; File : Virtual_File);
   --  Write the tree starting with N into a file File_Name. The generated
   --  file is valid XML, and can be parsed with the Parse function.
   --  If File_Name is the empty string, then the tree is printed on the
   --  standard output

   procedure Print
     (N       : Node_Ptr;
      File    : Virtual_File;
      Success : out Boolean;
      Style   : String := "");
   --  Same as above, with Success reporting the success of the operation.
   --  If style specified, write corresponding XML process instruction

   function Protect (S : String; Ignore_LF : Boolean := False) return String;
   --  Return a copy of S modified so that it is a valid XML value
   --  If Ignore_LF is set, then LF characters won't be translated as &#10;

   function Find_Tag (N : Node_Ptr; Tag : UTF8_String) return Node_Ptr;
   --  Find a tag Tag in N and its brothers

   function Get_Field (N : Node_Ptr; Field : UTF8_String) return String_Ptr;
   --  Return the value of the field 'Field' if present in the children of N.
   --  Return null otherwise.
   --  Do not free the returned value.

   function Is_Equal (Node1, Node2 : Node_Ptr) return Boolean;
   --  Compare two XML nodes recursively, and returns True if they are equal.
   --  Casing in attributes is relevant. Order of attributes is also
   --  relevant.

   procedure Add_Child
     (N : Node_Ptr; Child : Node_Ptr; Append : Boolean := False);
   --  Add a new child to a node.
   --  If Append is true, the child is added at the end of the current list of
   --  children.

   function Deep_Copy (N : Node_Ptr) return Node_Ptr;
   --  Return a deep copy of the tree starting with N. N can then be freed
   --  without affecting the copy.

   procedure Free (N : in out Node_Ptr);
   --  Free the memory allocated for a node and its children.
   --  It also disconnects N from its parent.
   --  If Free_Data is not null, it is used to free the memory occupied by
   --  the Specific_Data for each node.

   function Get_Attribute
     (N : Node_Ptr;
      Attribute_Name : UTF8_String;
      Default        : UTF8_String := "") return UTF8_String;
   --  Return the value of the attribute 'Attribute_Name' if present.
   --  Special XML characters have already been interpreted in the result
   --  string.
   --  Return Default otherwise.

   procedure Set_Attribute
     (N : Node_Ptr; Attribute_Name, Attribute_Value : UTF8_String);
   --  Create a new attribute, or replace an existing one. The attribute value
   --  is automatically protected for special XML characters

   function Find_Tag_With_Attribute
     (N     : Node_Ptr;
      Tag   : UTF8_String;
      Key   : UTF8_String;
      Value : UTF8_String := "") return Node_Ptr;
   --  Find a tag Tag in N that has a given key (and value if given)

   procedure Add_File_Child
     (N              : Node_Ptr;
      Tag            : UTF8_String;
      File           : Virtual_File;
      Use_VFS_Prefix : Boolean := True);
   --  Add File as a child of N using tag Tag.
   --  Note that Tag is not the actual XML tag, but is the same tag that should
   --  be used to retrieve the file using Get_File_Child.
   --  If Use_VFS_Prefix is True, then the actual node name will be
   --   "vfs_" & Tag

   function Get_File_Child
     (N              : Node_Ptr;
      Tag            : UTF8_String;
      Host           : String := "";
      Use_VFS_Prefix : Boolean := True) return Virtual_File;
   --  Retrieve the file stored using Add_File_Child
   --  If Host is set, the this host is used in case no host is already defined
   --   in the file node
   --  If Use_VFS_Prefix is True, then the actual node name will be
   --   "vfs_" & Tag

   function String_To_Encoded_ASCII (S : String) return String;
   --  Escape all special characters using the "#<character number;" method
   function Encoded_ASCII_To_String (S : String) return String;
   --  Reciprocal to String_To_Encoded_ASCII

end XML_Utils;
