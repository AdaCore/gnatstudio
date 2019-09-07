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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with GNATdoc.Utils;               use GNATdoc.Utils;
with Language;                    use Language;
with Language.Tree;               use Language.Tree;
with Language.Tree.Database;      use Language.Tree.Database;
with String_Utils;                use String_Utils;

package body GNATdoc.Comment is

   --  Local subprograms

   procedure Internal_Append_Tag
     (Comment   : Structured_Comment;
      Tag_Name  : Unbounded_String;
      Entity    : Root_Entity'Class;
      Attr_Name : Unbounded_String;
      Text      : Unbounded_String_Vectors.Vector);

   ----------------------
   -- Append_Field_Tag --
   ----------------------

   procedure Append_Field_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Field_Name : Unbounded_String;
      Text       : Unbounded_String_Vectors.Vector) is
   begin
      Internal_Append_Tag
        (Comment   => Comment,
         Tag_Name  => To_Unbounded_String ("field"),
         Entity    => Entity,
         Attr_Name => Field_Name,
         Text      => Text);
   end Append_Field_Tag;

   ----------------------
   -- Append_Param_Tag --
   ----------------------

   procedure Append_Param_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Param_Name : Unbounded_String;
      Text       : Unbounded_String_Vectors.Vector) is
   begin
      Internal_Append_Tag
        (Comment   => Comment,
         Tag_Name  => To_Unbounded_String ("param"),
         Entity    => Entity,
         Attr_Name => Param_Name,
         Text      => Text);
   end Append_Param_Tag;

   ----------------------
   -- Append_Value_Tag --
   ----------------------

   procedure Append_Value_Tag
     (Comment    : Structured_Comment;
      Entity     : Root_Entity'Class;
      Value_Name : Unbounded_String;
      Text       : Unbounded_String_Vectors.Vector) is
   begin
      Internal_Append_Tag
        (Comment   => Comment,
         Tag_Name  => To_Unbounded_String ("value"),
         Entity    => Entity,
         Attr_Name => Value_Name,
         Text      => Text);
   end Append_Value_Tag;

   ----------------
   -- Append_Tag --
   ----------------

   function Append_Tag
     (Comment   : Structured_Comment;
      Tag       : Unbounded_String;
      Entity    : Root_Entity'Class;
      Attribute : Unbounded_String;
      Text      : Unbounded_String_Vectors.Vector :=
        Unbounded_String_Vectors.Empty_Vector)
      return Tag_Cursor
   is
      New_Tag_Info : constant Tag_Info_Ptr :=
                       new Tag_Info'
                         (Tag    => Tag,
                          Entity => <>,
                          Attr   => Attribute,
                          Text   => To_Unbounded_String (Text));
      New_Node     : constant Node_Ptr :=
                       new Node'
                        (Tag_Info => New_Tag_Info,
                         Next     => null);

   begin
      New_Tag_Info.Entity.Replace_Element (Entity);

      if Comment.First_Tag = null then
         Comment.First_Tag := New_Node;
      else
         Comment.Last_Node.Next := New_Node;
      end if;

      Comment.Last_Node := New_Node;
      Comment.Count := Comment.Count + 1;

      return Tag_Cursor (New_Node);
   end Append_Tag;

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text (C : Tag_Cursor; Text : String) is
   begin
      Append (C.Tag_Info.Text, Text);
   end Append_Text;

   ------------
   -- At_End --
   ------------

   function At_End (C : Tag_Cursor) return Boolean is
   begin
      return C = null;
   end At_End;

   -----------------
   -- First_Field --
   -----------------

   function First_Field (Comment : Structured_Comment) return Tag_Cursor
     renames First_Param;

   -----------------
   -- First_Param --
   -----------------

   function First_Param (Comment : Structured_Comment) return Tag_Cursor
   is
   begin
      return Comment.First_Param;
   end First_Param;

   -----------------
   -- First_Value --
   -----------------

   function First_Value (Comment : Structured_Comment) return Tag_Cursor
     renames First_Param;

   ----------
   -- Free --
   ----------

   procedure Free
     (Comment : in out Structured_Comment)
   is
      procedure Free_Info is
        new Ada.Unchecked_Deallocation (Tag_Info, Tag_Info_Ptr);

      procedure Free_Node is
        new Ada.Unchecked_Deallocation (Node, Node_Ptr);

      procedure Free_Structured_Node is
        new Ada.Unchecked_Deallocation
          (Structured_Comment_Record, Structured_Comment);

      Node : Node_Ptr;
      Next : Node_Ptr;
   begin
      Node := Comment.First_Tag;
      while Node /= null loop
         Next := Node.Next;

         Node.Tag_Info.Tag  := Null_Unbounded_String;
         Node.Tag_Info.Attr := Null_Unbounded_String;
         Node.Tag_Info.Text := Null_Unbounded_String;

         Free_Info (Node.Tag_Info);
         Free_Node (Node);

         Node := Next;
      end loop;

      Free_Structured_Node (Comment);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (C : Tag_Cursor) return Tag_Info_Ptr is
   begin
      return Node_Ptr (C).Tag_Info;
   end Get;

   -------------------------
   -- Internal_Append_Tag --
   -------------------------

   procedure Internal_Append_Tag
     (Comment   : Structured_Comment;
      Tag_Name  : Unbounded_String;
      Entity    : Root_Entity'Class;
      Attr_Name : Unbounded_String;
      Text      : Unbounded_String_Vectors.Vector)
   is
      C : Tag_Cursor;
   begin
      C :=
        Append_Tag
          (Comment   => Comment,
           Tag       => Tag_Name,
           Entity    => Entity,
           Attribute => Attr_Name,
           Text      => Text);

      if Comment.First_Param = null then
         Comment.First_Param := C;
      end if;

      Comment.Last_Param := C;
   end Internal_Append_Tag;

   ----------------
   -- Last_Field --
   ----------------

   function Last_Field (Comment : Structured_Comment) return Tag_Cursor
     renames Last_Param;

   ----------------
   -- Last_Param --
   ----------------

   function Last_Param (Comment : Structured_Comment) return Tag_Cursor is
   begin
      return Comment.Last_Param;
   end Last_Param;

   ----------------
   -- Last_Value --
   ----------------

   function Last_Value (Comment : Structured_Comment) return Tag_Cursor
     renames Last_Param;

   ----------------
   -- New_Cursor --
   ----------------

   function New_Cursor (Comment : Structured_Comment) return Tag_Cursor is
   begin
      return Tag_Cursor (Comment.First_Tag);
   end New_Cursor;

   ----------------------------
   -- New_Structured_Comment --
   ----------------------------

   function New_Structured_Comment return Structured_Comment is
      H : Root_Entity_Ref;
      New_Info : Tag_Info_Ptr;
      New_Node : Node_Ptr;

   begin
      H.Replace_Element (No_Root_Entity);

      New_Info :=
        new Tag_Info'
          (Tag    => Null_Unbounded_String,
           Entity => H,
           Attr   => Null_Unbounded_String,
           Text   => Null_Unbounded_String);

      New_Node :=
        new Node'
          (Tag_Info => New_Info,
           Next     => null);

      return new Structured_Comment_Record'
        (First_Tag   => New_Node,
         First_Param => No_Cursor,
         Last_Param  => No_Cursor,
         Last_Node   => New_Node,
         Count       => 0);
   end New_Structured_Comment;

   ----------
   -- Next --
   ----------

   procedure Next
     (C : in out Tag_Cursor) is
   begin
      C := Tag_Cursor (Node_Ptr (C).Next);
   end Next;

   --------
   -- No --
   --------

   function No (Comment : Structured_Comment) return Boolean is
   begin
      return Comment = No_Structured_Comment;
   end No;

   -------------
   -- Present --
   -------------

   function Present (Comment : Structured_Comment) return Boolean is
   begin
      return Comment /= No_Structured_Comment;
   end Present;

   ------------------
   -- Search_Param --
   ------------------

   function Search_Param
     (Comment : Structured_Comment;
      Name    : String) return Tag_Cursor
   is
      C        : Tag_Cursor := New_Cursor (Comment);
      Tag_Info : Tag_Info_Ptr;

   begin
      while not At_End (C) loop
         Tag_Info := Get (C);

         if Tag_Info.Attr = Name then
            return C;
         end if;

         Next (C);
      end loop;

      return No_Cursor;
   end Search_Param;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (C    : Tag_Cursor;
      Text : Unbounded_String)
   is
      Tag_Info : Tag_Info_Ptr;
   begin
      pragma Assert (C /= No_Cursor);

      Tag_Info := Get (C);

      if Present (Tag_Info.Text) then
         raise Not_Empty;
      end if;

      Tag_Info.Text := Text;
   end Set_Text;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Comment : Structured_Comment;
      Prefix  : String := "";
      Mode    : String_Mode := Single_Line_Mode) return Unbounded_String
   is
      Printout : Unbounded_String;

      procedure Append (Tag_Info : Tag_Info_Ptr);
      --  Append the information of Tag_Info to Printout

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      ------------
      -- Append --
      ------------

      procedure Append (Tag_Info : Tag_Info_Ptr) is
      begin
         if Present (Tag_Info.Tag) then
            if Tag_Info.Tag = "param"
              and then No (Tag_Info.Text)
            then
               null;
            else
               Append_Line
                 ("@"
                  & To_String (Tag_Info.Tag)
                  & " "
                  & To_String (Tag_Info.Attr));
            end if;
         end if;

         if Present (Tag_Info.Text) then
            case Mode is
               when Single_Line_Mode =>
                  Append_Line
                    (Trim
                       (Reduce (To_String (Tag_Info.Text)),
                        Ada.Strings.Left));

               when Plain_Text_Mode =>
                  Append_Line
                    (To_String (Tag_Info.Text));
            end case;
         end if;
      end Append;

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Prefix & Text & ASCII.LF;
      end Append_Line;

      --  Local variables

      C : Tag_Cursor := New_Cursor (Comment);

   --  Start of processing for To_Unbounded_String

   begin
      while not At_End (C) loop
         Append (Get (C));
         Next (C);
      end loop;

      return Printout;
   end To_Unbounded_String;

end GNATdoc.Comment;
