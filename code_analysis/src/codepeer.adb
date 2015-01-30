------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body CodePeer is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Project_Data) is

      procedure Process_Message_Category
        (Position : Message_Category_Sets.Cursor);

      procedure Process_Annotation_Category
        (Position : Annotation_Category_Sets.Cursor);

      procedure Process_Entry_Point
        (Position : Entry_Point_Information_Sets.Cursor);
      --  Deallocates entry point information

      procedure Process_Object_Race (Position : Object_Race_Vectors.Cursor);
      --  Deallocates object race information

      ---------------------------------
      -- Process_Annotation_Category --
      ---------------------------------

      procedure Process_Annotation_Category
        (Position : Annotation_Category_Sets.Cursor)
      is
         Element : Annotation_Category_Access :=
                     Annotation_Category_Sets.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation_Category, Annotation_Category_Access);

      begin
         GNAT.Strings.Free (Element.Text);
         Free (Element);
      end Process_Annotation_Category;

      -------------------------
      -- Process_Entry_Point --
      -------------------------

      procedure Process_Entry_Point
        (Position : Entry_Point_Information_Sets.Cursor)
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (Entry_Point_Information, Entry_Point_Information_Access);

         Element : Entry_Point_Information_Access
           := Entry_Point_Information_Sets.Element (Position);

      begin
         GNAT.Strings.Free (Element.Name);
         Free (Element);
      end Process_Entry_Point;

      ------------------------------
      -- Process_Message_Category --
      ------------------------------

      procedure Process_Message_Category
        (Position : Message_Category_Sets.Cursor)
      is
         Element : Message_Category_Access :=
                     Message_Category_Sets.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Message_Category, Message_Category_Access);

      begin
         GNAT.Strings.Free (Element.Name);
         Free (Element);
      end Process_Message_Category;

      -------------------------
      -- Process_Object_Race --
      -------------------------

      procedure Process_Object_Race (Position : Object_Race_Vectors.Cursor) is
         Element : Object_Race_Information :=
           Object_Race_Vectors.Element (Position);

      begin
         GNAT.Strings.Free (Element.Name);
      end Process_Object_Race;

   begin
      Self.Message_Categories.Iterate (Process_Message_Category'Access);
      Self.Annotation_Categories.Iterate (Process_Annotation_Category'Access);
      Self.Entry_Points.Iterate (Process_Entry_Point'Access);
      Self.Object_Races.Iterate (Process_Object_Race'Access);
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Subprogram_Data) is

      procedure Process_Message (Position : Message_Vectors.Cursor);

      procedure Process_Annotations (Position : Annotation_Maps.Cursor);

      procedure Process_Annotation (Position : Annotation_Vectors.Cursor);

      ------------------------
      -- Process_Annotation --
      ------------------------

      procedure Process_Annotation (Position : Annotation_Vectors.Cursor) is
         Element : Annotation_Access := Annotation_Vectors.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation, Annotation_Access);

      begin
         GNAT.Strings.Free (Element.Text);
         Free (Element);
      end Process_Annotation;

      -------------------------
      -- Process_Annotations --
      -------------------------

      procedure Process_Annotations (Position : Annotation_Maps.Cursor) is
         Element : Annotation_Vector_Access :=
                     Annotation_Maps.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation_Vectors.Vector, Annotation_Vector_Access);

      begin
         Element.Iterate (Process_Annotation'Access);
         Element.Clear;
         Free (Element);
      end Process_Annotations;

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message (Position : Message_Vectors.Cursor) is

         procedure Free is
           new Ada.Unchecked_Deallocation (Message, Message_Access);
         procedure Free is
           new Ada.Unchecked_Deallocation
             (Audit_Record_V3, Audit_Record_V3_Access);

         Element : Message_Access := Message_Vectors.Element (Position);

      begin
         GNAT.Strings.Free (Element.Text);

         for J of Element.Audit_V3 loop
            Free (J);
         end loop;

         Element.Audit_V3.Clear;

         Free (Element);
      end Process_Message;

   begin
      Self.Messages.Iterate (Process_Message'Access);
      Self.Messages.Clear;
      Self.Annotations.Iterate (Process_Annotations'Access);
      Self.Annotations.Clear;
   end Finalize;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : CWE_Category) return String is
      Image : constant String := CWE_Identifier'Image (Item.Identifier);

   begin
      return "CWE-" & Image (Image'First + 1 .. Image'Last);
   end Get_Name;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Message_Category_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Item.Name.all);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Code_Analysis.File_Access) return Ada.Containers.Hash_Type is
   begin
      return Item.Name.Full_Name_Hash;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Entry_Point_Information_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Item.Name.all);
   end Hash;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : Annotation_Category_Access;
      Right : Annotation_Category_Access) return Boolean
   is
   begin
      return Left.Order < Right.Order;
   end Less;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : CWE_Category_Access;
      Right : CWE_Category_Access) return Boolean is
   begin
      return Left.Identifier < Right.Identifier;
   end Less;

   ----------
   -- Less --
   ----------

   function Less
     (Left, Right : CodePeer.Message_Category_Access) return Boolean is
   begin
      return Left.Name.all < Right.Name.all;
   end Less;

end CodePeer;
