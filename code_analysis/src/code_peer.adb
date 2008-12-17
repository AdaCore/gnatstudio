-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body Code_Peer is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Project_Data) is

      procedure Process_Message_Category
        (Position : Message_Category_Sets.Cursor);

      procedure Process_Annotation_Category
        (Position : Annotation_Category_Sets.Cursor);

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

   begin
      Self.Message_Categories.Iterate (Process_Message_Category'Access);
      Self.Annotation_Categories.Iterate (Process_Annotation_Category'Access);
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
         Element : Message_Access := Message_Vectors.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Message, Message_Access);

      begin
         GNAT.Strings.Free (Element.Text);
         Free (Element);
      end Process_Message;

   begin
      Self.Messages.Iterate (Process_Message'Access);
      Self.Messages.Clear;
      Self.Annotations.Iterate (Process_Annotations'Access);
      Self.Annotations.Clear;
   end Finalize;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Message_Category_Access) return Ada.Containers.Hash_Type is
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

end Code_Peer;
