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

      procedure Process_Category (Position : Message_Category_Sets.Cursor);

      ----------------------
      -- Process_Category --
      ----------------------

      procedure Process_Category (Position : Message_Category_Sets.Cursor) is
         Element : Message_Category_Access :=
                     Message_Category_Sets.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Message_Category, Message_Category_Access);

      begin
         GNAT.Strings.Free (Element.Name);
         Free (Element);
      end Process_Category;

   begin
      Self.Categories.Iterate (Process_Category'Access);
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Subprogram_Data) is

      procedure Process_Message (Position : Message_Vectors.Cursor);

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
      Self.Preconditions.Iterate (Process_Annotation'Access);
      Self.Postconditions.Iterate (Process_Annotation'Access);
   end Finalize;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Message_Category_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Item.Name.all);
   end Hash;

end Code_Peer;
