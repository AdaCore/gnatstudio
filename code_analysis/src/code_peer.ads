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

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with GNAT.Strings;

with Code_Analysis;

package Code_Peer is

   type Message_Probability_Level is
     (Informational, Low, Medium, High, Suppressed);

   type Message_Category is record
      Name : GNAT.Strings.String_Access;
   end record;

   type Message_Category_Access is access all Message_Category;

   function Hash
     (Item : Message_Category_Access) return Ada.Containers.Hash_Type;

   package Message_Category_Sets is new Ada.Containers.Hashed_Sets
     (Message_Category_Access, Hash, "=");

   type Message is record
      Line        : Positive;
      Column      : Positive;
      Category    : Message_Category_Access;
      Probability : Message_Probability_Level;
      Text        : GNAT.Strings.String_Access;
   end record;

   type Message_Access is access all Message;

   package Message_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Access);

   type Annotation_Category is record
      Order : Positive;
      Text  : GNAT.Strings.String_Access;
   end record;

   type Annotation_Category_Access is access all Annotation_Category;

   function Less
     (Left  : Annotation_Category_Access;
      Right : Annotation_Category_Access) return Boolean;

   type Annotation is record
      Text : GNAT.Strings.String_Access;
   end record;

   type Annotation_Access is access all Annotation;

   package Annotation_Vectors is new Ada.Containers.Vectors
     (Positive, Annotation_Access);

   type Annotation_Vector_Access is access all Annotation_Vectors.Vector;

   package Annotation_Maps is new Ada.Containers.Ordered_Maps
     (Annotation_Category_Access, Annotation_Vector_Access, Less);

   package Annotation_Category_Sets is new Ada.Containers.Ordered_Sets
     (Annotation_Category_Access, Less);

   type Project_Data is new Code_Analysis.Code_Peer_Data_Root with record
      Message_Categories    : Message_Category_Sets.Set;
      Annotation_Categories : Annotation_Category_Sets.Set;
   end record;
   --  This record has only one instance and associated with the node
   --  of the root project. It is an owner of the message categories and
   --  annotation categories stored in the Message_Categories and
   --  Annotation_Categories members.

   overriding procedure Finalize (Self : access Project_Data);

   type Subprogram_Data is
     new Code_Analysis.Code_Peer_Data_Root with record
      Messages      : Message_Vectors.Vector;
      Annotations   : Annotation_Maps.Map;
      Special_Lines : Natural := 0;
   end record;

   type Subprogram_Data_Access is access all Subprogram_Data'Class;

   overriding procedure Finalize (Self : access Subprogram_Data);

   Code_Peer_Editor_Mark_Name_Prefix : constant String := "CodePeer-";

end Code_Peer;
