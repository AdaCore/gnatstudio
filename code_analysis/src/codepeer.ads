------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with GNAT.Strings;

with GNATCOLL.VFS;
with GPS.Editors;
with GPS.Kernel.Messages;
with Code_Analysis;

package CodePeer is

   ----------------
   --  Messages  --
   ----------------

   type Lifeage_Kinds is (Added, Unchanged, Removed);

   type Message_Ranking_Level is
     (Suppressed, Informational, Low, Medium, High);

   type Message_Category is record
      Name : GNAT.Strings.String_Access;
   end record;

   type Message_Category_Access is access all Message_Category;

   function Hash
     (Item : Message_Category_Access) return Ada.Containers.Hash_Type;

   package Message_Category_Sets is new Ada.Containers.Hashed_Sets
     (Message_Category_Access, Hash, "=");

   type Audit_Record (Probability_Changed : Boolean) is record
      Timestamp   : GNAT.Strings.String_Access;
      Comment     : GNAT.Strings.String_Access;

      case Probability_Changed is
         when True =>
            Ranking : Message_Ranking_Level;

         when False =>
            null;
      end case;
   end record;

   type Audit_Record_Access is access all Audit_Record;

   package Audit_Vectors is
     new Ada.Containers.Vectors (Positive, Audit_Record_Access);

   package Natural_Sets is
     new Ada.Containers.Ordered_Sets (Natural);

   type Message is record
      Id               : Natural;
      Merged           : Natural_Sets.Set;
      Lifeage          : Lifeage_Kinds;
      Line             : Positive;
      Column           : Positive;
      Category         : Message_Category_Access;
      Is_Check         : Boolean;
      --  True means this message is check.
      Computed_Ranking : Message_Ranking_Level;
      Current_Ranking  : Message_Ranking_Level;
      Text             : GNAT.Strings.String_Access;
      Audit_Loaded     : Boolean;
      Audit            : Audit_Vectors.Vector;
      From_File        : GNATCOLL.VFS.Virtual_File;
      From_Line        : Positive;
      From_Column      : Positive;
      Message          : GPS.Kernel.Messages.Message_Access;
   end record;

   type Message_Access is access all Message;

   package Message_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Access);

   -------------------
   --  Annotations  --
   -------------------

   type Annotation_Category is record
      Order : Positive;
      Text  : GNAT.Strings.String_Access;
   end record;

   type Annotation_Category_Access is access all Annotation_Category;

   function Less
     (Left  : Annotation_Category_Access;
      Right : Annotation_Category_Access) return Boolean;

   type Annotation is record
      Lifeage : Lifeage_Kinds;
      Text    : GNAT.Strings.String_Access;
   end record;

   type Annotation_Access is access all Annotation;

   package Annotation_Vectors is new Ada.Containers.Vectors
     (Positive, Annotation_Access);

   type Annotation_Vector_Access is access all Annotation_Vectors.Vector;

   package Annotation_Maps is new Ada.Containers.Ordered_Maps
     (Annotation_Category_Access, Annotation_Vector_Access, Less);

   package Annotation_Category_Sets is new Ada.Containers.Ordered_Sets
     (Annotation_Category_Access, Less);

   -----------------------
   --  Race conditions  --
   -----------------------

   type Object_Access_Kinds is (Read, Update);

   type Object_Access_Information is record
      Kind   : Object_Access_Kinds;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Positive;
      Column : Positive;
   end record;

   package Object_Access_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Access_Information);

   type Entry_Point_Information is record
      Name   : GNAT.Strings.String_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Positive;
      Column : Positive;
   end record;

   type Entry_Point_Information_Access is access all Entry_Point_Information;

   function Hash
     (Item : Entry_Point_Information_Access) return Ada.Containers.Hash_Type;

   package Entry_Point_Information_Sets is
     new Ada.Containers.Hashed_Sets
       (Entry_Point_Information_Access, Hash, "=");

   type Entry_Point_Object_Access_Information is record
      Entry_Point     : Entry_Point_Information_Access;
      Object_Accesses : Object_Access_Vectors.Vector;
   end record;

   package Entry_Point_Object_Access_Vectors is
     new Ada.Containers.Vectors
       (Positive, Entry_Point_Object_Access_Information);

   type Object_Race_Information is record
      Name         : GNAT.Strings.String_Access;
      Entry_Points : Entry_Point_Object_Access_Vectors.Vector;
   end record;

   package Object_Race_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Race_Information);

   type Project_Data is new Code_Analysis.CodePeer_Data_Root with record
      Current_Inspection    : Natural;
      Baseline_Inspection   : Natural;
      Message_Categories    : Message_Category_Sets.Set;
      Annotation_Categories : Annotation_Category_Sets.Set;

      Check_Categories      : Message_Category_Sets.Set;
      Warning_Categories    : Message_Category_Sets.Set;
      --  These sets of categories are subsets of Message_Categories and
      --  are used by messages filter.

      Entry_Points          : Entry_Point_Information_Sets.Set;
      Object_Races          : Object_Race_Vectors.Vector;
   end record;
   --  This record has only one instance and associated with the node
   --  of the root project. It is an owner of the message categories and
   --  annotation categories stored in the Message_Categories and
   --  Annotation_Categories members.

   overriding procedure Finalize (Self : access Project_Data);

   type File_Data is new Code_Analysis.CodePeer_Data_Root with record
      Lifeage      : Lifeage_Kinds;
      Total_Checks : Natural;
   end record;

   type Editor_Mark_Access is access all GPS.Editors.Editor_Mark'Class;

   type Subprogram_Data is
     new Code_Analysis.CodePeer_Data_Root with record
      Lifeage       : Lifeage_Kinds;
      Messages      : Message_Vectors.Vector;
      Annotations   : Annotation_Maps.Map;
      Mark          : Editor_Mark_Access;
      Special_Lines : Natural := 0;
   end record;

   type Subprogram_Data_Access is access all Subprogram_Data'Class;

   overriding procedure Finalize (Self : access Subprogram_Data);

   --  Message filter criteria

   use type Code_Analysis.File_Access;
   --  ??? Remove this clase after I120-013 will be fixed

   function Hash
     (Item : Code_Analysis.File_Access) return Ada.Containers.Hash_Type;

   package File_Sets is
     new Ada.Containers.Hashed_Sets
          (Code_Analysis.File_Access, Hash, Code_Analysis."=");

   type Message_Ranking_Level_Flags is
     array (Message_Ranking_Level) of Boolean;

   type Lifeage_Kinds_Flags is array (Lifeage_Kinds) of Boolean;

   type Message_Filter_Criteria is record
      Files      : File_Sets.Set;
      --  Set of shown files.
      Categories : Message_Category_Sets.Set;
      Rankings   : Message_Ranking_Level_Flags;
      Lineages   : Lifeage_Kinds_Flags;
   end record;

   function Less
     (Left, Right : CodePeer.Message_Category_Access) return Boolean;

   package Message_Category_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (CodePeer.Message_Category_Access, Less, CodePeer."=");

end CodePeer;
