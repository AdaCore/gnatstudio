------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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
with Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Default_Preferences;
with GPS.Editors;
with GPS.Kernel.Messages;
with Code_Analysis;

package CodePeer is

   type Format_Version is new Positive;
   --  Version of format of interchange files.

   subtype Supported_Format_Version is Format_Version range 3 .. 5;
   --  Range of suppoted versions of format of interchange file.

   ----------------
   --  Messages  --
   ----------------

   type Lifeage_Kinds is (Added, Unchanged, Removed);

   type Message_Ranking_Level is
     (Not_An_Error, Suppressed, Info, Low, Medium, High);

   function Image (Level : CodePeer.Message_Ranking_Level) return String;

   type Audit_Status_Kinds is
     (Unclassified,
      Pending,
      Not_A_Bug,
      False_Positive,
      Intentional,
      Bug);

   function Image (Status : Audit_Status_Kinds) return String;

   type CWE_Identifier is new Natural;

   type CWE_Category is record
      Identifier : CWE_Identifier;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Get_Name (Item : CWE_Category) return String;

   function Get_Tooltip (Item : CWE_Category) return String;
   --  Returns tooltip's text to be displayed for given CWE category.

   type CWE_Category_Access is access all CWE_Category;

   function Less
     (Left : CWE_Category_Access; Right : CWE_Category_Access) return Boolean;

   package CWE_Category_Sets is new Ada.Containers.Ordered_Sets
     (CWE_Category_Access, Less);

   type Message_Category is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      CWEs : CWE_Category_Sets.Set;
   end record;

   function Get_Name (Self : Message_Category) return String;

   function Get_Tooltip (Item : Message_Category) return String is ("");
   --  Returns tooltip's text to be displayed for given message category.

   type Message_Category_Access is access all Message_Category;

   function Less
     (Left, Right : CodePeer.Message_Category_Access) return Boolean;

   package Message_Category_Sets is new Ada.Containers.Ordered_Sets
     (Message_Category_Access, Less, "=");

   type Audit_Record_V3 is record
      Timestamp   : Ada.Strings.Unbounded.Unbounded_String;
      Comment     : Ada.Strings.Unbounded.Unbounded_String;
      Approved_By : Ada.Strings.Unbounded.Unbounded_String;
      Status      : Audit_Status_Kinds;
   end record;

   type Audit_Record_V3_Access is access all Audit_Record_V3;

   package Audit_V3_Vectors is
     new Ada.Containers.Vectors (Positive, Audit_Record_V3_Access);

   package Natural_Sets is
     new Ada.Containers.Ordered_Sets (Natural);

   type Message is new GPS.Kernel.Messages.Primary_Abstract_Message with record
      Id               : Natural;
      File             : Code_Analysis.File_Access;
      Subprogram       : Code_Analysis.Subprogram_Access;
      Merged           : Natural_Sets.Set;
      Lifeage          : Lifeage_Kinds;
      Category         : Message_Category_Access;
      Is_Check         : Boolean;
      --  True means this message is check.
      Ranking          : Message_Ranking_Level;
      Status           : Audit_Status_Kinds;
      Status_Editable  : Boolean;
      --  True means that audit status can be changed by review.
      Text             : Ada.Strings.Unbounded.Unbounded_String;
      Audit_Loaded     : Boolean;
      Audit_V3         : Audit_V3_Vectors.Vector;
      Checks           : Message_Category_Sets.Set;
      Vns              : Natural_Sets.Set;
      CWEs             : CWE_Category_Sets.Set;
      Display_CWEs     : Boolean;
      --  Enable displaying of CWEs
      Removed_Color    : Default_Preferences.Color_Preference;
      --  Reference to preference value of which is used for foreground color
      --  of removed messages.
   end record;

   overriding function Get_Text
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding procedure Finalize (Self : not null access Message);

   type Message_Access is access all Message;

   package Message_Vectors is
     new Ada.Containers.Vectors (Positive, Message_Access);

   package Message_Maps is
     new Ada.Containers.Ordered_Maps (Positive, Message_Access);

   -------------------
   --  Annotations  --
   -------------------

   type Annotation_Category is record
      Order : Positive;
      Text  : Ada.Strings.Unbounded.Unbounded_String;
      Vn    : Natural;
   end record;

   type Annotation_Category_Access is access all Annotation_Category;

   function Less
     (Left  : Annotation_Category_Access;
      Right : Annotation_Category_Access) return Boolean;

   type Annotation is record
      Lifeage : Lifeage_Kinds;
      Text    : Ada.Strings.Unbounded.Unbounded_String;
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
      Kind    : Object_Access_Kinds;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Positive;
      Column  : Positive;
      Message : GPS.Kernel.Messages.Message_Access;
   end record;

   package Object_Access_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Access_Information);

   type Entry_Point_Information is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
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
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      Entry_Points : Entry_Point_Object_Access_Vectors.Vector;
      File         : GNATCOLL.VFS.Virtual_File;
      Line         : Natural;
      Column       : Natural;
      Message      : GPS.Kernel.Messages.Message_Access;
   end record;

   package Object_Race_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Race_Information);

   type Project_Data is new Code_Analysis.CodePeer_Data_Root with record
      Current_Inspection    : Natural;
      Baseline_Inspection   : Natural;
      Message_Categories    : Message_Category_Sets.Set;
      Annotation_Categories : Annotation_Category_Sets.Set;
      CWE_Categories        : CWE_Category_Sets.Set;

      Check_Subcategories   : Message_Category_Sets.Set;
      Warning_Subcategories : Message_Category_Sets.Set;
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
      Lifeage            : Lifeage_Kinds;
      Total_Checks       : Natural;

      Annotations_File   : GNATCOLL.VFS.Virtual_File;
      Annotations_Loaded : Boolean;
      --  File that contains annotations and flag to mark that they was loaded.
      --  Introduced in version 5.
   end record;

   type Subprogram_Data is
     new Code_Analysis.CodePeer_Data_Root with record
      Lifeage       : Lifeage_Kinds;
      Messages      : Message_Vectors.Vector;
      Annotations   : Annotation_Maps.Map;
      Mark          : GPS.Editors.Editor_Mark_Holders.Holder;
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

   type Review_Status_Kinds_Flags is array (Audit_Status_Kinds) of Boolean;

   type Message_Filter_Criteria is record
      Files      : File_Sets.Set;
      --  Set of shown files.
      Categories : Message_Category_Sets.Set;
      CWEs       : CWE_Category_Sets.Set;
      Rankings   : Message_Ranking_Level_Flags;
      Lineages   : Lifeage_Kinds_Flags;
      Statuses   : Review_Status_Kinds_Flags;
   end record;

end CodePeer;
