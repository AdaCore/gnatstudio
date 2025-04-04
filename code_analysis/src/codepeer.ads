------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Calendar;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with VSS.Strings;
with VSS.Strings.Formatters.Generic_Integers;

with Default_Preferences;
with GPS.Editors;
with GPS.Kernel.Messages;
with Code_Analysis; use Code_Analysis;

package CodePeer is

   type Format_Version is new Positive;
   --  Version of format of interchange files.

   subtype Supported_Format_Version is Format_Version range 4 .. 6;
   --  Range of suppoted versions of format of interchange file.

   Unknown_Timestamp : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Ada.Calendar.Year_Number'First, 1, 1, 0.0);

   type Analyzer_Exe is (Codepeer_Exe, GNATSAS_Exe);
   Current_Analyzer : Analyzer_Exe := Codepeer_Exe;
   SAM_File         : Unbounded_String;

   function Is_GNATSAS return Boolean is (Current_Analyzer = GNATSAS_Exe);
   function Module_Name return VSS.Strings.Virtual_String is
     (if Is_GNATSAS then "GNATSAS" else "CodePeer");
   function Package_Name return String is
     (if Is_GNATSAS then "gnatsas" else "codepeer");
   function GPR_Name return String is
     (if Is_GNATSAS then "Analyzer" else "CodePeer");
   function Build_Mode return String is
     (if Is_GNATSAS then "" else "codepeer");

   package Annot_File_Sets is
     new Ada.Containers.Ordered_Sets (Unbounded_String);
   Annot_Files : Annot_File_Sets.Set;
   --  annotation files displayed, if any (for CPL use).

   ----------------
   --  Messages  --
   ----------------

   -- Lifeage_Kinds --

   type Lifeage_Kinds is (Added, Unchanged, Removed);
   type Lifeage_Kind_Access is access all Lifeage_Kinds;

   function Get_Name (Self : Lifeage_Kinds) return String;

   function Get_Tooltip (Dummy_Self : Lifeage_Kinds) return String is ("");
   --  Returns tooltip's text to be displayed for given lifeage category.

   function Less
     (Left, Right : CodePeer.Lifeage_Kind_Access) return Boolean;

   type Lifeage_Kinds_Flags is array (Lifeage_Kinds) of Boolean;

   package Lifeage_Kinds_Sets is new Ada.Containers.Ordered_Sets
     (Lifeage_Kind_Access, Less, "=");

   function To_Lifeage_Kinds_Flags
     (Set : Lifeage_Kinds_Sets.Set) return Lifeage_Kinds_Flags;

   -- Message_Ranking_Level --

   type Message_Ranking_Level is
     (Not_An_Error, Suppressed, Info, Low, Medium, High);

   function Image (Level : CodePeer.Message_Ranking_Level) return String;

   type Message_Ranking_Level_Flags is
     array (Message_Ranking_Level) of Boolean;

   --  Ranking_Kinds --

   subtype Ranking_Kinds is Message_Ranking_Level range Info .. High;
   type Ranking_Kind_Access is access all Ranking_Kinds;

   function Get_Tooltip (Dummy_Self : Ranking_Kinds) return String is ("");
   --  Returns tooltip's text to be displayed for given ranking category.

   function Less
     (Left, Right : CodePeer.Ranking_Kind_Access) return Boolean;

   package Ranking_Kinds_Sets is new Ada.Containers.Ordered_Sets
     (Ranking_Kind_Access, Less, "=");

   -- Audit_Status_Category --

   type Audit_Status_Category is (Uncategorized, Pending, Not_A_Bug, Bug);
   --  The categories of audit status

   type Audit_Status_Kinds is record
      Name     : Unbounded_String;
      Category : Audit_Status_Category;
      Id       : Positive;
   end record;
   --  Generic audit status, so that the user can define custom ones
   type Audit_Status_Access is access all Audit_Status_Kinds;

   function Image (Status : Audit_Status_Kinds) return String;

   function Get_Tooltip (Dummy_Self : Audit_Status_Kinds)
                         return String is ("");
   --  Returns tooltip's text to be displayed for given audit category.

   function Less
     (Left, Right : CodePeer.Audit_Status_Access) return Boolean;

   package Audit_Status_Sets is
     new Ada.Containers.Ordered_Sets (Audit_Status_Access, Less, "=");

   Audit_Statuses : Audit_Status_Sets.Set;
   --  The list of registered audit statuses

   Uncategorized_Status : constant Audit_Status_Kinds :=
     (To_Unbounded_String ("Uncategorized"), Uncategorized, 1);
   --  Default status

   procedure Clear_Audit_Statuses;
   --  Clear Audit_Statuses and reset cateogry ids.
   --  This procedure should be used instead of e.g. calling
   --  Audit_Statuses.Clear directly.

   procedure Add_Audit_Status
     (Status : String; Category : Audit_Status_Category);
   --  Add an audit status in Audit_Statuses

   function Get_Status (Id : Integer) return Audit_Status_Kinds;
   --  Return the Audit_Status_Kinds in Audit_Statuses with the given id.
   --  Return Uncategorized_Status if not found.

   function Get_Status (Name : String) return Audit_Status_Kinds;
   --  Return the Audit_Status_Kinds in Audit_Statuses with the given name.
   --  Add it with a Not_A_Bug category if not found.

   function Get_Status (Name : String; Category : Audit_Status_Category)
                       return Audit_Status_Kinds;
   --  Return the Audit_Status_Kinds in Audit_Statuses with the given name.
   --  Add it with 'Category' as category if not found.

   function Standardize (S : String) return String;
   --  Return a standardized string.
   --  Concretely this means convert S in lower case, and replace ' ' by '_'

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

   function Get_Tooltip (Dummy_Self : Message_Category) return String is ("");
   --  Returns tooltip's text to be displayed for given message category.

   type Message_Category_Access is access all Message_Category;

   function Less
     (Left, Right : CodePeer.Message_Category_Access) return Boolean;

   package Message_Category_Sets is new Ada.Containers.Ordered_Sets
     (Message_Category_Access, Less, "=");

   type Audit_Record is record
      Timestamp   : Unbounded_String;
      Comment     : Unbounded_String;
      Approved_By : Unbounded_String;
      Status      : Audit_Status_Kinds;
   end record;

   type Audit_Record_Access is access all Audit_Record;

   package Audit_Vectors is
     new Ada.Containers.Vectors (Positive, Audit_Record_Access);

   package Natural_Sets is
     new Ada.Containers.Ordered_Sets (Natural);

   type GNATSAS_Id_Type is record
      Prj     : Ada.Strings.Unbounded.Unbounded_String;
      File    : Ada.Strings.Unbounded.Unbounded_String;
      Subp    : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Ada.Strings.Unbounded.Unbounded_String;
      Key     : Ada.Strings.Unbounded.Unbounded_String;
      Key_Seq : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Structure used to identify a message with CodePeer. GS does not need
   --  to understand it, just store it and send it to the bridge when needed.

   type GNATSAS_Id_Access is access all GNATSAS_Id_Type;

   type Message is new GPS.Kernel.Messages.Primary_Abstract_Message with record
      Id              : Natural;
      File            : Code_Analysis.File_Access;
      Subprogram      : Ada.Strings.Unbounded.Unbounded_String;
      Merged          : Natural_Sets.Set;
      Lifeage         : Lifeage_Kinds;
      Category        : Message_Category_Access;
      Is_Check        : Boolean;
      --  True means this message is check.
      Ranking         : Message_Ranking_Level;
      Status          : Audit_Status_Kinds;
      Status_Editable : Boolean;
      --  True means that audit status can be changed by review.
      Text            : Ada.Strings.Unbounded.Unbounded_String;
      Audit_Loaded    : Boolean;
      Audit           : Audit_Vectors.Vector;
      Checks          : Message_Category_Sets.Set;
      CWEs            : CWE_Category_Sets.Set;
      Display_CWEs    : Boolean;
      --  Enable displaying of CWEs
      Removed_Color   : Default_Preferences.Color_Preference;
      --  Reference to preference value of which is used for foreground color
      --  of removed messages.
      Show_Msg_Id     : Boolean;
      GNATSAS_Id      : GNATSAS_Id_Access;
   end record;
   type Message_Access is access all Message;

   overriding function Get_Text
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Markup
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding function Get_Tooltip_Markup
     (Self : not null access Message)
      return Ada.Strings.Unbounded.Unbounded_String;

   overriding procedure Finalize (Self : not null access Message);

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
   --  Compare annotation categories to sort them in the following order:
   --  * (Pre)
   --  * (Presumption)
   --  * (Post)
   --  * Other categories are sorted according to the Order field. Their
   --    ordering does not matter because they are not displayed in the editor.

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
      Name         : VSS.Strings.Virtual_String;
      Entry_Points : Entry_Point_Object_Access_Vectors.Vector;
      File         : GNATCOLL.VFS.Virtual_File;
      Line         : Natural;
      Column       : Natural;
      Message      : GPS.Kernel.Messages.Message_Access;
   end record;

   package Object_Race_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Race_Information);

   type Inspection_Information is record
      Inspection    : Ada.Strings.Unbounded.Unbounded_String;
      Timestamp     : Ada.Calendar.Time;
      Main          : Ada.Strings.Unbounded.Unbounded_String;
      Switches      : Ada.Strings.Unbounded.Unbounded_String;
      Library_File  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Project_Data is new Code_Analysis.CodePeer_Data_Root with record
      Current               : Inspection_Information;
      Baseline              : Inspection_Information;
      Message_Categories    : Message_Category_Sets.Set;
      Annotation_Categories : Annotation_Category_Sets.Set;
      CWE_Categories        : CWE_Category_Sets.Set;

      Check_Subcategories   : Message_Category_Sets.Set;
      Warning_Subcategories : Message_Category_Sets.Set;
      Lifeage_Subcategories : Lifeage_Kinds_Sets.Set;
      Ranking_Subcategories : Ranking_Kinds_Sets.Set;
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
      Total_Checks       : Natural := 0;

      Annotations_File   : GNATCOLL.VFS.Virtual_File;
      Annotations_Loaded : Boolean;
      --  File that contains annotations and flag to mark that they was loaded.
      --  Introduced in version 5.
   end record;

   type Subprogram_Data is new Code_Analysis.CodePeer_Data_Root with record
      Lifeage       : Lifeage_Kinds;
      Messages      : Message_Vectors.Vector;
      Annotations   : Annotation_Maps.Map;
      Mark          : GPS.Editors.Editor_Mark_Holders.Holder;
      Special_Lines : Natural := 0;
   end record;

   type Subprogram_Data_Access is access all Subprogram_Data'Class;

   overriding procedure Finalize (Self : access Subprogram_Data);

   --  Message filter criteria

   function Hash
     (Item : Code_Analysis.File_Access) return Ada.Containers.Hash_Type;

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Code_Analysis.File_Access, Hash, Code_Analysis."=");

   type Review_Status_Kinds_Flags is array (1 .. 256) of Boolean;
   --  Use a hard coded max size "large enough" for convenience

   type Message_Filter_Criteria is record
      Files      : File_Sets.Set;
      --  Set of shown files.
      Categories : Message_Category_Sets.Set;
      CWEs       : CWE_Category_Sets.Set;
      Rankings   : Message_Ranking_Level_Flags;
      Lineages   : Lifeage_Kinds_Flags;
      Statuses   : Review_Status_Kinds_Flags := (others => False);
   end record;

   package Count_Type_Formatters is
     new VSS.Strings.Formatters.Generic_Integers (Ada.Containers.Count_Type);

end CodePeer;
