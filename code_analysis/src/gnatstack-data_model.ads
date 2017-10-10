------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;

with GPS.Editors;

package GNATStack.Data_Model is

   type Stack_Usage_Information is record
      Size      : Integer;
      Qualifier : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Subprogram_Location is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      File   : Ada.Strings.Unbounded.Unbounded_String;
      Line   : Positive;
      Column : Positive;
      Mark   : GPS.Editors.Editor_Mark_Holders.Holder;
      Lines  : Natural := 0;
   end record;

   function Hash
     (Item : Subprogram_Location) return Ada.Containers.Hash_Type;

   function "<"
     (Left  : Subprogram_Location;
      Right : Subprogram_Location) return Boolean;

   overriding function "="
     (Left  : Subprogram_Location;
      Right : Subprogram_Location) return Boolean;

   package Subprogram_Location_Sets is
     new Ada.Containers.Hashed_Sets (Subprogram_Location, Hash, "=");

   type Subprogram_Identifier is record
      Prefix_Name : Ada.Strings.Unbounded.Unbounded_String;
      Linker_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Linker_Name is name in encoded form, it is used to represent name in
      --  .ci files only.
      Locations   : Subprogram_Location_Sets.Set;
   end record;

   overriding function "="
     (Left  : Subprogram_Identifier;
      Right : Subprogram_Identifier) return Boolean;
   --  Encoded_Name is optional information and must be ignored by relationship
   --  operations.

   function "<"
     (Left  : Subprogram_Identifier;
      Right : Subprogram_Identifier) return Boolean;

   type Object_Information is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      File   : Ada.Strings.Unbounded.Unbounded_String;
      Line   : Positive;
      Column : Positive;
   end record;

   package Object_Information_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Information);

   type Indirect_Call_Information is record
      File : Ada.Strings.Unbounded.Unbounded_String;
      Line : Positive;
   end record;

   package Indirect_Call_Information_Vectors is
     new Ada.Containers.Vectors (Positive, Indirect_Call_Information);

   type Subprogram_Information;
   type Subprogram_Information_Access is access all Subprogram_Information;

   function Hash
     (Item : Subprogram_Information_Access) return Ada.Containers.Hash_Type;

   function Equivalent_Elements
     (Left  : Subprogram_Information_Access;
      Right : Subprogram_Information_Access) return Boolean;

   function Element_Is_Less
     (Left  : Subprogram_Information_Access;
      Right : Subprogram_Information_Access) return Boolean;
   --  Returns True if Prefix_Name of the Left is less than Prefix_Name of
   --  the Right.

   package Subprogram_Information_Vectors is
     new Ada.Containers.Vectors (Positive, Subprogram_Information_Access);

   package Subprogram_Information_Vector_Vectors is
     new Ada.Containers.Vectors
       (Positive,
        Subprogram_Information_Vectors.Vector,
        Subprogram_Information_Vectors."=");

   package Subprogram_Information_Sets is
     new Ada.Containers.Hashed_Sets
       (Subprogram_Information_Access, Hash, Equivalent_Elements);

   package Subprogram_Information_Ordered_Sets is
     new Ada.Containers.Ordered_Sets
       (Subprogram_Information_Access, Element_Is_Less, Equivalent_Elements);

   type Subprogram_Information is record
      Id           : Ada.Strings.Unbounded.Unbounded_String;
      Identifier   : Subprogram_Identifier;
      Global_Usage : Stack_Usage_Information;
      Local_Usage  : Stack_Usage_Information;
      Calls        : Subprogram_Information_Sets.Set;
      Unbounded    : Object_Information_Vectors.Vector;
      Indirects    : Indirect_Call_Information_Vectors.Vector;

      --  For entry subprograms

      Is_Entry     : Boolean := False;
      Entry_Usage  : Stack_Usage_Information;
      Chain        : Subprogram_Information_Vectors.Vector;

      --  For external subprograms

      Is_External  : Boolean := False;
   end record;

   package Subprogram_Information_Maps is
     new Ada.Containers.Hashed_Maps
       (Ada.Strings.Unbounded.Unbounded_String,
        GNATStack.Data_Model.Subprogram_Information_Access,
        Ada.Strings.Unbounded.Hash,
        Ada.Strings.Unbounded."=");

   type CI_Information is record
      File_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Subprograms : Subprogram_Information_Ordered_Sets.Set;
   end record;

   type CI_Information_Access is access all CI_Information;

   package CI_Vectors is
     new Ada.Containers.Vectors (Positive, CI_Information_Access);

   type Analysis_Information is record
      Accurate       : Boolean;
      Subprogram_Set : Subprogram_Information_Sets.Set;
      Subprogram_Map : Subprogram_Information_Maps.Map;
      --  This map is used to resolve subprogram by its 'linker name' when
      --  CI file is loaded.

      Unbounded_Set  : Subprogram_Information_Sets.Set;
      External_Set   : Subprogram_Information_Ordered_Sets.Set;
      Indirect_Set   : Subprogram_Information_Sets.Set;
      Cycle_Set      : Subprogram_Information_Vector_Vectors.Vector;
      Entry_Set      : Subprogram_Information_Sets.Set;
      CIs            : CI_Vectors.Vector;
   end record;

   function Image (Item : Stack_Usage_Information) return String;
   --  Returns textual representation of the stack usage information.

   procedure Clear (Item : in out Analysis_Information);
   --  Deallocates all information and clears containers.

end GNATStack.Data_Model;
