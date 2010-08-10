-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GPS.Editors;

package GNATStack.Data_Model is

   type Editor_Mark_Access is access all GPS.Editors.Editor_Mark'Class;

   type Stack_Usage_Information is record
      Size      : Integer;
      Qualifier : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Subprogram_Location is record
      Name   : Ada.Strings.Unbounded.Unbounded_String;
      File   : Ada.Strings.Unbounded.Unbounded_String;
      Line   : Positive;
      Column : Positive;
      Mark   : Editor_Mark_Access := null;
      Lines  : Natural := 0;
   end record;

   function Hash
     (Item : Subprogram_Location) return Ada.Containers.Hash_Type;

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

   function Hash
     (Item : Subprogram_Identifier)
      return Ada.Containers.Hash_Type;

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

   type Analysis_Information is record
      Accurate       : Boolean;
      Subprogram_Set : Subprogram_Information_Sets.Set;
      Unbounded_Set  : Subprogram_Information_Sets.Set;
      External_Set   : Subprogram_Information_Ordered_Sets.Set;
      Indirect_Set   : Subprogram_Information_Sets.Set;
      Cycle_Set      : Subprogram_Information_Vector_Vectors.Vector;
      Entry_Set      : Subprogram_Information_Sets.Set;
   end record;

   function Image (Item : Stack_Usage_Information) return String;
   --  Returns textual representation of the stack usage information.

   procedure Clear (Item : in out Analysis_Information);
   --  Deallocates all information and clears containers.

end GNATStack.Data_Model;
