------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

--  Type to hold breakpoints. Uses seleral lists: "flat" list that contains
--  all breakpoints and several lists for files and subprograms separately.

with Ada.Strings;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;           use GNATCOLL.VFS;

with GPS.Markers;            use GPS.Markers;
with DAP.Types;              use DAP.Types;
with Basic_Types;            use Basic_Types;

package DAP.Breakpoint_Maps is

   type Breakpoint_Disposition is (Delete, Disable, Keep);
   --  What to do with a breakpoint when it is reached.

   type Num_Location is record
      Num      : Breakpoint_Identifier := 0;
      Location : Location_Marker       := No_Marker;
      Address  : Address_Type := Invalid_Address;
   end record;

   package Locations_Vectors is
     new Ada.Containers.Vectors (Positive, Num_Location);

   type Breakpoint_Data is record
      Id          : Breakpoint_Identifier := No_Breakpoint;
      --  Unique breakpoint identifier because we can have
      --  the same Nums in the different debugging sessions

      Num         : Breakpoint_Identifier := No_Breakpoint;

      Locations   : Locations_Vectors.Vector;
      --  The locations of the breakpoint, may have several for subprograms

      Subprogram   : Ada.Strings.Unbounded.Unbounded_String;

      Disposition  : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      Condition    : Unbounded_String;
      --  Condition on which this breakpoint is activated

      Ignore       : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Enabled      : Boolean := True;
      Change_State : Boolean := False;

      Temporary    : Boolean := False;
      --  Temporary breakpoint that should be removed on the first hit.

      Executable   : Unbounded_String;
   end record;

   Empty_Breakpoint_Data : constant Breakpoint_Data :=
     Breakpoint_Data'(others => <>);

   function Breakpoint_Data_Equal
     (L, R : Breakpoint_Data) return Boolean is (L.Id = R.Id);

   function Get_Location (Data : Breakpoint_Data) return Location_Marker;
   function Get_Address  (Data : Breakpoint_Data) return Address_Type;
   function Is_Same_Location (L, R : Breakpoint_Data) return Boolean;

   function "="
     (Data : Breakpoint_Data;
      Num  : Breakpoint_Identifier) return Boolean;

   procedure Copy (To : in out Breakpoint_Data; From : Breakpoint_Data);
   --  Copy data set by debugger

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Breakpoint_Data,
      "="          => Breakpoint_Data_Equal);

   function Breakpoint_Vector_Equal
     (L, R : Breakpoint_Vectors.Vector) return Boolean;

   package Breakpoint_Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Breakpoint_Vectors.Vector,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => Breakpoint_Vectors."=");

   use Breakpoint_Hash_Maps;

   ----------------------------------
   -- Breakpoint_Persistent_Holder --
   ----------------------------------

   type Breakpoint_Persistent_Holder is tagged limited private;
   --  Used for store persistent breakpoints

   procedure Initialize
     (Self : in out Breakpoint_Persistent_Holder;
      List : Breakpoint_Vectors.Vector);

   function Get_Breakpoints_List
     (Self : Breakpoint_Persistent_Holder)
      return Breakpoint_Vectors.Vector;

   function Get_Breakpoints_List
     (Self       : Breakpoint_Persistent_Holder;
      Executable : Virtual_File)
      return Breakpoint_Vectors.Vector;

   function Get_Next_Id
     (Self : in out Breakpoint_Persistent_Holder)
      return Breakpoint_Identifier;

   procedure Added
     (Self : in out Breakpoint_Persistent_Holder;
      Data : Breakpoint_Data);

   procedure Deleted
     (Self : in out Breakpoint_Persistent_Holder;
      File : Virtual_File;
      Line : Editable_Line_Type);

   procedure Deleted
     (Self : in out Breakpoint_Persistent_Holder;
      Nums : Breakpoint_Identifier_Lists.List);

   procedure Clear (Self : in out Breakpoint_Persistent_Holder);

   function Contains
     (Self     : in out Breakpoint_Persistent_Holder;
      Location : Location_Marker)
      return Boolean;

   procedure Set_Enabled
     (Self  : in out Breakpoint_Persistent_Holder;
      Ids   : Breakpoint_Identifier_Lists.List;
      State : Boolean);

   procedure Replace
     (Self       : in out Breakpoint_Persistent_Holder;
      Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector);

   procedure Reorder (Self : in out Breakpoint_Persistent_Holder);

   -----------------------
   -- Breakpoint_Holder --
   -----------------------

   type Breakpoint_Holder is tagged limited private;

   procedure Initialize
     (Self : in out Breakpoint_Holder;
      List : Breakpoint_Vectors.Vector);
   --  Set initial breakpoints

   function Get_Breakpoints_List
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector;
   --  Return "flat" common list of breakpoints

   function Get_For_Files
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Hash_Maps.Map;
   --  Turn on synchronization and return lists for files

   function Get_For_File
     (Self : in out Breakpoint_Holder;
      File : Virtual_File)
      return Breakpoint_Vectors.Vector;
   --  Turn on synchronization and return a list for the file.
   --  Returns Subprogram breakpoints for No_File

   function Get_For_Subprograms
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Vectors.Vector;
   --  Turn on synchronization and return a list for subprograms

   function Get_Pending
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector;
   --  Return list of breakpoints that can't be set

   procedure Initialized_For_File
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Initialized_For_Subprograms
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Add
     (Self    : in out Breakpoint_Holder;
      Data    : in out Breakpoint_Data;
      Changed : out Breakpoint_Hash_Maps.Map);
   --  Return the prepared list with new breakpoint for sending to debugger,
   --  if synchronized.

   procedure Added
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean);
   --  Store new debugger breakpoint. Called from debuggers notifications
   --  or responces or when no debugger.

   procedure Added_Subprogram
     (Self    : in out Breakpoint_Holder;
      Added   : Breakpoint_Data;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean);

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Updated : out Boolean;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      Updated : out Boolean;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Delete_Disabled (Self : in out Breakpoint_Holder);

   procedure Deleted
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Updated : out Boolean);
   --  Delete breakpoints. Actual should contains actual breakpoints with Ids

   procedure Deleted_Subprogram
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Updated : out Boolean);

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      State   : Boolean;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Actual
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Boolean);

   procedure Actual_Subprogram
     (Self    : in out Breakpoint_Holder;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Boolean);
   --  Actual breakpoints after enable/disable actions.

   procedure Synch
     (Self   : in out Breakpoint_Holder;
      File   : Virtual_File;
      Actual : Breakpoint_Vectors.Vector);
   --  Synchronize breakpoints numbers after updates

private

   -----------------------
   -- Breakpoint_Holder --
   -----------------------

   type Breakpoint_Holder is tagged limited record
      All_Breakpoints : Breakpoint_Vectors.Vector;
      Per_Files       : Breakpoint_Hash_Maps.Map;
      Subprograms     : Breakpoint_Vectors.Vector;
      Pending         : Breakpoint_Vectors.Vector;
   end record;

   procedure Set_For_File
     (Self : in out Breakpoint_Holder;
      File : Virtual_File;
      List : Breakpoint_Vectors.Vector);

   procedure Delete_From_Lists
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);

   procedure Add_To_Changed
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : in out Breakpoint_Hash_Maps.Map);

   procedure Delete_From_Changed
     (Self    : in out Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : in out Breakpoint_Hash_Maps.Map);

   procedure Clear (Self : in out Breakpoint_Holder);
   --  Clear all breakpoints. Turn off synchronization.

   procedure Synchronize_Lists (Self : in out Breakpoint_Holder);
   --  Synchronize per file/subprograms lists with the general list

   ----------------------------------
   -- Breakpoint_Persistent_Holder --
   ----------------------------------

   type Breakpoint_Persistent_Holder is tagged limited record
      Id              : Breakpoint_Identifier := 0;
      All_Breakpoints : Breakpoint_Vectors.Vector;
   end record;
end DAP.Breakpoint_Maps;
