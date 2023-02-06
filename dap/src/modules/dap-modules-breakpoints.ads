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

--  Defines breakpoint type and holder for them.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Basic_Types;            use Basic_Types;

with GPS.Markers;            use GPS.Markers;
with DAP.Types;              use DAP.Types;

package DAP.Modules.Breakpoints is

   type Breakpoint_Disposition is (Keep, Delete, Pending);
   type Breakpoint_State is (Enabled, Changing, Disabled, Moved);

   type Location is record
      Num     : Breakpoint_Identifier := 0;
      Marker  : Location_Marker       := No_Marker;
      Address : Address_Type          := Invalid_Address;
   end record;

   package Location_Vectors is new Ada.Containers.Vectors (Positive, Location);

   type Breakpoint_Data is record
      Num         : Breakpoint_Identifier := No_Breakpoint;

      Locations   : Location_Vectors.Vector;
      --  The locations of the breakpoint, may have several for subprograms

      Subprogram  : Ada.Strings.Unbounded.Unbounded_String;

      Disposition : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      State       : Breakpoint_State := Enabled;

      Condition   : Unbounded_String;
      --  Condition on which this breakpoint is activated

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Executable  : Unbounded_String;
   end record;

   Empty_Breakpoint_Data : constant Breakpoint_Data :=
     Breakpoint_Data'(others => <>);

   function "="
     (Data : Breakpoint_Data;
      Num  : Breakpoint_Identifier)
      return Boolean;

   function Breakpoint_Data_Equal
     (L, R : Breakpoint_Data) return Boolean is (L.Num = R.Num);

   function Get_Location (Data : Breakpoint_Data) return Location_Marker;

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Breakpoint_Data,
      "="          => Breakpoint_Data_Equal);

   package Breakpoint_Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Breakpoint_Vectors.Vector,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => Breakpoint_Vectors."=");

   use Breakpoint_Hash_Maps;

   -- Breakpoint_Holder --

   type Breakpoint_Holder is tagged limited private;
   --  Used for store breakpoints

   procedure Initialize
     (Self   : in out Breakpoint_Holder;
      Vector : Breakpoint_Vectors.Vector;
      Clear  : Boolean := False);
   --  Initialize holder with the breakpoints in the Vector parameter

   procedure Initialized (Self : in out Breakpoint_Holder);
   --  Initializetion is done. Process notifications.

   function Get_Breakpoints
     (Self : Breakpoint_Holder)
      return Breakpoint_Vectors.Vector;
   --  Return all breakpoints

   function Get_Breakpoints
     (Self       : Breakpoint_Holder;
      Executable : Virtual_File)
      return Breakpoint_Vectors.Vector;
   --  Return breakpoints for executable and unassigned

   function Get_Next_Id
     (Self : in out Breakpoint_Holder)
      return Breakpoint_Identifier;
   --  Get Id for new breakpoint

   procedure Added
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);
   --  New breakpoint has been added

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      File : Virtual_File;
      Line : Editable_Line_Type);
   --  Breakpoint on file/line has been deleted

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      Nums : Breakpoint_Identifier_Lists.List);
   --  List of breakpoints hasve been deleted

   procedure Deleted
     (Self : in out Breakpoint_Holder;
      Num  : Breakpoint_Identifier);
   --  Breakpoint with Num has been deleted

   procedure Clear (Self : in out Breakpoint_Holder);
   --  Remove all breakpoints

   function Contains
     (Self   : in out Breakpoint_Holder;
      Marker : Location_Marker)
      return Boolean;
   --  Do we already have a breakpoint for location?

   procedure Set_Enabled
     (Self  : in out Breakpoint_Holder;
      Ids   : Breakpoint_Identifier_Lists.List;
      State : Boolean);
   --  Enable/disable breakpoints

   procedure Replace
     (Self       : in out Breakpoint_Holder;
      Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector);
   --  Replace all breakpoints for executable from the list

   procedure Set_Numbers (Self : in out Breakpoint_Holder);
   --  Set breakpoints numbers

   function Get_For_Files
     (Self : Breakpoint_Holder)
      return Breakpoint_Hash_Maps.Map;
   --  Get breakpoints ordered by files

   function Get_For_Subprograms
     (Self          : Breakpoint_Holder;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector;
   --  Get breakpoints for subprograms

   procedure Initialized_For_File
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map);
   --  Feedback after breakpoints are set for the file to synchronize data

   procedure Add
     (Self    : Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector);
   --  Prepare a list to send with the line breakpoint

   procedure Add_Subprogram
     (Self    : Breakpoint_Holder;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector);
   --  Prepare a list to send with the subprogram breakpoint

   procedure Added
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Data    : Breakpoint_Data;
      Changed : out Breakpoint_Vectors.Vector;
      Update  : out Boolean);
   --  Feedback after breakpoints is added to synchronize data

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean);
   --  Prepare a list to send with line breakpoint is removed

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Nums    : DAP.Types.Breakpoint_Identifier_Lists.List;
      Changed : out Breakpoint_Hash_Maps.Map;
      Updated : out Boolean);
   --  Prepare a list to send with breakpoints are removed by numbers

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Nums    : Breakpoint_Identifier_Lists.List;
      State   : Boolean;
      Changed : out Breakpoint_Hash_Maps.Map);
   --  Prepare a list to send with breakpoints are enabled/disabled

   procedure Status_Changed
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Actual  : Breakpoint_Vectors.Vector;
      Changed : out Breakpoint_Hash_Maps.Map);
   --  Feedback after breakpoints stats are changed to synchronize data

   procedure Initialized_For_Subprograms
     (Self   : in out Breakpoint_Holder;
      Actual : Breakpoint_Vectors.Vector;
      Last   : Boolean);
   --  Feedback after breakpoints are set for the subprograms
   --  to synchronize data

   procedure Added_Subprogram
     (Self   : in out Breakpoint_Holder;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector);
   --  Feedback after breakpoints for subprogram is added to synchronize data

   procedure Subprogram_Status_Changed
     (Self   : in out Breakpoint_Holder;
      Actual : Breakpoint_Vectors.Vector;
      Last   : Boolean);
   --  Feedback after breakpoints stats are changed to synchronize data

   procedure Changed
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);
   --  The breakpoint data is changed.

private

   -----------------------
   -- Breakpoint_Holder --
   -----------------------

   type Breakpoint_Holder is tagged limited record
      Id     : Breakpoint_Identifier := 0;
      Vector : Breakpoint_Vectors.Vector;

      In_Initialization : Boolean := False;
   end record;

   function Get_For_File
     (Self          : Breakpoint_Holder;
      File          : Virtual_File;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector;

   procedure Delete_Duplicates
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Changed : out Breakpoint_Hash_Maps.Map);

   procedure Delete_Fake_Subprogram (Self : in out Breakpoint_Holder);

   package Line_Sets is
     new Ada.Containers.Ordered_Sets (Editable_Line_Type);

end DAP.Modules.Breakpoints;
