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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GNATCOLL.VFS;           use GNATCOLL.VFS;

with VSS.Strings;            use VSS.Strings;

with Basic_Types;            use Basic_Types;

with GPS.Markers;            use GPS.Markers;
with DAP.Types;              use DAP.Types;

package DAP.Modules.Breakpoints is

   type Breakpoint_Disposition is (Keep, Delete, Pending);
   type Breakpoint_State is (Enabled, Changing, Disabled, Moved);
   type Breakpoint_Kind is (On_Line, On_Subprogram, On_Address, On_Exception);
   type Breakpoint_Event is (Added, Deleted, Changed);
   --  TODO: doc

   type Location_Type is record
      Num     : Breakpoint_Identifier := 0;
      Marker  : Location_Marker       := No_Marker;
      Address : Address_Type          := Invalid_Address;
   end record;

   package Location_Vectors is
     new Ada.Containers.Vectors (Positive, Location_Type);

   type Breakpoint_Data (Kind : Breakpoint_Kind := On_Line) is record
      Num         : Breakpoint_Identifier := No_Breakpoint;
      --  The breakpoint identifier set on DAP server's side.

      Disposition : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      State       : Breakpoint_State := Enabled;

      Condition   : Virtual_String := Empty_Virtual_String;
      --  Condition on which this breakpoint is activated

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Commands    : Virtual_String := Empty_Virtual_String;
      --  Commands to execute when the debugger stops at this breakpoint

      Executable  : Virtual_File := No_File;

      Verified    : Boolean := True;
      --  Is bp verified on the gdb side

      case Kind is
         when On_Line =>
            Location : Location_Type;

         when On_Subprogram =>
            Subprogram : Ada.Strings.Unbounded.Unbounded_String;

         when On_Address =>
            Address : Address_Type := Invalid_Address;

         when On_Exception =>
            Except    : Ada.Strings.Unbounded.Unbounded_String;
            Unhandled : Boolean := False;
      end case;
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
   --  TODO: doc

   function Get_Ignore (Data : Breakpoint_Data) return Virtual_String;
   --  TODO: doc

   function To_String (Data : Breakpoint_Data) return String;
   --  Return a location string representation to display for the given
   --  breakpoint.

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Breakpoint_Data,
      "="          => Breakpoint_Data_Equal);

   package Breakpoint_Hash_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Breakpoint_Index_Lists.List,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=",
      "="             => Breakpoint_Index_Lists."=");

   use Breakpoint_Hash_Maps;

   -- Breakpoint_Holder --

   type Breakpoint_Holder is tagged limited private;
   --  Used for store breakpoints

   procedure Initialize
     (Self      : out Breakpoint_Holder;
      Vector    : Breakpoint_Vectors.Vector;
      Full_Copy : Boolean := False);
   --  Initialize then holder by copying the given breakpoints.
   --  If Full_Copy is True, the breakpoints' fields set by the running
   --  debugger once the debuggee is known will also be copied (e.g:
   --  breakpoint's ID, address of the breakpoint's SLOC...). Otherwise,
   --  only the information that needs to be persistent will be copied (e.g:
   --  breakpoint's type, SLOC...).

   function Get_Breakpoints
     (Self    : Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List :=
        Breakpoint_Index_Lists.Empty_List) return Breakpoint_Vectors.Vector;
   --  Return all the breakpoints at the given indexes, or all the holder's
   --  breakpoints if no indexes are specified.

   function Get_Breakpoint_From_Index
     (Self : Breakpoint_Holder;
      Idx  : Positive) return Breakpoint_Data;
   --  Return the breakpoint located at the given index in the holder

   function Get_Breakpoints
     (Self       : Breakpoint_Holder;
      Executable : Virtual_File)
      return Breakpoint_Vectors.Vector;
   --  Return breakpoints for executable and unassigned

   function Get_Breakpoint_From_Id
     (Self : Breakpoint_Holder;
      Id   : Breakpoint_Identifier) return Breakpoint_Data;
   --  TODO: doc

   procedure Clear (Self : in out Breakpoint_Holder);
   --  Remove all breakpoints

   function Contains
     (Self   : in out Breakpoint_Holder;
      Marker : Location_Marker)
      return Boolean;
   --  Do we already have a breakpoint for location?

   procedure Append
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);
   --  Append the given breakpoint
   --  TODO: should we handle duplicates?

   procedure Replace_From_Id
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);
   --  Replace the breakpoint.
   --  TODO: doc

   procedure Replace
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data;
      Idx  : Positive);
   --  TODO: doc

   procedure Replace
     (Self        : in out Breakpoint_Holder;
      Executable  : Virtual_File;
      Breakpoints : Breakpoint_Vectors.Vector;
      Full_Copy   : Boolean := False);
   --  Replace all the breakpoints designed by the given Breakpoints for the
   --  given executable.
   --  If Full_Copy is True, the fields set by the running debugger once the
   --  debuggee is known will also be copied (e.g: breakpoint's number, address
   --  of the breakpoint's SLOC...). Otherwise, only the information that needs
   --  to be persistent will be copied (e.g: breakpoint's type, SLOC...).

   procedure Delete
     (Self : in out Breakpoint_Holder;
      Num  : Breakpoint_Identifier);
   --  TODO: doc

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List);
   --  TODO: doc

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type);
   --  TODO: doc

   function Get_For_Files
     (Self : Breakpoint_Holder)
      return Breakpoint_Hash_Maps.Map;
   --  Get breakpoints ordered by files

   function Get_For_File
     (Self          : Breakpoint_Holder;
      File          : Virtual_File;
      With_Changing : Boolean := False)
      return Breakpoint_Vectors.Vector;
   --  Get breakpoints for the given file

   function Get_For_File
     (Self          : Breakpoint_Holder;
      File          : Virtual_File;
      With_Changing : Boolean := False)
      return Breakpoint_Index_Lists.List;
   --  Get breakpoints' indexes for the given file

   function Get_For_Kind
     (Self          : Breakpoint_Holder;
      Kind          : Breakpoint_Kind;
      With_Changing : Boolean := False)
      return Breakpoint_Index_Lists.List;
   --  TODO: doc

   procedure Set_Enabled
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean);
   --  TODO: doc

   procedure Set_Ignore_Count
     (Self    : in out Breakpoint_Holder;
      Id      : Breakpoint_Identifier;
      Count   : Natural);
   --  Sets ignore count for the breakpoint, returning the changed
   --  breakpoints in Changed.

private

   -----------------------
   -- Breakpoint_Holder --
   -----------------------

   type Breakpoint_Holder is tagged limited record
      Vector : Breakpoint_Vectors.Vector;
   end record;

end DAP.Modules.Breakpoints;
