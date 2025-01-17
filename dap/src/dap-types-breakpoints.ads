------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2024, AdaCore                  --
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

--  Define breakpoint types and holders for them.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with GNATCOLL.VFS;           use GNATCOLL.VFS;

with VSS.Strings;            use VSS.Strings;

with Basic_Types;            use Basic_Types;

with GPS.Markers;            use GPS.Markers;

package DAP.Types.Breakpoints is

   type Breakpoint_Disposition is (Keep, Delete);
   --  The breakpoint's disposition.
   --
   --  * Keep: the breakpoint should always be kept.
   --  * Delete: the breakpoint should be deleted the first time it gets hit.

   type Breakpoint_Kind is
     (On_Line, On_Subprogram, On_Instruction, On_Exception);
   --  The breakpoint's kind.
   --
   --  * On_Line: breakpoint on a source location. Corresponding to the
   --    'setBreakpoints' DAP request.
   --  * On_Subprogram: breakpoint on a subprogram. Corresponding to the
   --    'setFunctionBreakpoints' DAP request.
   --  * On_Instruction: breakpoint on an instruction. Corresponding to the
   --    'setInstructionBreakpoints' DAP request.
   --  * On_Exception: breakpoint for exceptions. Corresponding to the
   --    'setExceptionBreakpoints' DAP request.

   type Breakpoint_Event is (Added, Deleted, Changed);
   --  Type for breakpoint events.
   --
   --  * Added: a new breakpoint has been added.
   --  * Deleted: a breakpoint has been deleded.
   --  * Changed: an existing breakpoint has changed.

   type Breakpoint_Identifier is new Integer;
   No_Breakpoint : constant Breakpoint_Identifier := 0;
   --  Breakpoint identifiers on DAP server's side.
   --  These identifiers are set by the DAP server, in response of the
   --  breakpoint-related DAP requests.

   package Breakpoint_Identifier_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Breakpoint_Identifier);
   --  Lists of breakpoint identifiers.

   package Breakpoint_Index_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Positive);
   --  This type is used when doing the same debugger action on a list
   --  of breakpoints (delete/enable/disable).
   --  Indexes are different from breakpoint identifiers: breakpoint
   --  identifiers are set by the DAP server once the breakpoint has been
   --  properly recognized, while indexes refer to the position of the
   --  breakpoint in the holders' vectors used to store them.

   type Breakpoint_Location_Type is record
      Marker  : Location_Marker := No_Marker;
      --  The editor's location.

      Address : Address_Type := Invalid_Address;
      --  The intruction's address.
   end record;

   package Breakpoint_Location_Vectors is
     new Ada.Containers.Vectors (Positive, Breakpoint_Location_Type);

   type Breakpoint_Data (Kind : Breakpoint_Kind := On_Line) is record
      Num         : Breakpoint_Identifier := No_Breakpoint;
      --  The breakpoint's identifier set on DAP server's side.

      Disposition : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      Enabled     : Boolean := True;
      --  The breakpoint's state. Disabled breakpoints are not known for the
      --  underlying DAP server

      Condition   : Virtual_String := Empty_Virtual_String;
      --  Condition on which this breakpoint is activated

      Ignore      : Natural := 0;
      --  Number of breakpoint hits that will be ignored before actually
      --  stopping.

      Commands    : Virtual_String := Empty_Virtual_String;
      --  Commands to execute when the debugger stops at this breakpoint

      Verified    : Boolean := True;
      --  True if the breakpoint has been verified on server-side (e.g: if the
      --  specified SLOC actually maps the executable's source files).

      Continue_Until : Boolean := False;
      --  This breakpoint is set for the "continue until" action

      case Kind is
         when On_Line | On_Instruction =>
            Location : Breakpoint_Location_Type;
            --  The line breakpoint's location.

         when On_Subprogram =>
            Subprogram : VSS.Strings.Virtual_String;
            --  The name of the subprogram, for subprogram breakpoints.

         when On_Exception =>
            Exception_Name : VSS.Strings.Virtual_String;
            --  The exception's name.

            Unhandled : Boolean := False;
            --  Whether we should break on unhandled ones.
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
   --  Get the breakpoint's location. This works only for line breakpoints:
   --  an empty location is returned for other breakpoint kinds.

   function Get_Ignore (Data : Breakpoint_Data) return Virtual_String;
   --  Get the ignore count for the given breakpoint.

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
   --  Initialize the holder by copying the given breakpoints.
   --  If Full_Copy is True, the breakpoints' fields set by the running
   --  debugger once the debuggee is known will also be copied (e.g:
   --  breakpoint's ID, address of the breakpoint's SLOC...). Otherwise,
   --  only the information that needs to be persistent will be copied (e.g:
   --  breakpoint's type, SLOC...).

   function Get_Breakpoints
     (Self : Breakpoint_Holder) return Breakpoint_Vectors.Vector;
   --  Return all the breakpoints stored in this holder.

   function Get_Breakpoints
     (Self    : Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List) return Breakpoint_Vectors.Vector;
   --  Return all the breakpoints at the given indexes.

   function Get_Breakpoint_From_Index
     (Self : Breakpoint_Holder;
      Idx  : Positive) return Breakpoint_Data;
   --  Return the breakpoint located at the given index in the holder

   function Get_Breakpoint_From_Id
     (Self : Breakpoint_Holder;
      Id   : Breakpoint_Identifier) return Breakpoint_Data;
   --  Get the breakpoint stored with the given Id.
   --  An empty breakpoint is returned if it does not exist.

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
   --  Append the given breakpoint.

   procedure Replace
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data);
   --  Replace the breakpoint using it's ID, if any.

   procedure Replace
     (Self : in out Breakpoint_Holder;
      Data : Breakpoint_Data;
      Idx  : Positive);
   --  Replace the breakpoint located at the given index, if any.

   procedure Replace
     (Self        : in out Breakpoint_Holder;
      Breakpoints : Breakpoint_Vectors.Vector;
      Full_Copy   : Boolean := False);
   --  Replace all the breakpoints designated by the given Breakpoints for the
   --  current project.
   --  If Full_Copy is True, the fields set by the running debugger once the
   --  debuggee is known will also be copied (e.g: breakpoint's number, address
   --  of the breakpoint's SLOC...). Otherwise, only the information that needs
   --  to be persistent will be copied (e.g: breakpoint's type, SLOC...).

   procedure Delete
     (Self : in out Breakpoint_Holder;
      Id   : Breakpoint_Identifier);
   --  Delete the breakpoint at the given index, if any.

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List);
   --  Delete the breakpoints at the given indexes, if any.

   procedure Delete
     (Self    : in out Breakpoint_Holder;
      File    : Virtual_File;
      Line    : Editable_Line_Type);
   --  Delete the breakpoints sharing the given location, if any.

   function Get_For_Files
     (Self         : Breakpoint_Holder;
      Enabled_Only : Boolean := True)
      return Breakpoint_Hash_Maps.Map;
   --  Get breakpoints ordered by files
   --  When Enabled_Only is True, only the enabled ones are returned.

   function Get_For_File
     (Self         : Breakpoint_Holder;
      File         : Virtual_File;
      Enabled_Only : Boolean := True)
      return Breakpoint_Vectors.Vector;
   --  Get the breakpoints for the given file.
   --  When Enabled_Only is True, only the enabled ones are returned.

   function Get_For_File
     (Self         : Breakpoint_Holder;
      File         : Virtual_File;
      Enabled_Only : Boolean := True)
      return Breakpoint_Index_Lists.List;
   --  Get breakpoints' indexes for the given file.
   --  When Enabled_Only is True, only the enabled ones are returned.

   function Get_For_Kind
     (Self         : Breakpoint_Holder;
      Kind         : Breakpoint_Kind;
      Enabled_Only : Boolean := True)
      return Breakpoint_Index_Lists.List;
   --  Get the brekpoints of the given kind.
   --  When Enabled_Only is True, only the enabled ones are returned.

   procedure Set_Breakpoints_State
     (Self    : in out Breakpoint_Holder;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean);
   --  Set the state of the breakpoints located at the given indexes.

   procedure Set_Ignore_Count
     (Self    : in out Breakpoint_Holder;
      Id      : Breakpoint_Identifier;
      Count   : Natural);
   --  Set the 'ignore' count for the breakpoint refered by Id.

   function Is_Empty (Self : Breakpoint_Holder) return Boolean;
   --  Returns True if no breakpoints

private

   -----------------------
   -- Breakpoint_Holder --
   -----------------------

   type Breakpoint_Holder is tagged limited record
      Vector : Breakpoint_Vectors.Vector;
   end record;

end DAP.Types.Breakpoints;
