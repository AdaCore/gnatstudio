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

--  Used to manage DAP breakpoints.

with GNATCOLL.VFS;            use GNATCOLL.VFS;

with VSS.Strings;

with Basic_Types;             use Basic_Types;
with GPS.Kernel;              use GPS.Kernel;
with DAP.Types;               use DAP.Types;
with DAP.Types.Breakpoints; use DAP.Types.Breakpoints;

--  Used to manage DAP breakpoints as a whole, dealing with persistant
--  breakpoints and debugger-specific ones.
--  This will look in look in persistent breakpoints if there is no running,
--  debugger or in the current debugger's breakpoints if there is one.

package DAP.Module.Breakpoints is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module that manages DAP breakpoints.

   function Get_Persistent_Breakpoints return Breakpoint_Vectors.Vector;
   --  Return the list of persistent breakpoints.

   function Get_Breakpoint_From_Id
     (Id : Breakpoint_Identifier) return Breakpoint_Data;
   --  Return the breakpoint with the given ID.
   --  An empty breakpoint is returned if there is no breakpoint for this ID.

   function Get_Breakpoint_From_Index
     (Index  : Positive) return Breakpoint_Data;
   --  Retrieve breakpoint at the given index.

   procedure Set_Breakpoint_At_Index
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : Breakpoint_Data;
      Index  : Positive);
   --  Update breakpoint at the given index.

   procedure Delete_Multiple_Breakpoints
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Indexes : Breakpoint_Index_Lists.List);
   --  Go through the list and delete the breakpoints, using the given indexes.

   procedure Clear_All_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Delete all breakpoints.

   procedure Set_Breakpoints_State
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean);
   --  Enable or disable the breakpoints at the given indexes.

   procedure On_Debugging_Terminated
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the last running debugger has terminated.

   procedure On_Destroy;
   --  Called when GNAT Studio is terminating.

   procedure Store_As_Persistent
     (Executable  : Virtual_File;
      Breakpoints : Breakpoint_Vectors.Vector);
   --  Store the given list of breakpoints in the persistent ones.
   --  Persistent breakpoints will be restored in newer sessions on the same
   --  project.

   procedure Break
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : in out Breakpoint_Data);
   --  Add the given breakpoint.

   procedure Break_Source
     (Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the given SLOC.

   procedure Break_Subprogram
     (Kernel     : not null access Kernel_Handle_Record'Class;
      Subprogram : VSS.Strings.Virtual_String;
      Temporary  : Boolean := False;
      Condition  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore     : Natural := 0;
      Commands   : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the given subprogram.

   procedure Break_Exception
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False);
   --  Add a breakpoint for the given exception name.

   procedure Break_On_All_Exceptions
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Unhandled : Boolean := False);
   --  Break on all exception kinds.
   --  When Unhandled is True, it will only break on unhandled exceptions. When
   --  False, it will also break on handled ones.

   procedure Break_Address
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the given address.

   procedure Unbreak_Source
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Editable_Line_Type);
   --  Remove all the breakpoints set for the given SLOC.

   procedure Save_Persistent_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Save persistent breakpoints to properties database

   All_Exceptions_Filter : constant String := "exception";
   --  Filter that contains the name that means "all exceptions"

end DAP.Module.Breakpoints;
