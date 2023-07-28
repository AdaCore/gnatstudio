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

with GNATCOLL.VFS;            use GNATCOLL.VFS;

with VSS.Strings;

with Basic_Types;             use Basic_Types;
with GPS.Kernel;              use GPS.Kernel;
with DAP.Types;               use DAP.Types;
with DAP.Modules.Breakpoints; use DAP.Modules.Breakpoints;

package DAP.Modules.Persistent_Breakpoints is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   function Get_Persistent_Breakpoints return Breakpoint_Vectors.Vector;

   function Get_Persistent_For_Executable
     (Executable : Virtual_File) return Breakpoint_Vectors.Vector;

   procedure Show_Breakpoint
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data);

   procedure Show_Breakpoints_In_All_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Update the side column for all editors, and show the persistent
   --  breakpoints info

   procedure Hide_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);

   procedure Delete_Multiple_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List);
   --  Go through the list and delete the breakpoints. The list is not freed
   --  by this procedure.

   procedure Clear_All_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Delete all breakpoints

   procedure Set_Breakpoints_State
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List;
      State  : Boolean);

   procedure On_Debugging_Terminated
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the last debugger is finished

   procedure On_Destroy;
   --  Called when GNAT Studio is terminating

   function Get_Next_Id return Breakpoint_Identifier;

   procedure Store
     (Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector);

   procedure Break
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : in out Breakpoint_Data);

   procedure Break_Source
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Num       : Breakpoint_Identifier;
      File      : Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   --  Add breakpoint for the source line

   procedure Break_Subprogram
     (Kernel     : not null access Kernel_Handle_Record'Class;
      Num        : Breakpoint_Identifier;
      Subprogram : String;
      Temporary  : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add breakpoint for the subprogram

   procedure Break_Exception
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Num       : Breakpoint_Identifier;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False);
   --  Add breakpoint for the exception

   procedure Break_Address
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Num       : Breakpoint_Identifier;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add breakpoint for the address

   procedure Unbreak_Source
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Editable_Line_Type);
   --  Set a breakpoint on the given location.
   --  If no debugger is currently running, the breakpoint will be applied when
   --  one is started. If one or more debuggers are running, they all break
   --  at that location

   procedure Save_Persistent_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Save persistent breakpoints to properties database

end DAP.Modules.Persistent_Breakpoints;
