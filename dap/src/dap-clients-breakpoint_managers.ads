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

with GNATCOLL.VFS;                use GNATCOLL.VFS;

with VSS.Strings;                 use VSS.Strings;

with Basic_Types;                 use Basic_Types;
with GPS.Kernel;
with GPS.Markers;                 use GPS.Markers;

with DAP.Modules.Breakpoints;     use DAP.Modules.Breakpoints;
with DAP.Types;                   use DAP.Types;
with DAP.Tools;

package DAP.Clients.Breakpoint_Managers is

   type Breakpoint_Manager_Type
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited private;
   --  Breakpoints manager when debugging is in progress

   type Breakpoint_Manager_Access is access
     all Breakpoint_Manager_Type'Class;

   procedure Initialize
     (Self : not null access Breakpoint_Manager_Type);
   --  Initialize the breakpoints' manager and set the initial breakpoints
   --  on the server's side.

   procedure Finalize (Self : not null access Breakpoint_Manager_Type);
   --  Finalize the breakpoints' manager, saving the persistant breakpoints
   --  if needed.

   procedure Stopped
     (Self         : not null access Breakpoint_Manager_Type;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer;
      Address      : out Address_Type);
   --  Called when the debugger is stopped

   procedure Break
     (Self : not null access Breakpoint_Manager_Type;
      Data : Breakpoint_Data);
   --  Add the given breakpoint

   procedure Break_Source
     (Self      : not null access Breakpoint_Manager_Type;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add breakpoint for the file/line

   procedure Break_Subprogram
     (Self       : not null access Breakpoint_Manager_Type;
      Subprogram : String;
      Temporary  : Boolean := False;
      Condition  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add breakpoint for the subprogram

   procedure Break_Exception
     (Self      : not null access Breakpoint_Manager_Type;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False);
   --  Add breakpoint for the exception

   procedure Break_Address
     (Self      : not null access Breakpoint_Manager_Type;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add a breakpoint for the address

   procedure Toggle_Instruction_Breakpoint
     (Self    : not null access Breakpoint_Manager_Type;
      Address : Address_Type);
   --  Add/delete a breakpoint for the address

   procedure Remove_Breakpoint_At
     (Self      : not null access Breakpoint_Manager_Type;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type);
   --  Remove breakpoint for the file/line

   procedure Remove_Breakpoints
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : DAP.Types.Breakpoint_Index_Lists.List);
   --  Remove breakpoints designed by the given indexes

   procedure Remove_Breakpoints
     (Self : not null access Breakpoint_Manager_Type;
      Ids  : DAP.Types.Breakpoint_Identifier_Lists.List);
   --  Remove breakpoints designed by the given IDs, which are set and
   --  recognized by the DAP server.

   procedure Remove_All_Breakpoints
     (Self : not null access Breakpoint_Manager_Type);
   --  Remove all breakpoints

   procedure Set_Breakpoints_State
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean);
   --  Enable/disable breakpoints

   procedure Set_Breakpoint_Command
     (Self    : not null access Breakpoint_Manager_Type;
      Id      : Breakpoint_Identifier;
      Command : VSS.Strings.Virtual_String);
   --  TODO: doc

   procedure Set_Ignore_Count
     (Self  : not null access Breakpoint_Manager_Type;
      Id    : Breakpoint_Identifier;
      Count : Natural);
   --  Sets ignore count for the breakpoint (i.e: the number of times
   --  the breakpoint should be ignored before stopping on it).

   function Get_Breakpoints
     (Self : Breakpoint_Manager_Access)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector;
   --  Returns the list of the breakpoints

   function Get_Breakpoint_From_Id
     (Self : not null access Breakpoint_Manager_Type;
      Id   : Breakpoint_Identifier) return Breakpoint_Data;
   --  TODO: doc

   procedure On_Notification
     (Self  : not null access Breakpoint_Manager_Type;
      Event : DAP.Tools.BreakpointEvent_body);
   --  Process DAP breakpoints notifications

   procedure Send_Commands
     (Self : not null access Breakpoint_Manager_Type;
      Data : DAP.Modules.Breakpoints.Breakpoint_Data);
   --  Set commands for the breakpoint if any

   procedure Send_Commands
     (Self : not null access Breakpoint_Manager_Type;
      Data : Breakpoint_Vectors.Vector);
   --  Set commands for the breakpoints

   function Has_Breakpoint
     (Self   : not null access Breakpoint_Manager_Type;
      Marker : Location_Marker)
      return Boolean;
   --  Return True if a breakpoint exists for the given location

private

   type Breakpoint_Manager_Type
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited record
      Holder : Breakpoint_Holder;
      --  actual breakpoints

      Requests_Count : Natural := 0;
      --  TODO: doc
   end record;

   type Synchonization_Data is record
      Files_To_Sync     : File_Sets.Set;
      Sync_Functions    : Boolean := False;
      Sync_Exceptions   : Boolean := False;
      Sync_Instructions : Boolean := False;
   end record;
   --  TODO: doc

   procedure Send_Line
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : Breakpoint_Index_Lists.List;
      Kind    : Breakpoint_Kind;
      File    : GNATCOLL.VFS.Virtual_File := No_File);
   --  TODO: doc

   procedure Synchronize_Breakpoints
     (Self      : not null access Breakpoint_Manager_Type;
      Sync_Data : Synchonization_Data);
   --  TODO: doc

   procedure Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      Item   : DAP.Tools.Breakpoint;
      Data   : in out Breakpoint_Data;
      File   : Virtual_File := No_File);
   --  TODO: doc

   procedure On_Breakpoint_Request_Response
     (Self            : not null access Breakpoint_Manager_Type;
      Client          : not null access DAP.Clients.DAP_Client'Class;
      New_Breakpoints : DAP.Tools.Breakpoint_Vector;
      Old_Breakpoints : Breakpoint_Index_Lists.List;
      File            : Virtual_File := No_File);
   --  TODO: doc

end DAP.Clients.Breakpoint_Managers;
