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

with GNATCOLL.VFS;                use GNATCOLL.VFS;

with VSS.Strings;                 use VSS.Strings;

with Basic_Types;                 use Basic_Types;
with GPS.Kernel;
with GPS.Markers;                 use GPS.Markers;

with DAP.Types.Breakpoints;       use DAP.Types.Breakpoints;
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

   procedure Initialize_Breakpoints
     (Self : not null access Breakpoint_Manager_Type);
   --  Send all initial breakpoints to DAP server.

   procedure Get_Stopped_Event_Location
     (Self         : not null access Breakpoint_Manager_Type;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer;
      Address      : out Address_Type);
   --  Return the location where the debugger has stopped, using the hit
   --  breakpoint's location.

   procedure Break
     (Self : not null access Breakpoint_Manager_Type;
      Data : Breakpoint_Data);
   --  Add the given breakpoint.

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
      Subprogram : VSS.Strings.Virtual_String;
      Temporary  : Boolean := False;
      Condition  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Add breakpoint for the subprogram

   procedure Break_Exception
     (Self           : not null access Breakpoint_Manager_Type;
      Name           : String;
      Unhandled_Only : Boolean := False;
      Temporary      : Boolean := False);
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
      Indexes : Breakpoint_Index_Lists.List);
   --  Remove breakpoints designed by the given indexes

   procedure Remove_Breakpoints
     (Self : not null access Breakpoint_Manager_Type;
      Ids  : Breakpoint_Identifier_Lists.List);
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

   function Get_Breakpoint_From_Index
     (Self   : not null access Breakpoint_Manager_Type;
      Index  : Positive)
      return DAP.Types.Breakpoints.Breakpoint_Data;
   --  Retrieve data for breakpoint at Index

   procedure Replace_Breakpoint_At_Index
     (Self   : not null access Breakpoint_Manager_Type;
      Data   : DAP.Types.Breakpoints.Breakpoint_Data;
      Index  : Positive);
   --  Replace data for breakpoint at Index

   procedure Set_Breakpoint_Command
     (Self    : not null access Breakpoint_Manager_Type;
      Id      : Breakpoint_Identifier;
      Command : VSS.Strings.Virtual_String);
   --  Set a command for the given breakpoint.
   --  The command will then be executed each time the breakpoint is hit.

   procedure Set_Ignore_Count
     (Self  : not null access Breakpoint_Manager_Type;
      Id    : Breakpoint_Identifier;
      Count : Natural);
   --  Sets ignore count for the breakpoint (i.e: the number of times
   --  the breakpoint should be ignored before stopping on it).

   function Get_Breakpoints
     (Self : not null access Breakpoint_Manager_Type)
      return DAP.Types.Breakpoints.Breakpoint_Vectors.Vector;
   --  Returns the list of the breakpoints

   function Get_Breakpoint_From_Id
     (Self : not null access Breakpoint_Manager_Type;
      Id   : Breakpoint_Identifier) return Breakpoint_Data;
   --  Return the debugger's breakpoint with the given ID.

   procedure On_Notification
     (Self  : not null access Breakpoint_Manager_Type;
      Event : DAP.Tools.BreakpointEvent_body);
   --  Process DAP breakpoints notifications

   procedure Send_Commands
     (Self : not null access Breakpoint_Manager_Type;
      Data : DAP.Types.Breakpoints.Breakpoint_Data);
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

   procedure Continue_Until_Location
     (Self     : not null access Breakpoint_Manager_Type;
      Location : Breakpoint_Location_Type);
   --  Continue the program until the given location.

private

   type Breakpoint_Manager_Type
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited record
      Holder : Breakpoint_Holder;
      --  Actual breakpoints set for the debugger.

      Initial_Requests_Count : Natural := 0;
      --  The number of initial requests that still need to be processed. This
      --  is used to know when to send the 'configurationDone' DAP request,
      --  after having processed all the initial breakpoints' requests when
      --  starting the debugger.
   end record;

   procedure Send_Breakpoint_Request
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : Breakpoint_Index_Lists.List;
      Kind    : Breakpoint_Kind;
      File    : GNATCOLL.VFS.Virtual_File := No_File);
   --  Send the correspoding DAP request with the breakpoints located at the
   --  given indexes.
   --  Kind is used to know which DAP request should be sent for these
   --  breakpoints.
   --  File needs to be set only when sending SLOC breakpoints.

   type Synchonization_Data is record
      Files_To_Sync     : File_Sets.Set;
      --  The files that need to be synchonized.

      Sync_Functions    : Boolean := False;
      --  True if function breakpoints should be synchonized.

      Sync_Exceptions   : Boolean := False;
      --  True if exception breakpoints should be synchonized.

      Sync_Instructions : Boolean := False;
      --  True if instruction breakpoints should be synchonized.
   end record;
   --  Data representing which kind of breakpoints need to be synchonized.

   procedure Synchonize_Breakpoints
     (Self      : not null access Breakpoint_Manager_Type;
      Sync_Data : Synchonization_Data);
   --  Synchonize the manager's breakpoints with the underlying DAP server,
   --  sending the needed requests according to Sync_Data.

   procedure Update
     (Kernel : GPS.Kernel.Kernel_Handle;
      Item   : DAP.Tools.Breakpoint;
      Data   : in out Breakpoint_Data;
      File   : Virtual_File := No_File);
   --  Update the given breakpoint according to the specified DAP breakpoint
   --  response, updating the fields that are relevant to the breakpoint's
   --  kind (i.e: it will update the breakpoint's location if it's a SLOC
   --  breakpoint).

   procedure On_Breakpoint_Request_Response
     (Self            : not null access Breakpoint_Manager_Type;
      Client          : not null access DAP.Clients.DAP_Client'Class;
      New_Breakpoints : DAP.Tools.Breakpoint_Vector;
      Old_Breakpoints : Breakpoint_Index_Lists.List;
      File            : Virtual_File := No_File);
   --  Called in response of all the breakpoint-related DAP requests, updating
   --  the breakpoints stored in Old_Breakpoints indexes according to the ones
   --  received from the DAP server in New_Breakpoints.
   --  File should be set when receiving a response for SLOC breakpoints
   --  (i.e: DAP's 'setBreakpoints' request).

end DAP.Clients.Breakpoint_Managers;
