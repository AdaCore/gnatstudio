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

with Basic_Types;                 use Basic_Types;
with GPS.Kernel;

with DAP.Modules.Breakpoints;     use DAP.Modules.Breakpoints;
with DAP.Types;                   use DAP.Types;
with DAP.Tools;
with DAP.Requests;                use DAP.Requests;

limited with DAP.Clients;

private with GPS.Editors;

package DAP.Modules.Breakpoint_Managers is

   type DAP_Client_Breakpoint_Manager
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited private;
   --  Breakpoints manager when debugging is in progress

   type DAP_Client_Breakpoint_Manager_Access is access
     all DAP_Client_Breakpoint_Manager'Class;

   procedure Initialize (Self : DAP_Client_Breakpoint_Manager_Access);
   procedure Finalize (Self : DAP_Client_Breakpoint_Manager_Access);

   procedure Stopped
     (Self         : DAP_Client_Breakpoint_Manager_Access;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer;
      Address      : out Address_Type);
   --  Called when the debugger is stopped

   procedure Break
     (Self : DAP_Client_Breakpoint_Manager_Access;
      Data : Breakpoint_Data);
   --  Add breakpoint

   procedure Break_Sorce
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False);
   --  Add breakpoint for the file/line

   procedure Break_Subprogram
     (Self       : DAP_Client_Breakpoint_Manager_Access;
      Subprogram : String;
      Temporary  : Boolean := False);
   --  Add breakpoint for the subprogram

   procedure Break_Exception
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False);
   --  Add breakpoint for the exception

   procedure Toggle_Instruction_Breakpoint
     (Self    : DAP_Client_Breakpoint_Manager_Access;
      Address : Address_Type);
   --  Add/delete a breakpoint for the address

   procedure Remove_Breakpoint_At
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type);
   --  Remove breakpoint for the file/line

   procedure Remove_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access;
      Nums : DAP.Types.Breakpoint_Identifier_Lists.List);
   --  Remove breakpoints included in the list

   procedure Remove_All_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access);
   --  Remove all breakpoints

   procedure Set_Breakpoints_State
     (Self  : DAP_Client_Breakpoint_Manager_Access;
      Nums  : Breakpoint_Identifier_Lists.List;
      State : Boolean);
   --  Enable/disable breakpoints

   function Get_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector;
   --  Returns the list of the breakpoints

   procedure Show_Breakpoints (Self : in out DAP_Client_Breakpoint_Manager);
   --  Show breakpoints on the side column of the editors

   procedure On_Notification
     (Self  : DAP_Client_Breakpoint_Manager_Access;
      Event : DAP.Tools.BreakpointEvent_body);
   --  Process DAP breakpoints notifications

private

   type DAP_Client_Breakpoint_Manager
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited record
      Requests_Count : Integer := 0;
      Holder         : Breakpoint_Holder;
      --  actual breakpoints
   end record;

   type Action_Kind is
     (Init, Add, Delete, Enable, Disable, Synch);
   --  Type of a request to DAP adapter:
   --   Init: set breakpoints initially
   --   Add: add one new breakpoint
   --   Delete: delete one or multiple breakpoints
   --   Enable/Disable: Enable/disable one or more breakpoints
   --   Synch: set actual breakpoints after delete duplicates for example

   function Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind) return DAP_Request_Access;
   --  Send a request for line breakpoints

   procedure Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind);
   --  Send a request for line breakpoints

   function Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
      return DAP_Request_Access;
   --  Send a request for subprograms breakpoints

   procedure Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind;
      Bunch  : Boolean);
   --  Send a request for subprogram breakpoints

   procedure Send_Exception
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind);

   procedure Send_Addresses
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind);

   procedure Dec_Response
     (Self   : in out DAP_Client_Breakpoint_Manager;
      Action : Action_Kind);
   --  To calculate responses and make actions when all of them are processed

   procedure Send
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Map    : Breakpoint_Hash_Maps.Map;
      Action : Action_Kind;
      Bunch  : Boolean);
   --  Send breakpoints request (lines & subprograms)

   procedure Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : Virtual_File;
      Holder : GPS.Editors.Controlled_Editor_Buffer_Holder;
      Data   : in out Breakpoint_Data;
      Item   : DAP.Tools.Breakpoint);

   function Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      DAP_Bp : DAP.Tools.Breakpoint) return Breakpoint_Data;

end DAP.Modules.Breakpoint_Managers;
