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

with VSS.Strings;

with Basic_Types;                 use Basic_Types;
with GPS.Kernel;

with DAP.Breakpoint_Maps;         use DAP.Breakpoint_Maps;
with DAP.Types;                   use DAP.Types;
with DAP.Tools;
with DAP.Requests;                use DAP.Requests;

limited with DAP.Clients;

private with DAP.Requests.Breakpoints;
private with DAP.Requests.Function_Breakpoints;

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
      Stopped_Line : out Integer);
   --  Called when the debugger is stopped

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

   procedure Remove_Breakpoint_At
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type);
   --  Remove breakpoint for the file/line

   procedure Remove_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access;
      List : DAP.Types.Breakpoint_Identifier_Lists.List);
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
      return DAP.Breakpoint_Maps.Breakpoint_Vectors.Vector;
   --  Returns the list of the breakpoints

   procedure Show_Breakpoints (Self : in out DAP_Client_Breakpoint_Manager);
   --  Show breakpoints on the side column of the editors

private

   type DAP_Client_Breakpoint_Manager
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class) is
     tagged limited record
      Requests_Count : Integer := 0;
      Holder         : Breakpoint_Holder;
      --  actual breakpoints
   end record;

   type Action_Kind is (Init, Add, Edit, Delete, Change_Status, Feedback);
   --  Type of a request to DAP adapter:
   --   Init: set breakpoints initially
   --   Add: add one new breakpoint
   --   Edit:
   --   Delete: delete one or multiple breakpoints
   --   Change_Status: Enable/disable one or more breakpoints
   --   Feedback: set actual breakpoints after delete duplicates for example

   function Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind) return DAP_Request_Access;

   procedure Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind);
   --  Send a request for line breakpoints

   function Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
      return DAP_Request_Access;

   procedure Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind);
   --  Send a request for subprogram breakpoints

   procedure Dec_Response
     (Self   : in out DAP_Client_Breakpoint_Manager;
      Action : Action_Kind);
   --  To calculate responses and make actions when all of them are processed

   -- Source_Line_Request --

   type Source_Line_Request is
     new DAP.Requests.Breakpoints.Breakpoint_DAP_Request
   with record
      Manager : DAP_Client_Breakpoint_Manager_Access;
      File    : GNATCOLL.VFS.Virtual_File;
      Action  : Action_Kind;
      Sent    : Breakpoint_Vectors.Vector;
   end record;

   type Source_Line_Request_Access is access all Source_Line_Request;

   overriding procedure On_Result_Message
     (Self        : in out Source_Line_Request;
      Result      : in out DAP.Tools.SetBreakpointsResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected (Self : in out Source_Line_Request);

   overriding procedure On_Error_Message
     (Self    : in out Source_Line_Request;
      Message : VSS.Strings.Virtual_String);

   --  Function_Breakpoint_DAP_Request --

   type Function_Breakpoint_Request is
     new DAP.Requests.Function_Breakpoints.Function_Breakpoint_DAP_Request
   with record
      Manager : DAP_Client_Breakpoint_Manager_Access;
      Action  : Action_Kind;
      Data    : Breakpoint_Data;
      Sent    : Breakpoint_Vectors.Vector;
   end record;

   type Function_Breakpoint_Request_Access is
     access all Function_Breakpoint_Request;

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_Request;
      Result      : in out DAP.Tools.SetFunctionBreakpointsResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected
     (Self : in out Function_Breakpoint_Request);

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_Request;
      Message : VSS.Strings.Virtual_String);

end DAP.Modules.Breakpoint_Managers;
