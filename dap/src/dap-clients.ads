------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with VSS.Strings;
with GPS.Kernel;
with LSP.Raw_Clients;
with DAP.Requests;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with LSP.Types;
private with LSP.JSON_Streams;

package DAP.Clients is

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client with private;

   type DAP_Client_Access is access all DAP_Client'Class;

   procedure Start
     (Self    : in out DAP_Client;
      Adapter : String;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access);

   procedure Quit (Self : in out DAP_Client);

   procedure On_Configured (Self : in out DAP_Client);
   procedure On_Terminated (Self : in out DAP_Client);

private

   package Requests_Lists is new
     Ada.Containers.Doubly_Linked_Lists
       (DAP.Requests.DAP_Request_Access,
        "=" => DAP.Requests."=");

   function Hash
     (Item : LSP.Types.LSP_Number)
      return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Val (Item));

   package Requests_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_Number,
      Element_Type    => DAP.Requests.DAP_Request_Access,
      Hash            => Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => DAP.Requests."=");

   type DAP_Client
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : Positive) is new LSP.Raw_Clients.Raw_Client
   with record
      This    : DAP_Client_Access := DAP_Client'Unchecked_Access;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : Ada.Strings.Unbounded.Unbounded_String;

      Is_Attached    : Boolean := False;
      Is_Ready       : Boolean := False;
      Is_Terminating : Boolean := False;

      Queue      : Requests_Lists.List;
      Sent       : Requests_Maps.Map;

      Request_Id : LSP.Types.LSP_Number := 1;
      Error_Msg  : VSS.Strings.Virtual_String;
   end record;

   overriding function Error_Message
     (Self : DAP_Client) return VSS.Strings.Virtual_String;
   --  Error message for the last detected "internal error".

   overriding procedure On_Raw_Message
     (Self    : in out DAP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);

   overriding procedure On_Started (Self : in out DAP_Client);
   --  Send initialization request on successful startup of the language
   --  server process.

   overriding procedure On_Finished (Self : in out DAP_Client);
   --  Handle termination of the language server process. If this wasn't
   --  expected and we're within the acceptable throttling limits, relaunch.

   --  overriding procedure On_Error
   --    (Self  : in out DAP_Client;
   --     Error : String);

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access);

   function Get_Request_ID
     (Self : in out DAP_Client) return LSP.Types.LSP_Number;

   procedure Reject_All_Requests (Self : in out DAP_Client);

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class;
      Event  : VSS.Strings.Virtual_String);

end DAP.Clients;
