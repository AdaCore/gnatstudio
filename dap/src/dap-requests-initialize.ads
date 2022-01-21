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

with Ada.Strings.Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with DAP.Tools;

package DAP.Requests.Initialize is

   type Initialize_DAP_Request is new DAP_Request with private;

   type Initialize_DAP_Request_Access is access all Initialize_DAP_Request;

   procedure Initialize
     (Self    : in out Initialize_DAP_Request;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String);

   overriding procedure Write
     (Self   : Initialize_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Result      : DAP.Tools.InitializeResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected (Self : in out Initialize_DAP_Request);

   overriding procedure On_Error_Message
     (Self    : in out Initialize_DAP_Request;
      Message : VSS.Strings.Virtual_String);

   overriding procedure Set_Seq
     (Self : in out Initialize_DAP_Request;
      Id   : LSP.Types.LSP_Number);

private

   type Initialize_DAP_Request is new DAP_Request with record
      Parameters : aliased DAP.Tools.InitializeRequest :=
        DAP.Tools.InitializeRequest'
          (seq       => 0,
           a_type    => "request",
           command   => "initialize",
           arguments =>
             (adapterID                    => "0",
              clientID                     => "0",
              clientName                   => "GNATSTUDIO",
              locale                       => "en-US",
              pathFormat                   => "path",
              columnsStartAt1              => True,
              linesStartAt1                => True,
              supportsInvalidatedEvent     => False,
              supportsMemoryReferences     => False,
              supportsProgressReporting    => True,
              supportsRunInTerminalRequest => False,
              supportsVariablePaging       => False,
              supportsVariableType         => False));

      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end DAP.Requests.Initialize;
