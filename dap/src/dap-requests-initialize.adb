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

with GNATCOLL.Traces;       use GNATCOLL.Traces;

with VSS.Strings.Conversions;

with DAP.Requests.Launch;

package body DAP.Requests.Initialize is

   Me : constant Trace_Handle := Create ("DAP.Requests.Initialize", On);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Initialize_DAP_Request;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String) is
   begin
      Self.Project := Project;
      Self.File    := File;
      Self.Args    := Ada.Strings.Unbounded.To_Unbounded_String (Args);
   end Initialize;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Initialize_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      DAP.Tools.InitializeRequest'Write (Stream, Self.Parameters);
   end Write;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Initialize_DAP_Request) is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Initialize_DAP_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("[Debug]:" &
           VSS.Strings.Conversions.To_UTF_8_String (Message));

      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.InitializeResponse;

   begin
      DAP.Tools.InitializeResponse'Read (Stream, Response);
      Initialize_DAP_Request'Class
        (Self).On_Result_Message (Response, New_Request);
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Result      : DAP.Tools.InitializeResponse;
      New_Request : in out DAP_Request_Access)
   is

      Launch : constant DAP.Requests.Launch.Launch_DAP_Request_Access :=
        new DAP.Requests.Launch.Launch_DAP_Request (Self.Kernel);
   begin
      Launch.Initialize (Self.Project, Self.File, Self.Args);
      New_Request := DAP_Request_Access (Launch);
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Initialize_DAP_Request;
      Id   : LSP.Types.LSP_Number) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Initialize;
