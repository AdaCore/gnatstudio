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

with VSS.String_Vectors;

with DAP.Clients;
with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body DAP.Requests.Loaded_Sources is

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Loaded_Sources_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_LoadedSourcesRequest (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Loaded_Sources_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.LoadedSourcesResponse;
   begin
      DAP.Tools.Inputs.Input_LoadedSourcesResponse (Stream, Response, Success);

      if Success then
         Loaded_Sources_DAP_Request'Class
           (Self).On_Result_Message (Response, New_Request);
      end if;
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Loaded_Sources_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Loaded_Sources_DAP_Request;
      Result      : DAP.Tools.LoadedSourcesResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;
      Source_Files : VSS.String_Vectors.Virtual_String_Vector;
   begin
      New_Request := null;

      for Index in 1 .. Length (Result.a_body.sources) loop
         declare
            Src : constant Source := Result.a_body.sources (Index);
         begin
            Source_Files.Append (Src.path);
         end;
      end loop;

      Self.Client.Set_Source_Files (Source_Files);
      Self.Client.On_Launched;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Loaded_Sources_DAP_Request) is
   begin
      DAP_Request (Self).On_Rejected;
      Self.Client.On_Launched;
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Loaded_Sources_DAP_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP_Request (Self).On_Error_Message (Message);
      Self.Client.On_Launched;
   end On_Error_Message;

end DAP.Requests.Loaded_Sources;
