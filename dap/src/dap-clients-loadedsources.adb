------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023-2026, AdaCore                  --
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

with DAP.Tools;

package body DAP.Clients.LoadedSources is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : not null Kernel_Handle)
      return Loaded_Sources_Request_Access
   is
      Self : constant Loaded_Sources_Request_Access :=
        new Loaded_Sources_Request (Kernel);
   begin
      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Loaded_Sources_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
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

      Client.Set_Source_Files (Source_Files);
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Loaded_Sources_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.LoadedSources.On_Rejected
        (DAP.Requests.LoadedSources.Loaded_Sources_DAP_Request (Self),
         Client);
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Loaded_Sources_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.LoadedSources.On_Error_Message
        (DAP.Requests.LoadedSources.Loaded_Sources_DAP_Request (Self),
         Client, Message);
   end On_Error_Message;

end DAP.Clients.LoadedSources;
