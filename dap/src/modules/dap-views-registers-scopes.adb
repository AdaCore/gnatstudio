------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with VSS.Strings; use VSS.Strings;
with DAP.Tools;   use DAP.Tools;

with DAP.Views.Registers.Variables;
use DAP.Views.Registers.Variables;

package body DAP.Views.Registers.Scopes is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Scopes_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.ScopesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use Registers_MDI_Views;

      View : constant DAP_Registers_View :=
        Registers_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      if View = null then
         return;
      end if;

      for Index in 1 .. Length (Result.a_body.scopes) loop
         if Result.a_body.scopes (Index).name = "Registers" then
            View.Registers_Id :=
              Result.a_body.scopes (Index).variablesReference;

            declare
               Req : constant Variables_Request_Access :=
                 new Variables_Request (Self.Kernel);
            begin
               Req.Kind := Self.Kind;
               Req.Parameters.arguments.variablesReference :=
                 View.Registers_Id;
               New_Request := DAP.Requests.DAP_Request_Access (Req);
            end;

            exit;
         end if;
      end loop;
   end On_Result_Message;

end DAP.Views.Registers.Scopes;
