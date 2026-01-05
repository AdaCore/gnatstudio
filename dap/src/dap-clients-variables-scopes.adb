------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023-2026, AdaCore                     --
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

with VSS.Strings;             use VSS.Strings;
with DAP.Clients.Stack_Trace; use DAP.Clients.Stack_Trace;

package body DAP.Clients.Variables.Scopes is

   -------------------------
   -- Send_Scopes_Request --
   -------------------------

   procedure Send_Scopes_Request
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters)
   is
      Req : Scopes_Request_Access := new Scopes_Request
        (GPS.Kernel.Kernel_Handle (Client.Kernel), Params.Kind);
   begin
      Req.Params := Params;
      Req.Parameters.arguments.frameId :=
        Client.Get_Stack_Trace.Get_Current_Frame_Id;
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Send_Scopes_Request;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Scopes_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.ScopesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      Var : constant Variables_Holder_Access := Client.Get_Variables;
   begin
      New_Request := null;
      if Result.success then
         Var.Has_Scopes_Ids := True;
         for Index in 1 .. Length (Result.a_body.scopes) loop
            if Result.a_body.scopes (Index).name = "Locals" then
               Var.Locals_Scope_Id :=
                 Result.a_body.scopes (Index).variablesReference;

            elsif Result.a_body.scopes (Index).name = "Globals" then
               Var.Globals_Scope_Id :=
                 Result.a_body.scopes (Index).variablesReference;

            elsif Result.a_body.scopes (Index).name = "Arguments" then
               Var.Arguments_Scope_Id :=
                 Result.a_body.scopes (Index).variablesReference;
            end if;
         end loop;

         Var.On_Scopes_Result (Self.Params);

      else
         Self.Kernel.Get_Messages_Window.Insert_Error ("Can't get scopes ids");
         Free (Self.Params);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Scopes_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.Scopes.Scopes_DAP_Request
        (Self).On_Error_Message (Client, Message);
      Client.Variables.On_Variable_Not_Found (Self.Params);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Scopes_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.Scopes.Scopes_DAP_Request (Self).On_Rejected (Client);
      Client.Variables.On_Variable_Request_Rejected (Self.Params);
   end On_Rejected;

end DAP.Clients.Variables.Scopes;
