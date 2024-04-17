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

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with DAP.Views.Variables;

package body DAP.Clients.Variables.Variables is

   Me : constant Trace_Handle := Create ("DAP.Variables_Request", Off);

   ----------------------------
   -- Send_Variables_Request --
   ----------------------------

   procedure Send_Variables_Request
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Id     : Integer;
      Params : Request_Parameters)
   is
      use DAP.Modules.Variables.Items;

      Req : Variables_Request_Access := new Variables_Request
        (GPS.Kernel.Kernel_Handle (Client.Kernel), Params.Kind);
   begin
      if Id > 0 then
         Req.Params := Params;
         Req.Parameters.arguments.variablesReference := Id;
         if Params.Item.Info.Format /= Default_Format then
            Req.Parameters.arguments.format :=
              (Is_Set => True, Value => Params.Item.Info.Format);
         end if;

         Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
      end if;
   end Send_Variables_Request;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Variables_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.VariablesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      Var     : constant Variables_Holder_Access := Client.Get_Variables;
      Id      : constant Integer :=
        Self.Parameters.arguments.variablesReference;
      C       : Variables_References_Trees.Cursor;
      Next_Id : Integer := 0;
      Kind    : Variable_Kind;
   begin
      Trace (Me, "On_Result_Message");
      New_Request := null;

      C := Var.Find_By_Id (Id);
      if C = Variables_References_Trees.No_Element then
         --  We did not find any entrance
         return;
      end if;

      if Id = Var.Arguments_Scope_Id then
         Kind := Arguments;

      elsif Id = Var.Locals_Scope_Id then
         Kind := Locals;

      else
         --  Clear children, they will be rewrited,
         --  if we don't process Locals or Arguments
         Kind := Non_Specified;
         Var.Scopes.Delete_Children (C);
      end if;

      --  Add childs to Id
      if Result.a_body.variables.Length = 0 then
         Var.Scopes.Append_Child (C, Empty_Variable_Data);
      else
         for Index in 1 .. Result.a_body.variables.Length loop
            Var.Scopes.Append_Child
              (C, (Kind, Result.a_body.variables (Index)));
         end loop;
      end if;

      if Id = Var.Arguments_Scope_Id then
         Next_Id := Var.Locals_Scope_Id;
      end if;

      if Next_Id /= 0 then
         --  We loaded Locals and Arguments also exist, so load them too
         Trace (Me, "Load Locals");
         Send_Variables_Request (Client, Next_Id, Self.Params);

      else
         --  Locals and Arguments are loaded, trying to load nested items
         --  if needed.
         Var.On_Variables_Response (Self.Params);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Variables_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.Variables.Variables_DAP_Request
        (Self).On_Error_Message (Client, Message);

      DAP.Views.Variables.On_Variable_Not_Found (Client, Self.Params);
      Free (Self.Params);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Variables_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.Variables.Variables_DAP_Request (Self).On_Rejected (Client);
      Free (Self.Params);
   end On_Rejected;

end DAP.Clients.Variables.Variables;
