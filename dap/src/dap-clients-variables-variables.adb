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
with VSS.Strings.Conversions;

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
      Next_Id : Integer := -1;
      Kind    : Variable_Kind;
      Scope   : VSS.Strings.Virtual_String := "Locals";
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

      elsif Id = Var.Globals_Scope_Id then
         Kind := Globals;
      else
         --  Clear children, they will be rewrited,
         --  if we don't process Locals or Arguments
         Kind := Non_Specified;
         Var.Scopes.Delete_Children (C);
      end if;

      --  Add childs to Id
      if Result.a_body.variables.Length = 0 then
         --  GDB did not return nested elements, for example:
         --  {"request_seq": 17, "type": "response", "command": "variables",
         --   "success": true, "body": {"variables": []}, "seq": 146}
         --  so display "[]" as a value
         Var.Scopes.Append_Child
           (C,
            (Non_Specified,
             (name               => "<>",
              value              => "[]",
              a_type             => "<>",
              presentationHint   => (Is_Set => False),
              evaluateName       => <>,
              variablesReference => 0,
              namedVariables     => (Is_Set => False),
              indexedVariables   => (Is_Set => False),
              memoryReference    => <>)));

      else
         for Index in 1 .. Result.a_body.variables.Length loop
            Var.Scopes.Append_Child
              (C, (Kind, Result.a_body.variables (Index)));
         end loop;
      end if;

      if Id = Var.Arguments_Scope_Id then
         Next_Id := Var.Locals_Scope_Id;
      elsif Id = Var.Locals_Scope_Id then
         Next_Id := Var.Globals_Scope_Id;
         Scope := "Globals";
      end if;

      if Next_Id = 0 then
         Next_Id := Var.Globals_Scope_Id;
      end if;

      if Next_Id > 0 then
         --  Load next kind of variables
         Trace (Me, "Load " & VSS.Strings.Conversions.To_UTF_8_String (Scope));
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
      Message : VSS.Strings.Virtual_String)
   is
      Var     : constant Variables_Holder_Access := Client.Get_Variables;
      Id      : constant Integer :=
        Self.Parameters.arguments.variablesReference;
      C       : Variables_References_Trees.Cursor;
   begin
      DAP.Requests.Variables.Variables_DAP_Request
        (Self).On_Error_Message (Client, Message);

      C := Var.Find_By_Id (Id);
      if C = Variables_References_Trees.No_Element then
         --  We did not find any entrance
         return;
      end if;

      if Id = Var.Arguments_Scope_Id
        or else Id = Var.Locals_Scope_Id
        or else Id = Var.Globals_Scope_Id
        or else Message.Is_Empty
      then
         Client.Variables.On_Variable_Not_Found (Self.Params);

      else
         --  Can't get nested elements for some reason, display
         --  error message as a value to let user know about the problem.
         Var.Scopes.Append_Child
           (C,
            (Non_Specified,
             (name               => "<>",
              value              => Message,
              a_type             => "<>",
              presentationHint   => (Is_Set => False),
              evaluateName       => <>,
              variablesReference => 0,
              namedVariables     => (Is_Set => False),
              indexedVariables   => (Is_Set => False),
              memoryReference    => <>)));
         Var.On_Variables_Response (Self.Params);
      end if;
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Variables_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Client.Variables.On_Variable_Request_Rejected (Self.Params);
      DAP.Requests.Variables.Variables_DAP_Request (Self).On_Rejected (Client);
   end On_Rejected;

end DAP.Clients.Variables.Variables;
