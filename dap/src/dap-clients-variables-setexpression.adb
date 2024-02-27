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

with VSS.Strings.Conversions;
with DAP.Clients.Stack_Trace;     use DAP.Clients.Stack_Trace;

package body DAP.Clients.Variables.SetExpression is

   ---------------------------------
   -- Send_Set_Expression_Request --
   ---------------------------------

   procedure Send_Set_Expression_Request
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters)
   is
      Req : Set_Expression_Request_Access := new Set_Expression_Request
        (GPS.Kernel.Kernel_Handle (Client.Kernel), Params.Kind);
   begin
      Req.Params := Params;
      Req.Parameters.arguments.expression := Params.Name;
      Req.Parameters.arguments.value      := Params.Value;
      Req.Parameters.arguments.frameId    :=
        Client.Get_Stack_Trace.Get_Current_Frame_Id;
      if Params.Item.Format /= Default_Format then
         Req.Parameters.arguments.format :=
           (Is_Set => True, Value => Params.Item.Format);
      end if;

      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Send_Set_Expression_Request;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Set_Expression_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetExpressionResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      Var      : constant Variables_Holder_Access := Client.Get_Variables;
      Cursor   : Variables_References_Trees.Cursor;
      Found    : Boolean;
      Variable : Variable_Data;
   begin
      New_Request := null;

      if Result.success then
         Cursor := Var.Scopes.Root;
         Find_Name_Or_Parent (Self.Params.Name, Cursor, Found);
         if Found then
            Variable := Element (Cursor);

            Variable.Data.a_type             := Result.a_body.a_type;
            Variable.Data.value              := Result.a_body.value;
            Variable.Data.indexedVariables   := Result.a_body.indexedVariables;
            Variable.Data.namedVariables     := Result.a_body.namedVariables;
            Variable.Data.presentationHint   := Result.a_body.presentationHint;
            Variable.Data.variablesReference :=
              (if Result.a_body.variablesReference.Is_Set
               then Result.a_body.variablesReference.Value
               else 0);

            Var.Scopes.Replace_Element (Cursor, Variable);
            Var.On_Variable_Set (Self.Params, Variable.Data);
         end if;

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           (VSS.Strings.Conversions.To_UTF_8_String (Self.Params.Name) &
              " is not set.");

         Free (Self.Params);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Set_Expression_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.SetExpression.Set_Expression_DAP_Request
        (Self).On_Error_Message (Client, Message);
      Free (Self.Params);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Set_Expression_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.SetExpression.Set_Expression_DAP_Request
        (Self).On_Rejected (Client);
      Free (Self.Params);
   end On_Rejected;

end DAP.Clients.Variables.SetExpression;
