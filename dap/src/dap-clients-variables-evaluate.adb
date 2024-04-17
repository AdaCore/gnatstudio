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

with DAP.Clients.Stack_Trace; use DAP.Clients.Stack_Trace;
with DAP.Views.Variables;

package body DAP.Clients.Variables.Evaluate is

   ---------------------------
   -- Send_Evaluate_Request --
   ---------------------------

   procedure Send_Evaluate_Request
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Params : Request_Parameters)
   is
      Req : Evaluate_Request_Access := new Evaluate_Request
        (GPS.Kernel.Kernel_Handle (Client.Kernel), Params.Kind);
   begin
      Req.Params := Params;
      Req.Parameters.arguments.expression := Params.Item.Info.Get_Name;
      Req.Parameters.arguments.frameId :=
        Client.Get_Stack_Trace.Get_Current_Frame_Id;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);

      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Send_Evaluate_Request;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      Holder   : constant Variables_Holder_Access := Client.Get_Variables;
      Variable : Variable_Data;
   begin
      New_Request := null;

      if Result.success then
         Variable.Data.a_type           := Result.a_body.a_type;
         Variable.Data.name             := Self.Params.Item.Info.Get_Name;
         Variable.Data.indexedVariables := Result.a_body.indexedVariables;
         Variable.Data.memoryReference  := Result.a_body.memoryReference;
         Variable.Data.namedVariables   := Result.a_body.namedVariables;
         Variable.Data.presentationHint := Result.a_body.presentationHint;
         Variable.Data.value            := Result.a_body.result;

         Holder.Scopes.Append_Child (Holder.Scopes.Root, Variable);
         Holder.On_Variables_Response (Self.Params);

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           (Self.Params.Item.Info.Get_Name & " is not found.");
         Free (Self.Params);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.Evaluate.Evaluate_DAP_Request
        (Self).On_Error_Message (Client, Message);

      DAP.Views.Variables.On_Variable_Not_Found (Client, Self.Params);
      Free (Self.Params);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self    : in out Evaluate_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.Evaluate.Evaluate_DAP_Request (Self).On_Rejected (Client);
      Free (Self.Params);
   end On_Rejected;

end DAP.Clients.Variables.Evaluate;
