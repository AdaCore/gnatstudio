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

package body DAP.Views.Variables.Evaluate_Requests is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use type DAP.Requests.DAP_Request_Access;

      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

      C   : Variables_References_Trees.Cursor;
      Var : DAP.Tools.Variable;
   begin
      New_Request := null;

      if View = null then
         return;
      end if;

      C := View.Scopes.Root;
      Var.a_type             := Result.a_body.a_type;
      Var.name               := Self.Item.Cmd;
      Var.indexedVariables   := Result.a_body.indexedVariables;
      Var.memoryReference    := Result.a_body.memoryReference;
      Var.namedVariables     := Result.a_body.namedVariables;
      Var.presentationHint   := Result.a_body.presentationHint;
      Var.value              := Result.a_body.result;
      --  Do not allow to expand the 'command' variable
      Var.variablesReference := 0;

      View.Scopes.Append_Child (C, Var);

      View.Publish_Or_Request
        (Self.Item, Self.Position, False, Self.Path, New_Request);

      if Self.Path /= Null_Gtk_Tree_Path then
         Path_Free (Self.Path);
      end if;

      if New_Request = null
        and then Self.Position /= 0
      then
         View.Continue_Update (Self.Position, New_Request);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String)
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      DAP.Requests.Evaluate.Evaluate_DAP_Request
        (Self).On_Error_Message (Client, Message);

      if View /= null then
         if Self.Position /= 0 then
            --  continue updating
            declare
               use type DAP.Requests.DAP_Request_Access;

               New_Request : DAP.Requests.DAP_Request_Access;
            begin
               if View /= null then
                  View.Continue_Update (Self.Position, New_Request);

                  if New_Request /= null then
                     Client.Enqueue (New_Request);
                  end if;
               end if;
            end;

         elsif Self.Item.Id /= Unknown_Id then
            View.Tree.Add_Row
              (Self.Item, Variables_References_Trees.No_Element, Null_Iter);
         end if;
      end if;
   end On_Error_Message;

end DAP.Views.Variables.Evaluate_Requests;
