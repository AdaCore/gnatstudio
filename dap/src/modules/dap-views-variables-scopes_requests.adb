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

with DAP.Views.Variables.Variables_Requests;
use DAP.Views.Variables.Variables_Requests;

with DAP.Views.Variables.Evaluate_Requests;
use DAP.Views.Variables.Evaluate_Requests;

package body DAP.Views.Variables.Scopes_Requests is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Scopes_Request;
      Result      : in out DAP.Tools.ScopesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      for Index in 1 .. Length (Result.a_body.scopes) loop
         if Result.a_body.scopes (Index).name = "Locals" then
            View.Locals_Id := Result.a_body.scopes (Index).variablesReference;

            if Self.Item.Cmd.Is_Empty then
               --  it is not a command, use variable request
               declare
                  Req : constant Variables_Request_Access :=
                    new Variables_Request (Self.Kernel);
               begin
                  Req.Client   := Self.Client;
                  Req.Item     := Self.Item;
                  Req.Position := Self.Position;
                  Req.Childs   := Self.Childs;
                  if Self.Path /= Null_Gtk_Tree_Path then
                     Req.Path := Copy (Self.Path);
                  end if;
                  Req.Ref := View.Locals_Id;
                  Req.Parameters.arguments.variablesReference := Req.Ref;
                  New_Request := DAP.Requests.DAP_Request_Access (Req);
               end;

            else
               --  it is command, use evaluate request
               declare
                  Req : constant DAP.Views.Variables.Evaluate_Requests.
                    Evaluate_Request_Access :=
                      new DAP.Views.Variables.Evaluate_Requests.
                        Evaluate_Request (Self.Kernel);
               begin
                  Req.Client   := Self.Client;
                  Req.Item     := Self.Item;
                  Req.Position := Self.Position;
                  if Self.Path /= Null_Gtk_Tree_Path then
                     Req.Path := Copy (Self.Path);
                  end if;

                  Req.Parameters.arguments.expression := Self.Item.Cmd;
                  Req.Parameters.arguments.frameId :=
                    Self.Client.Get_Selected_Frame_Id;
                  Req.Parameters.arguments.context :=
                    (Is_Set => True, Value => DAP.Tools.Enum.repl);

                  New_Request := DAP.Requests.DAP_Request_Access (Req);
               end;
            end if;

            exit;
         end if;
      end loop;

      if Self.Path /= Null_Gtk_Tree_Path then
         Path_Free (Self.Path);
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Scopes_Request;
      Message : VSS.Strings.Virtual_String)
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      DAP.Requests.Scopes.Scopes_DAP_Request
        (Self).On_Error_Message (Message);

      if View /= null
        and then Self.Item.Id /= Unknown_Id
      then
         View.Tree.Add_Row
           (Self.Item, Variables_References_Trees.No_Element, Null_Iter);
      end if;
   end On_Error_Message;

end DAP.Views.Variables.Scopes_Requests;
