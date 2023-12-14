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

package body DAP.Views.Variables.Variables is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Variables_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.VariablesResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use type DAP.Requests.DAP_Request_Access;

      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

      C : Variables_References_Trees.Cursor;
   begin
      New_Request := null;

      if View = null then
         return;
      end if;

      C := View.Find_Ref (Self.Ref);
      if C = Variables_References_Trees.No_Element then
         return;
      end if;

      if Self.Ref /= View.Arguments_Scope_Id
        and then Self.Ref /= View.Locals_Scope_Id
      then
         --  Do not clear when we load Locals or Arguments
         View.Scopes.Delete_Children (C);
      end if;

      for Index in 1 .. Length (Result.a_body.variables) loop
         View.Scopes.Append_Child (C, Result.a_body.variables (Index));
      end loop;

      if Self.Ref = View.Locals_Scope_Id
        and then View.Arguments_Scope_Id /= 0
      then
         --  We loaded Locals and Arguments also exist, so load them too
         declare
            Req : constant Variables_Request_Access :=
              new Variables_Request (Self.Kernel);
         begin
            Req.Item     := Self.Item;
            Req.Position := Self.Position;
            Req.Childs   := Self.Childs;
            if Self.Path /= Null_Gtk_Tree_Path then
               Req.Path := Copy (Self.Path);
            end if;
            Req.Ref := View.Arguments_Scope_Id;
            Req.Parameters.arguments.variablesReference := Req.Ref;
            New_Request := DAP.Requests.DAP_Request_Access (Req);
         end;

      else
         --  Locals and Arguments are loaded, trying to load nested items
         --  if needed.

         View.Publish_Or_Request
           (Item     => Self.Item,
            Position => Self.Position,
            Childs   => Self.Childs,
            Path     => Self.Path,
            Request  => New_Request);
         --  Fill the model or create new request if variable is not loaded yet

         if Self.Path /= Null_Gtk_Tree_Path then
            Path_Free (Self.Path);
         end if;

         if New_Request = null
           and then Self.Position /= 0
         then
            --  We have finished updating the current variable
            --  (no more requests) and updating the view/all variables
            --  (Position /= 0). Informing the view so it can continue
            --  updating the next variable.

            View.Continue_Update (Self.Position, New_Request);
         end if;
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
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

   begin
      DAP.Requests.Variables.Variables_DAP_Request
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

end DAP.Views.Variables.Variables;
