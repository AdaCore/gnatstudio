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

with Ada.Characters.Handling;
with Glib_Values_Utils;           use Glib_Values_Utils;
with VSS.Strings.Conversions;
with Gdk.RGBA;                    use Gdk.RGBA;

with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with DAP.Utils;                   use DAP.Utils;
with GUI_Utils;

package body DAP.Views.Variables.SetExpression is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Set_Expression_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetExpressionResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      View : constant DAP_Variables_View :=
        Variables_MDI_Views.Retrieve_View
          (Self.Kernel,
           Visible_Only => False);

      Iter   : Gtk_Tree_Iter;
      Cursor : Variables_References_Trees.Cursor;
      Found  : Boolean;
      Var    : DAP.Tools.Variable;
   begin
      New_Request := null;

      if Result.success then
         if View /= null then
            Cursor := View.Scopes.Root;
            Find_Best_Ref (Self.Name, Cursor, Found);
            if Found then
               Var := Element (Cursor);

               Var.a_type             := Result.a_body.a_type;
               Var.value              := Result.a_body.value;
               Var.indexedVariables   := Result.a_body.indexedVariables;
               Var.namedVariables     := Result.a_body.namedVariables;
               Var.presentationHint   := Result.a_body.presentationHint;
               Var.variablesReference :=
                 (if Result.a_body.variablesReference.Is_Set
                  then Result.a_body.variablesReference.Value
                  else 0);

               View.Scopes.Replace_Element (Cursor, Var);
            end if;

            if Self.Path = Null_Gtk_Tree_Path then
               Iter := GUI_Utils.Find_Node
                 (Model     => View.Tree.Model,
                  Name      => Ada.Characters.Handling.To_Lower
                    (VSS.Strings.Conversions.To_UTF_8_String
                         (Self.Name)),
                  Column    => Column_Full_Name,
                  Recursive => False);

            else
               --  The user has edited the row with the given path: use
               --  directly the path instead of searching for the
               --  variable's row

               Iter := View.Tree.Model.Get_Iter (Self.Path);
            end if;

            if Iter /= Null_Iter then
               Set_And_Clear
                 (View.Tree.Model,
                  Iter    => Iter,
                  Columns => (Column_Value, Column_Value_Fg),
                  Values  =>
                    (1 => As_String (UTF8 (Result.a_body.value)),
                     2 => As_String (To_String (Numbers_Style.Get_Pref_Fg))));
            end if;
         end if;

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           (UTF8 (Self.Name) & " is not set.");
      end if;

      Path_Free (Self.Path);
   end On_Result_Message;

end DAP.Views.Variables.SetExpression;
