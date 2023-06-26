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

with Gtk.Tree_Model;              use Gtk.Tree_Model;

with VSS.Strings;                 use VSS.Strings;

with DAP.Modules.Variables.Items; use DAP.Modules.Variables.Items;
with DAP.Requests.Evaluate;
with DAP.Tools;

private package DAP.Views.Variables.Evaluate_Requests is

   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with record
      Client   : DAP.Clients.DAP_Client_Access;
      Item     : Item_Info;
      Position : Natural;
      Ref      : Integer;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path;
   end record;

   type Evaluate_Request_Access is access all Evaluate_Request;

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String);

end DAP.Views.Variables.Evaluate_Requests;
