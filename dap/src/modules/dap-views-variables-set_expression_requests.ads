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
with DAP.Requests.Set_Expression;
with DAP.Tools;

private package DAP.Views.Variables.Set_Expression_Requests is

   type Set_Expression_Request is
     new DAP.Requests.Set_Expression.Set_Expression_DAP_Request
   with record
      Name : Virtual_String;
      Path : Gtk.Tree_Model.Gtk_Tree_Path := Null_Gtk_Tree_Path;
   end record;

   type Set_Expression_Request_Access is access all Set_Expression_Request;

   overriding procedure On_Result_Message
     (Self        : in out Set_Expression_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetExpressionResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

end DAP.Views.Variables.Set_Expression_Requests;
