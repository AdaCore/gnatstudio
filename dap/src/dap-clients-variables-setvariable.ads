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

with VSS.Strings;                 use VSS.Strings;

with DAP.Modules.Variables.Items; use DAP.Modules.Variables.Items;
with DAP.Requests.SetVariable;
with DAP.Tools;

private package DAP.Clients.Variables.SetVariable is

   type Set_Variable_Request (<>) is
     new DAP.Requests.SetVariable.Set_Variable_DAP_Request
   with private;
   type Set_Variable_Request_Access is access all Set_Variable_Request;

   procedure Send_Set_Variable_Request
     (Client : not null access DAP.Clients.DAP_Client'Class;
      Id     : Integer;
      Params : Request_Parameters);
   --  Sends the set variable's value request

private

   type Set_Variable_Request
     (Kernel : GPS.Kernel.Kernel_Handle;
      Kind   : Request_Params_Kind) is
     new DAP.Requests.SetVariable.Set_Variable_DAP_Request (Kernel)
   with record
      Params : Request_Parameters (Kind);
   end record;

   overriding procedure On_Result_Message
     (Self        : in out Set_Variable_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetVariableResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out Set_Variable_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   overriding procedure On_Rejected
     (Self   : in out Set_Variable_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

end DAP.Clients.Variables.SetVariable;
