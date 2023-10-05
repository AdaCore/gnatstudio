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

with DAP.Requests.Set_Variable;
with DAP.Tools;

private package DAP.Views.Registers.Set_Variable_Requests is

   type Set_Variable_Request is
     new DAP.Requests.Set_Variable.Set_Variable_DAP_Request
   with null record;

   type Set_Variable_Request_Access is access all Set_Variable_Request;

   overriding procedure On_Result_Message
     (Self        : in out Set_Variable_Request;
      Result      : in out DAP.Tools.SetVariableResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

end DAP.Views.Registers.Set_Variable_Requests;
