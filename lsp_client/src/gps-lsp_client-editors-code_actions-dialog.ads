------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2022-2026, AdaCore                     --
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

--  This package provides the intermediary dialog for code actions that
--  require input.

with Language; use Language;
with LSP.Messages;

package GPS.LSP_Client.Editors.Code_Actions.Dialog is

   procedure Execute_Request_Via_Dialog
      (Kernel  : Kernel_Handle;
       Lang    : Language_Access;
       Request : in out Execute_Command_Request_Access);
   --  Look at the specifics of Request and determine whether
   --  extra input is required from the user.
   --
   --  If a dialog is required, create the UI, and execute the
   --  request once the user has entered input. If there is a
   --  validation function available through the language server
   --  for this particular action, it is used to validate the
   --  input.
   --
   --  If a dialog is not required, execute the request immediately.

   procedure Set_Result_Message
     (Response : LSP.Messages.ALS_Check_Syntax_Result);
   --  If the input window is still up, populate it with the contents
   --  of Response.

end GPS.LSP_Client.Editors.Code_Actions.Dialog;
