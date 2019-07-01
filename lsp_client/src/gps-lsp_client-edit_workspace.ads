------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GPS.Kernel;    use GPS.Kernel;
with LSP.Messages;

package GPS.LSP_Client.Edit_Workspace is

   procedure Edit
     (Kernel         : Kernel_Handle;
      Workspace_Edit : LSP.Messages.WorkspaceEdit;
      Old_Name       : String;
      Title          : String;
      Make_Writable  : Boolean;
      Auto_Save      : Boolean);
     --  Apply edit changes.
     --  Title is used for information/error dialogs
     --  Make_Writable controls whether changing read-only files.

end GPS.LSP_Client.Edit_Workspace;
