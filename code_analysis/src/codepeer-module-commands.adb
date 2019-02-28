------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;

package body CodePeer.Module.Commands is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Review_Message_Command) return Command_Return_Type
   is
      Context  : constant GPS.Kernel.Selection_Context :=
        Self.Module.Kernel.Get_Current_Context;
      Messages : Standard.CodePeer.Message_Vectors.Vector;

   begin
      for Message of Messages_Information (Context) loop
         if Message.all in Standard.CodePeer.Message'Class then
            Messages.Append (Standard.CodePeer.Message_Access (Message));
         end if;
      end loop;

      if not Messages.Is_Empty then
         Self.Module.Review_Messages (Messages);
      end if;

      return Standard.Commands.Success;
   end Execute;

end CodePeer.Module.Commands;
