------------------------------------------------------------------------------
--                               GNAT Studio                                --
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
--  Commands of CodePeer module.

private package CodePeer.Module.Commands is

   use Standard.Commands;

   type CodePeer_Root_Command
     (Module : not null CodePeer_Module_Id) is
     abstract new Root_Command with null record;

   type Review_Message_Command is new CodePeer_Root_Command with null record;
   overriding function Execute
     (Self : access Review_Message_Command) return Command_Return_Type;
   --  Called to review message

end CodePeer.Module.Commands;
