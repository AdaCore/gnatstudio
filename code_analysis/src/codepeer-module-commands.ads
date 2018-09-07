------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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
--  Commands and filters for actions of CodePeer module.

with Commands.Interactive;

private package CodePeer.Module.Commands is

   ----------------------------
   -- Review_Message_Command --
   ----------------------------

   type Review_Message_Command
     (Module : CodePeer.Module.CodePeer_Module_Id) is
     new Standard.Commands.Root_Command with null record;

   overriding function Execute
     (Self : access Review_Message_Command)
      return Standard.Commands.Command_Return_Type;

   ------------------------------
   -- Show_Annotations_Command --
   ------------------------------

   type Show_Annotations_Command
     (Module : CodePeer.Module.CodePeer_Module_Id) is
     new Standard.Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Self    : access Show_Annotations_Command;
      Context : Standard.Commands.Interactive.Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;

   ------------------------------
   -- Hide_Annotations_Command --
   ------------------------------

   type Hide_Annotations_Command
     (Module : CodePeer.Module.CodePeer_Module_Id) is
     new Standard.Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Self    : access Hide_Annotations_Command;
      Context : Standard.Commands.Interactive.Interactive_Command_Context)
      return Standard.Commands.Command_Return_Type;

   --------------------------------
   -- Is_Hide_Annotations_Filter --
   --------------------------------

   type Is_Hide_Annotations_Filter
     (Module : not null CodePeer.Module.CodePeer_Module_Id) is
     new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Hide_Annotations_Filter;
      Context : Selection_Context) return Boolean;

   --------------------------------
   -- Is_Show_Annotations_Filter --
   --------------------------------

   type Is_Show_Annotations_Filter
     (Module : not null CodePeer.Module.CodePeer_Module_Id) is
     new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Show_Annotations_Filter;
      Context : Selection_Context) return Boolean;

end CodePeer.Module.Commands;
