------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package body VCS.Branching_Commands is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Branching_Command_Record) return Command_Return_Type
   is
      Result : Command_Return_Type;
      C      : Cursor := No_Element;
   begin
      Result := Execute (Command.Command);

      case Result is
         when Execute_Again =>
            return Result;

         when Success =>
            C := Command.Consequence_Actions.First;

         when Failure =>
            C := Command.Alternate_Actions.First;
      end case;

      while Has_Element (C) loop
         Launch_Background_Command
           (Kernel          => Command.Kernel,
            Command         => Element (C),
            Active          => False,
            Show_Bar        => True,
            Queue_Id        => To_String (Command.Queue_Id),
            Block_Exit      => False);
         C := Next (C);
      end loop;

      return Success;
   end Execute;

   ----------------------------
   -- Add_Consequence_Action --
   ----------------------------

   procedure Add_Consequence_Action
     (Command : Branching_Command;
      Action  : Command_Access)
   is
   begin
      Command.Consequence_Actions.Append (Action);
   end Add_Consequence_Action;

   --------------------------
   -- Add_Alternate_Action --
   --------------------------

   procedure Add_Alternate_Action
     (Command : Branching_Command;
      Action  : Command_Access)
   is
   begin
      Command.Alternate_Actions.Append (Action);
   end Add_Alternate_Action;

   ------------
   -- Create --
   ------------

   function Create
     (Kernel   : access Kernel_Handle_Record'Class;
      Command  : Command_Access;
      Queue_ID : String) return Branching_Command
   is
      C : Branching_Command;
   begin
      C := new Branching_Command_Record;
      C.Kernel := Kernel;
      C.Queue_Id := To_Unbounded_String (Queue_ID);
      C.Command := Command;

      return C;
   end Create;

end VCS.Branching_Commands;
