------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with GNATCOLL.Arg_Lists;  use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package body Commands.Socket is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Socket_Command_Access;
      Kernel  : Kernel_Handle;
      Command : String;
      Shell   : String := GPS.Kernel.Scripts.GPS_Shell_Name;
      Stream  : Stream_Access) is
   begin
      Item         := new Socket_Command;
      Item.Kernel  := Kernel;
      Item.Command := new String'(Command);
      Item.Shell   := new String'(Shell);
      Item.Stream  := Stream;
   end Create;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (X : in out Socket_Command) is
   begin
      Free (X.Command);
      Free (X.Shell);
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Socket_Command) return Command_Return_Type
   is
      Errors : aliased Boolean;
      Script : Scripting_Language;
   begin
      if Command.Command /= null then
         Script := Command.Kernel.Scripts.Lookup_Scripting_Language
           (Command.Shell.all);
         String'Write
           (Command.Stream,
            Execute_Command
              (Script,
               Parse_String
                 (Command.Command.all, Command_Line_Treatment (Script)),
               null, True, True,
               Errors'Unchecked_Access) &
            ASCII.LF & "GPS>> ");
      end if;

      return Success;
   end Execute;

end Commands.Socket;
