------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

pragma Warnings (Off);
with GNAT.Expect.TTY;    use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.Strings;
with GNATCOLL.VFS;

with Glib.Main;          use Glib.Main;
with GPS.Kernel;         use GPS.Kernel;
with String_List_Utils;  use String_List_Utils;

package Commands.External is

   type External_Command is new Root_Command with private;
   type External_Command_Access is access all External_Command;

   overriding procedure Primitive_Free (Command : in out External_Command);
   --  Free memory associated to D

   type String_List_Handler is access
     function (Kernel : not null access Kernel_Handle_Record'Class;
               Head   : String_List.Vector;
               List   : String_List.Vector) return Boolean;
   --  Parses the output of a command, contained in List.
   --  Return True if the command was executed successfully.
   --  This function should NOT modify data referenced by Head and List.

   procedure Create
     (Item           : out External_Command_Access;
      Kernel         : not null access Kernel_Handle_Record'Class;
      Command        : String;
      Dir            : GNATCOLL.VFS.Virtual_File;
      Args           : String_List.Vector;
      Head           : String_List.Vector;
      Handler        : String_List_Handler;
      Description    : String;
      Check_Password : Boolean := False);
   --  Copies of Args and Head are made internally.
   --  Command is spawned as a shell command, with Args as its arguments.
   --  Head and the output of this command are then passed to Handler.
   --  When the command is executed, its output is passed to Handler,
   --  the result of which determines the success of the execution.
   --  If Handler is null, the output of the command is discarded, and
   --  the commands always executes successfully.
   --  If Dir is empty, the current directory will be used.
   --  Description is a short description of the command.
   --  Check_Password tells if a password prompt is expected from the external
   --  command.

   overriding function Execute
     (Command : access External_Command) return Command_Return_Type;
   --  Execute Command, and launch the associated Handler.
   --  See comments for Create.

   overriding function Name (Command : access External_Command) return String;
   --  Return a description of the command

private

   package String_List_Idle is
     new Glib.Main.Generic_Sources (External_Command_Access);

   type External_Command is new Root_Command with record
      Kernel          : access Kernel_Handle_Record'Class;
      Fd              : TTY_Process_Descriptor;
      Command         : GNAT.Strings.String_Access;
      Dir             : GNATCOLL.VFS.Virtual_File;
      Args            : String_List.Vector;
      Head            : String_List.Vector;
      Handler         : String_List_Handler;
      Output          : String_List.Vector;

      Running         : Boolean := False;
      Check_Password  : Boolean := False;
      Nb_Pwd          : Natural := 0;

      Description     : GNAT.Strings.String_Access;

      Handler_Success : Boolean := False;
   end record;

end Commands.External;
