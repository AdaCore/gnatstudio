-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Main;
with GNAT.Expect;
with GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Interactive_Consoles;

package Glide_Kernel.Timeout is

   type Process_Data;

   type Output_Callback is
     access procedure (Data : Process_Data; Output : String);
   --  This callback is called whenever some output is read from the file
   --  descriptor.

   type Exit_Callback is
     access procedure (Data : Process_Data; Status : Integer);
   --  Callback called when an underlying process launched by Launch_Process
   --  terminates.

   type Process_Data is record
      Kernel        : Kernel_Handle;
      Descriptor    : GNAT.Expect.Process_Descriptor_Access;
      Callback      : Output_Callback;
      Exit_Cb       : Exit_Callback;
      Callback_Data : System.Address;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package Process_Timeout is new Gtk.Main.Timeout (Process_Data);

   procedure Launch_Process
     (Kernel        : Kernel_Handle;
      Command       : String;
      Arguments     : GNAT.OS_Lib.Argument_List;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Callback      : Output_Callback := null;
      Exit_Cb       : Exit_Callback := null;
      Success       : out Boolean;
      Show_Command  : Boolean := True;
      Callback_Data : System.Address := System.Null_Address;
      Line_By_Line  : Boolean := False);
   --  Launch a given command with arguments.
   --  Set Success to True if the command could be spawned.
   --  Callback will be called asynchronousely when some new data is
   --  available from the process.
   --  Exit_Callback will be called when the underlying process dies.
   --
   --  Output is sent to Console, if not null, or discarded otherwise.
   --  Check Glide_Kernel.Console.Create_Interactive_Console and
   --  Glide_Kernel.Console.Get_Console.
   --  If Show_Command is True and the output is displayed, the command
   --  itself is displayed in the console.
   --
   --  If Line_By_Line is True, then the output of the command is processed
   --  line by line, instead of being processed with as big chunks as possible.
   --  If it is false, there is no garantee where the chunks will be splitted.

end Glide_Kernel.Timeout;
