-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

package Glide_Kernel.Timeout is

   type Process_Data;

   type Process_Callback is access procedure (Data : Process_Data);
   --  Callback called when data is received from an underlying process
   --  launched by Launch_Process.

   type Exit_Callback is
     access procedure (Data : Process_Data; Status : Integer);
   --  Callback called when an underlying process launched by Launch_Process
   --  terminates.

   type Process_Data is record
      Kernel     : Kernel_Handle;
      Descriptor : GNAT.Expect.Process_Descriptor_Access;
      Name       : GNAT.OS_Lib.String_Access;
      Callback   : Process_Callback;
      Exit_Cb    : Exit_Callback;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package Process_Timeout is new Gtk.Main.Timeout (Process_Data);

   procedure Launch_Process
     (Kernel      : Kernel_Handle;
      Command     : String;
      Arguments   : GNAT.OS_Lib.Argument_List;
      Title       : String := "";
      Callback    : Process_Callback := null;
      Exit_Cb     : Exit_Callback := null;
      Name        : String;
      Success     : out Boolean;
      Interactive : Boolean := False);
   --  Launch a given command with arguments.
   --  Set Success to True if the command could be spawned.
   --  Title is a short title used for the MDI window.
   --  Callback will be called asynchronousely when the process has terminated.
   --  Name is the string to set in Process_Data when calling Callback.
   --  Exit_Callback will be called when the underlying process dies.
   --  If Interactive is True, the process will be launched in its own
   --  interactive console.

end Glide_Kernel.Timeout;
