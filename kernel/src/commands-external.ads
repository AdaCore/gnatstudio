-----------------------------------------------------------------------
--                              G P S                                --
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

with GNAT.Expect.TTY;      use GNAT.Expect.TTY;
with Gtk.Main;             use Gtk.Main;
with Glide_Kernel;         use Glide_Kernel;
with String_List_Utils;    use String_List_Utils;

package Commands.External is

   type External_Command is new Root_Command with private;
   type External_Command_Access is access all External_Command;

   type String_List_Handler is access
     function (Kernel : Kernel_Handle;
               Head   : String_List.List;
               List   : String_List.List) return Boolean;
   --  ??? Where are the params of handlers freed ?

   procedure Create
     (Item         : out External_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String_List.List;
      Dir          : String_List.List;
      Args         : String_List.List;
      Head         : String_List.List;
      Handler      : String_List_Handler);
   --  ???

   function Execute (Command : access External_Command) return Boolean;

private

   package String_List_Idle is
      new Gtk.Main.Timeout (External_Command_Access);

   type External_Command is new Root_Command with record
      Kernel  : Kernel_Handle;
      Fd      : TTY_Process_Descriptor;
      Command : String_List.List;
      Dir     : String_List.List;
      Args    : String_List.List;
      Head    : String_List.List;
      Handler : String_List_Handler;
      Output  : String_List.List;
   end record;

end Commands.External;
