-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Expect;                use GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;             use Ada.Exceptions;

with Traces;                     use Traces;
with Glide_Intl;                 use Glide_Intl;
with Glide_Kernel.Console;       use Glide_Kernel.Console;

package body Commands.External is

   Me : constant Debug_Handle := Create ("Commands.External");

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Destroy (D : in External_Command_Access);
   --  ???
   pragma Warnings (Off, Destroy);

   procedure Free is new Ada.Unchecked_Deallocation
     (External_Command, External_Command_Access);

   function Atomic_Command (D : External_Command_Access) return Boolean;
   --  ???

   -------------
   -- Destroy --
   -------------

   procedure Destroy (D : in External_Command_Access) is
      D_Copy : External_Command_Access := D;
   begin
      Free (D_Copy);
   end Destroy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out External_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String_List.List;
      Dir          : String_List.List;
      Args         : String_List.List;
      Head         : String_List.List;
      Handler      : String_List_Handler) is
   begin
      Item := new External_Command;
      Item.Kernel  := Kernel;
      Item.Command := Copy_String_List (Command);
      Item.Dir     := Copy_String_List (Dir);
      Item.Args    := Copy_String_List (Args);
      Item.Head    := Copy_String_List (Head);
      Item.Handler := Handler;
   end Create;

   --------------------
   -- Atomic_Command --
   --------------------

   function Atomic_Command
     (D : External_Command_Access) return Boolean
   is
      Match : Expect_Match := 1;
   begin
      loop
         Expect (D.Fd, Match, "\n", 1);

         case Match is
            when Expect_Timeout =>
               return True;
            when others =>
               String_List.Append (D.Output, Expect_Out (D.Fd));
         end case;
      end loop;

   exception
      when Process_Died =>
         declare
            S       : String := Expect_Out (D.Fd);
            Success : Boolean;
         begin
            if S /= "" then
               String_List.Append (D.Output, S);
            end if;

            Close (D.Fd);
            Success := D.Handler (D.Kernel, D.Head, D.Output);
            Pop_State (D.Kernel);
            Command_Finished (D, Success);

         exception
            when E : others =>
               Trace
                 (Me, "Unexpected exception: " & Exception_Information (E));
         end;

         return False;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Atomic_Command;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access External_Command) return Boolean is
      use String_List;

      Id        : Timeout_Handler_Id;
      Args      : GNAT.OS_Lib.Argument_List
        (1 .. String_List.Length (Command.Args));
      Temp_Args : List_Node := First (Command.Args);

      Old_Dir   : Dir_Name_Str := Get_Current_Dir;

   begin
      --  ??? Must add many checks for empty lists, etc.

      if not String_List.Is_Empty (Command.Dir) then
         Change_Dir (String_List.Head (Command.Dir));
      end if;

      for J in Args'Range loop
         Args (J) := new String' (Data (Temp_Args));
         Temp_Args := Next (Temp_Args);
      end loop;

      Non_Blocking_Spawn
        (Command.Fd,
         Data (First (Command.Command)),
         Args,
         Err_To_Out => True,
         Buffer_Size => 0);

      for J in Args'Range loop
         GNAT.OS_Lib.Free (Args (J));
      end loop;

      String_List.Free (Command.Args);

      if not String_List.Is_Empty (Command.Dir) then
         Change_Dir (Old_Dir);
      end if;

      Id := String_List_Idle.Add
        (50, Atomic_Command'Access, External_Command_Access (Command));

      Push_State (Command.Kernel, Processing);
      return True;

   exception
      when Directory_Error =>
         Insert (Command.Kernel,
                 -"Directory error: cannot access "
                 & String_List.Head (Command.Dir),
                 False, True, Error);
         return False;
   end Execute;

end Commands.External;
