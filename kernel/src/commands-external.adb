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

with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Unchecked_Deallocation;

with Glide_Kernel.Console; use Glide_Kernel.Console;

with VCS; use VCS;
--  ??? we are withing this package only for Copy_String_List: we should put
--  that elsewhere.

package body Commands.External is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Destroy (D : in External_Command_Access);
   pragma Unreferenced (Destroy);

   procedure Free is new Unchecked_Deallocation
     (External_Command, External_Command_Access);

   function Atomic_Command (D : External_Command_Access) return Boolean;

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
      Handler      : String_List_Handler;
      Next_Command : Command_Access := null) is
   begin
      Item := new External_Command;
      Item.Kernel  := Kernel;
      Item.Command := Copy_String_List (Command);
      Item.Dir     := Copy_String_List (Dir);
      Item.Args    := Copy_String_List (Args);
      Item.Head    := Copy_String_List (Head);
      Item.Handler := Handler;
      Item.Next_Command := Next_Command;
   end Create;

   --------------------
   -- Atomic_Command --
   --------------------

   function Atomic_Command
     (D : External_Command_Access) return Boolean
   is
      Match  : Expect_Match := 1;
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
            S : String := Expect_Out (D.Fd);
         begin
            if S /= "" then
               String_List.Append (D.Output, S);
            end if;
         end;

         Close (D.Fd);
         declare
            Success : Boolean;
         begin
            Success := D.Handler (D.Kernel, D.Head, D.Output);

            if D.Next_Command /= null then
               Success := Execute (D.Next_Command);
            else
               Command_Finished (D.Queue);
               Pop_State (D.Kernel);
            end if;
         end;

         return False;
   end Atomic_Command;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access External_Command)
                    return Boolean
   is
      Id        : Timeout_Handler_Id;
      Args      : Argument_List (1 .. String_List.Length (Command.Args));
      Temp_Args : String_List.List := Command.Args;

      Old_Dir   : Dir_Name_Str := Get_Current_Dir;
      New_Dir   : Dir_Name_Str := String_List.Head (Command.Dir);
   begin
      --  ??? Must add many checks for empty lists, etc.

      Change_Dir (New_Dir);

      for J in Args'Range loop
         Args (J) := new String' (String_List.Head (Temp_Args));
         Temp_Args := String_List.Next (Temp_Args);
      end loop;

      Non_Blocking_Spawn (Command.Fd,
                          String_List.Head (Command.Command),
                          Args,
                          Err_To_Out => True,
                          Buffer_Size => 0);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      Temp_Args := Command.Args;
      String_List.Free (Temp_Args);

      Change_Dir (Old_Dir);

      Id := String_List_Idle.Add (50,
                                  Atomic_Command'Access,
                                  External_Command_Access (Command));

      Push_State (Command.Kernel, Processing);
      return True;

   exception
      when Directory_Error =>
         Insert (Command.Kernel,
                 "Directory error : cannot access " & New_Dir,
                 False, True, Error);
         return False;
   end Execute;

end Commands.External;
