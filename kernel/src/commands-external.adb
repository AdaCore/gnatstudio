-----------------------------------------------------------------------
--                              G P S                                --
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

with GVD; use GVD;

with GNAT.Expect;                use GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Strings;

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Traces;                     use Traces;
with Glide_Intl;                 use Glide_Intl;
with Glide_Kernel.Console;       use Glide_Kernel.Console;
with String_Utils;               use String_Utils;

package body Commands.External is

   Me : constant Debug_Handle := Create ("Commands.External");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Atomic_Command (D : External_Command_Access) return Boolean;
   --  ???

   ----------
   -- Free --
   ----------

   procedure Free (D : in out External_Command) is
      use String_List;

   begin
      Free (D.Args);
      Free (D.Head);
      Free (D.Command);
      Free (D.Dir);
      Free (D.Output);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out External_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String;
      Dir          : String;
      Args         : String_List.List;
      Head         : String_List.List;
      Handler      : String_List_Handler) is
   begin
      Item := new External_Command;
      Item.Kernel  := Kernel;
      Item.Command := new String'(Command);

      if Dir = "" then
         Item.Dir := new String'(Get_Current_Dir);
      else
         Item.Dir := new String'(Dir);
      end if;

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
               String_List.Append (D.Output, Strip_CR (Expect_Out (D.Fd)));
         end case;
      end loop;

   exception
      when Process_Died =>
         declare
            S       : constant String := Strip_CR (Expect_Out (D.Fd));
            Success : Boolean := True;
         begin
            if S /= "" then
               String_List.Append (D.Output, S);
            end if;

            Close (D.Fd);

            if D.Handler /= null then
               Success := D.Handler (D.Kernel, D.Head, D.Output);
            end if;

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
      pragma Unreferenced (Id);

      Args      : GNAT.OS_Lib.Argument_List (1 .. Length (Command.Args));
      Real_Args : GNAT.OS_Lib.Argument_List_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.OS_Lib.Argument_List, GNAT.OS_Lib.Argument_List_Access);

      Exec_Command_Args : GNAT.OS_Lib.Argument_List_Access;
      Temp_Args : List_Node := First (Command.Args);

      Old_Dir   : constant Dir_Name_Str := Get_Current_Dir;

      use type GNAT.Strings.String_List;

   begin
      --  ??? Must add many checks for empty lists, etc.

      if Command.Dir'Length > 0 then
         Change_Dir (Command.Dir.all);
      end if;

      for J in Args'Range loop
         Args (J) := new String'(Data (Temp_Args));
         Temp_Args := Next (Temp_Args);
      end loop;

      if Command.Dir'Length > 0 then
         Trace
           (Me,
            "spawn from " & Command.Dir.all & ": " &
            Command.Command.all & " " & Argument_List_To_String (Args));

      else
         Trace (Me, "spawn: " &
                Command.Command.all & " " & Argument_List_To_String (Args));
      end if;

      --  If we are under Windows, spawn the command using "cmd /c".
      --  Otherwise spawn it directly.
      --  ??? This implementation is temporary, a general way of spawning
      --  commands should not be system-dependant

      if Host = Windows then
         Exec_Command_Args :=
           GNAT.OS_Lib.Argument_String_To_List (Exec_Command);

         Real_Args := new GNAT.OS_Lib.Argument_List (1 .. 1);

         Real_Args (1) := new String'(Command.Command.all);

         Non_Blocking_Spawn
           (Command.Fd,
            Exec_Command_Args (Exec_Command_Args'First).all,
            Exec_Command_Args
              (Exec_Command_Args'First + 1 .. Exec_Command_Args'Last)
            & Real_Args.all
            & Args,
            Err_To_Out => True,
            Buffer_Size => 0);

         for J in Real_Args'Range loop
            GNAT.OS_Lib.Free (Real_Args (J));
         end loop;

         Unchecked_Free (Real_Args);

         for J in Exec_Command_Args'Range loop
            GNAT.OS_Lib.Free (Exec_Command_Args (J));
         end loop;

         Unchecked_Free (Exec_Command_Args);

      else
         Non_Blocking_Spawn
           (Command.Fd,
            Command.Command.all,
            Args,
            Err_To_Out => True,
            Buffer_Size => 0);
      end if;

      for J in Args'Range loop
         GNAT.OS_Lib.Free (Args (J));
      end loop;

      String_List.Free (Command.Args);

      if Command.Dir.all /= ""  then
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
                   & Command.Dir.all, Mode => Error);
         return False;

      when Invalid_Process =>
         Insert (Command.Kernel,
                 -"Could not launch command "
                   & Command.Command.all, Mode => Error);
         return False;

   end Execute;

end Commands.External;
