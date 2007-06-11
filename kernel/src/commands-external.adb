-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
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

with GNAT.Expect;                use GNAT.Expect;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Traces;                     use Traces;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with Password_Manager;           use Password_Manager;
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

      Fd      : TTY_Process_Descriptor renames D.Fd;
      PID     : GNAT.Expect.Process_Id;
   begin
      Free (D.Args);
      Free (D.Head);
      GNAT.Strings.Free (D.Command);
      GNAT.Strings.Free (D.Dir);
      Free (D.Output);

      PID := Get_Pid (Fd);

      if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
         Interrupt (Fd);
         Close (Fd);
      end if;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out External_Command_Access;
      Kernel         : Kernel_Handle;
      Command        : String;
      Dir            : String;
      Args           : String_List.List;
      Head           : String_List.List;
      Handler        : String_List_Handler;
      Description    : String;
      Check_Password : Boolean := False) is
   begin
      Item := new External_Command;
      Item.Kernel  := Kernel;
      Item.Command := new String'(Command);

      if Dir = "" then
         Item.Dir := new String'(Get_Current_Dir);
      else
         Item.Dir := new String'(Dir);
      end if;

      Item.Args           := Copy_String_List (Args);
      Item.Head           := Copy_String_List (Head);
      Item.Handler        := Handler;
      Item.Check_Password := Check_Password;

      Item.Description := new String'(Description);
   end Create;

   --------------------
   -- Atomic_Command --
   --------------------

   function Atomic_Command (D : External_Command_Access) return Boolean
   is
      Match : Expect_Match := 1;
   begin
      loop
         if not D.Check_Password then
            Expect (D.Fd, Match, "\n", 1);

            case Match is
               when Expect_Timeout =>
                  return True;
               when others =>
                  String_List.Append (D.Output, Strip_CR (Expect_Out (D.Fd)));
            end case;

         else
            declare
               Regexp_Array : Compiled_Regexp_Array (1 .. 3);
               Matched      : GNAT.Regpat.Match_Array (0 .. 1);
            begin
               Regexp_Array (1) :=
                 new Pattern_Matcher'(Get_Default_Password_Regexp);
               Regexp_Array (2) :=
                 new Pattern_Matcher'(Get_Default_Passphrase_Regexp);
               Regexp_Array (3) :=
                 new Pattern_Matcher'(Compile ("\n"));
               Expect (D.Fd, Match, Regexp_Array, Matched, 1, False);

               case Match is
                  when Expect_Timeout =>
                     return True;

                  when 1 | 2 =>
                     --  Password/passphrase prompt
                     declare
                        Password : constant String :=
                          Get_Tool_Password (D.Command.all,
                                             D.Nb_Pwd > 0);
                     begin
                        D.Nb_Pwd := D.Nb_Pwd + 1;
                        Send (D.Fd, Password);
                     end;

                  when others =>
                     String_List.Append
                       (D.Output, Strip_CR (Expect_Out (D.Fd)));
               end case;
            end;
         end if;
      end loop;

   exception
      when Process_Died =>
         declare
            S : constant String := Strip_CR (Expect_Out (D.Fd));
         begin
            if S /= "" then
               String_List.Append (D.Output, S);
            end if;

            Close (D.Fd);

            if D.Handler /= null then
               D.Handler_Success := D.Handler (D.Kernel, D.Head, D.Output);
            end if;

            Command_Finished (D, D.Handler_Success);

         exception
            when E : others => Trace (Exception_Handle, E);
         end;

         return False;

      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Atomic_Command;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access External_Command) return Command_Return_Type
   is
      use String_List;

      use type GNAT.Strings.String_List;

      Execute : Boolean;
   begin
      if Command.Running then
         Execute := Atomic_Command (External_Command_Access (Command));

         if Execute then
            return Execute_Again;

         else
            if Command.Handler_Success then
               return Success;
            else
               return Failure;
            end if;
         end if;
      else
         declare
            Len       : constant Natural := Length (Command.Args);
            Args      : GNAT.OS_Lib.Argument_List (1 .. Len);
            Temp_Args : List_Node := First (Command.Args);
            Old_Dir   : constant Dir_Name_Str := Get_Current_Dir;

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
                  Command.Command.all & " " &
                  Krunch (Argument_List_To_String (Args), 128));

            else
               Trace
                 (Me, "spawn: " &
                  Command.Command.all & " " &
                  Krunch (Argument_List_To_String (Args), 128));
            end if;

            Non_Blocking_Spawn
              (Command.Fd,
               Command.Command.all,
               Args,
               Err_To_Out => True,
               Buffer_Size => 0);

            for J in Args'Range loop
               GNAT.OS_Lib.Free (Args (J));
            end loop;

            String_List.Free (Command.Args);

            if Command.Dir.all /= ""  then
               Change_Dir (Old_Dir);
            end if;
         end;

         Command.Running := True;

         Command.Progress.Activity := Running;

         return Execute_Again;
      end if;

   exception
      when Directory_Error =>
         Insert (Command.Kernel,
                 -"Directory error: cannot access "
                   & Command.Dir.all, Mode => Error);
         return Failure;

      when Invalid_Process =>
         Insert (Command.Kernel,
                 -"Could not launch command "
                   & Command.Command.all, Mode => Error);
         return Failure;

   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access External_Command) return String is
   begin
      return Command.Description.all;
   end Name;

end Commands.External;
