------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GNAT.Expect;                use GNAT.Expect;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GPS.Intl;                   use GPS.Intl;
with Password_Manager;           use Password_Manager;
with String_Utils;               use String_Utils;

package body Commands.External is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.EXTERNAL_COMMANDS");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Atomic_Command (Command : External_Command_Access) return Boolean;
   --  ???

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Command : in out External_Command) is
      use String_List;

      Fd  : TTY_Process_Descriptor renames Command.Fd;
      PID : GNAT.Expect.Process_Id;
   begin
      Command.Args.Clear;
      Command.Head.Clear;
      GNAT.Strings.Free (Command.Command);
      Command.Output.Clear;
      Command.Dir := GNATCOLL.VFS.No_File;

      PID := Get_Pid (Fd);

      if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
         Interrupt (Fd);
         Close (Fd);
      end if;
   end Primitive_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out External_Command_Access;
      Kernel         : not null access Kernel_Handle_Record'Class;
      Command        : String;
      Dir            : GNATCOLL.VFS.Virtual_File;
      Args           : String_List.Vector;
      Head           : String_List.Vector;
      Handler        : String_List_Handler;
      Description    : String;
      Check_Password : Boolean := False) is
   begin
      Item := new External_Command;
      Item.Kernel  := Kernel;
      Item.Command := new String'(Command);

      if Dir = No_File then
         Item.Dir := Get_Current_Dir;
      else
         Item.Dir := Dir;
      end if;

      Item.Args           := Args;
      Item.Head           := Head;
      Item.Handler        := Handler;
      Item.Check_Password := Check_Password;

      Item.Description := new String'(Description);
   end Create;

   --------------------
   -- Atomic_Command --
   --------------------

   function Atomic_Command
     (Command : External_Command_Access) return Boolean
   is
      Match : Expect_Match;
   begin
      loop
         if not Command.Check_Password then
            Expect (Command.Fd, Match, "\n", 1);

            case Match is
               when Expect_Timeout =>
                  return True;
               when others =>
                  String_List.Append
                    (Command.Output, Strip_CR (Expect_Out (Command.Fd)));
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
               Expect (Command.Fd, Match, Regexp_Array, Matched, 1, False);

               case Match is
                  when Expect_Timeout =>
                     return True;

                  when 1 | 2 =>
                     --  Password/passphrase prompt
                     declare
                        Password : constant String :=
                                     Get_Tool_Password
                                       (Command.Command.all,
                                        Command.Nb_Pwd > 0);
                     begin
                        Command.Nb_Pwd := Command.Nb_Pwd + 1;
                        Send (Command.Fd, Password);
                     end;

                  when others =>
                     String_List.Append
                       (Command.Output, Strip_CR (Expect_Out (Command.Fd)));
               end case;
            end;
         end if;
      end loop;

   exception
      when Process_Died =>
         declare
            S : constant String := Strip_CR (Expect_Out (Command.Fd));
         begin
            if S /= "" then
               String_List.Append (Command.Output, S);
            end if;

            Close (Command.Fd);

            if Command.Handler /= null then
               Command.Handler_Success := Command.Handler
                 (Command.Kernel, Command.Head, Command.Output);
            end if;

            Command_Finished (Command, Command.Handler_Success);

         exception
            when E : others => Trace (Me, E);
         end;

         return False;

      when E : others =>
         Trace (Me, E);
         return False;
   end Atomic_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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
            Args      : GNAT.OS_Lib.Argument_List
              (1 .. Natural (Command.Args.Length));
            Temp_Args : Cursor := Command.Args.First;
            Old_Dir   : constant Virtual_File := Get_Current_Dir;

         begin
            --  ??? Must add many checks for empty lists, etc

            if Command.Dir /= No_File then
               Change_Dir (Command.Dir);
            end if;

            for J in Args'Range loop
               Args (J) := new String'(Element (Temp_Args));
               Next (Temp_Args);
            end loop;

            if Command.Dir /= No_File then
               Trace
                 (Me,
                  "spawn from " & Command.Dir.Display_Full_Name & ": " &
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
               Err_To_Out  => True,
               Buffer_Size => 0);

            for J in Args'Range loop
               GNAT.OS_Lib.Free (Args (J));
            end loop;

            Command.Args.Clear;

            if Command.Dir /= No_File then
               Change_Dir (Old_Dir);
            end if;
         end;

         Command.Running := True;

         Command.Progress.Activity := Running;

         return Execute_Again;
      end if;

   exception
      when VFS_Directory_Error =>
         Insert (Command.Kernel,
                 -"Directory error: cannot access "
                   & Command.Dir.Display_Full_Name, Mode => Error);
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

   overriding function Name
     (Command : access External_Command) return String is
   begin
      return Command.Description.all;
   end Name;

end Commands.External;
