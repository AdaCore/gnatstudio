-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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

with GVD.Status_Bar;            use GVD.Status_Bar;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;

with Glide_Intl;                use Glide_Intl;
with GNAT.Expect;               use GNAT.Expect;

with Glide_Main_Window;         use Glide_Main_Window;

with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with String_Utils;              use String_Utils;
with String_List_Utils;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with VFS;                       use VFS;

with Builder_Module; use Builder_Module;

package body Commands.Builder is

   Me : constant Debug_Handle := Create ("Commands.Builder");

   procedure Parse_Compiler_Output
     (Kernel : Kernel_Handle;
      Output : String);
   --  Parse the output of build engine and insert the result
   --    - in the GPS results view if it corresponds to a file location
   --    - in the GPS console if it is a general message.

   ------------
   -- Create --
   ------------

   procedure Create
     (Item : out Build_Command_Access;
      Data : Process_Data)
   is
   begin
      Item := new Build_Command;
      Item.Data := Data;
   end Create;

   ---------------------------
   -- Parse_Compiler_Output --
   ---------------------------

   procedure Parse_Compiler_Output
     (Kernel : Kernel_Handle;
      Output : String) is
   begin
      Insert (Kernel, Output, Add_LF => False);
      String_List_Utils.String_List.Append
        (Builder_Module_ID_Access (Builder_Module_ID).Output,
         Output);
      Parse_File_Locations (Kernel, Output, "Builder Results", True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Parse_Compiler_Output;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Build_Command) is
      pragma Unreferenced (D);
   begin
      null;
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Build_Command) return Command_Return_Type
   is
      Data    : Process_Data renames Command.Data;
      Kernel  : Kernel_Handle renames Data.Kernel;
      Fd      : Process_Descriptor_Access := Data.Descriptor;

      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Matched      : Match_Array (0 .. 3);
      Result       : Expect_Match;
      Matcher      : constant Pattern_Matcher := Compile
        ("completed ([0-9]+) out of ([0-9]+) \((.*)%\)\.\.\.");
      Timeout      : Integer := 1;
      Line_Matcher : constant Pattern_Matcher :=
        Compile ("^.*?\n", Multiple_Lines);
      Buffer       : String_Access := new String (1 .. 1024);
      Buffer_Pos   : Natural := Buffer'First;
      Min_Size     : Natural;
      New_Size     : Natural;
      Tmp          : String_Access;
      Status       : Integer;

   begin
      Command.Progress.Activity := Running;

      if Top.Interrupted then
         Interrupt (Fd.all);
         Console.Insert (Kernel, "<^C>");
         Top.Interrupted := False;
         Print_Message
           (Top.Statusbar, GVD.Status_Bar.Help, -"Interrupting build...");
         Timeout := 10;
      end if;

      loop
         Expect (Fd.all, Result, Line_Matcher, Timeout => Timeout);

         exit when Result = Expect_Timeout;

         declare
            S : constant String := Strip_CR (Expect_Out (Fd.all));
         begin
            Match (Matcher, S, Matched);

            if Matched (0) = No_Match then
               --  Coalesce all the output into one single chunck, which is
               --  much faster to display in the console.

               Min_Size := Buffer_Pos + S'Length;

               if Buffer'Last < Min_Size then
                  New_Size := Buffer'Length * 2;

                  while New_Size < Min_Size loop
                     New_Size := New_Size * 2;
                  end loop;

                  Tmp := new String (1 .. New_Size);
                  Tmp (1 .. Buffer_Pos - 1) := Buffer (1 .. Buffer_Pos - 1);
                  Free (Buffer);
                  Buffer := Tmp;
               end if;

               Buffer (Buffer_Pos .. Buffer_Pos + S'Length - 1) := S;
               Buffer_Pos := Buffer_Pos + S'Length;

            else
               Command.Progress.Current := Natural'Value
                 (S (Matched (1).First .. Matched (1).Last));
               Command.Progress.Total := Natural'Value
                 (S (Matched (2).First .. Matched (2).Last));
            end if;
         end;
      end loop;

      if Buffer_Pos /= Buffer'First then
         Parse_Compiler_Output
           (Kernel, Buffer (Buffer'First .. Buffer_Pos - 1));
      end if;

      Free (Buffer);

      return Execute_Again;

   exception
      when Process_Died =>
         if Buffer_Pos /= Buffer'First then
            Parse_Compiler_Output
              (Kernel,
               Buffer (Buffer'First .. Buffer_Pos - 1) & Expect_Out (Fd.all));
         end if;

         Free (Buffer);
         Parse_Compiler_Output (Kernel, Expect_Out (Fd.all));
         Close (Fd.all, Status);

         if Status = 0 then
            Console.Insert
              (Kernel, ASCII.LF & (-"successful compilation/build"));
         else
            Console.Insert
              (Kernel,
               ASCII.LF & (-"process exited with status ") & Image (Status));
         end if;

         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Free (Fd);

         Compilation_Finished (Kernel, VFS.No_File);
         return Success;

      when E : others =>
         Free (Buffer);
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Close (Fd.all);
         Free (Fd);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         return Failure;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access Build_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"Building";
   end Name;

end Commands.Builder;
