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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Glide_Result_View;         use Glide_Result_View;

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

   procedure Parse_Compiler_Output
     (Kernel           : Kernel_Handle;
      Category         : String;
      Warning_Category : String;
      Style_Category   : String;
      Output           : String;
      Quiet            : Boolean := False);
   --  Parse the output of build engine and insert the result
   --    - in the GPS results view if it corresponds to a file location
   --    - in the GPS console if it is a general message.
   --  Category is the category to add to in the results view.
   --  Warning_Category and Style_Category correspond to the categories
   --  used for warnings and style errors.

   ------------
   -- Create --
   ------------

   procedure Create
     (Item  : out Build_Command_Access;
      Data  : Process_Data;
      Quiet : Boolean := False;
      Files : File_Array_Access := null)
   is
   begin
      Item := new Build_Command;
      Item.Data := Data;
      Item.Quiet := Quiet;
      Item.Files := Files;

      if Quiet then
         Item.Main_Error_Category := new String'(-Shadow_Category);
         Item.Style_Category      := new String'(-Shadow_Category);
         Item.Warning_Category    := new String'(-Shadow_Category);
      else
         Item.Main_Error_Category := new String'(-Error_Category);
         Item.Style_Category      := new String'(-Style_Category);
         Item.Warning_Category    := new String'(-Warning_Category);
      end if;
   end Create;

   ---------------------------
   -- Parse_Compiler_Output --
   ---------------------------

   procedure Parse_Compiler_Output
     (Kernel           : Kernel_Handle;
      Category         : String;
      Warning_Category : String;
      Style_Category   : String;
      Output           : String;
      Quiet            : Boolean := False) is
   begin
      if not Quiet then
         Insert (Kernel, Output, Add_LF => False);
      end if;

      String_List_Utils.String_List.Append
        (Builder_Module_ID_Access (Builder_Module_ID).Output,
         Output);
      Parse_File_Locations
        (Kernel,
         Output,
         Category           => Category,
         Highlight          => True,
         Highlight_Category => Category,
         Style_Category     => Style_Category,
         Warning_Category   => Warning_Category,
         Quiet              => Quiet);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Parse_Compiler_Output;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Build_Command) is
      Data    : Process_Data renames D.Data;
      Fd      : Process_Descriptor_Access renames Data.Descriptor;
      PID     : GNAT.Expect.Process_Id;
   begin
      if Fd /= null then
         PID := Get_Pid (Fd.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (Fd.all);
            Console.Insert (Data.Kernel, "<^C>");
            Close (Fd.all);
            Free (Data.Descriptor);
         end if;
      end if;

      --  Delete the associated files.

      if D.Files /= null then
         for J in D.Files'Range loop
            if Is_Regular_File (D.Files (J)) then
               Delete (D.Files (J));
            end if;
         end loop;

         Unchecked_Free (D.Files);
      end if;

      Free (D.Main_Error_Category);
      Free (D.Warning_Category);
      Free (D.Style_Category);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Build_Command) return Command_Return_Type
   is
      Data    : Process_Data renames Command.Data;
      Kernel  : Kernel_Handle renames Data.Kernel;
      Fd      : Process_Descriptor renames Data.Descriptor.all;

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
         Interrupt (Fd);
         Console.Insert (Kernel, "<^C>");
         Top.Interrupted := False;
         Timeout := 10;
      end if;

      loop
         Expect (Fd, Result, Line_Matcher, Timeout => Timeout);

         exit when Result = Expect_Timeout;

         declare
            S : constant String := Strip_CR (Expect_Out (Fd));
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
           (Kernel,
            Command.Main_Error_Category.all,
            Command.Warning_Category.all,
            Command.Style_Category.all,
            Buffer (Buffer'First .. Buffer_Pos - 1),
            Command.Quiet);
      end if;

      Free (Buffer);

      return Execute_Again;

   exception
      when Process_Died =>
         if Buffer_Pos /= Buffer'First then
            Parse_Compiler_Output
              (Kernel,
               Command.Main_Error_Category.all,
               Command.Warning_Category.all,
               Command.Style_Category.all,
               Buffer (Buffer'First .. Buffer_Pos - 1) & Expect_Out (Fd),
               Command.Quiet);
         end if;

         Free (Buffer);
         Parse_Compiler_Output
           (Kernel,
            Command.Main_Error_Category.all,
            Command.Warning_Category.all,
            Command.Style_Category.all,
            Expect_Out (Fd),
            Command.Quiet);
         Close (Fd, Status);

         if not Command.Quiet then
            if Status = 0 then
               Console.Insert
                 (Kernel, ASCII.LF & (-"successful compilation/build"));
            else
               Console.Insert
                 (Kernel,
                  ASCII.LF
                  & (-"process exited with status ") & Image (Status));
            end if;
         end if;

         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Free (Data.Descriptor);

         Compilation_Finished
           (Kernel, VFS.No_File, Command.Main_Error_Category.all);
         return Success;

      when E : others =>
         Free (Buffer);
         Pop_State (Kernel);
         Set_Sensitive_Menus (Kernel, True);
         Close (Fd);
         Free (Data.Descriptor);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));

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
