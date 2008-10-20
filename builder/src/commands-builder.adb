-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2008, AdaCore             --
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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Expect;           use GNAT.Expect;
with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.String_Split;     use GNAT.String_Split;
pragma Warnings (Off);
with GNAT.Expect.TTY;       use GNAT.Expect.TTY;
pragma Warnings (On);

with GNATCOLL.Scripts.Utils;

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Console;    use GPS.Kernel.Console;
with GPS.Kernel.Styles;     use GPS.Kernel.Styles;
with GPS.Kernel.Timeout;    use GPS.Kernel.Timeout;
with GPS.Location_View;     use GPS.Location_View;
with GPS.Intl;              use GPS.Intl;
with Traces;                use Traces;
with Basic_Types;           use Basic_Types;
with UTF8_Utils;            use UTF8_Utils;
with String_Utils;          use String_Utils;

with Builder_Facility_Module; use Builder_Facility_Module;

package body Commands.Builder is

   Shell_Env : constant String := Getenv ("SHELL").all;

   type Build_Callback_Data is new Callback_Data_Record with record
      Target_Name : Unbounded_String;
      --  The name of the target being built

      Quiet : Boolean := False;
      --  Whether the target should be Quiet.
      --  A Quiet target does not cause the cursor to jump to the first
      --  error found. This is useful for builds that occur on saving, or in
      --  a background mode.

      Buffer : Unbounded_String;
      --  Stores the incomplete lines returned by the compilation process
   end record;

   type Build_Callback_Data_Access is access all Build_Callback_Data'Class;
   overriding procedure Destroy (Data : in out Build_Callback_Data);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Parse_Compiler_Output
     (Kernel           : Kernel_Handle;
      Category         : String;
      Error_Category   : Style_Access;
      Warning_Category : Style_Access;
      Style_Category   : Style_Access;
      Output           : String;
      Quiet            : Boolean := False);
   --  Parse the output of build engine and insert the result
   --    - in the GPS results view if it corresponds to a file location
   --    - in the GPS console if it is a general message.
   --  Category is the category to add to in the results view.
   --  Warning_Category and Style_Category correspond to the categories
   --  used for warnings and style errors.

   procedure Build_Callback (Data : Process_Data; Output : String);
   --  Callback for the build output

   procedure End_Build_Callback (Data : Process_Data; Status : Integer);
   --  Called at the end of the build

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Data : in out Build_Callback_Data) is
   begin
      null;
   end Destroy;

   ---------------------------------------
   -- Target_Name_To_Locations_Category --
   ---------------------------------------

   function Target_Name_To_Locations_Category (Name : String) return String is
   begin
      if Name = "" then
         return Error_Category;
      else
         return -"Build results: " & Name;
      end if;
   end Target_Name_To_Locations_Category;

   ------------------------
   -- End_Build_Callback --
   ------------------------

   procedure End_Build_Callback
     (Data : Process_Data; Status : Integer) is
   begin
      --  Raise the messages window is compilation was unsuccessful
      --  and no error was parsed. See D914-005

      if Category_Count (Data.Kernel, Error_Category) = 0
        and then Status /= 0
      then
         Console.Raise_Console (Data.Kernel);
      end if;

      --  ??? should also pass the Status value to Compilation_Finished
      --  and to the corresponding hook

      Compilation_Finished (Data.Kernel, Error_Category);
   end End_Build_Callback;

   --------------------
   -- Build_Callback --
   --------------------

   procedure Build_Callback (Data : Process_Data; Output : String) is
      Last_EOL : Natural := 1;
      Str      : GNAT.OS_Lib.String_Access;

      Build_Data : Build_Callback_Data
        renames Build_Callback_Data (Data.Callback_Data.all);
   begin
      if not Data.Process_Died then
         Last_EOL := Index (Output, (1 => ASCII.LF), Backward);

         --  In case we did not find any LF in the output, we'll just append it
         --  in the current buffer.

         if Last_EOL = 0 then
            Append (Build_Data.Buffer, Output);
            return;
         end if;

      else
         if Output'Length > 0 then
            Last_EOL := Output'Length + 1;
         end if;
      end if;

      --  Collect the relevant portion of the output

      if Output'Length > 0 then
         Str := new String'
           (To_String (Build_Data.Buffer)
            & Output (Output'First .. Last_EOL - 1) & ASCII.LF);

         Build_Data.Buffer :=
           To_Unbounded_String (Output (Last_EOL + 1 .. Output'Last));

      elsif Data.Process_Died then
         Str := new String'(To_String (Build_Data.Buffer) & ASCII.LF);
         Build_Data.Buffer := Null_Unbounded_String;
      else
         return;
      end if;

      --  If we reach this point, this means we have collected some output to
      --  parse. In this case, verify that it is proper UTF-8 before
      --  transmitting it to the rest of GPS.

      --  It is hard to determine which encoding the compiler result is,
      --  especially given that we are supporting third-party compilers, build
      --  scripts, etc. Therefore, we call Unknown_To_UTF8.

      declare
         Output : Basic_Types.Unchecked_String_Access;
         Len    : Natural;
         Valid  : Boolean;

      begin
         Unknown_To_UTF8 (Str.all, Output, Len, Valid);

         if Valid then
            if Output = null then
               Process_Builder_Output
                 (Kernel  => Data.Kernel,
                  Command => Data.Command,
                  Output  => Str.all,
                  Quiet   => Build_Data.Quiet,
                  Target  => To_String (Build_Data.Target_Name));

            else
               Process_Builder_Output
                 (Kernel  => Data.Kernel,
                  Command => Data.Command,
                  Output  => Output (1 .. Len),
                  Quiet   => Build_Data.Quiet,
                  Target  => To_String (Build_Data.Target_Name));
            end if;
         else
            Console.Insert
              (Data.Kernel,
               -"Could not convert compiler output to UTF8",
               Mode => Console.Error);
         end if;

         Free (Str);
         Basic_Types.Free (Output);
      end;
   end Build_Callback;

   ---------------------------
   -- Parse_Compiler_Output --
   ---------------------------

   procedure Parse_Compiler_Output
     (Kernel           : Kernel_Handle;
      Category         : String;
      Error_Category   : Style_Access;
      Warning_Category : Style_Access;
      Style_Category   : Style_Access;
      Output           : String;
      Quiet            : Boolean := False)
   is
      pragma Unreferenced (Category);
      Last  : Natural;
      Lines : Slice_Set;
   begin
      Insert (Kernel, Output, Add_LF => False);

      if Output'Length = 0
        or else
          (Output'Length = 1
           and then Output (Output'First) = ASCII.LF)
      then
         return;
      end if;

      --  A string terminated by an ASCII.LF is split into the some string plus
      --  an extra empty one. We therefore need to remove the trailing ASCII.LF
      --  from the string before splitting it.

      Last := Output'Last;

      if Last > Output'First
        and then Output (Last) = ASCII.LF
      then
         Last := Last - 1;
      end if;

      Create
        (Lines,
         From       => Output (Output'First .. Last),
         Separators => To_Set (ASCII.LF),
         Mode       => Single);

      for J in 1 .. Slice_Count (Lines) loop
         Append_To_Build_Output (Kernel, Slice (Lines, J));
      end loop;

      Parse_File_Locations
        (Kernel,
         Output,
         Category           => Commands.Builder.Error_Category,
         Highlight          => True,
         Highlight_Category => Error_Category,
         Style_Category     => Style_Category,
         Warning_Category   => Warning_Category,
         Quiet              => Quiet,
         Remove_Duplicates  => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Parse_Compiler_Output;

   ----------------------------
   -- Process_Builder_Output --
   ----------------------------

   Completed_Matcher : constant Pattern_Matcher := Compile
     ("completed ([0-9]+) out of ([0-9]+) \(([^\n]*)%\)\.\.\.\n",
      Single_Line);
   --  ??? This is configurable in some cases (from XML for instance), so
   --  we should not have a hard coded regexp here.

   procedure Process_Builder_Output
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : Commands.Command_Access;
      Output  : Glib.UTF8_String;
      Quiet   : Boolean;
      Target  : String := "")
   is
      Start   : Integer := Output'First;
      Matched : Match_Array (0 .. 3);
      Buffer  : Unbounded_String;
   begin
      while Start <= Output'Last loop
         Match (Completed_Matcher, Output (Start .. Output'Last), Matched);
         exit when Matched (0) = No_Match;

         Command.Progress.Current := Natural'Value
           (Output (Matched (1).First .. Matched (1).Last));
         Command.Progress.Total := Natural'Value
           (Output (Matched (2).First .. Matched (2).Last));

         Append (Buffer, Output (Start .. Matched (0).First - 1));
         Start := Matched (0).Last + 1;
      end loop;

      Append (Buffer, Output (Start .. Output'Last));

      if Length (Buffer) /= 0 then
         Parse_Compiler_Output
           (Kernel_Handle (Kernel),
            Target_Name_To_Locations_Category (Target),
            Builder_Errors_Style,
            Builder_Warnings_Style,
            Builder_Style_Style,
            To_String (Buffer),
            Quiet);
      end if;
   end Process_Builder_Output;

   --------------------------
   -- Launch_Build_Command --
   --------------------------

   procedure Launch_Build_Command
     (Kernel         : Kernel_Handle;
      CL             : GNAT.OS_Lib.String_List_Access;
      Target_Name    : String;
      Mode_Name      : String;
      Server         : Server_Type;
      Quiet          : Boolean;
      Synchronous    : Boolean;
      Use_Shell      : Boolean)
   is
      Data     : Build_Callback_Data_Access;
      Success  : Boolean;
      Args     : Argument_List_Access;
      Cmd_Name : Unbounded_String;

   begin
      Data := new Build_Callback_Data;
      Data.Target_Name := To_Unbounded_String (Target_Name);
      Data.Quiet := Quiet;

      if Compilation_Starting
        (Kernel,
         To_String (Data.Target_Name),
         Quiet => Quiet)
      then
         Append_To_Build_Output
           (Kernel,
            GNATCOLL.Scripts.Utils.Argument_List_To_Quoted_String
              (CL.all, Quote_Backslash => False));

         if Mode_Name /= "default" then
            Cmd_Name := To_Unbounded_String
              (Target_Name & " (" & Mode_Name & ")");
         else
            Cmd_Name := To_Unbounded_String (Target_Name);
         end if;

         if Use_Shell
           and then Shell_Env /= ""
           and then Is_Local (Server)
         then
            Args := new Argument_List'(new String'("-c"), new String'
                                         (Argument_List_To_String (CL.all)));

            Launch_Process
              (Kernel,
               Command              => Shell_Env,
               Arguments            => Args.all,
               Server               => Server,
               Console              => Get_Console (Kernel),
               Show_Command         => True,
               Show_Output          => False,
               Callback_Data        => Data.all'Access,
               Success              => Success,
               Line_By_Line         => False,
               Callback             => Build_Callback'Access,
               Exit_Cb              => End_Build_Callback'Access,
               Show_In_Task_Manager => True,
               Name_In_Task_Manager => To_String (Cmd_Name),
               Synchronous          => Synchronous,
               Show_Exit_Status     => True);

            Free (Args);

         else
            Launch_Process
              (Kernel,
               Command              => CL (CL'First).all,
               Arguments            => CL (CL'First + 1 .. CL'Last),
               Server               => Server,
               Console              => Get_Console (Kernel),
               Show_Command         => True,
               Show_Output          => False,
               Callback_Data        => Data.all'Access,
               Success              => Success,
               Line_By_Line         => False,
               Callback             => Build_Callback'Access,
               Exit_Cb              => End_Build_Callback'Access,
               Show_In_Task_Manager => True,
               Name_In_Task_Manager => To_String (Cmd_Name),
               Synchronous          => Synchronous,
               Show_Exit_Status     => True);
         end if;

         --  ??? check value of Success
      end if;
   end Launch_Build_Command;

end Commands.Builder;
