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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with GNAT.Expect;           use GNAT.Expect;
with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.String_Split;     use GNAT.String_Split;
pragma Warnings (Off);
with GNAT.Expect.TTY;       use GNAT.Expect.TTY;
pragma Warnings (On);

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Console;    use GPS.Kernel.Console;
with GPS.Kernel.Styles;     use GPS.Kernel.Styles;
with GPS.Kernel.Timeout;    use GPS.Kernel.Timeout;
with GPS.Location_View;     use GPS.Location_View;
with Builder_Module;        use Builder_Module;
with GPS.Intl;              use GPS.Intl;

with Remote;                use Remote;
with String_Utils;          use String_Utils;
with Traces;                use Traces;
with Basic_Types;           use Basic_Types;
with UTF8_Utils;            use UTF8_Utils;

package body Commands.Builder is

   type Build_Callback_Data is new Callback_Data_Record with record
      Buffer : Unbounded_String;
      --  Stores the incomplete lines returned by the compilation process

      Output     : String_List_Utils.String_List.List;
      --  The full build output
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
   begin
      if not Data.Process_Died then
         Last_EOL := Index (Output, (1 => ASCII.LF), Backward);

         --  In case we did not find any LF in the output, we'll just append it
         --  in the current buffer.

         if Last_EOL = 0 then
            Append
              (Build_Callback_Data (Data.Callback_Data.all).Buffer, Output);
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
           (To_String (Build_Callback_Data (Data.Callback_Data.all).Buffer)
            & Output (Output'First .. Last_EOL - 1) & ASCII.LF);

         Build_Callback_Data (Data.Callback_Data.all).Buffer :=
           To_Unbounded_String (Output (Last_EOL + 1 .. Output'Last));

      elsif Data.Process_Died then
         Str := new String'
           (To_String (Build_Callback_Data (Data.Callback_Data.all).Buffer)
            & ASCII.LF);
         Build_Callback_Data (Data.Callback_Data.all).Buffer :=
           Null_Unbounded_String;
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
                  Quiet   => False);

            else
               Process_Builder_Output
                 (Kernel  => Data.Kernel,
                  Command => Data.Command,
                  Output  => Output (1 .. Len),
                  Quiet   => False);
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
      Last  : Natural;
      Lines : Slice_Set;
   begin
      if not Quiet then
         Insert (Kernel, Output, Add_LF => False);
      end if;

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
         Category           => Category,
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
      Quiet   : Boolean)
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
            -Error_Category,
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
      Locations_Name : String)
   is
      Data    : Build_Callback_Data_Access;
      Success : Boolean;
   begin
      Data := new Build_Callback_Data;

      if Compilation_Starting (Kernel, Locations_Name, Quiet => False) then
         String_List_Utils.String_List.Append
           (Data.Output,
            Argument_List_To_Quoted_String (CL.all, Quote_Backslash => False));

         Launch_Process
           (Kernel,
            Command              => CL (CL'First).all,
            Arguments            => CL (CL'First + 1 .. CL'Last),
            Server               => Build_Server,
            Console              => Get_Console (Kernel),
            Show_Command         => True,
            Show_Output          => False,
            Callback_Data        => Data.all'Access,
            Success              => Success,
            Line_By_Line         => False,
            Callback             => Build_Callback'Access,
            Exit_Cb              => End_Build_Callback'Access,
            Show_In_Task_Manager => True,
            Synchronous          => False,
            Show_Exit_Status     => True);

         --  ??? check value of Success
      end if;

   end Launch_Build_Command;

end Commands.Builder;
