-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
--                              AdaCore                              --
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
with GPS.Location_View;     use GPS.Location_View;
with String_List_Utils;
with Builder_Module;        use Builder_Module;
with GPS.Intl;              use GPS.Intl;
with Traces;                use Traces;

package body Commands.Builder is

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
         String_List_Utils.String_List.Append
           (Builder_Module_ID.Output, Slice (Lines, J));
      end loop;

      Parse_File_Locations
        (Kernel,
         Output,
         Category           => Category,
         Highlight          => True,
         Highlight_Category => Error_Category,
         Style_Category     => Style_Category,
         Warning_Category   => Warning_Category,
         Quiet              => Quiet);

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
      Output  : String;
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

end Commands.Builder;
