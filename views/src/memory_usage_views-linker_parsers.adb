------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
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

with Ada.Assertions;
with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.Regpat;     use GNAT.Regpat;

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.Utils;  use GNATCOLL.Utils;
with GNATCOLL.VFS;    use GNATCOLL.VFS;

with GPS.Kernel;      use GPS.Kernel;
with GPS.Kernel.Hooks;                   use GPS.Kernel.Hooks;

package body Memory_Usage_Views.Linker_Parsers is

   Me : constant Trace_Handle := Create ("Memory_Usage_Views.Linker_Parsers");

   Linker_Parsers_Module : Linker_Parser_Module;

   function Is_Linker_Supported
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean;
   --  Return True if the current toolchain's linker supports the
   --  '--print-memory-usage' option and if the current build mode is set to
   --  'default', since we don't want any extra arguments to be appended after
   --  the '-largs' section (e.g: in the 'gnatcoverage' build mode, the
   --  '--subdirs' option is appended to the command line).

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project has changed.
   --  Used to check if the linker used by the newly loaded project supports
   --  the '--print-memory-usage' option.

   type Linker_Supported_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Linker_Supported_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the currently used linker is supported, False otherwise.

   -------------------------
   -- Is_Linker_Supported --
   -------------------------

   function Is_Linker_Supported
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean
   is
      Supported      : Boolean := False;
      Target         : constant String := Kernel.Get_Target;
      Linker_Exe     : constant Filesystem_String :=
                         Filesystem_String ((if Target /= "" then
                                               Target & "-ld"
                                            else
                                               "ld"));
      Linker         : constant Virtual_File := Locate_On_Path (Linker_Exe);
      Version_Arg    : aliased String := "--version";
      Version_Regexp : constant Pattern_Matcher :=
                         Compile ("GNU ld \(GNU Binutils\)"
                                  & " ([0-9]+)\.([0-9]+)"
                                  & "\.([0-9]+)?\.?([0-9]+)?\.?([0-9]+)?");

      subtype Linker_Version_Match_Array is Match_Array (0 .. 5);

      function Is_Version_Supported
        (Output  : String;
         Matched : Linker_Version_Match_Array) return Boolean;

      --------------------------
      -- Is_Version_Supported --
      --------------------------

      function Is_Version_Supported
        (Output  : String;
         Matched : Linker_Version_Match_Array) return Boolean
      is
         Is_Higher  : Boolean;
         Min_Values : constant array (1 .. 5) of Integer :=
                        (1 => 2,
                         2 => 25,
                         3 => 51,
                         4 => 0,
                         5 => 4);
         Match      : Match_Location;
         Value      : Integer;
      begin
         for I in Min_Values'Range loop
            Match := Matched (I);
            Value := (if Match /= (0, 0) then
                         Integer'Value
                        (Output (Match.First .. Match.Last))
                      else
                         Integer'Last);
            Is_Higher := Value >= Min_Values (I);

            exit when Is_Higher = False;
         end loop;

         return Is_Higher;
      end Is_Version_Supported;

   begin
      if Linker /= No_File then
         declare
            Descriptor : TTY_Process_Descriptor;
            Match      : Expect_Match;
            Matched    : Linker_Version_Match_Array;
         begin
            Non_Blocking_Spawn
              (Descriptor  => Descriptor,
               Command     => Linker.Display_Full_Name,
               Args        => (1 => Version_Arg'Unchecked_Access),
               Err_To_Out  => True);

            begin
               Expect (Descriptor => Descriptor,
                       Result     => Match,
                       Regexp     => Version_Regexp,
                       Matched    => Matched,
                       Timeout    => 1_000);
            exception
               when Process_Died =>
                  Close (Descriptor);
                  return False;
            end;

            case Match is
               when Expect_Timeout =>
                  Supported := False;
               when others =>
                  Supported :=
                    Is_Version_Supported (Expect_Out (Descriptor), Matched);
            end case;

            Close (Descriptor);
         end;
      end if;

      return Supported;
   end Is_Linker_Supported;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Linker_Supported_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Target : constant String :=
                 Kernel.Get_Project_Tree.Root_Project.Get_Target;
   begin
      Linker_Parsers_Module.Is_Linker_Supported :=
        (Target /= "" and then Target /= "native"
         and then Kernel.Get_Build_Mode = "default"
         and then Is_Linker_Supported (Kernel));

      return Linker_Parsers_Module.Is_Linker_Supported;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      --  Close the view each time a project is loaded
      Memory_Usage_MDI_Views.Close (Kernel);
   end Execute;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Linker_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is

      function Is_Relevant_Output return Boolean
      is
        (Linker_Parsers_Module.Is_Linker_Supported
         and then (Command.Name = "Build All"
                   or else Command.Name = "Build Main"));
      --  Return True if the output to parse may contain memory usage
      --  information.

      procedure Parse_Memory_Region_Desciption;
      --  Parse Item to get the associated memory region description

      ------------------------------------
      -- Parse_Memory_Region_Desciption --
      ------------------------------------

      procedure Parse_Memory_Region_Desciption is
         Memory_Region   : Memory_Region_Description;
         First_Non_Blank : Integer := Item'First;
      begin
         Skip_Blanks (Item, First_Non_Blank);

         declare
            Words : constant Unbounded_String_Array :=
                      Split (Item (First_Non_Blank .. Item'Last - 2),
                             On => ' ');
            --  Don't keep the '%' and ASCII.LF characters placed at the end.
         begin
            begin
               Assert (Me,
                       Words'Length = 6,
                       " linker's output format for --print-memory-usage has "
                    & "not been recognized");
            exception
               when Ada.Assertions.Assertion_Error =>
                  return;
            end;

            Memory_Region.Name := Words (Words'First);
            Delete (Memory_Region.Name,
                    From    => Length (Memory_Region.Name),
                    Through => Length (Memory_Region.Name));
            Memory_Region.Used_Size :=
              Words (Words'First + 1) & ' ' & Words (Words'First + 2);
            Memory_Region.Total_Size :=
              Words (Words'First + 3) & ' ' & Words (Words'First + 4);
            Memory_Region.Percentage_Used :=
              Float'Value (To_String (Words (Words'First + 5)));

            Self.Memory_Regions.Append (Memory_Region);
         end;
      end Parse_Memory_Region_Desciption;

   begin
      --  Check whether the output may contain information about memory usage
      if Is_Relevant_Output then

         --  If the memory usage output header has not been detected yet, try
         --  to detect it.
         --  Otherwise, parse the memory region description contained in Item.
         if not Self.Memory_Usage_Output_Detected then
            declare
               Memory_Usage_Output : constant Pattern_Matcher :=
                                       Compile ("^Memory region\s+Used Size\s+"
                                                & "Region Size\s+%age Used");
               Matched             : Match_Array (0 .. 0);
            begin
               Match (Memory_Usage_Output, Item, Matched);

               if Matched (0) /= No_Match then
                  Self.Memory_Usage_Output_Detected := True;
               end if;
            end;
         else
            Parse_Memory_Region_Desciption;
         end if;
      end if;

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Linker_Parser;
      Status  : Integer;
      Command : access Root_Command'Class) is
   begin
      --  Spawn the memory usage view with the newly parsed memory descriptions
      if Status = 0 and then not Self.Memory_Regions.Is_Empty then
         declare
            Kernel : constant Kernel_Handle :=
                       Memory_Usage_MDI_Views.Get_Module.Get_Kernel;
            View : constant Memory_Usage_MDI_Views.View_Access :=
                       Memory_Usage_MDI_Views.Get_Or_Create_View (Kernel);
         begin
            View.Refresh (Self.Memory_Regions);

            Self.Memory_Regions.Clear;
            Self.Memory_Usage_Output_Detected := False;
         end;
      end if;

      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);
   end End_Of_Stream;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Linker_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
     (new Linker_Parser'
        (Child                        => Child,
         Memory_Usage_Output_Detected => False,
         Memory_Regions               => <>));

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Parser_Fabric : constant access Linker_Parsers.Linker_Parser_Fabric :=
                        new Linker_Parsers.Linker_Parser_Fabric;
      Linker_Filter : constant Action_Filter := new Linker_Supported_Filter;
   begin
      Project_Changed_Hook.Add (new On_Project_Changed);

      Linker_Parsers_Module := new Linker_Parser_Module_ID_Record;
      Register_Module (Linker_Parsers_Module, Kernel, "linker_parsers");

      Register_Filter (Kernel, Linker_Filter, "Linker is supported");

      Register_Output_Parser (Parser_Fabric, "linker_parser");
   end Register_Module;

end Memory_Usage_Views.Linker_Parsers;
