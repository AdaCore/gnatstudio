------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

--  Parsers used to retrieve the linker's output when the
--  '--print-memory-usage' switch is present on the 'Build All'/'Build Main'
--  command line. This switch is enabled by default on these two Build Targets
--  and disabled by a switch filter when the linker does not support it.
--
--  The parsed output (if any) is then given to the Memory Usage View which
--  displays it.

with Commands;           use Commands;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Tools_Output;   use GPS.Tools_Output;

private package Memory_Usage_Views.Linker_Parsers is

   type Linker_Parser_Module_ID_Record is new Module_ID_Record with private;
   type Linker_Parser_Module is
     access all Linker_Parser_Module_ID_Record'Class;

   type Linker_Parser is new Tools_Output_Parser with private;
   --  This parser reads output from linkers that support the
   --  '--print-memory-usage' option.
   --
   --  Here is a an example of the '--print-memory-usage' option output:
   --
   --     Memory region         Used Size  Region Size  %age Used
   --       flash :               87632 B         1 MB      8.36%
   --      sram12 :               53984 B       128 KB     41.19%
   --         ccm :                1408 B        64 KB      2.15%

   overriding procedure Parse_Standard_Output
     (Self    : not null access Linker_Parser;
      Item    : String;
      Command : access Root_Command'Class);

   overriding procedure End_Of_Stream
     (Self    : not null access Linker_Parser;
      Status  : Integer;
      Command : access Root_Command'Class);

   type Linker_Parser_Fabric is new Output_Parser_Fabric with private;
   --  Used to create linker parsers when building

   overriding function Create
     (Self  : access Linker_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the the linker parser fabric

private

   type Linker_Parser_Module_ID_Record is new Module_ID_Record with record
      Is_Linker_Supported : Boolean := False;
      --  Whether the currently used linker is supported by the parser.
      --  This boolean is set each time a project is loaded.
   end record;

   type Linker_Parser is new Tools_Output_Parser with record
      Memory_Usage_Output_Detected : Boolean := False;
      --  Used to know if the output to parse is part of the linker's memory
      --  usage output.

      Memory_Regions               : Memory_Region_Description_Lists.List;
      --  The memory regions descriptions that have been parser by the linker.
   end record;

   type Linker_Parser_Fabric is new Output_Parser_Fabric with null record;

end Memory_Usage_Views.Linker_Parsers;
