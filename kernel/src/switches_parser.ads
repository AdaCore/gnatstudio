-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides functions for interfacing between XML and our
--  internal representation for switches.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Switches_Chooser;      use Switches_Chooser;

with Glib.Xml_Int;

package Switches_Parser is

   type Other_Config_Finder is access
     function (Name : String) return Switches_Editor_Config;
   --  See Parse_Switches_Node

   procedure Parse_Switches_Node
     (Current_Tool_Name    : String;
      Current_Tool_Config  : out Switches_Editor_Config;
      Error_Message        : out Unbounded_String;
      Finder               : Other_Config_Finder;
      Node                 : Glib.Xml_Int.Node_Ptr);
   --  Parse a <switches> node, and returns the corresponding configuration
   --  Current_Tool_Name is the name of the tool we are currently parsing.
   --  Node contains the XML data to parse.
   --  Result of the parsing is returned in Current_Tool_Config, and the
   --  error messages are returned in Error_Message.
   --  Finder is a function that looks up another tool configuration based on
   --  the tool's name. This is needed only for the <dependency> node. Parsers
   --  which have no need to support the <dependency> node can set the Finder
   --  parameter to null.

end Switches_Parser;
