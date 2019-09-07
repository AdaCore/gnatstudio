------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  This package provides functions for interfacing between XML and our
--  internal representation for switches.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Switches_Chooser;      use Switches_Chooser;
with XML_Utils;

package Switches_Parser is

   type Other_Config_Finder is access
     function (Name : String) return Switches_Editor_Config;
   --  See Parse_Switches_Node

   procedure Parse_Switches_Node
     (Current_Tool_Name   : Unbounded_String;
      Current_Tool_Config : out Switches_Editor_Config;
      Error_Message       : out Unbounded_String;
      Finder              : Other_Config_Finder;
      Node                : XML_Utils.Node_Ptr);
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
