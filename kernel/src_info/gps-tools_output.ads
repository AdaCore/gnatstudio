------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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

--  Root of filters hierarchy to parse output of tools running by GPS.

with Commands; use Commands;
with String_List_Utils;

package GPS.Tools_Output is

   type Tools_Output_Parser is tagged;
   type Tools_Output_Parser_Access is access all Tools_Output_Parser'Class;

   type Tools_Output_Parser (Child : Tools_Output_Parser_Access) is
     abstract tagged null record;
   --  Output parsers organized in the chains.
   --  Child is pointer to next item in the chain.

   procedure Parse_Standard_Output
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : Command_Access);
   --  Parse a piece of an output passed as Item.
   --  Default implementation just pass Item to the Child if any.

   procedure Parse_Standard_Error
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : Command_Access);
   --  Parse a piece of an error output passed as Item.
   --  Default implementation just pass Item to the Child if any.

   procedure End_Of_Stream
     (Self    : not null access Tools_Output_Parser;
      Status  : Integer;
      Command : Command_Access);
   --  Process end of streams (both output and error).
   --  Default implementation just call the Child if any.

   procedure Destroy (Self : not null access Tools_Output_Parser);
   --  Free internal allocated data

   procedure Free (Self : in out Tools_Output_Parser_Access);
   --  Deallocate Self object and it's children

   type Output_Parser_Fabric is abstract tagged limited null record;
   --  Abstract fabric to create output parser objects

   function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is abstract;
   --  Create output parser object. Set Child as next item in the chain

   procedure Register_Output_Parser
     (Fabric   : access Output_Parser_Fabric'Class;
      Name     : String);
   --  Register new output parser fabric with given name

   function New_Parser_Chain
     (Name_List : String_List_Utils.String_List.List)
      return Tools_Output_Parser_Access;
   --  Create new chain of Tools_Output_Parsers.
   --  Result should be deallocated after use

   type External_Parser_Fabric is abstract tagged limited null record;

   procedure Create_External_Parsers
     (Self        : access External_Parser_Fabric;
      Parser_List : in out String_List_Utils.String_List.List_Node;
      Child       : in out Tools_Output_Parser_Access;
      Found       : out Boolean) is abstract;
   --  This procedure iterates over Parser_List and creates output parser chain
   --  in scripting language. Then it creates wrapper and adds it to Child.
   --  It stops when unknown parser encountered.
   --  If even very first parser not found it returns Found = False and
   --  unmodified Child and Parser_List.

   type External_Parser_Fabric_Access is
     access all External_Parser_Fabric'Class;

   procedure Set_External_Parser_Fabric
     (Value : External_Parser_Fabric_Access);
   --  Define external parser fabric

   function Default_Parser_Names (Is_Run_Target : Boolean) return String;
   --  List of parsers used by default by a target. Is_Run_Target specifies
   --  if target of the interest is a run target or not. Run targets have
   --  the simplest output processing

   Default_Macros : constant String;
   --  Macro to represent Default_Parser_Names in list of parser names

private

   Default_Macros : constant String := "[default]";

end GPS.Tools_Output;
