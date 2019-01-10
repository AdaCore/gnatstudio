------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

--  Parses or dump the toolchain definition part of a Project
--
--  Supported patterns are:
--
--  package IDE is
--    for GNAT use "name";
--    for GNATlist use "name";
--    for Compiler_Command ("c") use "name";
--    for Compiler_Command ("ada") use "name";
--  end IDE;
--
--  type Target_Type is ("cross-triplet", ...)
--  Target : Target_Type := external ("TARGET", "native")
--  package IDE is
--    for GNAT use Target & "-gnat";
--    for GNATlist use Target & "-gnatls";
--    for Compiler_Command ("c") use Target & "-gnat";
--    for Compiler_Command ("ada") use Target & "-gcc";
--  end IDE;
--
--  type Target_Type is (<native | aamp | custom>, "cross-triplet", ...)
--  Target : Target_Type := external ("TARGET", "native")
--  package IDE is
--    case Target is
--       when <native | aamp | custom> =>
--          for GNAT use "name";
--          for GNATlist use "name";
--          for Compiler_Command ("c") use "name";
--          for Compiler_Command ("ada") use "name";
--       when others =>
--          for GNAT use Target & "-gnat";
--          for GNATlist use Target & "-gnatls";
--          for Compiler_Command ("c") use Target & "-gnat";
--          for Compiler_Command ("ada") use Target & "-gcc";
--    end case;
--  end IDE;
--
--  These patterns can be read and analyzed by the parser, and are generated
--  depending on the selected toolchains.
--
--  The parser is able to detect that it can't handle the toolchain description
--  and will provide a way to get a message in such case.
--
--  The parser is able to follow package renaming, and will offer update
--  capabilities on the renamed package.

with GPR; use GPR;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

package Toolchains.Parsers is

   type Parsed_Project_Record is private;
   type Parsed_Project is access all Parsed_Project_Record;

   type Project_Parser_Record is limited private;
   type Project_Parser is access all Project_Parser_Record;

   --------------------
   -- Project_Parser --
   --------------------

   procedure Parse
     (This         : in out Project_Parser_Record;
      Manager      : Toolchain_Manager;
      Path         : Virtual_File;
      Project_Path : File_Array);
   --  Parse a project according to its location (path) and the project path
   --  (GNAT_Project_Path).

   function Get_Manager
     (This : Project_Parser_Record) return Toolchain_Manager;

   function Is_Valid (This : Project_Parser_Record) return Boolean;
   --  Return true if the parser could correctly parse the project, false
   --  otherwise. And invalid project may be semantically correct, but doesn't
   --  fall into the standard supported toolchain description.

   function Get_Toolchains
     (This : Project_Parser_Record) return Toolchain_Array;
   --  Return the toolchains red from the project file.

   procedure Set_Toolchains
     (This       : in out Project_Parser_Record;
      Toolchains : Toolchain_Array);
   --  Modifies the stucture of the project so that it supports the toolchains
   --  given in parameter

   function Is_Supported (This : Project_Parser_Record) return Boolean;
   --  Return true if the toolchain definition is supported, false otherwise

   function Get_Parsed_Project
     (This : Project_Parser_Record)
      return Parsed_Project;
   --  Return the parsed project from where this toolchain has been extracted.

   function Get_Error_Message (This : Project_Parser_Record) return String;
   --  Return the error message associated to the parsing of this toolchain,
   --  if any.

   --------------------
   -- Parsed_Project --
   --------------------

   function Get_Root_Project
     (This : Project_Parser_Record) return Parsed_Project;

   function Get_Parsed_Project
     (This : Project_Parser_Record;
      Node : Project_Node_Id) return Parsed_Project;

   function Get_Variable
     (This : Parsed_Project_Record; Name : String) return Project_Node_Id;
   --  Return the project node id corresponding to the name given in parameter,
   --  Empty_Node if none.

   function Get_Project_Node
     (This : Parsed_Project_Record) return Project_Node_Id;
   --  Return the project node associated to this project

   function Get_Project_Declaration
     (This : Parsed_Project_Record) return Project_Node_Id;
   --  Return the project declaration that has been parsed

   function Is_Root (This : Parsed_Project_Record) return Boolean;
   --  Return true if the project given in parameter is a root project, false
   --  if it's withed by the root project.

   function Get_Path (This : Parsed_Project_Record) return Virtual_File;
   --  Return this project path.

   procedure Save (This : Parsed_Project_Record);
   --  Save the project to the file from where it has been loaded

   procedure Save (This : Parsed_Project_Record; To : Virtual_File);
   --  Save the current version of the project tree on the file given in
   --  parameter.

private

   package Prj_Node_Sets is new Ada.Containers.Ordered_Sets (Project_Node_Id);

   type Toolchain_Parser_Record
     (Enclosing_Parser : access Project_Parser_Record)
   is record
      Node_Data        : GPR.Project_Node_Tree_Ref;
      Project          : Parsed_Project;

      Error            : String_Access;

      IDE_Package              : Project_Node_Id := Empty_Project_Node;
      Toolchain_Case_Statement : Project_Node_Id := Empty_Project_Node;
      Variable_Node            : Project_Node_Id := Empty_Project_Node;

      Attributes       : Prj_Node_Sets.Set;
      Toolchains       : Toolchain_Maps.Map;
   end record;
   type Toolchain_Parser is access all Toolchain_Parser_Record;

   --------------------
   -- Parsed_Project --
   --------------------

   package Parsed_Projects_Maps is new Ada.Containers.Ordered_Maps
     (Project_Node_Id, Parsed_Project);

   type Project_Parser_Record is limited record
      Manager                : Toolchain_Manager;

      Tree_Data              : Project_Tree_Ref;
      Node_Data              : GPR.Project_Node_Tree_Ref;
      Enclosing_Project_Node : Project_Node_Id;
      Root_Project_Node      : Project_Node_Id;
      Is_Valid               : Boolean := False;

      Toolchain_Found        : Toolchain_Parser_Record
        (Project_Parser_Record'Access);
      Root_Project           : Parsed_Project;
      Parsed_Projects        : Parsed_Projects_Maps.Map;
   end record;

   package Tree_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Project_Node_Id);

   type Parsed_Project_Record is record
      Project_Node : Project_Node_Id;
      Node_Data    : GPR.Project_Node_Tree_Ref;
      Variables    : Tree_Node_Maps.Map;
      Path         : Virtual_File;
      Is_Root      : Boolean;
   end record;

   procedure Initialize
     (This         : Parsed_Project;
      Parser       : in out Project_Parser_Record;
      Node_Data    : GPR.Project_Node_Tree_Ref;
      Project_Node : Project_Node_Id);

end Toolchains.Parsers;
