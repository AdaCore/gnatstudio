-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This package contains a number of high-level functions to manipulate
--  a project tree (see Prj.Tree).
--  </description>

with Types;
with Prj;        use Prj;
with Prj.Tree;   use Prj.Tree;
with Snames;

package Prj_API is

   function Create_Project (Name, Path : String) return Project_Node_Id;
   --  Create a new empty project.
   --  You must have called "Project_Nodes.Set_Last (Empty_Node)" first.

   function Get_Or_Create_Declaration (Project : Project_Node_Id)
      return Project_Node_Id;
   --  Create (or get) the declaration associated with project
   --  This returns a N_Project_Declaration

   function Get_Or_Create_Variable
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Create (or get an existing) variable by Name.
   --  The new variable will be added either to the project (global variable)
   --  or in one of its packages, both are specified in Prj_Or_Pkg.
   --
   --  The variable is added before the others in the list of declarations.
   --  If the variable is a list, it also creates the associated
   --  N_Literal_String_List node.

   procedure Append_To_List (Var : Project_Node_Id; Value : String);
   --  Append a simple string to Var.
   --  Var must be a list, and contain a N_Literal_String_List, as created
   --  by Get_Or_Create_Variable above

   procedure Set_Value (Var : Project_Node_Id; Value : String);
   --  Set the value for a variable. Var mustn't be a list.

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id;
   --  Create (or get an existing) package in project.


   ---------------------------------------
   --  Table interface to project files --
   ---------------------------------------
   --  The functions below should be removed eventually, but are kept until
   --  they are converted to Prj.Tree interface. They still use the table
   --  interface to projects.

   function Parse_Project (Name : String) return Prj.Project_Id;
   --  Parse the project Name, and return a handler to it.

   function Get_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      In_Package : Types.Name_Id := Snames.Name_Compiler)
     return Prj.Variable_Value;
   --  Return the list of specific switches to use for a file.
   --  Note that the return value can be either a single switch or a list of
   --  them.
   --  In_Package should be one of Name_Compiler, Name_Binder, Name_Linker

   function Get_Default_Switches
     (Project    : Prj.Project_Id;
      In_Package : Types.Name_Id := Snames.Name_Compiler)
     return Prj.Variable_Value;
   --  Return the list of default switches to use for a file
   --  In_Package should be one of Name_Compiler, Name_Binder, Name_Linker

   function Find_Project_Containing
     (Root : Prj.Project_Id; File : String) return Prj.Project_Id;
   --  Find the sub-project of Root to which File_Name belong.

   function Find_Project (Name : String) return Prj.Project_Id;
   --  Find the project whose name is Name.

   -------------------------
   -- Setting up projects --
   -------------------------

   procedure Set_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      Switches   : String;
      In_Package : Types.Name_Id := Snames.Name_Compiler);
   --  Set the switches to use to compile File_Name.
   --  The string Switches is split as required.
   --
   --  In_Package should be one of "Name_Compiler", "Name_Binder",
   --  "Name_Linker".

   function Create_Project (Name : String) return Prj.Project_Id;
   --  Create a new empty project.

   procedure Import_Project (Parent, Imported : Prj.Project_Id);
   --  Make Parent import Imported

   procedure Add_Source_Dir (Project : Prj.Project_Id; Dir : String);
   --  Add a new directory to the list of source directories for Project.
   --  It doesn't check if the directory is already there.

end Prj_API;
