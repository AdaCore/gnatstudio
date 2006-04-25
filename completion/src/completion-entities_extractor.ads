-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Provides a completer working on entities for Ada

with Entities;          use Entities;
with Language.Tree;     use Language.Tree;
with Projects;          use Projects;
with Language_Handlers; use Language_Handlers;

package Completion.Entities_Extractor is

   type Entity_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on xrefs informations

   function New_Entity_Completion_Resolver
     (Tree    : Construct_Tree_Access;
      Project : Project_Type;
      Handler : access Language_Handler_Record'Class)
      return Entity_Completion_Resolver;
   --  Return an initialized completion resolver based on the relevant datas

   function Get_Possibilities
     (Resolver   : access Entity_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter) return Completion_List;
   --  See inherited documentation

   procedure Free (This : in out Entity_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private

   type Entity_Completion_Resolver is new Completion_Resolver with record
      Tree    : Construct_Tree_Access;
      Project : Project_Type;
      Handler : Language_Handler;
   end record;

   type Entity_Completion_Proposal is new Completion_Proposal with record
      Entity : Entity_Information;
   end record;

   function Get_Name (Proposal : Entity_Completion_Proposal) return String;
   --  See inherited documentation

   function Get_Category (Proposal : Entity_Completion_Proposal)
     return Language_Category;
   --  See inherited documentation

   function Get_Composition
     (Proposal : Entity_Completion_Proposal; Offset : Positive)
      return Completion_List;
   --  See inherited documentation

   function Get_Number_Of_Parameters (Proposal : Entity_Completion_Proposal)
     return Natural;
   --  See inherited documentation

   type Unit_Completion_Proposal is new Completion_Proposal with record
      Info   : Entity_Information;
      Nested : Boolean;
   end record;

   function Get_Name (Proposal : Unit_Completion_Proposal) return String;
   --  See inherited documentation

   function Get_Category (Proposal : Unit_Completion_Proposal)
     return Language_Category;
   --  See inherited documentation

   function Get_Composition
     (Proposal : Unit_Completion_Proposal; Offset : Positive)
      return Completion_List;
   --  See inherited documentation

   function Get_Number_Of_Parameters (Proposal : Unit_Completion_Proposal)
     return Natural;
   --  See inherited documentation

   function Get_Entities
     (Name         : String;
      Unit_Info    : Entity_Information;
      View_Private : Boolean;
      Resolver     : Completion_Resolver_Access;
      Is_Partial   : Boolean := False) return Completion_List;
   --  Return all the entities in the given file of the current name
   --  referenced in the given unit.

   function Get_Source_For_Unit
     (Handler   : access Language_Handler_Record'Class;
      Project   : Project_Type;
      Unit_Name : String) return Source_File;
   --  Return the source file corresponding to the unit name given in parameter

   function Get_Unit_Info (Source : Source_File) return Entity_Information;
   --  Return the Entity_Information corresponding to the source file given in
   --  parameter

end Completion.Entities_Extractor;
