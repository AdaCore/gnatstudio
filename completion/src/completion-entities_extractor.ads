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

with Entities;                 use Entities;
with Entities.Queries;         use Entities.Queries;
with Language.Tree;            use Language.Tree;
with Projects;                 use Projects;
with Language_Handlers;        use Language_Handlers;
with VFS;                      use VFS;

package Completion.Entities_Extractor is

   type Entity_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on xrefs informations

   function New_Entity_Completion_Resolver
     (Tree    : Construct_Tree_Access;
      Project : Project_Type;
      Handler : access Language_Handler_Record'Class)
      return Entity_Completion_Resolver;
   --  Return an initialized completion resolver based on the relevant datas

   procedure Get_Possibilities
     (Resolver   : access Entity_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List);
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

   function Get_Completion (Proposal : Entity_Completion_Proposal)
      return UTF8_String;
   --  See inherited documentation

   function Get_Category (Proposal : Entity_Completion_Proposal)
     return Language_Category;
   --  See inherited documentation

   procedure Get_Composition
     (Proposal : Entity_Completion_Proposal;
      Offset   : Positive;
      Result   : in out Completion_List);
   --  See inherited documentation

   function Get_Number_Of_Parameters (Proposal : Entity_Completion_Proposal)
     return Natural;
   --  See inherited documentation

   procedure Free (Proposal : in out Entity_Completion_Proposal);
   --  See inherited documentation

   function Get_Source_For_Unit
     (Handler   : access Language_Handler_Record'Class;
      Project   : Project_Type;
      Unit_Name : String) return Source_File;
   --  Return the source file corresponding to the unit name given in parameter

   function Get_Unit_Info (Source : Source_File) return Entity_Information;
   --  Return the Entity_Information corresponding to the source file given in
   --  parameter

   --------------------------
   -- Completion_Iterators --
   --------------------------

   -- Entity_Iterator --

   type Entity_Tree_Wrapper is new Completion_List_Pckg.Virtual_List_Component
   with record
      Handler    : LI_Handler;
      Resolver   : Completion_Resolver_Access;
      Name       : String_Access;
      Is_Partial : Boolean;
      Filter     : Possibilities_Filter;
   end record;

   type Entity_Iterator_Wrapper is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      It         : LI_Entities_Iterator;
      Resolver   : Completion_Resolver_Access;
      Is_Partial : Boolean;
      Name       : String_Access;
      Filter     : Possibilities_Filter;
   end record;

   function First (Tree : Entity_Tree_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Entity_Iterator_Wrapper) return Boolean;
   --  See inherited documentation

   procedure Next (It : in out Entity_Iterator_Wrapper);
   --  See inherited documentation

   function Get (This : Entity_Iterator_Wrapper)
      return Completion_Proposal'Class;
   --  See inherited documentation

   procedure Free (This : in out Entity_Iterator_Wrapper);

   -- Call_Iterator --

   type Calls_Wrapper is new Completion_List_Pckg.Virtual_List_Component
   with record
      Scope    : Entity_Information;
      Resolver : Completion_Resolver_Access;
   end record;

   type Calls_Iterator_Wrapper is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      It       : Calls_Iterator;
      Scope    : Entity_Information;
      Resolver : Completion_Resolver_Access;
   end record;

   function First (Scope : Calls_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Calls_Iterator_Wrapper) return Boolean;
   --  See inherited documentation

   procedure Next (It : in out Calls_Iterator_Wrapper);
   --  See inherited documentation

   function Get (This : Calls_Iterator_Wrapper)
      return Completion_Proposal'Class;
   --  See inherited documentation

   procedure Free (This : in out Calls_Iterator_Wrapper);
   --  See inherited documentation

   function Is_Valid (It : Calls_Iterator_Wrapper) return Boolean;
   --  Return false if the iterator is invalid, e.g. shouldn't be used by the
   --  user. Note that the end iterator is a valid iterator regarding this
   --  definition.

   -- Child_Iterator --

   type Child_Wrapper is new Completion_List_Pckg.Virtual_List_Component
   with record
      Parent   : Entity_Information;
      Resolver : Completion_Resolver_Access;
   end record;

   type Child_Iterator_Wrapper is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      It       : Child_Type_Iterator;
      Resolver : Completion_Resolver_Access;
   end record;

   function First (Parent : Child_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Child_Iterator_Wrapper) return Boolean;
   --  See inherited documentation

   procedure Next (It : in out Child_Iterator_Wrapper);
   --  See inherited documentation

   function Get (This : Child_Iterator_Wrapper)
      return Completion_Proposal'Class;
   --  See inherited documentation

   procedure Free (This : in out Child_Iterator_Wrapper);
   --  See inherited documentation

   -- Source_File_Iterator --

   type Source_File_Component is new
     Completion_List_Pckg.Virtual_List_Component
   with record
      Files    : VFS.File_Array_Access;
      Resolver : Completion_Resolver_Access;
      Parent   : Entity_Information;
   end record;

   type Source_File_Iterator is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      Files    : VFS.File_Array_Access;
      It       : Integer;
      Unit     : Entity_Information;
      Resolver : Completion_Resolver_Access;
      Parent   : Entity_Information;
   end record;

   function First (List : Source_File_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Source_File_Iterator) return Boolean;
   --  See inherited documentation

   procedure Next (It : in out Source_File_Iterator);
   --  See inherited documentation

   function Get (This : Source_File_Iterator)
      return Completion_Proposal'Class;
   --  See inherited documentation

   procedure Free (This : in out Source_File_Component);
   --  See inherited documentation

   procedure Set_Unit (It : in out Source_File_Iterator);
   --  See inherited documentation

end Completion.Entities_Extractor;
