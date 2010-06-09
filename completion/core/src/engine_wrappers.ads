-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2010, AdaCore                 --
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

--  This package provides wrapping structures for exploring entities
--  with a common interface, whether they come from the completion or the
--  entities database.

with Ada.Unchecked_Deallocation;

with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Language;               use Language;
with Language.Tree.Database; use Language.Tree.Database;
with Basic_Types;            use Basic_Types;
with Completion;             use Completion;
with Ada_Semantic_Tree;      use Ada_Semantic_Tree;

package Engine_Wrappers is

   --------------
   -- Proposal --
   --------------

   type Root_Proposal is abstract tagged null record;

   type Root_Proposal_Access is access all Root_Proposal'Class;

   function Get_Label (Proposal : Root_Proposal) return String is abstract;
   function Get_Completion
     (Proposal : Root_Proposal) return String is abstract;
   function Get_Visibility
     (Proposal : Root_Proposal) return Construct_Visibility is abstract;
   function Get_Category
     (Proposal : Root_Proposal) return Language_Category is abstract;
   function Get_Caret_Offset
     (Proposal : Root_Proposal) return Character_Offset_Type is abstract;
   function Get_Documentation
     (Proposal : Root_Proposal) return String is abstract;
   function Get_Location
     (Proposal : Root_Proposal) return File_Location is abstract;

   procedure Free (X : in out Root_Proposal) is abstract;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Proposal'Class, Root_Proposal_Access);

   --------------
   -- Iterator --
   --------------

   type Root_Iterator is abstract tagged null record;

   type Root_Iterator_Access is access all Root_Iterator'Class;

   function At_End (Iter : Root_Iterator) return Boolean is abstract;
   function Is_Valid (Iter : Root_Iterator) return Boolean is abstract;
   procedure Next (Iter : in out Root_Iterator) is abstract;
   function Get_Proposal
     (Iter    : Root_Iterator) return Root_Proposal'Class is abstract;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Iterator'Class, Root_Iterator_Access);

   ----------------------------------------------
   -- Implementation for the Completion engine --
   ----------------------------------------------

   --  Proposal

   type Comp_Proposal is new Root_Proposal with private;

   overriding function Get_Label (Proposal : Comp_Proposal) return String;
   overriding function Get_Completion
     (Proposal : Comp_Proposal) return String;
   overriding function Get_Visibility
     (Proposal : Comp_Proposal) return Construct_Visibility;
   overriding function Get_Category
     (Proposal : Comp_Proposal) return Language_Category;
   overriding function Get_Caret_Offset
     (Proposal : Comp_Proposal) return Character_Offset_Type;
   overriding function Get_Documentation
     (Proposal : Comp_Proposal) return String;
   overriding function Get_Location
     (Proposal : Comp_Proposal) return File_Location;

   overriding procedure Free (X : in out Comp_Proposal);

   function Get_Underlying_Proposal
     (C : Comp_Proposal) return Completion_Proposal_Access;
   --  Return the proposal

   --  Iterator

   type Comp_Iterator is new Root_Iterator with record
      I : Completion_Iterator;
   end record;

   overriding function At_End (Iter : Comp_Iterator) return Boolean;
   overriding function Is_Valid (Iter : Comp_Iterator) return Boolean;
   overriding procedure Next (Iter : in out Comp_Iterator);
   overriding function Get_Proposal
     (Iter    : Comp_Iterator) return Root_Proposal'Class;

   -------------------------------------------------
   -- Implementation for the entity search engine --
   -------------------------------------------------

   --  Proposal

   type Entity_Proposal is new Root_Proposal with private;

   overriding function Get_Label (Proposal : Entity_Proposal) return String;
   overriding function Get_Completion
     (Proposal : Entity_Proposal) return String;
   overriding function Get_Visibility
     (Proposal : Entity_Proposal) return Construct_Visibility;
   overriding function Get_Category
     (Proposal : Entity_Proposal) return Language_Category;
   overriding function Get_Caret_Offset
     (Proposal : Entity_Proposal) return Character_Offset_Type;
   overriding function Get_Documentation
     (Proposal : Entity_Proposal) return String;
   overriding function Get_Location
     (Proposal : Entity_Proposal) return File_Location;

   overriding procedure Free (X : in out Entity_Proposal);

   --  Iterator

   type Entity_Iterator is new Root_Iterator with record
      I : Ada_Semantic_Tree.Entity_Iterator;
   end record;

   overriding function At_End (Iter : Entity_Iterator) return Boolean;
   overriding function Is_Valid (Iter : Entity_Iterator) return Boolean;
   overriding procedure Next (Iter : in out Entity_Iterator);
   overriding function Get_Proposal
     (Iter    : Entity_Iterator) return Root_Proposal'Class;

private

   type Comp_Proposal is new Root_Proposal with record
      P : Completion_Proposal_Access;
   end record;

   type Entity_Proposal is new Root_Proposal with record
      File      : Virtual_File;
      Construct : Simple_Construct_Information;
      Documentation : String_Access;
   end record;

end Engine_Wrappers;
