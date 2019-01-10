------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package provides wrapping structures for exploring entities
--  with a common interface, whether they come from the completion or the
--  entities database.

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Language;               use Language;
with Language.Tree.Database; use Language.Tree.Database;
with Basic_Types;            use Basic_Types;
with Completion;             use Completion;
with Ada_Semantic_Tree;      use Ada_Semantic_Tree;
with GPS.Kernel;
with Xref;

package Engine_Wrappers is

   --------------
   -- Proposal --
   --------------

   type Root_Proposal is abstract tagged null record;

   type Root_Proposal_Access is access all Root_Proposal'Class;

   function Get_Label
     (Proposal : Root_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return String is abstract;
   function Get_Completion
     (Proposal : Root_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return String
      is abstract;
   function Get_Visibility
     (Proposal : Root_Proposal) return Construct_Visibility is abstract;
   function Get_Category
     (Proposal : Root_Proposal) return Language_Category is abstract;
   function Get_Caret_Offset
     (Proposal : Root_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Character_Offset_Type is abstract;
   function Get_Location
     (Proposal : Root_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return File_Location is abstract;

   function Get_Documentation
     (Proposal : Root_Proposal;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
      return String is abstract;
   --  Some extra explanation on the proposal, to be displayed in the
   --  completion dialog. Typically, this will display the documentation for
   --  the corresponding entity if the proposal is based on a source code
   --  entity.

   function Get_Custom_Icon_Name
     (Proposal : Root_Proposal)
      return String is ("");
   --  If the proposal has a custom icon (not determined by the completion
   --  type), this function will return its name

   function Is_Accessible
     (Proposal : Root_Proposal)
      return Boolean is (True);
   --  Returns true if the proposal is accessible from the context of the
   --  current completion

   procedure Free (X : in out Root_Proposal) is null;

   procedure Shallow_Free (X : in out Root_Proposal) is null;
   --  That is special version for use only with instances
   --  that obtained by call of Get_Proposal.

   function Deep_Copy
     (Proposal : Root_Proposal) return Root_Proposal'Class is abstract;
   --  Make a deep copy of Proposal. Result should be freed by the caller.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Proposal'Class, Root_Proposal_Access);

   --------------
   -- Iterator --
   --------------

   type Root_Iterator is abstract tagged null record;

   type Root_Iterator_Access is access all Root_Iterator'Class;

   function At_End (Iter : Root_Iterator) return Boolean is abstract;
   function Is_Valid (Iter : Root_Iterator) return Boolean is abstract;
   procedure Next
     (Iter : in out Root_Iterator;
      Db : access Xref.General_Xref_Database_Record'Class) is abstract;
   function Get_Proposal
     (Iter    : Root_Iterator) return Root_Proposal'Class is abstract;
   --  Get a proposal. The result should not be stored.
   --  If you want to store a copy, call Deep_Copy.
   --  You should call Shallow_Free on the result in any case.

   procedure Free (Iter : in out Root_Iterator) is abstract;
   --  Free allocated memory

   procedure Free (Iter : in out Root_Iterator_Access);
   --  Free allocated memory by Iterator and free iterator

   ----------------------------------------------
   -- Implementation for the Completion engine --
   ----------------------------------------------

   --  Proposal

   type Comp_Proposal is new Root_Proposal with private;

   overriding function Get_Label
     (Proposal : Comp_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return String;
   overriding function Get_Completion
     (Proposal : Comp_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return String;
   overriding function Get_Visibility
     (Proposal : Comp_Proposal) return Construct_Visibility;
   overriding function Get_Category
     (Proposal : Comp_Proposal) return Language_Category;
   overriding function Get_Caret_Offset
     (Proposal : Comp_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Character_Offset_Type;
   overriding function Get_Location
     (Proposal : Comp_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return File_Location;
   overriding function Get_Documentation
     (Proposal : Comp_Proposal;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
      return String;
   overriding function Get_Custom_Icon_Name
     (Proposal : Comp_Proposal) return String;
   overriding function Is_Accessible
     (Proposal : Comp_Proposal)
      return Boolean;

   overriding procedure Free (X : in out Comp_Proposal);
   overriding procedure Shallow_Free (X : in out Comp_Proposal);

   function Get_Underlying_Proposal
     (C : Comp_Proposal) return Completion_Proposal_Access;
   --  Return the proposal

   --  Iterator

   type Comp_Iterator is new Root_Iterator with record
      I : Completion_Iterator;
   end record;

   overriding procedure Free (Iter : in out Comp_Iterator);

   procedure Set_Completion_Iterator
     (Comp_Iter : in out Comp_Iterator;
      Completion_Iter : Completion_Iterator);

   overriding function At_End (Iter : Comp_Iterator) return Boolean;
   overriding function Is_Valid (Iter : Comp_Iterator) return Boolean;
   overriding procedure Next
     (Iter : in out Comp_Iterator;
      Db   : access Xref.General_Xref_Database_Record'Class);
   overriding function Get_Proposal
     (Iter    : Comp_Iterator) return Root_Proposal'Class;

   -------------------------------------------------
   -- Implementation for the entity search engine --
   -------------------------------------------------

   --  Proposal

   type Entity_Proposal is new Root_Proposal with private;

   overriding function Get_Label
     (Proposal : Entity_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return String;
   overriding function Get_Completion
     (Proposal : Entity_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return String;
   overriding function Get_Visibility
     (Proposal : Entity_Proposal) return Construct_Visibility;
   overriding function Get_Category
     (Proposal : Entity_Proposal) return Language_Category;
   overriding function Get_Caret_Offset
     (Proposal : Entity_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Character_Offset_Type;
   overriding function Get_Location
     (Proposal : Entity_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return File_Location;
   overriding function Get_Documentation
     (Proposal : Entity_Proposal;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
      return String;

   --  Iterator

   type Entity_Iterator is new Root_Iterator with record
      I : Ada_Semantic_Tree.Entity_Iterator;
   end record;

   overriding procedure Free (Iter : in out Entity_Iterator);

   overriding function At_End (Iter : Entity_Iterator) return Boolean;
   overriding function Is_Valid (Iter : Entity_Iterator) return Boolean;
   overriding procedure Next
     (Iter : in out Entity_Iterator;
      Db : access Xref.General_Xref_Database_Record'Class);
   overriding function Get_Proposal
     (Iter    : Entity_Iterator) return Root_Proposal'Class;

private

   type Comp_Proposal is new Root_Proposal with record
      P : Completion_Proposal_Access;
   end record;

   overriding function Deep_Copy
     (Proposal : Comp_Proposal) return Root_Proposal'Class;

   type Entity_Proposal is new Root_Proposal with record
      File      : Virtual_File;
      Construct : Simple_Construct_Information;
   end record;

   overriding function Deep_Copy
     (Proposal : Entity_Proposal) return Root_Proposal'Class;

end Engine_Wrappers;
