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

--  Provides the base structures / subprograms for the completion manager. A
--  completion manager is the base class of the completion mechanism. Before
--  using it, you have to add manually the completion resolvers you want to
--  use. A completion manager has to be build at each completion request, since
--  it might depend on different datas. You can see for example the subprogram
--  Full_Test in the test driver Completion.Test.

with Glib;         use Glib;

with Basic_Types;  use Basic_Types;
with Language;     use Language;
with Generic_List;
with Virtual_Lists;
with Virtual_Lists.Extensive;

package Completion is

   type Completion_List is private;
   --  This type hold a set of completions

   procedure Free (List : in out Completion_List);
   --  Free the memory associated to the completion list.

   function Get_Completed_String (This : Completion_List) return String;
   --  Return the string that have been analyzed and is in the process of
   --  being completed. This is just the part of the string already in the
   --  buffer, and should be replaced by the string found in the completion,
   --  in order to have the proper casing.

   Null_Completion_List : constant Completion_List;

   -------------------------
   -- Completion_Resolver --
   -------------------------

   type Completion_Resolver is abstract tagged private;
   --  This types holds a completion engine. Various completion engines can be
   --  created, based on constructs or xrefs.

   type Completion_Resolver_Access is access all Completion_Resolver'Class;

   procedure Free (Resolver : in out Completion_Resolver_Access);
   --  Frees a completion resolver access. This will also call the internal
   --  free procedure

   function Next (Resolver : access Completion_Resolver'Class)
      return Completion_Resolver_Access;
   --  Return the next completion resolver in the parent Completion_Manager,
   --  null if none.

   type Possibilities_Filter is mod 2 ** 32;

   All_Visible_Entities : Possibilities_Filter := 1;
   All_Accessible_Units : Possibilities_Filter := 2;
   Everything           : Possibilities_Filter := 16#FFFFFF#;

   procedure Get_Possibilities
     (Resolver   : access Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List) is abstract;
   --  Return the possible completion for the given identifier, using the
   --  resolver given in parameter. If Is_Partial is false, then only
   --  identifiers matching exactly the profile will be returned. Even in this
   --  case, there might be serveal identifiers returned in case of overloaded
   --  subprogram. However, this procedure should take care of visibility
   --  problems, and return only the visible possibilities. Offset should be
   --  the offset of the place from where we try to find the corresponding
   --  identifier, and visiblity will be calculated for this offset.

   procedure Free (Resolver : in out Completion_Resolver) is abstract;
   --  Free the data of a Resolver.

   type Completion_Manager is abstract tagged private;
   --  A completion manager is a type holding a list of completions resolvers.
   --  Resolvers will be called in the order they are referenced. This object
   --  also holds a couple of general datas, such as the buffer from where the
   --  completion is done.

   type Completion_Manager_Access is access all Completion_Manager'Class;

   procedure Free (This : in out Completion_Manager_Access);
   --  Free the memory associated to a completion manager access. This does not
   --  free the referenced resolvers which have to be freed separately.

   procedure Set_Buffer
     (Manager : in out Completion_Manager; Buffer : String_Access);
   --  Set the buffer from where the competion is done. This has to be called
   --  before any completion attempt.

   function Get_Buffer (Manager : Completion_Manager) return String_Access;
   --  Return the buffer associated to this manager.

   procedure Register_Resolver
     (Manager  : access Completion_Manager;
      Resolver : access Completion_Resolver'Class);
   --  Add a resolver to this manager. A given resolver can only be added in
   --  one manager (it knows its manager). Resolvers will be called in the
   --  order that they have been registred.

   -------------------------
   -- Completion_Proposal --
   -------------------------

   type Completion_Proposal is abstract tagged private;
   --  This is the type of a proposal.

   function Get_Resolver (Proposal : Completion_Proposal)
     return Completion_Resolver_Access;
   --  Returns the resolver that have been used to create this proposal.

   type Proposal_Mode is (Show_Parameters, Show_Identifiers);
   --  Various mode for the proposal.
   --  Show_Parameters: Means that the completion proposal should only shows
   --     the possible parameters for the entity. This does not perform any
   --     actual completion.
   --  Show_Identifiers: Means that the completion is an identifier that is
   --     suitable for completion

   procedure Set_Mode
     (Proposal : in out Completion_Proposal; Mode : Proposal_Mode);
   --  Set the display mode

   function Get_Completion (Proposal : Completion_Proposal) return UTF8_String
     is abstract;
   --  Return the text that has to be used for the completion, may be different
   --  from the label.

   function Get_Label (Proposal : Completion_Proposal) return UTF8_String;
   --  Return the label of the completion proposal. By defaut, return the
   --  completion

   function Get_Id (Proposal : Completion_Proposal) return UTF8_String;
   --  Return the identifier of the entity referenced in the proposal. This
   --  identifier can be different from the completion propsed and the label.
   --  By default, return the completion.

   function Get_Category (Proposal : Completion_Proposal)
     return Language_Category is abstract;
   --  Return the category of the object proposed for the completion

   procedure Get_Composition
     (Proposal   : Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List) is abstract;
   --  See inherited documentation is abstract;
   --  Return the possible children of this completion,
   --  Null_Completion_Proposal if none.

   function Get_Number_Of_Parameters
     (Proposal : Completion_Proposal) return Natural is abstract;
   --  If the completion proposal is a subprogram, then this will return the
   --  number of its parameters, otherwise 0.

   function Get_Initial_Completion_List
     (Manager      : Completion_Manager;
      Start_Offset : Natural) return Completion_List is abstract;
   --  Generates an initial completion list, for the cursor pointing at the
   --  given offset. This operation is time consuming, so it would be good
   --  to use the one below afterwards, until the completion process is done.

   procedure Free (Proposal : in out Completion_Proposal) is abstract;
   --  Free the memory associated to the proposal.

   -------------------------
   -- Completion_Iterator --
   -------------------------

   type Completion_Iterator is private;
   --  This type is used to iterate over the various possibilities of a
   --  completion.

   function First (This : Completion_List) return Completion_Iterator;
   --  Return the first proposal of the completion list.

   procedure Next (This : in out Completion_Iterator);
   --  Gets the next proposal of the completion list.

   function Get_Proposal
     (This : Completion_Iterator) return Completion_Proposal'Class;
   --  Return the actual proposal for the given iterator.

   procedure Free (This : in out Completion_Iterator);
   --  Free the data associated to a completion iterator.

   function At_End (This : Completion_Iterator) return Boolean;
   --  Return true if the iterator is after the last element of its list.

   Null_Completion_Iterator : constant Completion_Iterator;
   --  Default value for an empty iterator.

private

   type Completion_Resolver is abstract tagged record
      Manager : Completion_Manager_Access;
   end record;

   package Completion_Resolver_List_Pckg is new Generic_List
     (Completion_Resolver_Access);

   use Completion_Resolver_List_Pckg;

   type Completion_Manager is abstract tagged record
      Buffer    : String_Access;
      Resolvers : Completion_Resolver_List_Pckg.List;
   end record;

   type Completion_Proposal is abstract tagged record
      Mode             : Proposal_Mode := Show_Identifiers;
      Resolver         : Completion_Resolver_Access;
   end record;

   procedure Free_Proposal (Proposal : in out Completion_Proposal'Class);
   --  Used to instantiate the generic list (this is not actually doing
   --  anything)

   ---------------------
   -- Completion_List --
   ---------------------

   package Completion_List_Pckg is new Virtual_Lists
     (Completion_Proposal'Class, Free => Free_Proposal);

   package Completion_List_Extensive_Pckg is new
     Completion_List_Pckg.Extensive;

   type Completion_List is record
      List                : Completion_List_Pckg.Virtual_List;
      Is_Partial          : Boolean;
      Searched_Identifier : String_Access;
   end record;

   type Completion_Iterator is record
      It                  : Completion_List_Pckg.Virtual_List_Iterator;
      Is_Partial          : Boolean;
      Searched_Identifier : String_Access;
   end record;

   Null_Completion_List : constant Completion_List :=
     (Completion_List_Pckg.Null_Virtual_List, False, null);

   Null_Completion_Iterator : constant Completion_Iterator :=
     (It                  => Completion_List_Pckg.Null_Virtual_List_Iterator,
      Is_Partial          => False,
      Searched_Identifier => null);

   --------------------------------
   -- Simple_Completion_Proposal --
   --------------------------------

   type Simple_Completion_Proposal is new Completion_Proposal with record
      Name : String_Access;
   end record;

   function Get_Completion (Proposal : Simple_Completion_Proposal)
      return UTF8_String;
   --  See inherited documentation

   function Get_Category (Proposal : Simple_Completion_Proposal)
     return Language_Category;
   --  See inherited documentation

   procedure Get_Composition
     (Proposal   : Simple_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List);
   --  See inherited documentation

   function Get_Number_Of_Parameters (Proposal : Simple_Completion_Proposal)
     return Natural;
   --  See inherited documentation

   procedure Free (Proposal : in out Simple_Completion_Proposal);
   --  See inherited documentation

   function Match (Seeked_Name, Tested_Name : String; Is_Partial : Boolean)
      return Boolean;
   --  Return true if Tested_Name matches Seeked_Name, possibly only partially
   --  (in which case Seeked_Name is the beginning of Tested_Name), false
   --  otherwise

end Completion;
