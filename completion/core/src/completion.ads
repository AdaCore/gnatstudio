------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  Provides the base structures / subprograms for the completion manager. A
--  completion manager is the base class of the completion mechanism. Before
--  using it, you have to add manually the completion resolvers you want to
--  use. A completion manager has to be build at each completion request, since
--  it might depend on different datas. You can see for example the subprogram
--  Full_Test in the test driver Completion.Test.

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

with GNAT.Strings; use GNAT.Strings;
with Basic_Types;  use Basic_Types;
with Language;     use Language;
with Virtual_Lists;
with Virtual_Lists.Extensive;

with GNATCOLL.VFS;     use GNATCOLL.VFS;
with Xref;

private with GPS_Vectors;

package Completion is

   type Completion_Proposal;
   --  This is the type of a proposal.

   type Completion_Proposal_Access is access all Completion_Proposal'Class;

   procedure Free (This : in out Completion_Proposal_Access);

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

   ------------------------
   -- Completion_Context --
   ------------------------

   type Completion_Context is private;
   --  A context holds data used by the completion engine to precise the
   --  completion. It holds at least the buffer and the offset of the
   --  completion, but some completion engines might want to attach specific
   --  data to it.

   procedure Free (Context : in out Completion_Context);
   --  Free data associated to the context given in parameter.

   function Get_Buffer
     (Context : Completion_Context) return String_Access;
   --  Return the buffer associated to this context.

   function Get_Completion_Offset
     (Context : Completion_Context) return String_Index_Type;
   --  Return the offset associated to this context.

   function Get_File
     (Context : Completion_Context) return GNATCOLL.VFS.Virtual_File;
   --  Return the file associated with this context

   -------------------
   -- Completion_Id --
   -------------------

   type Completion_Id (Id_Length : Integer) is record
      Resolver_Id : String (1 .. 8);
      --  This id identifies in an unique way a resolver. Users are responsible
      --  of avoiding name clashes.

      Id          : String (1 .. Id_Length);
      File        : Virtual_File := No_File;
      Line        : Natural := 0;
      Column      : Natural := 0;
   end record;
   --  This type is used to store a completion proposal in an long time basis -
   --  while the Completion_Proposal lifecycle is bound to its resolver one. A
   --  resolver is supposed to be able to retreive a proposal based on an id,
   --  if the proposal is still valid.

   function "<" (Left, Right : Completion_Id) return Boolean;
   --  Arbitrary comparison between two completion ids.

   overriding function "=" (Left, Right : Completion_Id) return Boolean;
   --  Return true if the two ids are equals.

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

   procedure Get_Completion_Root
     (Resolver : access Completion_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List) is abstract;
   --  Starts a completion, looking from the offset given in parameter.
   --  Offset should be the offset of the place from where we try to find the
   --  corresponding identifier, in bytes, and visiblity will be calculated
   --  for this offset. If offset is lower than zero, it means that the
   --  completion is done from the very begining of the file, and therefore
   --  nothing from the file should be extracted. The completions returned by
   --  this procedure are either resolved, or need to be expanded by an other
   --  completion resolver.

   procedure Free (Resolver : in out Completion_Resolver) is abstract;
   --  Free the data of a Resolver.

   function Get_Id (Resolver : Completion_Resolver) return String is abstract;
   --  Return a unique ID corresponing to this resolver.

   ------------------------
   -- Completion_Manager --
   ------------------------

   type Completion_Manager is abstract tagged private;
   --  A completion manager is a type holding a list of completions resolvers.
   --  Resolvers will be called in the order they are referenced. This object
   --  also holds a couple of general datas, such as the buffer from where the
   --  completion is done.

   type Completion_Manager_Access is access all Completion_Manager'Class;

   procedure Free (This : in out Completion_Manager_Access);
   --  Free the memory associated to a completion manager access. This does not
   --  free the referenced resolvers which have to be freed separately.

   procedure Register_Resolver
     (Manager  : access Completion_Manager;
      Resolver : access Completion_Resolver'Class);
   --  Add a resolver to this manager. A given resolver can only be added in
   --  one manager (it knows its manager). Resolvers will be called in the
   --  order that they have been registred.

   function Create_Context
     (Manager : access Completion_Manager;
      File    : GNATCOLL.VFS.Virtual_File;
      Buffer  : String_Access;
      Lang    : Language_Access;
      Offset  : String_Index_Type) return Completion_Context;
   --  Creates a new context for this manager, with the offset and the buffer
   --  given in parameter.

   function Get_Resolver
     (Manager : access Completion_Manager;
      Name    : String) return Completion_Resolver_Access;
   --  Return the resolver registered in the manager of the given name.

   -------------------------
   -- Completion_Proposal --
   -------------------------

   type Completion_Proposal is abstract tagged record
      Resolver : access Completion_Resolver'Class;
   end record;

   type File_Location is record
      File_Path : aliased Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
   end record;

   Null_File_Location : constant File_Location;

   Null_Completion_Proposal : constant Completion_Proposal'Class;

   function Get_Resolver
     (Proposal : Completion_Proposal) return Completion_Resolver_Access;
   --  Returns the resolver that have been used to create this proposal.

   function Get_Completion
     (Proposal : Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String is abstract;
   --  Return the text that has to be used for the completion, may be different
   --  from the label.

   function Get_Label
     (Proposal : Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String;
   --  Return the label of the completion proposal. By defaut, return the
   --  completion

   function Is_Accessible
     (Proposal : Completion_Proposal)
      return Boolean
   is (True);
   --  Returns True if the completion is accessible in the current unit.

   function Get_Id
     (Proposal : Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String;
   --  Return the identifier of the entity referenced in the proposal. This
   --  identifier can be different from the completion propsed and the label.
   --  By default, return the completion.

   function Get_Caret_Offset
     (Proposal : Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Basic_Types.Character_Offset_Type;
   --  Return the offset where the editor caret is supposed to be after the
   --  completion. In the default implementation, it's always moved at the
   --  end of the inserted text.

   function Get_Location
     (Proposal : Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return File_Location;
   --  Return the location of the object pointed by the given proposal, null
   --  if none. By default, return Null_Location.

   function Get_Category
     (Proposal : Completion_Proposal) return Language_Category is abstract;
   --  Return the category of the object proposed for the completion

   function Get_Visibility
     (Proposal : Completion_Proposal) return Construct_Visibility is abstract;
   --  Return the visibility of the object proposed for completion

   function Get_Action_Name
     (Proposal : Completion_Proposal)
      return String;
   --  Return the action name associated with the completion proposal
   --  The action will be used as a substitute for the completion text
   --  when it is supplied.
   --  WARNING : This is a GPS specific feature for the moment
   --  The default implementation returns the empty string.

   function Get_Custom_Icon_Name
     (Proposal : Completion_Proposal)
      return String is ("");
   --  If the completion needs to display a custom icon, this will
   --  return its name

   function Get_Documentation
     (Proposal : Completion_Proposal)
      return String is abstract;
   --  Return custom documentation associated with this proposal

   function Is_Valid (Proposal : Completion_Proposal) return Boolean;
   --  Return true if the proposal should be accessible by the user. By
   --  default, this is always true. Unvalid proposal are automatically skipped
   --  by the Next & First subprogram. However, an unvalid completion can be
   --  returned by an iterator if changes are made between two iterations.
   --  Users using such a behavior have to ensure that the iterator result of
   --  the iterator is still valid after such a modification. A call to next
   --  on the iterator will make the completion valid again.

   function Get_Initial_Completion_List
     (Manager : access Completion_Manager; Context : Completion_Context)
      return Completion_List is abstract;
   --  Generates an initial completion list, for the cursor pointing at the
   --  given offset. This operation is time consuming, so it would be good
   --  to use the one below afterwards, until the completion process is done.

   function Match
     (Proposal   : Completion_Proposal;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean is abstract;
   --  Return true if the proposal given in parameter matches the completion
   --  search parameters given, false otherwise.

   function To_Completion_Id
     (Proposal : Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id is abstract;
   --  Creates a completion id able to retreive this completion proposal later
   --  on.
   --  WARNING : This completion Id is used to check a proposal's identity
   --  This means that if two proposals have the same Id, only one will show
   --  up in the completion window

   procedure Free (Proposal : in out Completion_Proposal) is abstract;
   --  Free the memory associated to the proposal.

   function Deep_Copy
     (Proposal : Completion_Proposal)
      return Completion_Proposal'Class is abstract;
   --  Make a deep copy of Proposal. Result should be freed by the caller.

   -------------------------
   -- Completion_Iterator --
   -------------------------

   type Completion_Iterator is private;
   --  This type is used to iterate over the various possibilities of a
   --  completion.

   function First
     (This : Completion_List;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Completion_Iterator;
   --  Return the first proposal of the completion list.

   procedure Next
     (This : in out Completion_Iterator;
      Db   : access Xref.General_Xref_Database_Record'Class);
   --  Gets the next proposal of the completion list.

   function Get_Proposal
     (This : Completion_Iterator) return Completion_Proposal'Class;
   --  Return the actual proposal for the given iterator.
   --  The returned value should NOT be freed by the user, and not be stored.
   --  If you want to store a proposal, make a Deep_Copy of it.

   procedure Free (This : in out Completion_Iterator);
   --  Free the data associated to a completion iterator.

   function At_End (This : Completion_Iterator) return Boolean;
   --  Return true if the iterator is after the last element of its list.

   function Is_Valid (It : Completion_Iterator) return Boolean;
   --  Return true if the iterator should be used by the user, false otherwise.
   --  iterators returned by Next and First subprograms are always valid (even
   --  if, in the case of Next, the iterator given in parameter is not). A
   --  valid iterator can be invalidated if the iterated structure changes
   --  during the iteration.

   Null_Completion_Iterator : constant Completion_Iterator;
   --  Default value for an empty iterator.

private

   type Completion_Context_Record is tagged record
      Buffer : String_Access;
      --  Buffer.all should be encoded in UTF8.

      Offset : String_Index_Type;
      Lang   : Language_Access;
      File   : GNATCOLL.VFS.Virtual_File;
   end record;

   type Completion_Context is access all Completion_Context_Record'Class;

   procedure Free (Context : in out Completion_Context_Record);

   type Completion_Resolver is abstract tagged record
      Manager : Completion_Manager_Access;
   end record;

   package Completion_Resolver_Map_Pckg is new
     Ada.Containers.Indefinite_Ordered_Maps
       (String, Completion_Resolver_Access);

   package Completion_Resolver_List_Pckg is new
     Ada.Containers.Doubly_Linked_Lists (Completion_Resolver_Access);

   use Completion_Resolver_Map_Pckg;
   use Completion_Resolver_List_Pckg;

   package Context_List_Pckg is new GPS_Vectors (Completion_Context);

   use Context_List_Pckg;

   type Completion_Manager is abstract tagged record
      Resolvers         : Completion_Resolver_Map_Pckg.Map;
      Ordered_Resolvers : Completion_Resolver_List_Pckg.List;
      Contexts          : Context_List_Pckg.Vector;
   end record;

   Null_File_Location : constant File_Location := (No_File, 0, 0);

   procedure Free_Proposal (Proposal : in out Completion_Proposal'Class);
   --  Used to instantiate the generic list (this is not actually doing
   --  anything)

   ---------------------
   -- Completion_List --
   ---------------------

   package Completion_List_Pckg is new Virtual_Lists
     (Completion_Proposal'Class);

   package Completion_List_Extensive_Pckg is new
     Completion_List_Pckg.Extensive (Free => Free_Proposal);

   type Completion_List is record
      List                : Completion_List_Pckg.Virtual_List;
      Searched_Identifier : String_Access;
   end record;

   package Completion_Id_Set is new
     Ada.Containers.Indefinite_Ordered_Sets (Completion_Id);

   use Completion_Id_Set;

   type Completion_Iterator is record
      It : Completion_List_Pckg.Virtual_List_Iterator;
      Already_Extracted : Completion_Id_Set.Set;
   end record;

   Null_Completion_List : constant Completion_List :=
     (Completion_List_Pckg.Null_Virtual_List, null);

   Null_Completion_Iterator : constant Completion_Iterator :=
     (It => Completion_List_Pckg.Null_Virtual_List_Iterator, others => <>);

   --------------------------------
   -- Simple_Completion_Proposal --
   --------------------------------

   type Simple_Completion_Proposal is new Completion_Proposal with record
      Name          : String_Access;
      Category      : Language_Category := Cat_Unknown;
   end record;

   overriding function Get_Completion
     (Proposal : Simple_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return UTF8_String;
   --  See inherited documentation

   overriding function Get_Category
     (Proposal : Simple_Completion_Proposal) return Language_Category;
   --  See inherited documentation

   overriding function Get_Visibility
     (Proposal : Simple_Completion_Proposal) return Construct_Visibility;
   --  See inherited documentation

   overriding function Get_Documentation
     (Proposal : Simple_Completion_Proposal) return String is ("");

   overriding function Match
     (Proposal : Simple_Completion_Proposal;
      Context  : Completion_Context;
      Offset   : String_Index_Type) return Boolean;
   --  See inherited documentation

   overriding function To_Completion_Id
     (Proposal : Simple_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id;
   --  See inherited documentation

   overriding procedure Free (Proposal : in out Simple_Completion_Proposal);
   --  See inherited documentation

   overriding function Deep_Copy
     (Proposal : Simple_Completion_Proposal)
      return Completion_Proposal'Class;
   --  See inherited documentation

   function Match
     (Seeked_Name, Tested_Name : String; Is_Partial : Boolean) return Boolean;
   --  Return true if Tested_Name matches Seeked_Name, possibly only partially
   --  (in which case Seeked_Name is the beginning of Tested_Name), false
   --  otherwise

   Null_Completion_Proposal : constant Completion_Proposal'Class :=
                                Simple_Completion_Proposal'
                                  (Resolver => null,
                                   Name     => null,
                                   Category => Cat_Unknown);

end Completion;
