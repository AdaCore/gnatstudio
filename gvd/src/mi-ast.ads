------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

--  This package declares the AST-like structure types.  The MI output is
--  record-based, which means that the result are packed into defined record
--  structures.  Types declared here stick to this representation to provide
--  the most natural interface for a GDB/MI user.  Higher level tools are
--  provided in this API to manipulate these types and extract relevant
--  information.  This package also provide the visitor interface to walk
--  through the AST.

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with Ada.Unchecked_Deallocation;

package MI.Ast is

   --  The AST is composed of the following hierarchy:
   --
   --  * MI_Value
   --    |__ String_Value
   --    |__ Result_List_Value
   --    \__ Value_List_Value
   --
   --  * MI_Record
   --    |__ Stream_Output_Record
   --    \__ Result_Record
   --
   --  * Result_Pair

   -----------------------------------
   -- Visitor interface declaration --
   -----------------------------------

   type Visitor is interface;
   --  Declaration of the Visitor abstract class.  Visitors must implement this
   --  interface to walk through the resulting AST after a MI output parsing.

   -----------------------------------
   -- Visitor interface declaration --
   -----------------------------------

   type Mutable_Visitor is interface;
   --  Declaration of the Visitor abstract class.  Visitors must implement this
   --  interface to walk through the resulting AST after a MI output parsing.

   -------------------------------------
   -- MI_Record interface declaration --
   -------------------------------------

   type MI_Record is abstract tagged null record;

   type MI_Record_Access is access all MI_Record'Class;
   --  MI_Record is an interface, and base class for all record extracted from
   --  the GDB/MI output, since this output is a list of records of different
   --  types.

   procedure Accept_Visitor
     (This : in out MI_Record;
      V    : in out Visitor'Class) is abstract;
   --  MI_Record abstract method to force implementation in all derived
   --  classes.

   procedure Accept_Visitor
     (This : in out MI_Record;
      V    : in out Mutable_Visitor'Class) is abstract;
   --  MI_Record abstract method to force implementation in all derived
   --  classes.

   package Record_Lists is new Doubly_Linked_Lists (MI_Record_Access);
   subtype Record_List is Record_Lists.List;
   --  Definition of a list of MI_Record_Access. Use of access type is
   --  mandatory since MI_Record is an abstract type.

   procedure Clear_MI_Record (Rec : MI_Record_Access);
   --  Frees the memory allocated for the record fields.

   procedure Free_MI_Record is
      new Ada.Unchecked_Deallocation (MI_Record'Class, MI_Record_Access);
   --  Frees the memory allocated for the given MI_Record but not the memory
   --  pointed by this very same access type. For this one needs to call
   --  Clear_MI_Record.

   procedure Clear_Record_List (List : in out Record_List);
   --  Frees the each element of the list before clearing the list itself.

   -------------------------------------------
   -- Stream_Output_Record type declaration --
   -------------------------------------------

   type Stream_Output_Record_Type is (Console, Target, Log);

   type Stream_Output_Record is new MI_Record with record
      Output_Type : Stream_Output_Record_Type;
      Content     : String_Access;
   end record;

   type Stream_Output_Record_Access is access all Stream_Output_Record'Class;
   --  Stream_Output_Record holds a GDB/MI stream output type (either console,
   --  target or log) and its associated message.  It derives from MI_Record
   --  interface and implements its abstract methods.

   overriding
   procedure Accept_Visitor
     (This : in out Stream_Output_Record;
      V    : in out Visitor'Class);
   --  MI_Record abstract method overriding implementation.  It simply
   --  automatically calls the appropriate overloaded Visit method from the
   --  visitor using type dispatching.

   overriding
   procedure Accept_Visitor
     (This : in out Stream_Output_Record;
      V    : in out Mutable_Visitor'Class);
   --  MI_Record abstract method overriding implementation.  It simply
   --  automatically calls the appropriate overloaded Visit method from the
   --  visitor using type dispatching.

   procedure Clear_Stream_Output_Record (Rec : in out Stream_Output_Record);
   --  Frees the memory allocated for the content of the given
   --  Stream_Output_Record.

   ------------------------------------
   -- MI_Value interface declaration --
   ------------------------------------

   type MI_Value is abstract tagged null record;

   type MI_Value_Access is access all MI_Value'Class;
   --  Declaration of the MI_Value interface.  In the MI output grammar, a
   --  `value' can be either a 'const', a 'tuple' or a 'list', which basically
   --  leads to either a string, a list of result or a list of value.  Because
   --  this type can take many forms and can be though as a recursive type, one
   --  way to handle it is to use an interface and its derived types.

   procedure Accept_Visitor
     (This : in out MI_Value;
      V    : in out Visitor'Class) is abstract;
   --  The same abstract method as defined in the MI_Record interface.  It has
   --  exactly the same goal, and is named identically for consistency
   --  purposes in visitors.

   procedure Accept_Visitor
     (This : in out MI_Value;
      V    : in out Mutable_Visitor'Class) is abstract;
   --  The same abstract method as defined in the MI_Record interface.  It has
   --  exactly the same goal, and is named identically for consistency
   --  purposes in visitors.

   package Value_Lists is new Doubly_Linked_Lists (MI_Value_Access);
   subtype Value_List is Value_Lists.List;
   --  Definition of a list of MI_Value_Access, using access type because
   --  MI_Value is an abstract type.

   procedure Clear_MI_Value (Value : MI_Value_Access);
   --  Frees the memory allocated for the value fields.

   procedure Free_MI_Value is
      new Ada.Unchecked_Deallocation (MI_Value'Class, MI_Value_Access);
   --  Frees the memory allocated for the given MI_Value but not the memory
   --  pointed by this very same access type. For this one needs to call
   --  Clear_MI_Value.

   procedure Clear_Value_List (List : in out Value_List);
   --  Frees the each element of the list before clearing the list itself.

   -----------------------------------
   -- Result_Pair type declaration  --
   -----------------------------------

   type Result_Pair is record
      Variable : String_Access   := null;
      Value    : MI_Value_Access := null;
   end record;
   --  A result pair is a record containing a pair (variable, value).  The
   --  variable is a string which represent the variable's name, and the value
   --  is of the MI_Value abstract type.

   overriding function "=" (Left, Right : Result_Pair) return Boolean;
   --  Equality function between two Result_Pair(s) need to instanciate the
   --  generic package Doubly_Linked_Lists with the Result_Pair type.

   package Result_Pair_Lists is new Doubly_Linked_Lists (Result_Pair);
   subtype Result_Pair_List is Result_Pair_Lists.List;
   --  Declaration of a Result_Pair list to hold several instance of a
   --  Result_Pair since a MI record comes generally with a bunch of result
   --  pairs.

   procedure Clear_Result_Pair (Pair : in out Result_Pair);
   --  Releases memory allocated for the given Result_Pair and reset its access
   --  types to the value null.

   procedure Clear_Result_Pair_List (List : in out Result_Pair_List);
   --  Releases memory allocated and stored in the given list, and clears this
   --  list.

   -----------------------------------
   -- String_Value type declaration --
   -----------------------------------

   type String_Value is new MI_Value with record
      Value : String_Access;
   end record;

   type String_Value_Access is access all String_Value;
   --  Declaration of the String_Value type, derivating from the MI_Value
   --  interface.  This type only holds one string, corresponding to the
   --  'const' rule of the MI output grammar.

   overriding
   procedure Accept_Visitor
     (This : in out String_Value;
      V    : in out Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   overriding
   procedure Accept_Visitor
     (This : in out String_Value;
      V    : in out Mutable_Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   procedure Clear_String_Value (Value : in out String_Value);
   --  Releases the memory allocated for the given String_Value and reset its
   --  access type to the value null.

   ----------------------------------------
   -- Result_List_Value type declaration --
   ----------------------------------------

   type Result_List_Value is new MI_Value with record
      Value : Result_Pair_List;
   end record;

   type Result_List_Value_Access is access all Result_List_Value;
   --  Declaration of the Result_List_Value type, implementing the MI_Value
   --  interface, and holding a list of Result_Pair(s).

   overriding
   procedure Accept_Visitor
     (This : in out Result_List_Value;
      V    : in out Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   overriding
   procedure Accept_Visitor
     (This : in out Result_List_Value;
      V    : in out Mutable_Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   procedure Clear_Result_List_Value (List : in out Result_List_Value);
   --  Releases the memory allocated for the given Result_List_Value.

   ---------------------------------------
   -- Value_List_Value type declaration --
   ---------------------------------------

   type Value_List_Value is new MI_Value with record
      Value : Value_List;
   end record;

   type Value_List_Value_Access is access all Value_List_Value;
   --  Declaration of the Value_List_Value type, which implements the MI_Value
   --  interface, and holds a list of Value(s).

   overriding
   procedure Accept_Visitor
     (This : in out Value_List_Value;
      V    : in out Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   overriding
   procedure Accept_Visitor
     (This : in out Value_List_Value;
      V    : in out Mutable_Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   procedure Clear_Value_List_Value (List : in out Value_List_Value);
   --  Releases the memory allocated for the given Value_List_Value.

   ------------------------------------
   -- Result_Record type declaration --
   ------------------------------------

   type Result_Record_Type is (Sync_Result, Async_Exec, Async_Status,
                               Async_Notify);

   type Result_Record is new MI_Record with record
      Token   : Integer := -1;
      R_Type  : Result_Record_Type := Sync_Result;
      Class   : String_Access := null;
      Results : Result_Pair_List;
   end record;

   type Result_Record_Access is access all Result_Record'Class;
   --  Declaration of a Result_Record type, which implements the MI_Record
   --  interface.  This type is one of the top level type in the MI output
   --  grammar and represent either a synchronous result record or an
   --  asynchronous result record.  It holds a token number, which is the Token
   --  value in the grammar if any specified, or -1 if none ; a result record
   --  type, which is one of the Result_Record_Type enumeration ; a class
   --  string which reprensent the result-class or async-class from the grammar
   --  ; and finally a list of result pairs.

   overriding
   procedure Accept_Visitor
     (This : in out Result_Record;
      V    : in out Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   overriding
   procedure Accept_Visitor
     (This : in out Result_Record;
      V    : in out Mutable_Visitor'Class);
   --  Implementaton of the MI_Value abstract method which basically only call
   --  the Visitor.Visit method on itself.

   procedure Clear_Result_Record (Rec : in out Result_Record);
   --  Releases the memory allocated for the given Result_Record.

   ---------------------------------------------------
   -- Visitor interface abstract method declaration --
   ---------------------------------------------------

   procedure Visit
     (This   : in out Visitor;
      Object : Record_List) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : Result_List_Value'Class) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : Result_Pair) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : Result_Record'Class) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : Stream_Output_Record'Class) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : String_Value'Class) is abstract;

   procedure Visit
     (This   : in out Visitor;
      Object : Value_List_Value'Class) is abstract;

   -----------------------------------------------------------
   -- Mutable_Visitor interface abstract method declaration --
   -----------------------------------------------------------

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Record_List) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Result_List_Value'Class) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Result_Pair) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Result_Record'Class) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Stream_Output_Record'Class) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out String_Value'Class) is abstract;

   procedure Visit
     (This   : in out Mutable_Visitor;
      Object : in out Value_List_Value'Class) is abstract;

end MI.Ast;
