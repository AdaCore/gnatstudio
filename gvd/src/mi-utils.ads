------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  This package is a collection of functions which purpose is to extract
--  relevant information from an MI response given the query.

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with MI.Ast;                             use MI.Ast;

package MI.Utils is

   Utils_Error : exception;
   --  The error type which is raised on fatal errors

   Not_Yet_Implemented_Error : exception;
   --  Exception raised when calling a routine that is not yet implemented

   Result_Not_Found : exception;
   --  Exception raised when no result was found

   Multiple_Results_Found : exception;
   --  Exception raised when more than one result was found

   type Var_Obj_Type;
   --  Forward declaration of the Var_Obj_Type to declare an access to it

   type Var_Obj_Access is access all Var_Obj_Type;
   --  Access to a Var_Obj used by the Var_Obj_List doubly linked list

   package Var_Obj_Lists is new Doubly_Linked_Lists (Var_Obj_Access);
   subtype Var_Obj_List is Var_Obj_Lists.List;
   --  Declaration of the type doubly linked list of access to Var_Obj_Type

   package String_Lists is new Doubly_Linked_Lists (String_Access);
   subtype String_List is String_Lists.List;
   --  Declaration of the type doubly linked list of access to String

   type Var_Obj_Type is record
      Name             : String_Access  := null;
      Expression       : String_Access  := null;
      Path_Exp         : String_Access  := null;
      Num_Child        : Natural        := 0;
      Value            : String_Access  := null;
      Format           : String_Access  := null;
      Type_Desc        : String_Access  := null;
      Has_More         : Natural        := 0;
      Children         : Var_Obj_List;
      Children_Fetched : Boolean        := False;
   end record;
   --  Record containing information about a variable object.  Informations
   --  about this varobj can be fetch from the GDB process through several call
   --  to this API.  These calls fill the structure accordingly to the
   --  responses received from GDB.  This record is intended to be a
   --  representation of the GDB varobj structure on GPS side.  Commands sent
   --  to GDB should ensure the synchronisation between this type and the GDB
   --  internal representation.

   type Frame_Type is record
      Address       : String_Access := null;
      Function_Name : String_Access := null;
      Args          : String_List;
      File_Name     : String_Access := null;
      File_Fullname : String_Access := null;
      Line          : Natural       := 0;  -- 0 here means no value
   end record;
   --  As for Var_Obj_Type, Frame_Type is a structure intended to be the
   --  GPS-side representation of a GDB internal frame structure.  It is
   --  supposedly kept synchronized with the GDB process using the information
   --  provided by GDB's MI mode and output.

   type Breakpoint_Type is record
      Number            : Natural       := 0;
      Type_Desc         : String_Access := null;
      Disp              : String_Access := null;
      Enabled           : Boolean       := False;
      Frame             : Frame_Type;
      Times             : Natural       := 0;
      Original_Location : String_Access := null;
   end record;
   --  Ditto the Var_Obj_Type and Frame_Type records.  GDB's MI output refers
   --  to breakpoints by specifying many "attributes" which are stored in GPS
   --  using this structure.  As for the two types mentionned before, a
   --  Breakpoint_Type should be kept synchronized along the debugging session.

   package Breakpoint_Lists is new Doubly_Linked_Lists (Breakpoint_Type);
   subtype Breakpoint_List is Breakpoint_Lists.List;
   --  Declaration of a list of Breakpoint_Type

   type Notification_Type is record
      null;  -- ???
   end record;

   -------------------------------
   -- Memory management helpers --
   -------------------------------

   procedure Clear_Var_Obj (Var_Obj : in out Var_Obj_Type);
   --  Releases the memory allocated by a given Var_Obj_Type object, i.e.
   --  frees inner structures.

   procedure Free_Var_Obj (Var_Obj : in out Var_Obj_Access);
   --  Releases the memory allocated by and for a given Var_Obj_Type object,
   --  i.e. frees both inner and main structures.

   procedure Clear_Var_Obj_List (List : in out Var_Obj_List);
   --  Clear the list by freeing every element before calling the List.Clear
   --  method.

   procedure Clear_String_List (List : in out String_List);
   --  Clear the list by freeing every element before calling the List.Clear
   --  method.

   procedure Clear_Breakpoint (Breakpoint : in out Breakpoint_Type);
   --  Releases the memory allocated by a given Breakpoint_Type object, i.e.
   --  frees inner structures.

   procedure Clear_Breakpoint_List (Bkpts : in out Breakpoint_List);
   --  Clear each element of the list, then the list itself.

   procedure Clear_Frame_Type (Frame : in out Frame_Type);
   --  Releases the memory allocated by a given Frame_Type object, i.e.  frees
   --  inner structures.

   --  Note about this package's memory management policy: The following
   --  subprograms work on the result of the MI scanner/parser.  For commodity
   --  reasons, they always return new elements which means that both the
   --  scanner/parser structures and these results need to be freed by the
   --  user.  This is easier to handle, especially in error cases.  To be
   --  perfectly clear: None of the below subprograms modifies/frees the
   --  scanner/parser structure, and they return newly allocated structures
   --  that can be freed with the corresponding procedures defined above
   --  (Clear_*, Free_*, ...).  The only frees this package implicitly do is
   --  when updating a given structure, e.g. Process_Var_Info_Type which frees
   --  the previous corresponding Var_Obj.Type_Desc field if it differs from
   --  null.
   --
   --  As a result, this allows the user to purge the scanner/parser structures
   --  once it has finished working with them, and safely keep the top-level
   --  structures defined in this package.  Structures defined here are fully
   --  decoupled from the ones resulting from the scanner/parser. They can be
   --  manipulated freely and can have different life time.

   --------------------
   -- Error handlers --
   --------------------

   function Is_Error (Result : Result_Record) return Boolean;
   --  Returns whether or not a given Result_Record is holding an error record
   --  from GDB.  Error record are represented, in MI mode, by the following
   --  string:
   --    ^error,attr=...

   function Process_Error (Result : Result_Record) return String_Access;
   --  This function processes a result record corresponding to an error,
   --  extracts its associated message, and returns a copy of it.  The user is
   --  responsible for freeing the returned access type.

   ---------------------------------
   -- Async notification handlers --
   ---------------------------------

   function Process_Async (Result : Result_Record) return Notification_Type;
   --  ??? This function handles an async record and returns a
   --  Notification_Type.
   --  An async record can be one of the following (in MI representation):
   --    *async-class,attr=...
   --    =async-class,attr=...
   --    +async-class,attr=...

   function Process_Stream_Output
     (Stream : Stream_Output_Record) return String_Access;
   --  Handles an asynchronous stream-output-record, which basically only
   --  contains one important information: its message.  This function extracts
   --  and returns that message.  A stream-output-record can be any one of the
   --  following:
   --    ~"the message"
   --    &"the message"
   --    @"the message"
   --
   --  This subprogram returns a newly allocated String which contains the
   --  stream message.  It is the user concern to free this result.

   --------------------------------
   -- Sync notification handlers --
   --------------------------------

   function Process_Exec_Run (Result : Result_Record) return Boolean;
   --  This function handles the synchronous result-record received after a
   --  call to the GDB/MI command `-exec-run'.  It returns whether or not the
   --  command was successfully executed.

   ----------------------------
   -- Break command handlers --
   ----------------------------

   procedure Process_Breakpoint_Type
     (Result     : Result_Pair;
      Breakpoint : out Breakpoint_Type);
   --  This subprogram processes a fragment of record of the form:
   --       bkpt={number="1",type="breakpoint",...
   --
   --  This procedure allocates the necessary memory for the Breakpoint_Type
   --  field, and it is therefore the user responsibility to use the
   --  Clear_Breakpoint routine to avoid leaks.  On error, Breakpoint is
   --  returned empty.

   function Process_Break_Insert
     (Result : Result_Record) return Breakpoint_Type;
   --  This function processes a synchronous result-record received after a
   --  break instruction.  It returns a Breakpoint_Type, which is the internal
   --  representation of a breakpoint.
   --
   --  This procedure allocates the necessary memory for the Breakpoint_Type
   --  field, and it is therefore the user responsibility to use the
   --  Clear_Breakpoint routine to avoid leaks.  On error, Breakpoint is
   --  returned empty.

   function Process_Break_List (Result : Result_Record) return Breakpoint_List;
   --  This function handles the result-record received after a call to the
   --  GDB/MI command `-break-list'.  It returns the list of breakpoints that
   --  returned GDB.
   --  On error, the returned list may contains one or more breakpoints.  The
   --  list is not cleared and it is the user concern to free it after use.

   --------------------------------------
   -- Variable object command handlers --
   --------------------------------------

   function Process_Var_Create (Result : Result_Record) return Var_Obj_Access;
   --  Returns a pointer so that it can be used recursively and the result can
   --  be appened to an other varobj children list.

   function Process_Var_Delete (Result : Result_Record) return Natural;
   --  Handles the result of the MI command `-var-delete'.  Returns the content
   --  of the ndelete attribute.

   procedure Process_Var_Set_Format
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-set-format' and updates the
   --  Var_Obj_Type accordingly.

   procedure Process_Var_Info_Num_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-info-num-children' and
   --  updates the Var_Obj_Type accordingly.

   procedure Process_Var_List_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-list-children' and updates
   --  the Var_Obj_Type accordingly.

   procedure Process_Var_Info_Type
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-info-type' and updates the
   --  Var_Obj_Type accordingly.

   procedure Process_Var_Info_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-info-expression' and updates
   --  the Var_Obj_Type accordingly.

   procedure Process_Var_Info_Path_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-info-path-expression' and
   --  updates the Var_Obj_Type accordingly.

   procedure Process_Var_Show_Attribute
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-show-attribute' and updates
   --  the Var_Obj_Type accordingly.

   procedure Process_Var_Evaluate_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-evaluate-expression' and
   --  updates the Var_Obj_Type accordingly.

   procedure Process_Var_Assign
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-assign' and updates the
   --  Var_Obj_Type accordingly.

   procedure Process_Var_Update
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-update' and updates the
   --  Var_Obj_Type accordingly.

   procedure Process_Var_Set_Frozen (Result  : Result_Record);
   --  Handles the result of the MI command `-var-set-frozen' and checks its
   --  correctness.

   procedure Process_Var_Set_Update_Range
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-set-update-range' and updates
   --  the Var_Obj_Type accordingly.

   procedure Process_Var_Visualizer
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-visualizer' and updates the
   --  Var_Obj_Type accordingly.

   function Get_Stream_Content
     (Records : Record_List;
      Stream_Type : Stream_Output_Record_Type) return String_List;
   --  return from the records the content of stream of requested type

   function Get_Stream_Content
     (Records : Record_List;
      Stream_Type : Stream_Output_Record_Type) return String;
   --  return from the records the content of stream of requested type

   function Get_Result_Record
     (Records : Record_List) return Result_Record;
   --  return from records the first result record found

   function Parse (Input : String) return Record_List;
   --  parse the GDB/MI stream

   function Get_Stream_Content
     (Input : String;
      Stream_Type : Stream_Output_Record_Type)
      return String_List;
   --  return from GDB/MI content the content of stream of requested type

end MI.Utils;
