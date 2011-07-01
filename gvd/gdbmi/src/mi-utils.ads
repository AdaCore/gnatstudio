------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
--                                                                          --
-- GPS is free software;  you can  redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
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
   subtype Breakpoint_List is String_Lists.List;
   --  Declaration of a list of Breakpoint_Type

   type Notification_Type is record
      null;  -- ???
   end record;

   --------------------
   -- Error handlers --
   --------------------

   function Is_Error (Result : Result_Record) return Boolean;
   --  Returns whether or not a given Result_Record is holding an error record
   --  from GDB.  Error record are represented, in MI mode, by the following
   --  string:
   --    ^error,attr=...

   function Process_Error (Result : Result_Record) return String_Access;
   --  ??? This function handles an error record.

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

   function Process_Break_Insert
     (Result : Result_Record) return Breakpoint_Type;
   --  This function processes a synchronous result-record received after a
   --  break instruction.  It returns a Breakpoint_Type, which is the internal
   --  representation of a breakpoint.

   function Process_Break_List (Result : Result_Record) return Breakpoint_List;
   --  This function handles the result-record received after a call to the
   --  GDB/MI command `-break-list'.  It returns the list of breakpoints that
   --  returned GDB.

   --------------------------------------
   -- Variable object command handlers --
   --------------------------------------

   function Process_Var_Create (Result : Result_Record) return Var_Obj_Access;
   --  Returns a pointer so that it can be used recursively and the result can
   --  be appened to an other varobj children list.

   function Process_Var_Delete (Result : Result_Record) return Boolean;
   --  Handles the result of the MI command `-var-delete'.  Returns whether the
   --  deletion was successful or not.

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

   procedure Process_Var_Set_Frozen
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type);
   --  Handles the result of the MI command `-var-set-frozen' and updates the
   --  Var_Obj_Type accordingly.

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

end MI.Utils;
