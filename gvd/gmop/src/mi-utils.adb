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

package body MI.Utils is

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Result : Result_Record) return Boolean is
   begin
      return Result.Class.all = "Error";
   end Is_Error;

   -------------------
   -- Process_Error --
   -------------------

   function Process_Error (Result : Result_Record) return String_Access is
   begin
      raise Not_Yet_Implemented_Error with "Process_Error";
      return null;
   end Process_Error;

   -------------------
   -- Process_Async --
   -------------------

   function Process_Async (Result : Result_Record) return Notification_Type is
      Notification : Notification_Type;
   begin
      raise Not_Yet_Implemented_Error with "Process_Async";
      return Notification;
   end Process_Async;

   ----------------------
   -- Process_Exec_run --
   ----------------------

   function Process_Exec_Run (Result : Result_Record) return Boolean is
   begin
      raise Not_Yet_Implemented_Error with "Process_Exec_Run";
      return False;
   end Process_Exec_Run;

   -------------------
   -- Process_Break --
   -------------------

   function Process_Break (Result : Result_Record) return Breakpoint_Type is
      Breakpoint : Breakpoint_Type;
      pragma Unreferenced (Result);
   begin
      raise Not_Yet_Implemented_Error with "Process_Break";
      return Breakpoint;
   end Process_Break;

   ------------------------
   -- Process_Break_List --
   ------------------------

   function Process_Break_List
     (Result : Result_Record) return Breakpoint_List
   is
      Breakpoints : Breakpoint_List;
      pragma Unreferenced (Result);
   begin
      raise Not_Yet_Implemented_Error with "Process_Break_List";
      return Breakpoints;
   end Process_Break_List;

   ---------------------------
   -- Process_Stream_Output --
   ---------------------------

   function Process_Stream_Output
     (Stream : Stream_Output_Record) return String_Access is
   begin
      raise Not_Yet_Implemented_Error with "Process_Stream_Output";
      return null;
   end Process_Stream_Output;

   ------------------------
   -- Process_Var_Create --
   ------------------------

   function Process_Var_Create
     (Result : Result_Record) return Var_Obj_Access is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Create";
      return null;
   end Process_Var_Create;

   ------------------------
   -- Process_Var_Delete --
   ------------------------

   function Process_Var_Delete (Result : Result_Record) return Boolean is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Delete";
      return False;
   end Process_Var_Delete;

   ----------------------------
   -- Process_Var_Set_Format --
   ----------------------------

   procedure Process_Var_Set_Format
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Set_Format";
   end Process_Var_Set_Format;

   -----------------------------------
   -- Process_Var_Info_Num_Children --
   -----------------------------------

   procedure Process_Var_Info_Num_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Info_Num_Children";
   end Process_Var_Info_Num_Children;

   -------------------------------
   -- Process_Var_List_Children --
   -------------------------------

   procedure Process_Var_List_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_List_Children";
   end Process_Var_List_Children;

   ---------------------------
   -- Process_Var_Info_Type --
   ---------------------------

   procedure Process_Var_Info_Type
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Info_Type";
   end Process_Var_Info_Type;

   ---------------------------------
   -- Process_Var_Info_Expression --
   ---------------------------------

   procedure Process_Var_Info_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Info_Expression";
   end Process_Var_Info_Expression;

   --------------------------------------
   -- Process_Var_Info_Path_Expression --
   --------------------------------------

   procedure Process_Var_Info_Path_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Info_Path_Expression";
   end Process_Var_Info_Path_Expression;

   --------------------------------
   -- Process_Var_Show_Attribute --
   --------------------------------

   procedure Process_Var_Show_Attribute
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Access) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Show_Attribute";
   end Process_Var_Show_Attribute;

   -------------------------------------
   -- Process_Var_Evaluate_Expression --
   -------------------------------------

   procedure Process_Var_Evaluate_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Access) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Evaluate_Expression";
   end Process_Var_Evaluate_Expression;

   ------------------------
   -- Process_Var_Assign --
   ------------------------

   procedure Process_Var_Assign
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Assign";
   end Process_Var_Assign;

   ------------------------
   -- Process_Var_Update --
   ------------------------

   procedure Process_Var_Update
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Update";
   end Process_Var_Update;

   ----------------------------
   -- Process_Var_Set_Frozen --
   ----------------------------

   procedure Process_Var_Set_Frozen
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Set_Frozen";
   end Process_Var_Set_Frozen;

   ----------------------------------
   -- Process_Var_Set_Update_Range --
   ----------------------------------

   procedure Process_Var_Set_Update_Range
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Set_Update_Range";
   end Process_Var_Set_Update_Range;

   ----------------------------
   -- Process_Var_Visualizer --
   ----------------------------

   procedure Process_Var_Visualizer
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Visualizer";
   end Process_Var_Visualizer;

end MI.Utils;
