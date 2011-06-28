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

with Ada.Unchecked_Deallocation;

package body MI.Utils is

   procedure Check_Is_String_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class);
   --  Checks that the given MI_Value is a String_Value and raise an exception
   --  if the assertion if False.

   ----------------------------------
   -- Check_Is_String_Value_Or_Die --
   ----------------------------------

   procedure Check_Is_String_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class) is
   begin
      if Value not in String_Value then
         raise Utils_Error with ("Expected attribute `" & Name & "' to be a "
                                 & "c-string");
      end if;
   end Check_Is_String_Value_Or_Die;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Result : Result_Record) return Boolean is
   begin
      return Result.Class.all = "error";
   end Is_Error;

   -------------------
   -- Process_Error --
   -------------------

   function Process_Error (Result : Result_Record) return String_Access
   is
      Cursor : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair   : Result_Pair;
   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "error" then
         return null;
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted error result record: expected "
                                 & "one and only one attribute 'msg'.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "msg"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted error result record: expected "
                                 & "attribute `msg' followed by a c-string.");
      end if;

      return String_Value (Pair.Value.all).Value;
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
      if Result.R_Type /= Sync_Result
         or else Result.Class.all /= "running" then
         return False;
      end if;

      if Result.Results.Length /= 0 then
         raise Utils_Error with "unexpected attribute to result-record";
      end if;

      return True;
   end Process_Exec_Run;

   -------------------
   -- Process_Break --
   -------------------

   function Process_Break_Insert
     (Result     : Result_Record) return Breakpoint_Type
   is
      Cursor     : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair       : Result_Pair;
      Breakpoint : Breakpoint_Type;

   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-break-insert");
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one attribute: bkpt");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "bkpt"
         or else Pair.Value.all not in Result_List_Value then
         raise Utils_Error with "Ill-formated `bkpt' attribute";
      end if;

      Cursor := Result_List_Value (Pair.Value.all).Value.First;

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Cursor := Result_Pair_Lists.Next (Cursor);

         if Pair.Variable.all = "number" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Number := Natural'Value (
               String_Value (Pair.Value.all).Value.all
            );

         elsif Pair.Variable.all = "type" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Type_Desc := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "disp" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Disp := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "enabled" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

            if String_Value (Pair.Value.all).Value.all = "y" then
               Breakpoint.Enabled := True;
            else
               Breakpoint.Enabled := False;
            end if;

         elsif Pair.Variable.all = "addr" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Frame.Address := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "func" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Frame.Function_Name := String_Value
                                                (Pair.Value.all).Value;

         elsif Pair.Variable.all = "file" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Frame.File_Name := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "fullname" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Frame.File_Fullname := String_Value
                                                (Pair.Value.all).Value;

         elsif Pair.Variable.all = "line" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Frame.Line := Natural'Value
                                       (String_Value
                                          (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "times" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Times := Natural'Value
                                  (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "original-location" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Original_Location := String_Value
                                              (Pair.Value.all).Value;

         else
            raise Utils_Error with ("Unexpected attribute: "
                                    & Pair.Variable.all);
         end if;
      end loop;

      return Breakpoint;
   end Process_Break_Insert;

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
      return Stream.Content;
   end Process_Stream_Output;

   ------------------------
   -- Process_Var_Create --
   ------------------------

   function Process_Var_Create
     (Result : Result_Record) return Var_Obj_Access
   is
      Cursor  : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;
      Var_Obj : Var_Obj_Access := null;

   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-create");
      end if;

      if Result.Results.Length /= 5 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "five attributes: name, numchild, value, "
                                 & "type and has_more.");
      end if;

      Var_Obj := new Var_Obj_Type;

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Cursor := Result_Pair_Lists.Next (Cursor);

         if Pair.Variable.all = "name" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Name := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "numchild" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Num_Child := Natural'Value (
               String_Value (Pair.Value.all).Value.all
            );

         elsif Pair.Variable.all = "value" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Value := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "type" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Type_Desc := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "has_more" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Has_More := Natural'Value (
               String_Value (Pair.Value.all).Value.all
            );

         else
            raise Utils_Error with ("Ill-formatted done result record: "
                                    & "expected attribute `name', `numchild', "
                                    & "`value', `type' or `has_more'.");
         end if;
      end loop;

      return Var_Obj;
   end Process_Var_Create;

   ------------------------
   -- Process_Var_Delete --
   ------------------------

   function Process_Var_Delete (Result : Result_Record) return Boolean
   is
      Cursor : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair   : Result_Pair;
   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         return False;
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one and only one attribute: ndelete.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "ndelete"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "attribute `ndelete' followed by a "
                                 & "c-string.");
      end if;

      return True;  -- ??? Should return the value of ndelete
   end Process_Var_Delete;

   ----------------------------
   -- Process_Var_Set_Format --
   ----------------------------

   procedure Process_Var_Set_Format
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);

      Cursor  : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;

   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-set-format");
      end if;

      if Result.Results.Length /= 2 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "two attributes: format and value.");
      end if;

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Cursor := Result_Pair_Lists.Next (Cursor);

         if Pair.Variable.all = "format" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

            if Var_Obj.Format /= null then
               Unchecked_Free (Var_Obj.Format);
            end if;

            Var_Obj.Format := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "value" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

            if Var_Obj.Value /= null then
               Unchecked_Free (Var_Obj.Value);
            end if;

            Var_Obj.Value := String_Value (Pair.Value.all).Value;

         else
            raise Utils_Error with ("Ill-formatted done result record: "
                                    & "expected attribute `format' or "
                                    & "`value'.");
         end if;
      end loop;

   end Process_Var_Set_Format;

   -----------------------------------
   -- Process_Var_Info_Num_Children --
   -----------------------------------

   procedure Process_Var_Info_Num_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type)
   is
      Cursor  : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;
   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-info-num-children");
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one and only one attribute: numchild.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "numchild"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "attribute `numchild' followed by a "
                                 & "c-string.");
      end if;

      Var_Obj.Num_Child := Natural'Value
                              (String_Value (Pair.Value.all).Value.all);
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
      Var_Obj : in out Var_Obj_Type)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);

      Cursor  : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;

   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-info-type");
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one and only one attribute: type.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "type"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "attribute `type' followed by a c-string.");
      end if;

      if Var_Obj.Type_Desc /= null then
         Unchecked_Free (Var_Obj.Type_Desc);
      end if;

      Var_Obj.Type_Desc := String_Value (Pair.Value.all).Value;
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
      Var_Obj : in out Var_Obj_Type)
   is
      Cursor  : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;
      pragma Unreferenced (Var_Obj);
   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-info-path-expression");
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one and only one attribute: path_expr.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "path_expr"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "attribute `path_expr' followed by a "
                                 & "c-string.");
      end if;

      --  ??? Convert Pair.Value.all and store it into Var_Obj
   end Process_Var_Info_Path_Expression;

   --------------------------------
   -- Process_Var_Show_Attribute --
   --------------------------------

   procedure Process_Var_Show_Attribute
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type) is
   begin
      raise Not_Yet_Implemented_Error with "Process_Var_Show_Attribute";
   end Process_Var_Show_Attribute;

   -------------------------------------
   -- Process_Var_Evaluate_Expression --
   -------------------------------------

   procedure Process_Var_Evaluate_Expression
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);

      Cursor  : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;

   begin
      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         raise Utils_Error with ("Invalid result-record for "
                                 & "-var-evaluate-expression");
      end if;

      if Result.Results.Length /= 1 then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "one and only one attribute: value.");
      end if;

      pragma Assert (Result_Pair_Lists.Has_Element (Cursor));
      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "value"
         or else Pair.Value.all not in String_Value then
         raise Utils_Error with ("Ill-formatted done result record: expected "
                                 & "attribute `value' followed by a "
                                 & "c-string.");
      end if;

      if Var_Obj.Value /= null then
         Unchecked_Free (Var_Obj.Value);
      end if;

      Var_Obj.Value := String_Value (Pair.Value.all).Value;
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
