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

   Type_Error : exception;
   --  This exception is raised by the following subprogram if th MI_Value is
   --  not derived from the expected type.

   procedure Check_Is_String_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class);
   --  Checks that the given MI_Value is a String_Value and raise an exception
   --  if the assertion if False.

   procedure Check_Is_Result_List_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class);
   --  Checks that the given MI_Value is a Result_List_Value and raise an
   --  exception if the assertion if False.

   procedure Check_Is_Value_List_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class);
   --  Checks that the given MI_Value is a Value_List_Value and raise an
   --  exception if the assertion if False.

   pragma Unreferenced (Check_Is_Value_List_Value_Or_Die);
   --  ??? Remove this once used.

   procedure Die (Message : String := "");
   --  Raise an Utils_Error exception with Message as inner message.

   ----------------------------------
   -- Check_Is_String_Value_Or_Die --
   ----------------------------------

   procedure Check_Is_String_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class) is
   begin
      if Value not in String_Value then
         raise Type_Error with ("Expected attribute `" & Name & "' to be a "
                                & "c-string");
      end if;
   end Check_Is_String_Value_Or_Die;

   ---------------------------------------
   -- Check_Is_Result_List_Value_Or_Die --
   ---------------------------------------

   procedure Check_Is_Result_List_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class) is
   begin
      if Value not in Result_List_Value then
         raise Type_Error with ("Expected attribute `" & Name & "' to be a "
                                & "list of result");
      end if;
   end Check_Is_Result_List_Value_Or_Die;

   --------------------------------------
   -- Check_Is_Value_List_Value_Or_Die --
   --------------------------------------

   procedure Check_Is_Value_List_Value_Or_Die
     (Name  : String;
      Value : MI_Value'Class) is
   begin
      if Value not in Value_List_Value then
         raise Type_Error with ("Expected attribute `" & Name & "' to be a "
                                & "list of mi-value");
      end if;
   end Check_Is_Value_List_Value_Or_Die;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Result : Result_Record) return Boolean is
   begin
      return Result.Class.all = "error";
   end Is_Error;

   ---------
   -- Die --
   ---------

   procedure Die (Message : String := "") is
   begin
      raise Utils_Error with Message;
   end Die;

   -------------------
   -- Process_Error --
   -------------------

   function Process_Error (Result : Result_Record) return String_Access
   is
      Cursor : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair   : Result_Pair;
   begin
      --  An error record must start by '^error'. Thus, we check for the '^'
      --  which means that the record is an synchronous one (Sync_Result) and
      --  that its class is equal to "error".

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "error" then
         Die ("Invalid error-record");
      end if;

      --  The record must be compliant to this form:
      --       '^error,msg="some error message"

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted error result record: expected one and only one "
              & "attribute 'msg'.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "msg" then
         Die ("Ill-formatted error result record: expected attribute `msg' "
              & "followed by a c-string.");
      end if;

      --  The content of the 'msg' attribute must be a c-string, i.e. a
      --  String_Value in this API representation.

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

      --  Extracts and returns this value.

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
      --  A running record is of the form:
      --       '^running'
      --  Thus we check for the Sync_Result type and the 'running' class.

      if Result.R_Type /= Sync_Result
         or else Result.Class.all /= "running" then
         Die ("Invalid result-record for -exec-run");
      end if;

      --  A running record has no attribute.

      if Result.Results.Length /= 0 then
         Die ("Unexpected attribute(s) to result-record");
      end if;

      --  Returns True because the running record was well-formatted.

      return True;
   end Process_Exec_Run;

   --------------------------
   -- Process_Break_Insert --
   --------------------------

   function Process_Break_Insert
     (Result     : Result_Record) return Breakpoint_Type
   is
      Cursor     : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair       : Result_Pair;
      Breakpoint : Breakpoint_Type;

   begin
      --  The result of a -break-insert command is of the form:
      --       '^done,bkpt={number="1",type="breakpoint",...'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -break-insert");
      end if;

      --  It must have only one attribute, 'bkpt' which holds a list of
      --  attributes.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done-result: expected one attribute: bkpt");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "bkpt" then
         Die ("Expected attribute `bkpt'");
      end if;

      Check_Is_Result_List_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
      Cursor := Result_List_Value (Pair.Value.all).Value.First;

      --  Iterates through the list and store each possible attribute in its
      --  associated field in the Breakpoint_Type record.

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Cursor := Result_Pair_Lists.Next (Cursor);

         if Pair.Variable.all = "number" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Number := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

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
              (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "times" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Times := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "original-location" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Breakpoint.Original_Location := String_Value
                                              (Pair.Value.all).Value;

         else
            Die ("Unexpected attribute: " & Pair.Variable.all);
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
      --  Simply returns the stream content wihtout any form of checking.
      return Stream.Content;
   end Process_Stream_Output;

   ------------------------
   -- Process_Var_Create --
   ------------------------

   function Process_Var_Create
     (Result : Result_Record) return Var_Obj_Access
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Var_Obj_Type, Var_Obj_Access);

      Cursor  : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair    : Result_Pair;
      Var_Obj : Var_Obj_Access := null;

   begin
      --  The result of a -break-insert command is of the form:
      --       '^done,name="var1",numchild="3",value="{...}",...'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-create");
      end if;

      if Result.Results.Length /= 5 then
         Die ("Ill-formatted done result record: expected five attributes: "
              & "name, numchild, value, type and has_more.");
      end if;

      Var_Obj := new Var_Obj_Type;

      --  Iterates through the list and build the new Var_Obj with each
      --  possible attribute associated to his fields.

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);
         Cursor := Result_Pair_Lists.Next (Cursor);

         if Pair.Variable.all = "name" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Name := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "numchild" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Num_Child := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "value" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Value := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "type" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Type_Desc := String_Value (Pair.Value.all).Value;

         elsif Pair.Variable.all = "has_more" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.all.Has_More := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

         else
            Unchecked_Free (Var_Obj);  --  ??? Use Var_Obj_Free
            Die ("Ill-formatted done result record: expected attribute "
                 & "`name', `numchild', `value', `type' or `has_more'.");
         end if;
      end loop;

      --  Returns the newly created Var_Obj.

      return Var_Obj;
   end Process_Var_Create;

   ------------------------
   -- Process_Var_Delete --
   ------------------------

   function Process_Var_Delete (Result : Result_Record) return Natural
   is
      Cursor : constant Result_Pair_Lists.Cursor := Result.Results.First;
      Pair   : Result_Pair;
   begin
      --  A result from a -var-delete commmand is of the following form:
      --       '^done,ndeleted="4"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-delete");
      end if;

      --  It must contain one and only on attribute named 'ndeleted'.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done result record: expected one and only one "
              & "attribute: ndelete.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "ndelete" then
         Die ("Ill-formatted done result record: expected attribute `ndelete' "
              & "followed by a c-string.");
      end if;

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

      --  Returns the content of the ndelete attribute.

      return Natural'Value (String_Value (Pair.Value.all).Value.all);
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
      --  A result from a -var-set-format commmand is of the following form:
      --       '^done,format="decimal",value="{...}"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-set-format");
      end if;

      --  It must contain two attributes: 'format' and 'value'.

      if Result.Results.Length /= 2 then
         Die ("Ill-formatted done result record: expected two attributes: "
              & "format and value.");
      end if;

      --  Iterate over the attributes to extract their value and store it in
      --  the given Var_Obj object.

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
            Die ("Ill-formatted done result record: expected attribute "
                 & "`format' or `value'.");
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
      --  A result from a -var-info-num-children commmand is of the following
      --  form:
      --          '^done,numchild="3"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-info-num-children");
      end if;

      --  It must contain one and only one attribute: numchild.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done result record: expected one and only one "
              & "attribute: numchild.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "numchild" then
         Die ("Ill-formatted done result record: expected attribute "
              & "`numchild' followed by a c-string.");
      end if;

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
      Var_Obj.Num_Child := Natural'Value
        (String_Value (Pair.Value.all).Value.all);
   end Process_Var_Info_Num_Children;

   -------------------------------
   -- Process_Var_List_Children --
   -------------------------------

   procedure Process_Var_List_Children
     (Result  : Result_Record;
      Var_Obj : in out Var_Obj_Type)
   is
      function Process_Child (Rec : Result_List_Value) return Var_Obj_Access;
      --  Process a child as described in the result of a -var-list-children
      --  command, i.e.
      --       `child={name="var1.10",exp="10",numchild="0",type="integer"}'
      --
      --  Returns a new Var_Obj object.

      -------------------
      -- Process_Child --
      -------------------

      function Process_Child (Rec : Result_List_Value) return Var_Obj_Access
      is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
            (Var_Obj_Type, Var_Obj_Access);

         Cursor  : Result_Pair_Lists.Cursor := Result_Pair_Lists.First
                                                 (Rec.Value);
         Pair    : Result_Pair;
         Var_Obj : Var_Obj_Access := null;

      begin
         while Result_Pair_Lists.Has_Element (Cursor) loop
            Pair := Result_Pair_Lists.Element (Cursor);

            --  Every element of this pair list is supposed to be a
            --  String_Value so we can procede to a general check.

            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj := new Var_Obj_Type;

            if Pair.Variable.all = "name" then
               Var_Obj.all.Name := String_Value (Pair.Value.all).Value;

            elsif Pair.Variable.all = "exp" then
               Var_Obj.all.Expression := String_Value (Pair.Value.all).Value;

            elsif Pair.Variable.all = "numchild" then
               Var_Obj.all.Num_Child := Natural'Value
                 (String_Value (Pair.Value.all).Value.all);

            elsif Pair.Variable.all = "type" then
               Var_Obj.all.Type_Desc := String_Value (Pair.Value.all).Value;

            else
               --  ??? Replace the following by a Free_Var_Obj procedure that
               --  frees the inner attributes.
               Unchecked_Free (Var_Obj);
               Die ("Unexpected attribute: " & Pair.Variable.all);
            end if;

            Cursor := Result_Pair_Lists.Next (Cursor);
         end loop;

         return Var_Obj;
      end Process_Child;

      --  Variables declaration
      Cursor : Result_Pair_Lists.Cursor := Result.Results.First;
      Pair   : Result_Pair;
      Child  : Var_Obj_Access;

   begin
      --  A result from a -var-list-children commmand is of the following form:
      --    '^done,numchild="3",children=[child={...}...],,has_more="0"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-list-children");
      end if;

      --  It contains either 2 or 3 attributes since there is no 'children'
      --  attribute for a node with no child.

      if Result.Results.Length /= 2 and Result.Results.Length /= 3 then
         Die ("Ill-formatted done result record: expected two or three "
              & "attributes: numchild, children? and has_more");
      end if;

      --  Iterates over the attributes.

      while Result_Pair_Lists.Has_Element (Cursor) loop
         Pair := Result_Pair_Lists.Element (Cursor);

         if Pair.Variable.all = "numchild" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.Num_Child := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "has_more" then
            Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
            Var_Obj.Has_More := Natural'Value
              (String_Value (Pair.Value.all).Value.all);

         elsif Pair.Variable.all = "children" then
            --  Retrieves the list of children and iterates over it to process
            --  each children using the Process_Child subprogram.

            Check_Is_Result_List_Value_Or_Die (Pair.Variable.all,
                                               Pair.Value.all);
            Cursor := Result_Pair_Lists.First
              (Result_List_Value (Pair.Value.all).Value);

            while Result_Pair_Lists.Has_Element (Cursor) loop
               Pair := Result_Pair_Lists.Element (Cursor);

               if Pair.Variable.all /= "child" then
                  Die ("Unexpected attribute: " & Pair.Variable.all);
               end if;

               Check_Is_Result_List_Value_Or_Die
                 (Pair.Variable.all, Pair.Value.all);
               Child := Process_Child (Result_List_Value (Pair.Value.all));
               Var_Obj.Children.Append (Child);

               Cursor := Result_Pair_Lists.Next (Cursor);
            end loop;

         else
            Die ("Unexpected attribute: " & Pair.Variable.all);
         end if;

         Cursor := Result_Pair_Lists.Next (Cursor);
      end loop;
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
      --  A result from a -var-info-type commmand is of the following form:
      --       '^done,type="records.r"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-info-type");
      end if;

      --  It must contain one and only one attribute: 'type'.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done result record: expected one and only one "
              & "attribute: type.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "type" then
         Die ("Ill-formatted done result record: expected attribute `type' "
              & "followed by a c-string.");
      end if;

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

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
   begin
      --  A result from a -var-info-path-expression commmand is of the
      --  following form:
      --       '^done,path_expr="Rec"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-info-path-expression");
      end if;

      --  It contains one and only one attribute: 'path_expr'.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done result record: expected one and only one "
              & "attribute: path_expr.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "path_expr" then
         Die ("Ill-formatted done result record: expected attribute "
              & "`path_expr' followed by a c-string.");
      end if;

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);
      Var_Obj.Path_Exp := String_Value (Pair.Value.all).Value;
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
      --  A result from a -var-evaluate-expression commmand is of the following
      --  form:
      --          '^done,value="{...}"'

      if Result.R_Type /= Sync_Result or else Result.Class.all /= "done" then
         Die ("Invalid result-record for -var-evaluate-expression");
      end if;

      --  It contains one and only one attribute: 'value'.

      if Result.Results.Length /= 1 then
         Die ("Ill-formatted done result record: expected one and only one "
              & "attribute: value.");
      end if;

      Pair := Result_Pair_Lists.Element (Cursor);

      if Pair.Variable.all /= "value" then
         Die ("Ill-formatted done result record: expected attribute `value' "
              & "followed by a c-string.");
      end if;

      Check_Is_String_Value_Or_Die (Pair.Variable.all, Pair.Value.all);

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
