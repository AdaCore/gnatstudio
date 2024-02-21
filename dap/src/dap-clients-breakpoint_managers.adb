------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with VSS.Characters;
with VSS.Characters.Latin;
with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors;
with VSS.Strings.Cursors.Iterators.Characters;

with DAP.Module;
with DAP.Module.Breakpoints;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Hooks;

with DAP.Clients.Breakpoint_Managers.SetBreakpoints;
with DAP.Clients.Breakpoint_Managers.SetExceptionBreakpoints;
with DAP.Clients.Breakpoint_Managers.SetFunctionBreakpoints;
with DAP.Clients.Breakpoint_Managers.SetInstructionBreakpoints;
with DAP.Clients.Evaluate;
with DAP.Modules.Preferences;
with DAP.Requests;                 use DAP.Requests;
with DAP.Utils;                    use DAP.Utils;

package body DAP.Clients.Breakpoint_Managers is

   type On_DAP_Request_Processed
   is new GPS.Kernel.Hooks.Dap_Message_Hooks_Function with record
      Manager : Breakpoint_Manager_Access;
   end record;

   overriding procedure Execute
      (Self   : On_DAP_Request_Processed;
       Kernel : not null access Kernel_Handle_Record'Class;
       Method : String);
   --  TODO: doc

   function Create_Line_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Indexes : Breakpoint_Index_Lists.List) return DAP_Request_Access;
   --  Send a request for line breakpoints

   function Create_Subprogram_Breakpoints_Request
     (Self         : not null access Breakpoint_Manager_Type'Class;
      Indexes      : Breakpoint_Index_Lists.List)
      return DAP_Request_Access;
   --  TODO: doc

   function Create_Address_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      Indexes : Breakpoint_Index_Lists.List)
      return DAP_Request_Access;
   --  TODO: doc

   function Create_Exception_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      Indexes : Breakpoint_Index_Lists.List)
      return DAP_Request_Access;
   --  TODO: doc

   procedure On_Initialized
     (Self : not null access Breakpoint_Manager_Type'Class);
   --  TODO: doc

   procedure Update_Sychronization_Data
     (Data       : in out Synchonization_Data;
      Breakpoint : Breakpoint_Data);
   --  TODO: doc

   --------------------------------
   -- Update_Sychronization_Data --
   --------------------------------

   procedure Update_Sychronization_Data
     (Data       : in out Synchonization_Data;
      Breakpoint : Breakpoint_Data) is
   begin
      case Breakpoint.Kind is
         when On_Line =>
            Data.Files_To_Sync.Include
              (Get_File (Breakpoint.Location.Marker));
         when On_Subprogram =>
            Data.Sync_Functions := True;
         when On_Exception =>
            Data.Sync_Exceptions := True;
         when On_Address =>
            Data.Sync_Instructions := True;
      end case;
   end Update_Sychronization_Data;

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      Item   : DAP.Tools.Breakpoint;
      Data   : in out Breakpoint_Data;
      File   : Virtual_File := No_File)
   is
      Line    : Basic_Types.Editable_Line_Type := 0;
      Address : Address_Type := Invalid_Address;
   begin
      if Item.id.Is_Set then
         Data.Num := Breakpoint_Identifier (Item.id.Value);
      end if;

      if not Item.instructionReference.Is_Empty then
         Address := String_To_Address (UTF8 (Item.instructionReference));

         if Item.offset.Is_Set then
            Address := Set_Offset (Address, Item.offset.Value);
         end if;
      end if;

      Data.Verified := Item.verified and then Item.line.Is_Set;

      if Data.Verified
        and then File /= No_File
        and then Item.line.Is_Set
      then
         Line := Basic_Types.Editable_Line_Type (Item.line.Value);

         declare
            use GPS.Editors;

            Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
              Kernel.Get_Buffer_Factory.Get_Holder (File);
         begin
            Data.Location :=
              (Data.Num,
               Kernel.Get_Buffer_Factory.Create_Marker
                 (File   => File,
                  Line   => Line,
                  Column => Holder.Editor.Expand_Tabs
                    (Line,
                     (if Item.column.Is_Set
                      then Basic_Types.Character_Offset_Type
                        (Item.column.Value)
                      else 1))),
               Address);
         end;
      end if;
   end Convert;

   ------------------------------------
   -- On_Breakpoint_Request_Response --
   ------------------------------------

   procedure On_Breakpoint_Request_Response
     (Self            : not null access Breakpoint_Manager_Type;
      Client          : not null access DAP.Clients.DAP_Client'Class;
      New_Breakpoints : DAP.Tools.Breakpoint_Vector;
      Old_Breakpoints : Breakpoint_Index_Lists.List;
      File            : Virtual_File := No_File)
   is
      Data   : Breakpoint_Data;
      Cursor : Breakpoint_Index_Lists.Cursor;
   begin
      --  We should have the same number of breakpoints in the reponse than
      --  the ones we have sent.
      if Integer (New_Breakpoints.Length)
        /= Integer (Old_Breakpoints.Length)
      then
         return;
      end if;

      Cursor := Old_Breakpoints.First;

      for Idx in 1 .. New_Breakpoints.Length loop
         Data := Self.Holder.Get_Breakpoint_From_Index (Cursor.Element);
         Convert
           (Kernel => Self.Kernel,
            Data   => Data,
            Item   => New_Breakpoints (Idx),
            File   => File);
         Self.Holder.Replace
           (Data => Data,
            Idx  => Cursor.Element);
         Send_Commands (Breakpoint_Manager_Access (Self), Data);
         Cursor.Next;
      end loop;

      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Kernel   => Self.Kernel,
         Debugger => Client.Get_Visual);
   end On_Breakpoint_Request_Response;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_DAP_Request_Processed;
       Kernel : not null access Kernel_Handle_Record'Class;
       Method : String)
   is
      --------------------------------------
      -- Is_On_DAP_Request_Processed_Func --
      --------------------------------------

      function Is_On_DAP_Request_Processed_Func
        (F : not null access Hook_Function'Class) return Boolean
      is (F.all in On_DAP_Request_Processed'Class);

   begin
      --  TODO: doc
      if Method = "setBreakpoints"
        or else Method = "setFunctionBreakpoints"
        or else Method = "setExceptionBreakpoints"
        or else Method = "setInstructionBreakpoints"
      then
         Self.Manager.Requests_Count := Self.Manager.Requests_Count - 1;
      end if;

      --  TODO: doc
      if Self.Manager.Requests_Count = 0 then
         GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Remove
           (Is_On_DAP_Request_Processed_Func'Unrestricted_Access);
         Self.Manager.On_Initialized;
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Breakpoint_Manager_Type)
   is
      Map     : Breakpoint_Hash_Maps.Map;
      Indexes : Breakpoint_Index_Lists.List;
   begin
      if Self.Client.Get_Executable /= No_File then
         --  Initialize the debugger's breakpoints with the persistent ones set
         --  for this executable outside the debugging session, if any.
         Self.Holder.Initialize
           (Vector    =>
              DAP.Module.Breakpoints.Get_Persistent_For_Executable
                (Self.Client.Get_Executable));

         --  Add an exception breakpoint to catch any exception if the
         --  corresponding preference is set.
         if DAP.Modules.Preferences.Break_On_Exception.Get_Pref then
            declare
               Data    : constant Breakpoint_Data := Breakpoint_Data'
                 (Kind        => On_Exception,
                  Num         => 0,
                  Except      => To_Unbounded_String
                    (DAP.Module.Breakpoints.All_Exceptions_Filter),
                  Unhandled   => False,
                  Disposition => Keep,
                  Executable  => Self.Client.Get_Executable,
                  others      => <>);
            begin
               Self.Holder.Append (Data);
            end;
         end if;

         --  TODO: doc
         GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Add
           (new On_DAP_Request_Processed'
              (Hook_Function with
               Manager => Breakpoint_Manager_Access (Self)));

         Map := Self.Holder.Get_For_Files;

         --  Send source line breakpoints first
         if not Map.Is_Empty then
            declare
               Cursor : Breakpoint_Hash_Maps.Cursor := Map.First;
            begin
               while Cursor.Has_Element loop
                  Self.Send_Line
                    (Indexes => Breakpoint_Hash_Maps.Element (Cursor),
                     Kind    => On_Line,
                     File    => Breakpoint_Hash_Maps.Key (Cursor));
                  Cursor.Next;
               end loop;
            end;
         end if;

         --  Send subprogram/exception/instruction breakpoints, if any
         for Kind in On_Subprogram .. On_Exception loop
            Indexes := Self.Holder.Get_For_Kind (Kind);

            if not Indexes.Is_Empty then
               Self.Send_Line
                 (Indexes => Indexes,
                  Kind    => Kind);
            end if;
         end loop;
      else
         Self.On_Initialized;
      end if;
   end Initialize;

   -----------
   -- Break --
   -----------

   procedure Break
     (Self : not null access Breakpoint_Manager_Type;
      Data : Breakpoint_Data)
   is
      Sync_Data : Synchonization_Data;
   begin
      Self.Holder.Append (Data);

      --  TOOD: doc
      Update_Sychronization_Data
        (Data       => Sync_Data,
         Breakpoint => Data);
      Self.Synchronize_Breakpoints (Sync_Data);
   end Break;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Self      : not null access Breakpoint_Manager_Type;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False)
   is
      Data    : constant Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Exception,
         Num         => 0,
         Except      => To_Unbounded_String (Name),
         Unhandled   => Unhandled,
         Disposition => (if Temporary then Delete else Keep),
         Executable  => Self.Client.Get_Executable,
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Exception;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Self      : not null access Breakpoint_Manager_Type;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      Data    : constant Breakpoint_Data := Breakpoint_Data'
        (Kind      => On_Line,
         Num       => 0,
         Location  =>
           (0,
            Self.Kernel.Get_Buffer_Factory.Create_Marker
              (File   => File,
               Line   => Line,
               Column => 1),
            Invalid_Address),
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Executable  => Self.Client.Get_Executable,
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Source;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : not null access Breakpoint_Manager_Type;
      Subprogram : String;
      Temporary  : Boolean := False;
      Condition  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      Data : constant Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Subprogram,
         Num         => 0,
         Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Executable  => Self.Client.Get_Executable,
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Subprogram;

   -----------------
   -- Break_Address --
   -------------------

   procedure Break_Address
     (Self      : not null access Breakpoint_Manager_Type;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      Data : constant Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Address,
         Num         => 0,
         Address     => Address,
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Executable  => Self.Client.Get_Executable,
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Address;

   -----------------------------------
   -- Toggle_Instruction_Breakpoint --
   -----------------------------------

   procedure Toggle_Instruction_Breakpoint
     (Self    : not null access Breakpoint_Manager_Type;
      Address : Address_Type)
   is
      Indexes_To_Remove : Breakpoint_Index_Lists.List;
      Address_Breakpoints : constant Breakpoint_Index_Lists.List :=
        Self.Holder.Get_For_Kind (On_Address);
      Data                : Breakpoint_Data;
      Sync_Data           : constant Synchonization_Data :=
        Synchonization_Data'
          (Files_To_Sync     => <>,
           Sync_Functions    => False,
           Sync_Exceptions   => False,
           Sync_Instructions => True);
   begin
      --  TODO: doc
      for Idx of Address_Breakpoints loop
         Data := Self.Holder.Get_Breakpoint_From_Index (Idx);

         if Data.Address = Address then
            Indexes_To_Remove.Append (Idx);
         end if;
      end loop;

      Self.Holder.Delete (Indexes => Indexes_To_Remove);

      if Indexes_To_Remove.Is_Empty then
         Self.Holder.Append
           (Breakpoint_Data'
              (Kind       => On_Address,
               Num        => 0,
               Address    => Address,
               Executable => Self.Client.Get_Executable,
               others     => <>));
      end if;

      Self.Synchronize_Breakpoints (Sync_Data);
   end Toggle_Instruction_Breakpoint;
   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : Breakpoint_Manager_Access)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector is
   begin
      return Self.Holder.Get_Breakpoints;
   end Get_Breakpoints;

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Self : not null access Breakpoint_Manager_Type;
      Id   : Breakpoint_Identifier) return Breakpoint_Data is
   begin
      return Self.Holder.Get_Breakpoint_From_Id (Id);
   end Get_Breakpoint_From_Id;

   --------------------
   -- Has_Breakpoint --
   --------------------

   function Has_Breakpoint
     (Self   : not null access Breakpoint_Manager_Type;
      Marker : Location_Marker)
      return Boolean is
   begin
      return Self.Holder.Contains (Marker);
   end Has_Breakpoint;

   ----------------------
   -- Show_Breakpoints --
   ----------------------

   procedure Show_Breakpoints
     (Self : in out Breakpoint_Manager_Type) is
   begin
      DAP.Module.Breakpoints.Hide_Breakpoints (Self.Kernel);

      for Data of Self.Holder.Get_Breakpoints loop
         DAP.Module.Breakpoints.Show_Breakpoint
           (Self.Kernel, Data);
      end loop;
   end Show_Breakpoints;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   procedure Set_Breakpoint_Command
     (Self    : not null access Breakpoint_Manager_Type;
      Id      : Breakpoint_Identifier;
      Command : VSS.Strings.Virtual_String)
   is
      use VSS.Characters;

      Cmd : VSS.Strings.Virtual_String;
   begin
      Cmd := VSS.Strings.Conversions.To_Virtual_String
        ("command" & Breakpoint_Identifier'Image (Id)
         & ASCII.LF) & Command;

      if not Command.Is_Empty
        and then VSS.Strings.Cursors.Iterators.Characters.Element
          (Command.At_Last_Character) /= VSS.Characters.Latin.Line_Feed
      then
         Cmd.Append (VSS.Characters.Latin.Line_Feed);
      end if;
      Cmd.Append (VSS.Strings.Conversions.To_Virtual_String ("end"));

      DAP.Clients.Evaluate.Send_Evaluate_Command_Request
        (Self.Client.all, Cmd);
   end Set_Breakpoint_Command;

   -------------------
   -- Send_Commands --
   -------------------

   procedure Send_Commands
     (Self : not null access Breakpoint_Manager_Type;
      Data : DAP.Modules.Breakpoints.Breakpoint_Data) is
   begin
      if not Data.Commands.Is_Empty then
         Self.Set_Breakpoint_Command (Data.Num, Data.Commands);
      end if;
   end Send_Commands;

   -------------------
   -- Send_Commands --
   -------------------

   procedure Send_Commands
     (Self : not null access Breakpoint_Manager_Type;
      Data : Breakpoint_Vectors.Vector) is
   begin
      for Bp of Data loop
         Self.Send_Commands (Bp);
      end loop;
   end Send_Commands;

   -------------------------------------
   -- Create_Line_Breakpoints_Request --
   -------------------------------------

   function Create_Line_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Indexes : Breakpoint_Index_Lists.List) return DAP_Request_Access
   is
      Req : constant SetBreakpoints.Source_Line_Request_Access :=
        new SetBreakpoints.Source_Line_Request (Self.Kernel);
      Sb  : DAP.Tools.SourceBreakpoint;
   begin
      Req.Manager     := Breakpoint_Manager_Access (Self);
      Req.File        := File;
      Req.Breakpoints := Indexes;

      Req.Parameters.arguments.source.name :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Base_Name (File));

      Req.Parameters.arguments.source.path :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Full_Name (File));

      Req.Parameters.arguments.sourceModified := False;

      for Data of Self.Holder.Get_Breakpoints (Indexes => Indexes) loop
         Sb.line   := Integer (GPS.Editors.Get_Line (Get_Location (Data)));
         Sb.column :=
           (Is_Set => True,
            Value  => Integer (GPS.Editors.Get_Column (Get_Location (Data))));
         Sb.condition    := Data.Condition;
         Sb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Sb);
      end loop;

      return DAP_Request_Access (Req);
   end Create_Line_Breakpoints_Request;

   ---------------
   -- Send_Line --
   ---------------

   procedure Send_Line
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : Breakpoint_Index_Lists.List;
      Kind    : Breakpoint_Kind;
      File    : GNATCOLL.VFS.Virtual_File := No_File)
   is
      Request : DAP_Request_Access;
   begin
      case Kind is
         when On_Line =>
            Request := Self.Create_Line_Breakpoints_Request (File, Indexes);
         when On_Subprogram =>
            Request := Self.Create_Subprogram_Breakpoints_Request (Indexes);
         when On_Exception =>
            Request := Self.Create_Exception_Breakpoints_Request (Indexes);
         when On_Address =>
            Request := Self.Create_Address_Breakpoints_Request (Indexes);
      end case;

      Self.Client.Enqueue (Request);
   end Send_Line;

   -------------------------------------------
   -- Create_Subprogram_Breakpoints_Request --
   -------------------------------------------

   function Create_Subprogram_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      Indexes : Breakpoint_Index_Lists.List)
      return DAP_Request_Access
   is
      Req : constant SetFunctionBreakpoints.
        Function_Breakpoint_Request_Access :=
        new SetFunctionBreakpoints.Function_Breakpoint_Request (Self.Kernel);
      Fb   : DAP.Tools.FunctionBreakpoint;
   begin
      Req.Manager     := Breakpoint_Manager_Access (Self);
      Req.Breakpoints := Indexes;

      for Data of Self.Holder.Get_Breakpoints (Indexes => Indexes) loop
         Fb.name := VSS.Strings.Conversions.To_Virtual_String
           (To_String (Data.Subprogram));
         Fb.condition    := Data.Condition;
         Fb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Fb);
      end loop;

      return DAP_Request_Access (Req);
   end Create_Subprogram_Breakpoints_Request;

   ------------------------------------------
   -- Create_Exception_Breakpoints_Request --
   ------------------------------------------

   function Create_Exception_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      Indexes : Breakpoint_Index_Lists.List)
      return DAP_Request_Access
   is
      Req : constant SetExceptionBreakpoints.
        Exception_Breakpoint_Request_Access :=
        new SetExceptionBreakpoints.Exception_Breakpoint_Request (Self.Kernel);
   begin
      Req.Manager     := Breakpoint_Manager_Access (Self);
      Req.Breakpoints := Indexes;

      for Data of Self.Holder.Get_Breakpoints (Indexes => Indexes) loop
         declare
            N : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (Data.Except);
         begin
            Req.Parameters.arguments.filters.Append (N);
            declare
               Option : DAP.Tools.ExceptionOptions;
               Names  : VSS.String_Vectors.Virtual_String_Vector;
            begin
               Names.Append (N);
               Option.path.Append ((negate => False, names => Names));
               if Data.Unhandled then
                  Option.breakMode := DAP.Tools.Enum.unhandled;
               else
                  Option.breakMode := DAP.Tools.Enum.always;
               end if;
               Req.Parameters.arguments.exceptionOptions.Append (Option);
            end;
         end;
      end loop;

      return DAP_Request_Access (Req);
   end Create_Exception_Breakpoints_Request;

   --------------------
   -- On_Initialized --
   --------------------

   procedure On_Initialized
     (Self : not null access Breakpoint_Manager_Type'Class) is
   begin
      Self.Show_Breakpoints;

      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Client.Get_Visual);

      Self.Client.On_Breakpoints_Set;
   end On_Initialized;

   ----------------------------------------
   -- Create_Address_Breakpoints_Request --
   ----------------------------------------

   function Create_Address_Breakpoints_Request
     (Self    : not null access Breakpoint_Manager_Type'Class;
      Indexes : Breakpoint_Index_Lists.List)
      return DAP_Request_Access
   is
      Req : constant SetInstructionBreakpoints.
        Instruction_Breakpoint_Request_Access :=
          new SetInstructionBreakpoints.Instruction_Breakpoint_Request
            (Self.Kernel);
      Fb  : DAP.Tools.InstructionBreakpoint;
   begin
      Req.Manager     := Breakpoint_Manager_Access (Self);
      Req.Breakpoints := Indexes;

      for Data of Self.Holder.Get_Breakpoints (Indexes => Indexes) loop
         Fb.instructionReference := VSS.Strings.Conversions.To_Virtual_String
           (Address_To_String (Data.Address));
         Fb.condition    := Data.Condition;
         Fb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Fb);
      end loop;

      return DAP_Request_Access (Req);
   end Create_Address_Breakpoints_Request;

   ----------
   -- Send --
   ----------

   procedure Synchronize_Breakpoints
     (Self      : not null access Breakpoint_Manager_Type;
      Sync_Data : Synchonization_Data) is
   begin
      for File of Sync_Data.Files_To_Sync loop
         Self.Send_Line
           (Indexes => Self.Holder.Get_For_File (File),
            Kind    => On_Line,
            File    => File);
      end loop;

      if Sync_Data.Sync_Exceptions then
         Self.Send_Line
           (Indexes => Self.Holder.Get_For_Kind (On_Exception),
            Kind    => On_Exception);
      end if;

      if Sync_Data.Sync_Functions then
         Self.Send_Line
           (Indexes => Self.Holder.Get_For_Kind (On_Subprogram),
            Kind    => On_Subprogram);
      end if;

      if Sync_Data.Sync_Instructions then
         Self.Send_Line
           (Indexes => Self.Holder.Get_For_Kind (On_Address),
            Kind    => On_Address);
      end if;
   end Synchronize_Breakpoints;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean)
   is
      Changed_Breakpoints : constant Breakpoint_Vectors.Vector :=
        Self.Holder.Get_Breakpoints (Indexes);
      Sync_Data           : Synchonization_Data;
   begin
      --  TODO: doc
      for Data of Changed_Breakpoints loop
         Update_Sychronization_Data (Sync_Data, Data);
      end loop;

      Self.Holder.Set_Enabled (Indexes, State);
      Self.Synchronize_Breakpoints (Sync_Data);
   end Set_Breakpoints_State;

   ----------------------
   -- Set_Ignore_Count --
   ----------------------

   procedure Set_Ignore_Count
     (Self  : not null access Breakpoint_Manager_Type;
      Id    : Breakpoint_Identifier;
      Count : Natural)
   is
      Sync_Data : Synchonization_Data;
   begin
      Update_Sychronization_Data
        (Data       => Sync_Data,
         Breakpoint => Self.Holder.Get_Breakpoint_From_Id (Id));
      Self.Holder.Set_Ignore_Count (Id, Count);
      Self.Synchronize_Breakpoints (Sync_Data);
   end Set_Ignore_Count;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self : not null access Breakpoint_Manager_Type;
      File : GNATCOLL.VFS.Virtual_File;
      Line : Editable_Line_Type)
   is
      Sync_Data : Synchonization_Data;
   begin
      Self.Holder.Delete (File, Line);
      Sync_Data.Files_To_Sync.Include (File);

      Self.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);

      Self.Synchronize_Breakpoints (Sync_Data);
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self    : not null access Breakpoint_Manager_Type;
      Indexes : DAP.Types.Breakpoint_Index_Lists.List)
   is
      Removed_Breakpoints : constant Breakpoint_Vectors.Vector :=
        Self.Holder.Get_Breakpoints (Indexes);
      Sync_Data           : Synchonization_Data;
   begin
      --  TODO: doc
      for Data of Removed_Breakpoints loop
         Update_Sychronization_Data (Sync_Data, Data);
      end loop;

      Self.Holder.Delete (Indexes);

      Self.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Client.Get_Visual);

      Self.Synchronize_Breakpoints (Sync_Data);
   end Remove_Breakpoints;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : not null access Breakpoint_Manager_Type;
      Ids  : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      Sync_Data : Synchonization_Data;
   begin
      --  TODO: doc
      for Id of Ids loop
         Update_Sychronization_Data
           (Data       => Sync_Data,
            Breakpoint => Self.Holder.Get_Breakpoint_From_Id (Id));
         Self.Holder.Delete (Id);
      end loop;

      Self.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Client.Get_Visual);

      Self.Synchronize_Breakpoints (Sync_Data);
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints
     (Self : not null access Breakpoint_Manager_Type)
   is
      use Breakpoint_Hash_Maps;

      Changed : Breakpoint_Hash_Maps.Map;
      Cursor  : Breakpoint_Hash_Maps.Cursor;
   begin
      Changed := Self.Holder.Get_For_Files;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         Self.Send_Line
           (Indexes => Breakpoint_Index_Lists.Empty_List,
            Kind    => On_Line,
            File    => Key (Cursor));
         Next (Cursor);
      end loop;

      for Kind in On_Subprogram .. On_Exception loop
         if not Self.Holder.Get_For_Kind (Kind).Is_Empty then
            Self.Send_Line
              (Indexes => Breakpoint_Index_Lists.Empty_List,
               Kind    => Kind);
         end if;
      end loop;

      Self.Holder.Clear;
   end Remove_All_Breakpoints;

   -------------
   -- Stopped --
   -------------

   procedure Stopped
     (Self         : not null access Breakpoint_Manager_Type;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer;
      Address      : out Address_Type)
   is
      use DAP.Tools;

      Breakpoints : constant Breakpoint_Vectors.Vector :=
        Self.Holder.Get_Breakpoints;
      Indexes : DAP.Types.Breakpoint_Index_Lists.List;
      Data    : Breakpoint_Data;
   begin
      Stopped_File := No_File;
      Stopped_Line := 0;

      --  TODO: simplify this...
      for Index in 1 .. Length (Event.a_body.hitBreakpointIds) loop
         declare
            Num : constant Integer_Constant_Reference :=
              Event.a_body.hitBreakpointIds (Index);
         begin
            for Idx in Breakpoints.First_Index ..  Breakpoints.Last_Index loop
               Data := Breakpoints (Idx);

               if Data = Breakpoint_Identifier (Num.Element.all)
                 and then Data.Kind = On_Line
                 and then Data.Location.Marker /= No_Marker
               then
                  if Stopped_File = No_File then
                     Stopped_File :=
                       GPS.Editors.Get_File (Data.Location.Marker);
                     Stopped_Line :=
                       Integer  (GPS.Editors.Get_Line (Data.Location.Marker));
                     Address := Data.Location.Address;
                  end if;

                  if Data.Disposition = Delete then
                     Indexes.Append (Idx);
                  end if;
               end if;
            end loop;
         end;
      end loop;

      if not Indexes.Is_Empty then
         Self.Remove_Breakpoints (Indexes);
      end if;
   end Stopped;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : not null access Breakpoint_Manager_Type) is
      use Breakpoint_Vectors;
   begin
      --  do not store breakpoints when we started debugging with no_executable
      if Self.Client.Get_Executable /= No_File then
         --  Store breakpoints in the persistent storage
         if DAP.Modules.Preferences.Break_On_Exception.Get_Pref then
            declare
               List : Breakpoint_Vectors.Vector;
            begin
               --  filtering out automatically set breakpoint for any exception
               for B of Self.Holder.Get_Breakpoints loop
                  if B.Kind /= On_Exception
                    or else B.Except /=
                      DAP.Module.Breakpoints.All_Exceptions_Filter
                  then
                     List.Append (B);
                  end if;
               end loop;

               DAP.Module.Breakpoints.Store
                 (Self.Client.Get_Executable, List);
            end;
         else
            DAP.Module.Breakpoints.Store
              (Self.Client.Get_Executable, Self.Holder.Get_Breakpoints);
         end if;
      end if;
   end Finalize;

   ---------------------
   -- On_Notification --
   ---------------------

   procedure On_Notification
     (Self  : not null access Breakpoint_Manager_Type;
      Event : DAP.Tools.BreakpointEvent_body)
   is
      use DAP.Tools;
      use DAP.Tools.Enum;

      function Convert return Breakpoint_Data;
      function Convert return Breakpoint_Data
      is
         File   : constant Virtual_File :=
           (if Event.breakpoint.source.Is_Set
            then To_File (Event.breakpoint.source.Value.path)
            else No_File);
         Data : Breakpoint_Data;
      begin
         Convert
           (Kernel => Self.Kernel,
            Item   => Event.breakpoint,
            Data   => Data,
            File   => File);
         Data.Executable := Self.Client.Get_Executable;
         return Data;
      end Convert;

      Data : constant Breakpoint_Data := Convert;
   begin
      case Event.reason.Kind is
         when changed =>
            Self.Holder.Replace_From_Id (Data);
            GPS.Kernel.Hooks.Debugger_Breakpoint_Changed_Hook.Run
              (Self.Kernel, Self.Client.Get_Visual, Integer (Data.Num));

         when a_new =>
            Self.Holder.Append (Data);
            GPS.Kernel.Hooks.Debugger_Breakpoint_Added_Hook.Run
              (Self.Kernel, Self.Client.Get_Visual, Integer (Data.Num));

         when removed =>
            if Event.breakpoint.id.Is_Set then
               Self.Holder.Delete
                 (Breakpoint_Identifier (Event.breakpoint.id.Value));

               GPS.Kernel.Hooks.Debugger_Breakpoint_Deleted_Hook.Run
                 (Self.Kernel, Self.Client.Get_Visual,
                  Event.breakpoint.id.Value);
            end if;

         when Custom_Value =>
            null;
      end case;

      Self.Show_Breakpoints;
   end On_Notification;

end DAP.Clients.Breakpoint_Managers;
