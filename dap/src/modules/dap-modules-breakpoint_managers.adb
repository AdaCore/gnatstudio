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

with GNATCOLL.Tribooleans;           use GNATCOLL.Tribooleans;

with VSS.String_Vectors;
with VSS.Strings.Conversions;

with DAP.Modules.Persistent_Breakpoints;
with DAP.Clients;
with DAP.Module;
with DAP.Views;

with Generic_Views;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Hooks;
with GPS.Debuggers;

with DAP.Modules.Breakpoint_Managers.Sources;
with DAP.Modules.Breakpoint_Managers.Exceptions;
with DAP.Modules.Breakpoint_Managers.Functions;
with DAP.Modules.Breakpoint_Managers.Instructions;

package body DAP.Modules.Breakpoint_Managers is

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : Virtual_File;
      Holder : GPS.Editors.Controlled_Editor_Buffer_Holder;
      Data   : in out Breakpoint_Data;
      Item   : DAP.Tools.Breakpoint)
   is
      Line    : Basic_Types.Editable_Line_Type := 0;
      Address : Address_Type := Invalid_Address;
   begin
      if Item.id.Is_Set then
         Data.Num := Breakpoint_Identifier (Item.id.Value);
      end if;

      if not Item.instructionReference.Is_Empty then
         Address := String_To_Address
           (VSS.Strings.Conversions.To_UTF_8_String
              (Item.instructionReference));

         if Item.offset.Is_Set then
            Address := Set_Offset (Address, Item.offset.Value);
         end if;
      end if;

      if File /= No_File
        and then Item.line.Is_Set
      then
         Line := Basic_Types.Editable_Line_Type (Item.line.Value);

         Data.Locations := Location_Vectors.To_Vector
           ((Data.Num,
            Kernel.Get_Buffer_Factory.Create_Marker
              (File   => File,
               Line   => Line,
               Column => Holder.Editor.Expand_Tabs
                 (Line,
                  (if Item.column.Is_Set
                   then Basic_Types.Character_Offset_Type (Item.column.Value)
                   else 1))),
            Address),
            1);
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      DAP_Bp : DAP.Tools.Breakpoint) return Breakpoint_Data
   is
      File   : constant Virtual_File :=
        (if DAP_Bp.source.Is_Set
         then Create
           (+(VSS.Strings.Conversions.To_UTF_8_String
            (if DAP_Bp.source.Value.path.Is_Empty
                 then DAP_Bp.source.Value.name
                 else DAP_Bp.source.Value.path)))
         else No_File);
      Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Kernel.Get_Buffer_Factory.Get_Holder (File => File);
      Data : Breakpoint_Data;
   begin
      Convert (Kernel, File, Holder, Data, DAP_Bp);
      return Data;
   end Convert;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : DAP_Client_Breakpoint_Manager_Access)
   is
      Map : Breakpoint_Hash_Maps.Map;
      V   : Breakpoint_Vectors.Vector;
   begin
      if Self.Client.Get_Executable /= No_File then
         Self.Holder.Initialize
           (DAP.Modules.Persistent_Breakpoints.Get_Persistent_For_Executable
              (Self.Client.Get_Executable),
            True);

         Map := Self.Holder.Get_For_Files;
         if not Map.Is_Empty then
            for Vector of Map loop
               Self.Send_Line
                 (GPS.Editors.Get_File (Get_Location (Vector.First_Element)),
                  Vector, Init);
            end loop;
         end if;

         V := Self.Holder.Get_For (On_Subprogram);
         if not V.Is_Empty then
            Self.Send_Subprogram (V, Init, True);
         end if;

         V := Self.Holder.Get_For (On_Exception);
         if not V.Is_Empty then
            Self.Send_Exception (V, Init);
         end if;
      end if;

      if Self.Client.Get_Executable = No_File
        or else Self.Requests_Count = 0
      then
         Self.Requests_Count := 1;
         Self.Dec_Response (Init);
      end if;
   end Initialize;

   -----------
   -- Break --
   -----------

   procedure Break
     (Self : DAP_Client_Breakpoint_Manager_Access;
      Data : Breakpoint_Data)
   is
      Changed : Breakpoint_Vectors.Vector;
      Updated : Triboolean;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      if Data.Num = No_Breakpoint then
         Self.Holder.Add (Data, Changed);
      else
         Self.Holder.Replace (Data, Updated);

         if Updated = GNATCOLL.Tribooleans.True then
            case Data.Kind is
               when On_Line =>
                  Changed := Self.Holder.Get_For_File
                    (Get_File (Data.Locations.First_Element.Marker));

               when others =>
                  Changed := Self.Holder.Get_For (Data.Kind);
            end case;

         elsif Updated = Indeterminate then
            Self.Send_Commands (Data);
         end if;
      end if;

      if not Changed.Is_Empty then
         case Data.Kind is
            when On_Line =>
               Self.Send_Line
                 (Get_File (Data.Locations.First_Element.Marker),
                  Changed,
                  Add);

            when On_Subprogram =>
               Self.Send_Subprogram (Changed, Add, False);

            when On_Address =>
               Self.Send_Addresses (Changed, Add);

            when On_Exception =>
               Self.Send_Exception (Changed, Add);
         end case;
      end if;
   end Break;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Self      : DAP_Client_Breakpoint_Manager_Access;
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
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Exception;

   -----------------
   -- Break_Sorce --
   -----------------

   procedure Break_Sorce
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False)
   is
      Data    : constant Breakpoint_Data := Breakpoint_Data'
        (Kind      => On_Line,
         Num       => 0,
         Locations => Location_Vectors.To_Vector
           ((0,
            Self.Kernel.Get_Buffer_Factory.Create_Marker
              (File   => File,
               Line   => Line,
               Column => 1),
            Invalid_Address), 1),
         Disposition => (if Temporary then Delete else Keep),
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Sorce;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : DAP_Client_Breakpoint_Manager_Access;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      Data    : constant Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Subprogram,
         Num         => 0,
         Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
   begin
      Self.Break (Data);
   end Break_Subprogram;

   -----------------------------------
   -- Toggle_Instruction_Breakpoint --
   -----------------------------------

   procedure Toggle_Instruction_Breakpoint
     (Self    : DAP_Client_Breakpoint_Manager_Access;
      Address : Address_Type)
   is
      Changed : Breakpoint_Vectors.Vector;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Break_Unbreak_Address
        (Address, +Base_Name (Self.Client.Get_Executable), Changed);
      Self.Send_Addresses (Changed, Add);
   end Toggle_Instruction_Breakpoint;

   ------------------
   -- Dec_Response --
   ------------------

   procedure Dec_Response
     (Self   : in out DAP_Client_Breakpoint_Manager;
      Action : Action_Kind)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Self.Requests_Count = 0 then
         return;
      end if;

      Self.Requests_Count := Self.Requests_Count - 1;

      if Self.Requests_Count = 0
        and then Action = Init
      then
         Self.Show_Breakpoints;

         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);

         if DAP.Module.Get_Breakpoints_View /= null then
            DAP.Views.View_Access
              (DAP.Module.Get_Breakpoints_View).On_Status_Changed
              (GPS.Debuggers.Debug_Available);
         end if;
         Self.Holder.Initialized;

         Self.Client.On_Breakpoints_Set;
      end if;
   end Dec_Response;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
      return DAP.Modules.Breakpoints.Breakpoint_Vectors.Vector is
   begin
      return Self.Holder.Get_Breakpoints;
   end Get_Breakpoints;

   ----------------------
   -- Show_Breakpoints --
   ----------------------

   procedure Show_Breakpoints
     (Self : in out DAP_Client_Breakpoint_Manager) is
   begin
      DAP.Modules.Persistent_Breakpoints.Hide_Breakpoints (Self.Kernel);

      for Data of Self.Holder.Get_Breakpoints loop
         DAP.Modules.Persistent_Breakpoints.Show_Breakpoint
           (Self.Kernel, Data);
      end loop;
   end Show_Breakpoints;

   ---------------
   -- Send_Line --
   ---------------

   function Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind) return DAP_Request_Access
   is
      Req : constant Sources.Source_Line_Request_Access :=
        new Sources.Source_Line_Request (Self.Kernel);
      Sb  : DAP.Tools.SourceBreakpoint;
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.File    := File;
      Req.Action  := Action;
      Req.Sent    := Actual;

      Req.Parameters.arguments.source.name :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Base_Name (File));

      Req.Parameters.arguments.source.path :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Full_Name (File));

      Req.Parameters.arguments.sourceModified := False;

      for Data of Actual loop
         Sb.line   := Integer (GPS.Editors.Get_Line (Get_Location (Data)));
         Sb.column :=
           (Is_Set => True,
            Value  => Integer (GPS.Editors.Get_Column (Get_Location (Data))));
         Sb.condition    := Data.Condition;
         Sb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Sb);
      end loop;

      return DAP_Request_Access (Req);
   end Send_Line;

   ---------------
   -- Send_Line --
   ---------------

   procedure Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
   is
      Request : DAP_Request_Access := Self.Send_Line (File, Actual, Action);
   begin
      Self.Requests_Count := Self.Requests_Count + 1;
      Self.Client.Enqueue (Request);
   end Send_Line;

   -------------------
   -- Send_Commands --
   -------------------

   procedure Send_Commands
     (Self : DAP_Client_Breakpoint_Manager_Access;
      Data : DAP.Modules.Breakpoints.Breakpoint_Data) is
   begin
      if not Data.Commands.Is_Empty then
         Self.Client.Set_Breakpoint_Command (Data.Num, Data.Commands);
      end if;
   end Send_Commands;

   ---------------------
   -- Send_Subprogram --
   ---------------------

   function Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
      return DAP_Request_Access
   is
      Req : constant Functions.Function_Breakpoint_Request_Access :=
        new Functions.Function_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.FunctionBreakpoint;
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Sent    := Actual;

      for Data of Actual loop
         Fb.name := VSS.Strings.Conversions.To_Virtual_String
           (To_String (Data.Subprogram));
         Fb.condition    := Data.Condition;
         Fb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Fb);
      end loop;

      return DAP_Request_Access (Req);
   end Send_Subprogram;

   ---------------------
   -- Send_Subprogram --
   ---------------------

   procedure Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind;
      Bunch  : Boolean)
   is
      Current : Breakpoint_Vectors.Vector;
      Request : DAP_Request_Access;
   begin
      if Bunch
        and then not Actual.Is_Empty
      then
         for Index in Actual.First_Index .. Actual.Last_Index loop
            Current.Append (Actual.Element (Index));
            Self.Requests_Count := Self.Requests_Count + 1;
            Request := Self.Send_Subprogram (Current, Action);
            if Index = Actual.Last_Index then
               Functions.Function_Breakpoint_Request_Access
                 (Request).Last := True;
            end if;
            Self.Client.Enqueue (Request);
         end loop;

      else
         Request := Self.Send_Subprogram (Actual, Action);
         Self.Client.Enqueue (Request);
      end if;
   end Send_Subprogram;

   --------------------
   -- Send_Exception --
   --------------------

   procedure Send_Exception
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
   is
      Req : Exceptions.Exception_Breakpoint_Request_Access :=
        new Exceptions.Exception_Breakpoint_Request (Self.Kernel);
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Sent    := Actual;

      for Data of Actual loop
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

      Self.Client.Enqueue (DAP_Request_Access (Req));
   end Send_Exception;

   --------------------
   -- Send_Addresses --
   --------------------

   procedure Send_Addresses
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
   is
      Req : Instructions.Instruction_Breakpoint_Request_Access :=
        new Instructions.Instruction_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.InstructionBreakpoint;
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Sent    := Actual;

      for Data of Actual loop
         Fb.instructionReference := VSS.Strings.Conversions.To_Virtual_String
           (Address_To_String (Data.Address));
         Fb.condition    := Data.Condition;
         Fb.hitCondition := Get_Ignore (Data);

         Req.Parameters.arguments.breakpoints.Append (Fb);
      end loop;

      Self.Client.Enqueue (DAP_Request_Access (Req));
   end Send_Addresses;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Map    : Breakpoint_Hash_Maps.Map;
      Action : Action_Kind;
      Bunch  : Boolean)
   is
      use Breakpoint_Hash_Maps;
      Cursor : Breakpoint_Hash_Maps.Cursor := Map.First;
   begin
      while Has_Element (Cursor) loop
         if Key (Cursor) = Subprograms_File then
            Self.Send_Subprogram (Element (Cursor), Action, Bunch);

         elsif Key (Cursor) = Addreses_File then
            Self.Send_Addresses (Element (Cursor), Action);

         elsif Key (Cursor) = Exceptions_File then
            Self.Send_Exception (Element (Cursor), Action);

         else
            Self.Send_Line (Key (Cursor), Element (Cursor), Action);
         end if;

         Next (Cursor);
      end loop;
   end Send;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self  : DAP_Client_Breakpoint_Manager_Access;
      Nums  : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      use Breakpoint_Hash_Maps;
      Changed : Breakpoint_Hash_Maps.Map;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Set_Enabled (Nums, State, Changed);
      Self.Send (Changed, (if State then Enable else Disable), True);
   end Set_Breakpoints_State;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self : DAP_Client_Breakpoint_Manager_Access;
      File : GNATCOLL.VFS.Virtual_File;
      Line : Editable_Line_Type)
   is
      use Breakpoint_Hash_Maps;

      Changed : Breakpoint_Hash_Maps.Map;
      Updated : Boolean;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Delete (File, Line, Changed, Updated);

      if Updated then
         Self.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);
      end if;

      Self.Send (Changed, Delete, False);
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access;
      Nums : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      use Breakpoint_Hash_Maps;

      Updated : Boolean;
      Changed : Breakpoint_Hash_Maps.Map;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Delete (Nums, Changed, Updated);

      if Updated then
         Self.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);
      end if;

      Self.Send (Changed, Delete, False);
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
   is
      use Breakpoint_Hash_Maps;

      Changed : Breakpoint_Hash_Maps.Map;
      Cursor  : Breakpoint_Hash_Maps.Cursor;

      Empty : Breakpoint_Vectors.Vector;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Changed := Self.Holder.Get_For_Files;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         Self.Send_Line (Key (Cursor), Empty, Delete);
         Next (Cursor);
      end loop;

      if not Self.Holder.Get_For (On_Subprogram).Is_Empty then
         Self.Send_Subprogram (Empty, Delete, False);
      end if;

      if not Self.Holder.Get_For (On_Exception).Is_Empty then
         Self.Send_Exception (Empty, Delete);
      end if;

      if not Self.Holder.Get_For (On_Address).Is_Empty then
         Self.Send_Addresses (Empty, Delete);
      end if;

      Self.Holder.Clear;
   end Remove_All_Breakpoints;

   -------------
   -- Stopped --
   -------------

   procedure Stopped
     (Self         : DAP_Client_Breakpoint_Manager_Access;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer;
      Address      : out Address_Type)
   is
      use DAP.Tools;
      Ids : DAP.Types.Breakpoint_Identifier_Lists.List;
   begin
      Stopped_File := No_File;
      Stopped_Line := 0;

      for Index in 1 .. Length (Event.a_body.hitBreakpointIds) loop
         declare
            Num : constant Integer_Constant_Reference :=
              Event.a_body.hitBreakpointIds (Index);
         begin
            for Data of Self.Holder.Get_Breakpoints loop
               if Data = Breakpoint_Identifier (Num.Element.all) then
                  for L of Data.Locations loop
                     if L.Num = Breakpoint_Identifier (Num.Element.all) then
                        Stopped_File := GPS.Editors.Get_File (L.Marker);
                        Stopped_Line := Integer
                          (GPS.Editors.Get_Line (L.Marker));
                        Address := L.Address;

                        if Data.Disposition = Delete then
                           Ids.Append (Data.Num);
                           Self.Remove_Breakpoints (Ids);
                        end if;

                        return;
                     end if;
                  end loop;
               end if;
            end loop;
         end;
      end loop;
   end Stopped;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : DAP_Client_Breakpoint_Manager_Access) is
      use Breakpoint_Vectors;
   begin
      --  Store breakpoints in the persistent storage
      DAP.Modules.Persistent_Breakpoints.Store
        (Self.Client.Get_Executable, Self.Holder.Get_Breakpoints);
   end Finalize;

   ---------------------
   -- On_Notification --
   ---------------------

   procedure On_Notification
     (Self  : DAP_Client_Breakpoint_Manager_Access;
      Event : DAP.Tools.BreakpointEvent_body)
   is
      use DAP.Tools;
      use DAP.Tools.Enum;

      function Convert return Breakpoint_Data;
      function Convert return Breakpoint_Data
      is
         File   : constant Virtual_File :=
           (if Event.breakpoint.source.Is_Set
            then Create
              (+(VSS.Strings.Conversions.To_UTF_8_String
               (Event.breakpoint.source.Value.path)))
            else No_File);
         Holder : constant GPS.Editors.
           Controlled_Editor_Buffer_Holder :=
             Self.Kernel.Get_Buffer_Factory.Get_Holder
               (File => File);
         Data : Breakpoint_Data;
      begin
         Convert (Self.Kernel, File, Holder, Data, Event.breakpoint);
         return Data;
      end Convert;

      Data       : constant Breakpoint_Data := Convert;
      Bp_Changed : Boolean;
   begin
      case Event.reason is
         when changed =>
            Self.Holder.Changed (Data);
            GPS.Kernel.Hooks.Debugger_Breakpoint_Changed_Hook.Run
              (Self.Kernel, Self.Client.Get_Visual, Integer (Data.Num));

         when a_new =>
            Self.Holder.Added (Data);
            GPS.Kernel.Hooks.Debugger_Breakpoint_Added_Hook.Run
              (Self.Kernel, Self.Client.Get_Visual, Integer (Data.Num));

         when removed =>
            if Event.breakpoint.id.Is_Set then
               Self.Holder.Deleted
                 (Breakpoint_Identifier (Event.breakpoint.id.Value),
                  Bp_Changed);

               if Bp_Changed then
                  GPS.Kernel.Hooks.Debugger_Breakpoint_Changed_Hook.Run
                    (Self.Kernel, Self.Client.Get_Visual,
                     Event.breakpoint.id.Value);
               else
                  GPS.Kernel.Hooks.Debugger_Breakpoint_Deleted_Hook.Run
                    (Self.Kernel, Self.Client.Get_Visual,
                     Event.breakpoint.id.Value);
               end if;
            end if;
      end case;

      Self.Show_Breakpoints;
   end On_Notification;

end DAP.Modules.Breakpoint_Managers;
