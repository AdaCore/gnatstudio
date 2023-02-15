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

package body DAP.Modules.Breakpoint_Managers is

   procedure Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : Virtual_File;
      Holder : GPS.Editors.Controlled_Editor_Buffer_Holder;
      Data   : in out Breakpoint_Data;
      Item   : DAP.Tools.Breakpoint);

   function Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      DAP_Bp : DAP.Tools.Breakpoint) return Breakpoint_Data;

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
            (DAP_Bp.source.Value.path)))
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
      Map         : Breakpoint_Hash_Maps.Map;
      Subprograms : Breakpoint_Vectors.Vector;
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

         Subprograms := Self.Holder.Get_For_Subprograms;
         if not Subprograms.Is_Empty then
            Self.Send_Subprogram (Subprograms, Init, True);
         end if;
      end if;

      if Self.Client.Get_Executable = No_File
        or else Self.Requests_Count = 0
      then
         Self.Requests_Count := 1;
         Self.Dec_Response (Init);
      end if;
   end Initialize;

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
        (Num       => 0,
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
      Changed : Breakpoint_Vectors.Vector;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Add (Data, Changed);
      Self.Send_Line (File, Changed, Add);
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
        (Num         => 0,
         Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
      Changed : Breakpoint_Vectors.Vector;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Add_Subprogram (Data, Changed);
      if not Changed.Is_Empty then
         Self.Send_Subprogram (Changed, Add, False);
      end if;
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

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Source_Line_Request) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Breakpoints.On_Rejected
        (DAP.Requests.Breakpoints.Breakpoint_DAP_Request (Self));
   end On_Rejected;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Function_Breakpoint_Request) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Function_Breakpoints.On_Rejected
        (DAP.Requests.Function_Breakpoints.Function_Breakpoint_DAP_Request
           (Self));
   end On_Rejected;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Instruction_Breakpoint_Request) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Instruction_Breakpoints.On_Rejected
        (DAP.Requests.Instruction_Breakpoints.
           Instruction_Breakpoint_DAP_Request (Self));
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Source_Line_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Breakpoints.On_Error_Message
        (DAP.Requests.Breakpoints.Breakpoint_DAP_Request (Self), Message);
   end On_Error_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Function_Breakpoints.On_Error_Message
        (DAP.Requests.Function_Breakpoints.Function_Breakpoint_DAP_Request
           (Self), Message);
   end On_Error_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Instruction_Breakpoint_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Instruction_Breakpoints.On_Error_Message
        (DAP.Requests.Instruction_Breakpoints.
           Instruction_Breakpoint_DAP_Request (Self), Message);
   end On_Error_Message;

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
         Self.Client.On_Ready;
         Self.Show_Breakpoints;

         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);

         if DAP.Module.Get_Breakpoints_View /= null then
            DAP.Views.View_Access
              (DAP.Module.Get_Breakpoints_View).On_Status_Changed
              (GPS.Debuggers.Debug_Available);
         end if;
         Self.Holder.Initialized;
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
      Req : constant Source_Line_Request_Access :=
        new Source_Line_Request (Self.Kernel);
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

   ---------------------
   -- Send_Subprogram --
   ---------------------

   function Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
      return DAP_Request_Access
   is
      Req : constant Function_Breakpoint_Request_Access :=
        new Function_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.FunctionBreakpoint;
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Sent    := Actual;

      for Data of Actual loop
         Fb.name := VSS.Strings.Conversions.To_Virtual_String
           (To_String (Data.Subprogram));
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
               Function_Breakpoint_Request_Access (Request).Last := True;
            end if;
            Self.Client.Enqueue (Request);
         end loop;

      else
         Request := Self.Send_Subprogram (Actual, Action);
         Self.Client.Enqueue (Request);
      end if;
   end Send_Subprogram;

   --------------------
   -- Send_Addresses --
   --------------------

   procedure Send_Addresses
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
   is
      Req : Instruction_Breakpoint_Request_Access :=
        new Instruction_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.InstructionBreakpoint;
   begin
      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Sent    := Actual;

      for Data of Actual loop
         Fb.instructionReference := VSS.Strings.Conversions.To_Virtual_String
           (Address_To_String (Data.Address));
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
         if Key (Cursor) = Subprogram_File then
            Self.Send_Subprogram (Element (Cursor), Action, Bunch);

         elsif Key (Cursor) = Address_File then
            Self.Send_Addresses (Element (Cursor), Action);

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

      if not Self.Holder.Get_For_Subprograms.Is_Empty then
         Self.Send_Subprogram (Empty, Delete, False);
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

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Source_Line_Request;
      Result      : in out DAP.Tools.SetBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Update : Boolean := False;

   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            declare
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Convert (Self.Kernel, Self.File, Holder, Data,
                           Result.a_body.breakpoints (Index));
                  Actual.Append (Data);
               end loop;

               --  Update breakpoints data like numbers and locations
               Self.Manager.Holder.Initialized_For_File
                 (Self.File, Actual, Changed);

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Synch);
               end if;
            end;
            Update := True;

         when Add =>
            declare
               Changed : Breakpoint_Vectors.Vector;
            begin
               Data := Self.Sent.Last_Element;
               Convert
                 (Self.Kernel, Self.File, Holder, Data,
                  Result.a_body.breakpoints
                    (Length (Result.a_body.breakpoints)));

               --  Update the breakpoint data because in notifications we
               --  don't have full information like disposition and so on
               Self.Manager.Holder.Added (Self.File, Data, Changed, Update);

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed, Synch);
               end if;
            end;

         when Delete =>
            --  Do nothing because we delete breakpoints by notification
            null;

         when Enable =>
            declare
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Convert (Self.Kernel, Self.File, Holder, Data,
                           Result.a_body.breakpoints (Index));
                  Actual.Append (Data);
               end loop;

               Self.Manager.Holder.Status_Changed (Self.File, Actual, Changed);

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Synch);
               else
                  Update := True;
               end if;
            end;

         when Disable =>
            --  Do nothing because we delete BP by notifications
            Update := True;

         when Synch =>
            --  Do nothing because we already have all data
            null;
      end case;

      --  Update visual representation
      if Update then
         Self.Manager.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Manager.Client.Get_Visual);
      end if;

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_Request;
      Result      : in out DAP.Tools.SetFunctionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Update : Boolean := False;
      Actual : Breakpoint_Vectors.Vector;
   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Initialized_For_Subprograms
              (Actual, Self.Last);

         when Add =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Added_Subprogram
              (Self.Sent.Last_Element, Actual);
            Update := True;

         when Delete =>
            --  Do nothing because we delete breakpoints by notification
            null;

         when Enable =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Subprogram_Status_Changed (Actual, Self.Last);
            Update := Self.Last;

         when Disable =>
            --  Do nothing because we delete BP by notifications
            Update := True;

         when Synch =>
            --  Do nothing because we already have all data
            null;
      end case;

      if Update then
         Self.Manager.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Manager.Client.Get_Visual);
      end if;

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Instruction_Breakpoint_Request;
      Result      : in out DAP.Tools.SetInstructionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Actual : Breakpoint_Vectors.Vector;
      Data   : Breakpoint_Data;
   begin
      New_Request := null;

      case Self.Action is
         when Init | Delete | Disable | Synch =>
            --  Do nothing
            null;

         when Add =>
            if not Self.Sent.Is_Empty then
               Data := Self.Sent.Last_Element;
               Data.Locations := Convert
                 (Self.Kernel,
                  Result.a_body.breakpoints
                    (Length (Result.a_body.breakpoints))).Locations;
               Data.Num := Data.Locations.First_Element.Num;

               Self.Manager.Holder.Address_Response (Data);
            end if;

         when Enable =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Address_Status_Changed (Actual);
      end case;

      Self.Manager.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Manager.Client.Get_Visual);

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

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

   begin
      case Event.reason is
         when changed =>
            Self.Holder.Changed (Convert);

         when a_new =>
            Self.Holder.Added (Convert);

         when removed =>
            if Event.breakpoint.id.Is_Set then
               Self.Holder.Deleted
                 (Breakpoint_Identifier (Event.breakpoint.id.Value));
            end if;
      end case;

      Self.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
      (Self.Kernel, Self.Client.Get_Visual);
   end On_Notification;

end DAP.Modules.Breakpoint_Managers;
