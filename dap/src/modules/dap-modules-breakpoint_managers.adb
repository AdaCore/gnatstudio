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

with DAP.Persistent_Breakpoints;
with DAP.Clients;
with DAP.Module;
with DAP.Views;

with Generic_Views;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Hooks;
with GPS.Debuggers;

package body DAP.Modules.Breakpoint_Managers is

   --  To-Do: remove temporary breakpoints on hit

   function Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : Virtual_File;
      Holder : GPS.Editors.Controlled_Editor_Buffer_Holder;
      Item   : DAP.Tools.Breakpoint)
      return Breakpoint_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (Kernel : GPS.Kernel.Kernel_Handle;
      File   : Virtual_File;
      Holder : GPS.Editors.Controlled_Editor_Buffer_Holder;
      Item   : DAP.Tools.Breakpoint)
      return Breakpoint_Data
   is
      Data    : Breakpoint_Data;
      Line    : Basic_Types.Editable_Line_Type := 1;
      Address : Address_Type := Invalid_Address;
   begin
      if Item.id.Is_Set then
         Data.Num := Breakpoint_Identifier (Item.id.Value);
      end if;

      if Item.line.Is_Set then
         Line := Basic_Types.Editable_Line_Type (Item.line.Value);
      end if;

      if not Item.instructionReference.Is_Empty then
         Address := String_To_Address
           (VSS.Strings.Conversions.To_UTF_8_String
              (Item.instructionReference));

         if Item.offset.Is_Set then
            Address := Set_Offset (Address, Item.offset.Value);
         end if;
      end if;

      Data.Locations := Locations_Vectors.To_Vector
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
      return Data;
   end Convert;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : DAP_Client_Breakpoint_Manager_Access) is
   begin
      if Self.Client.Get_Executable /= No_File then
         Self.Holder.Initialize
           (DAP.Persistent_Breakpoints.Get_Persistent_For_Executable
              (Self.Client.Get_Executable));

         if not Self.Holder.Get_For_Files.Is_Empty then
            for Vector of Self.Holder.Get_For_Files loop
               Self.Send_Line
                 (GPS.Editors.Get_File (Get_Location (Vector.First_Element)),
                  Vector,
                  Init);
            end loop;
         end if;

         if not Self.Holder.Get_For_Subprograms.Is_Empty then
            Self.Send_Subprogram
              (Empty_Breakpoint_Data, Self.Holder.Get_For_Subprograms, Init);
         end if;
      end if;

      if Self.Client.Get_Executable = No_File
        or else Self.Requests_Count = 0
      then
         Self.Client.On_Ready;
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
      Id      : constant Breakpoint_Identifier :=
        DAP.Persistent_Breakpoints.Get_Next_Id;
      Data    : Breakpoint_Data := Breakpoint_Data'
        (Id        => Id,
         Num       => Id,
         Locations => Locations_Vectors.To_Vector
           ((0, Self.Kernel.Get_Buffer_Factory.Create_Marker
            (File   => File,
             Line   => Line,
             Column => 1),
            Invalid_Address), 1),
         Disposition => (if Temporary then Delete else Keep),
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
      Changed : Breakpoint_Hash_Maps.Map;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Add (Data, Changed);

      if not Changed.Is_Empty then
         Self.Send_Line (File, Changed.Element (File), Add);
      end if;
   end Break_Sorce;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : DAP_Client_Breakpoint_Manager_Access;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      Id      : constant Breakpoint_Identifier :=
        DAP.Persistent_Breakpoints.Get_Next_Id;
      Data    : Breakpoint_Data := Breakpoint_Data'
        (Id  => Id,
         Num => Id,
         Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         Executable  => To_Unbounded_String
           (+Base_Name (Self.Client.Get_Executable)),
         others      => <>);
      Changed : Breakpoint_Hash_Maps.Map;
   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Add (Data, Changed);
      if not Changed.Is_Empty then
         Self.Send_Subprogram (Data, Changed.Element (No_File), Add);
      end if;
   end Break_Subprogram;

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
      end if;
   end Dec_Response;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
      return DAP.Breakpoint_Maps.Breakpoint_Vectors.Vector is
   begin
      return Self.Holder.Get_Breakpoints_List;
   end Get_Breakpoints;

   ----------------------
   -- Show_Breakpoints --
   ----------------------

   procedure Show_Breakpoints
     (Self : in out DAP_Client_Breakpoint_Manager) is
   begin
      DAP.Persistent_Breakpoints.Hide_Breakpoints (Self.Kernel);

      for Data of Self.Holder.Get_Breakpoints_List loop
         DAP.Persistent_Breakpoints.Show_Breakpoint (Self.Kernel, Data);
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
      Self.Requests_Count := Self.Requests_Count + 1;

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
      Self.Client.Enqueue (Request);
   end Send_Line;

   ---------------------
   -- Send_Subprogram --
   ---------------------

   function Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
      return DAP_Request_Access
   is
      Req : constant Function_Breakpoint_Request_Access :=
        new Function_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.FunctionBreakpoint;
   begin
      Self.Requests_Count := Self.Requests_Count + 1;

      Req.Manager := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Action  := Action;
      Req.Data    := Data;
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
      Data   : Breakpoint_Data;
      Actual : Breakpoint_Vectors.Vector;
      Action : Action_Kind)
   is
      Request : DAP_Request_Access := Self.Send_Subprogram
        (Data, Actual, Action);
   begin
      Self.Client.Enqueue (Request);
   end Send_Subprogram;

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
      Cursor  : Breakpoint_Hash_Maps.Cursor;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Set_Enabled (Nums, State, Changed);

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         if Key (Cursor) = No_File then
            Self.Send_Subprogram
              (Empty_Breakpoint_Data, Element (Cursor), Change_Status);
         else
            Self.Send_Line (Key (Cursor), Element (Cursor), Change_Status);
         end if;

         Next (Cursor);
      end loop;
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

      Updated : Boolean;
      Changed : Breakpoint_Hash_Maps.Map;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Delete (File, Line, Updated, Changed);

      if Updated then
         Self.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);
      end if;

      if not Changed.Is_Empty then
         if Key (Changed.First) = No_File then
            Self.Send_Subprogram
              (Empty_Breakpoint_Data, Changed.Element (No_File), Delete);
         else
            Self.Send_Line (File, Changed.Element (File), Delete);
         end if;
      end if;
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access;
      List : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      use Breakpoint_Hash_Maps;

      Updated : Boolean;
      Changed : Breakpoint_Hash_Maps.Map;
      Cursor  : Breakpoint_Hash_Maps.Cursor;

   begin
      if Self.Requests_Count /= 0 then
         Self.Client.Display_In_Debugger_Console
           ("Can't change breakpoints, another request is in progerss");
         return;
      end if;

      Self.Holder.Delete (List, Updated, Changed);

      if Updated then
         Self.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Client.Get_Visual);
      end if;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         if Key (Cursor) = No_File then
            Self.Send_Subprogram
              (Empty_Breakpoint_Data, Element (Cursor), Delete);
         else
            Self.Send_Line (Key (Cursor), Element (Cursor), Delete);
         end if;

         Next (Cursor);
      end loop;
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

      Self.Holder.Delete_Disabled;
      Changed := Self.Holder.Get_For_Files;

      Cursor := Changed.First;
      while Has_Element (Cursor) loop
         Self.Send_Line (Key (Cursor), Empty, Delete);
         Next (Cursor);
      end loop;

      if not Self.Holder.Get_For_Subprograms.Is_Empty then
         Self.Send_Subprogram (Empty_Breakpoint_Data, Empty, Delete);
      end if;
   end Remove_All_Breakpoints;

   -------------
   -- Stopped --
   -------------

   procedure Stopped
     (Self         : DAP_Client_Breakpoint_Manager;
      Event        : in out DAP.Tools.StoppedEvent;
      Stopped_File : out GNATCOLL.VFS.Virtual_File;
      Stopped_Line : out Integer)
   is
      use DAP.Tools;
   begin
      Stopped_File := No_File;
      Stopped_Line := 0;

      for Index in 1 .. Length (Event.a_body.hitBreakpointIds) loop
         declare
            Num : constant Integer_Constant_Reference :=
              Event.a_body.hitBreakpointIds (Index);
         begin
            for Data of Self.Holder.Get_Breakpoints_List loop
               if Data = Breakpoint_Identifier (Num.Element.all) then
                  for L of Data.Locations loop
                     if L.Num = Breakpoint_Identifier (Num.Element.all) then
                        Stopped_File := GPS.Editors.Get_File
                          (Get_Location (Data));
                        Stopped_Line := Integer
                          (GPS.Editors.Get_Line (Get_Location (Data)));
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

   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            declare
               Actual  : Breakpoint_Vectors.Vector;
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Actual.Append
                    (Convert
                       (Self.Kernel, Self.File, Holder,
                        Result.a_body.breakpoints (Index)));
               end loop;

               Self.Manager.Holder.Initialized_For_File
                 (Self.File, Actual, Changed);

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Feedback);
               end if;
            end;

         when Add =>
            declare
               Data    : Breakpoint_Data;
               List    : Breakpoint_Vectors.Vector;
               Updated : Boolean;
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Self.Sent.Last_Index loop
                  Data := Self.Sent.Element (Index);
                  Copy (Data, Convert
                        (Self.Kernel, Self.File, Holder,
                           Result.a_body.breakpoints (Index)));
                  List.Append (Data);
               end loop;

               Self.Manager.Holder.Added (Self.File, List, Changed, Updated);

               if Changed.Contains (Self.File) then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Feedback);
               else
                  if Updated then
                     Self.Manager.Show_Breakpoints;
                     GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                       (Self.Kernel, Self.Manager.Client.Get_Visual);
                  end if;
               end if;
            end;

         when Edit =>
            --  Editing is not implemented yet
            null;

         when Delete =>
            --  Remove the breakpoint(s)
            declare
               Data    : Breakpoint_Data;
               List    : Breakpoint_Vectors.Vector;
               Updated : Boolean;
            begin
               for Index in 1 .. Self.Sent.Last_Index loop
                  Data := Self.Sent.Element (Index);
                  Copy (Data, Convert
                        (Self.Kernel, Self.File, Holder,
                           Result.a_body.breakpoints (Index)));
                  List.Append (Data);
               end loop;

               Self.Manager.Holder.Deleted (Self.File, List, Updated);

               if Updated then
                  Self.Manager.Show_Breakpoints;
                  GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                    (Self.Kernel, Self.Manager.Client.Get_Visual);
               end if;
            end;

         when Change_Status =>
            declare
               Data    : Breakpoint_Data;
               List    : Breakpoint_Vectors.Vector;
               Updated : Boolean;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Copy (Data,
                        Convert
                          (Self.Kernel, Self.File, Holder,
                           Result.a_body.breakpoints (Index)));
                  List.Append (Data);
               end loop;

               Self.Manager.Holder.Actual (Self.File, List, Updated);

               --  Update visual representation
               if Updated then
                  Self.Manager.Show_Breakpoints;
                  GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                    (Self.Kernel, Self.Manager.Client.Get_Visual);
               end if;
            end;

         when Feedback =>
            declare
               Data : Breakpoint_Data;
               List : Breakpoint_Vectors.Vector;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Copy (Data,
                        Convert
                          (Self.Kernel, Self.File, Holder,
                           Result.a_body.breakpoints (Index)));
                  List.Append (Data);
               end loop;

               Self.Manager.Holder.Synch (Self.File, List);

               Self.Manager.Show_Breakpoints;
               GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                 (Self.Kernel, Self.Manager.Client.Get_Visual);
            end;
      end case;

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

      -------------
      -- Convert --
      -------------

      function Convert (Index : Integer) return Breakpoint_Data;
      function Convert (Index : Integer) return Breakpoint_Data
      is
         DAP_Bp : constant DAP.Tools.Breakpoint :=
           Result.a_body.breakpoints (Index);
         File   : constant Virtual_File := Create
           (+(VSS.Strings.Conversions.To_UTF_8_String
            (DAP_Bp.source.Value.path)));
         Holder : constant GPS.Editors.
           Controlled_Editor_Buffer_Holder :=
             Self.Kernel.Get_Buffer_Factory.Get_Holder
               (File => File);
      begin
         return Convert (Self.Kernel, File, Holder, DAP_Bp);
      end Convert;

   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            declare
               Actual  : Breakpoint_Vectors.Vector;
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Actual.Append (Convert (Index));
               end loop;

               Self.Manager.Holder.Initialized_For_Subprograms
                 (Actual, Changed);

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Subprogram
                    (Empty_Breakpoint_Data,
                     Changed.Element (No_File), Feedback);
               end if;
            end;

         when Add =>
            declare
               Changed : Breakpoint_Hash_Maps.Map;
               Updated : Boolean;
               List    : Breakpoint_Vectors.Vector;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  List.Append (Convert (Index));
               end loop;

               Self.Manager.Holder.Added_Subprogram
                 (Self.Data, List, Changed, Updated);

               if Changed.Contains (No_File) then
                  New_Request := Self.Manager.Send_Subprogram
                    (Empty_Breakpoint_Data,
                     Changed.Element (No_File), Feedback);
               else
                  if Updated then
                     Self.Manager.Show_Breakpoints;
                     GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                       (Self.Kernel, Self.Manager.Client.Get_Visual);
                  end if;
               end if;
            end;

         when Edit =>
            --  Editing is not implemented yet
            null;

         when Delete =>
            declare
               Data, D : Breakpoint_Data;
               Count   : Integer;
               List    : Breakpoint_Vectors.Vector;
               Updated : Boolean;
               Idx     : Integer := 1;
            begin
               for Index in 1 .. Self.Sent.Last_Index loop
                  Data  := Self.Sent.Element (Index);
                  Count := Data.Locations.Last_Index;
                  Copy (Data, Convert (Idx));
                  Idx := Idx + 1;
                  for Index in 2 .. Count loop
                     Copy (D, Convert (Idx));
                     Data.Locations.Append (D.Locations.First_Element);
                     Idx := Idx + 1;
                  end loop;
                  List.Append (Data);
               end loop;

               Self.Manager.Holder.Deleted_Subprogram (List, Updated);

               if Updated then
                  Self.Manager.Show_Breakpoints;
                  GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                    (Self.Kernel, Self.Manager.Client.Get_Visual);
               end if;
            end;

         when Change_Status =>
            declare
               List    : Breakpoint_Vectors.Vector;
               Updated : Boolean;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  List.Append (Convert (Index));
               end loop;

               Self.Manager.Holder.Actual_Subprogram (List, Updated);

               --  Update visual representation
               if Updated then
                  Self.Manager.Show_Breakpoints;
                  GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
                    (Self.Kernel, Self.Manager.Client.Get_Visual);
               end if;
            end;

         when Feedback =>
            Self.Manager.Show_Breakpoints;
      end case;

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : DAP_Client_Breakpoint_Manager_Access) is
      use Breakpoint_Vectors;
   begin
      --  Store breakpoints in the persistent storage
      DAP.Persistent_Breakpoints.Store
        (Self.Client.Get_Executable,
         Self.Holder.Get_Breakpoints_List & Self.Holder.Get_Pending);
   end Finalize;

end DAP.Modules.Breakpoint_Managers;
