------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with GPS.Kernel.Hooks;
with GPS.Debuggers;     use GPS.Debuggers;

with DAP.Clients.Stack_Trace.StackTrace;
with DAP.Modules.Preferences;
with DAP.Requests;
with DAP.Utils;
with VSS.Strings.Conversions; use VSS.Strings.Conversions;

package body DAP.Clients.Stack_Trace is

   type On_Debug_Process_Terminated is
     new GPS.Kernel.Hooks.Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debug_Process_Terminated;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class);
   --  Called when the process has terminated

   type On_Debugger_State_Changed is
     new GPS.Kernel.Hooks.Debugger_States_Hooks_Function with null record;
   overriding procedure Execute
     (Self      : On_Debugger_State_Changed;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
      New_State : GPS.Debuggers.Debugger_State);
   --  Called when the state of the debugger changes

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      GPS.Kernel.Hooks.Debugger_Process_Terminated_Hook.Add
        (new On_Debug_Process_Terminated);
      GPS.Kernel.Hooks.Debugger_State_Changed_Hook.Add
        (new On_Debugger_State_Changed);
   end Register_Module;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debug_Process_Terminated;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Client : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      if Client /= null
        and then Client.Get_Stack_Trace /= null
      then
         Client.Get_Stack_Trace.Clear;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : On_Debugger_State_Changed;
      Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger  : access GPS.Debuggers.Base_Visual_Debugger'Class;
      New_State : GPS.Debuggers.Debugger_State)
   is
      pragma Unreferenced (Self);
      Client : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      if New_State = Debug_Busy
        and then Client /= null
        and then Client.Get_Stack_Trace /= null
      then
         Client.Get_Stack_Trace.Clear;
      end if;
   end Execute;

   --------------------------
   -- Get_Current_Frame_Id --
   --------------------------

   function Get_Current_Frame_Id
     (Self : Stack_Trace_Access) return DAP.Tools.Optional_Integer is
   begin
      if Self /= null
        and then Self.Selected_Frame.Id >= 0
      then
         return (Is_Set => True, Value => Self.Selected_Frame.Id);
      else
         return (Is_Set => False);
      end if;
   end Get_Current_Frame_Id;

   --------------------------
   -- Get_Current_Frame_Id --
   --------------------------

   function Get_Current_Frame_Id (Self : Stack_Trace_Access) return Integer is
   begin
      if Self /= null then
         return Self.Selected_Frame.Id;
      else
         return -1;
      end if;
   end Get_Current_Frame_Id;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Self : Stack_Trace_Access) return GNATCOLL.VFS.Virtual_File is
   begin
      if Self /= null then
         return Self.Selected_Frame.File;
      else
         return No_File;
      end if;
   end Get_Current_File;

   ----------------------
   -- Get_Current_Line --
   ----------------------

   function Get_Current_Line (Self : Stack_Trace_Access) return Integer is
   begin
      if Self /= null then
         return Self.Selected_Frame.Line;
      else
         return 0;
      end if;
   end Get_Current_Line;

   -------------------------
   -- Get_Current_Address --
   -------------------------

   function Get_Current_Address
     (Self : Stack_Trace_Access) return Address_Type is
   begin
      if Self /= null then
         return Self.Selected_Frame.Address;
      else
         return Invalid_Address;
      end if;
   end Get_Current_Address;

   ---------------
   -- Get_Trace --
   ---------------

   function Get_Trace
     (Self : Stack_Trace_Access) return Frames_Vectors.Vector is
   begin
      if Self /= null then
         return Self.Frames;
      else
         return Frames_Vectors.Empty_Vector;
      end if;
   end Get_Trace;

   ------------------
   -- Select_Frame --
   ------------------

   procedure Select_Frame
     (Self   : Stack_Trace_Access;
      Frame  : Frame_Record;
      Client : access DAP.Clients.DAP_Client'Class) is
   begin
      Self.Selected_Frame := Frame;

      --  highlight selected location
      if Frame.File /= No_File then
         DAP.Utils.Highlight_Current_File_And_Line
           (Client.Kernel, Frame.File, Frame.Line);
      end if;

      if Client.Get_Status = Stopped then
         --  Triggering the hook to inform views only
         --  when the debugger has stopped.
         GPS.Kernel.Hooks.Debugger_Location_Changed_Hook.Run
           (Client.Kernel, Client.Get_Visual);
      end if;
   end Select_Frame;

   ------------------
   -- Select_Frame --
   ------------------

   procedure Select_Frame
     (Self   : Stack_Trace_Access;
      Id     : Natural;
      Client : access DAP.Clients.DAP_Client'Class)
   is
      Frames : Frames_Vectors.Vector renames Self.Frames;
   begin
      if Id in Integer (Frames.First_Index) .. Integer (Frames.Last_Index) then
         Self.Select_Frame
           (Frame  => Frames (Id),
            Client => Client);
      end if;
   end Select_Frame;

   --------------
   -- Frame_Up --
   --------------

   procedure Frame_Up
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class)
   is
      Next : Boolean := False;
   begin
      for Frame of Self.Frames loop
         if Next then
            Self.Select_Frame (Frame, Client);
            exit;

         elsif Frame.Id = Self.Selected_Frame.Id then
            Next := True;
         end if;
      end loop;
   end Frame_Up;

   ----------------
   -- Frame_Down --
   ----------------

   procedure Frame_Down
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class)
   is
      Next : Boolean := False;
   begin
      for Frame of reverse Self.Frames loop
         if Next then
            Self.Select_Frame (Frame, Client);
            exit;

         elsif Frame.Id = Self.Selected_Frame.Id then
            Next := True;
         end if;
      end loop;
   end Frame_Down;

   ---------------
   -- Set_Frame --
   ---------------

   procedure Set_Frame
     (Self    : Stack_Trace_Access;
      Id      : Natural;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Integer := 0;
      Address : Address_Type) is
   begin
      Self.Selected_Frame :=
        (Id, VSS.Strings.Empty_Virtual_String,
         File, Line, Address, File.Is_Regular_File);
   end Set_Frame;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : Stack_Trace_Access) is
   begin
      Self.Frames.Clear;
      Self.Selected_Frame := No_Frame;
      Self.Total_Count := 0;
   end Clear;

   procedure Merge_Response
     (Self          : Stack_Trace_Access;
      Client        : access DAP.Clients.DAP_Client'Class;
      Start_Frame   : Natural;
      Response      : DAP.Tools.StackTraceResponse;
      Append_More   : out Boolean;
      Selected_Index : out Natural)
   is
      use GNATCOLL.VFS;
      use DAP.Tools;
      Frames_Count : constant Natural :=
        Natural (Length (Response.a_body.stackFrames));
   begin
      Append_More := False;
      Selected_Index := 0;

      if Self = null then
        return;
      end if;

      if Start_Frame = 0 then
         Self.Clear;
      end if;

      for Index in 1 .. Frames_Count loop
         declare
            Frame_Ref : constant StackFrame_Variable_Reference :=
              Get_StackFrame_Variable_Reference
                (Response.a_body.stackFrames, Index);
            Location   : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
            Frame      : Frame_Record;
            App_Index  : Natural;
         begin
            if Frame_Ref.source.Is_Set then
               Location := To_File (Frame_Ref.source.Value.path);
            end if;

            Frame :=
              (Id              => Frame_Ref.id,
               Name            => Frame_Ref.name,
               File            => Location,
               Line            => Frame_Ref.line,
               Address         => Invalid_Address,
               Location_Exists => Location.Is_Regular_File);

            if not Frame_Ref.instructionPointerReference.Is_Empty then
               Frame.Address :=
                 DAP.Types.String_To_Address
                   (To_UTF8 (Frame_Ref.instructionPointerReference));
            end if;

            Self.Frames.Append (Frame);
            App_Index := Natural (Self.Frames.Last_Index);

            if Selected_Index = 0 and then Frame.Location_Exists then
               Selected_Index := App_Index;
            end if;
         end;
      end loop;

      if Response.a_body.totalFrames.Is_Set then
         Self.Total_Count := Response.a_body.totalFrames.Value;
      elsif Frames_Count = 0 and then Self.Total_Count = 0 then
         Self.Total_Count := 1;
      end if;

      Append_More := Self.Can_Upload (Client);
   end Merge_Response;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class)
   is
      Req : DAP.Clients.Stack_Trace.StackTrace.StackTrace_Request_Access :=
        DAP.Clients.Stack_Trace.StackTrace.Create
          (Client => Client,
           From   => (if Self.Frames.Is_Empty
                      then 0
                      else Self.Frames.Last_Element.Id + 1),
           Limit  => DAP.Modules.Preferences.Frames_Limit.Get_Pref);
   begin
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req), Force => True);
   end Send_Request;

   ----------------
   -- Can_Upload --
   ----------------

   function Can_Upload
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class) return Boolean is
   begin
      if Self = null then
         return False;
      end if;

      if Client.Get_Status /= Stopped then
         return False;

      elsif Self.Frames.Is_Empty then
         return True;

      elsif DAP.Modules.Preferences.Frames_Limit.Get_Pref = 0 then
         return False;

      else
         return Self.Total_Count = 0
           or else Self.Total_Count > Integer (Self.Frames.Length);
      end if;
   end Can_Upload;

end DAP.Clients.Stack_Trace;
