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

with Ada.Strings.Unbounded;

with GPS.Kernel.Hooks;

with DAP.Requests;
with DAP.Utils;

with DAP.Modules.Preferences;
with DAP.Clients.Stack_Trace.StackTrace;

package body DAP.Clients.Stack_Trace is

   --------------------------
   -- Get_Current_Frame_Id --
   --------------------------

   function Get_Current_Frame_Id
     (Self : Stack_Trace_Access) return DAP.Tools.Optional_Integer is
   begin
      if Self.Selected_Frame.Id >= 0 then
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
      return Self.Selected_Frame.Id;
   end Get_Current_Frame_Id;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Self : Stack_Trace_Access) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Selected_Frame.File;
   end Get_Current_File;

   ----------------------
   -- Get_Current_Line --
   ----------------------

   function Get_Current_Line (Self : Stack_Trace_Access) return Integer is
   begin
      return Self.Selected_Frame.Line;
   end Get_Current_Line;

   -------------------------
   -- Get_Current_Address --
   -------------------------

   function Get_Current_Address
     (Self : Stack_Trace_Access) return Address_Type is
   begin
      return Self.Selected_Frame.Address;
   end Get_Current_Address;

   ---------------
   -- Get_Trace --
   ---------------

   function Get_Trace
     (Self : Stack_Trace_Access) return Frames_Vectors.Vector is
   begin
      return Self.Frames;
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
        (Id, Ada.Strings.Unbounded.Null_Unbounded_String,
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
