------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023-2026, AdaCore                     --
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

--  Module that incapsulate StackTrace information

with GNATCOLL.VFS; use GNATCOLL.VFS;

with DAP.Types;    use DAP.Types;
with DAP.Tools;    use DAP.Tools;

package DAP.Clients.Stack_Trace is

   type Stack_Trace is tagged private;
   type Stack_Trace_Access is access all Stack_Trace'Class;

   function Get_Current_Frame_Id
     (Self : Stack_Trace_Access)
      return DAP.Tools.Optional_Integer;
   --  Returns the currently selected frame ID

   function Get_Current_Frame_Id (Self : Stack_Trace_Access) return Integer;
   --  Returns the currently selected frame ID

   function Get_Current_File
     (Self : Stack_Trace_Access) return GNATCOLL.VFS.Virtual_File;
   --  Returns the file for current frame

   function Get_Current_Line (Self : Stack_Trace_Access) return Integer;
   --  Returns the line for current frame

   function Get_Current_Address
     (Self : Stack_Trace_Access) return Address_Type;
   --  Returns the address for current frame

   function Get_Trace (Self : Stack_Trace_Access) return Frames_Vectors.Vector;
   --  Returns loaded stack trace

   procedure Frame_Up
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class);
   --  Select the next frame as the current

   procedure Frame_Down
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class);
   --  Select the previous frame as the current

   procedure Select_Frame
     (Self   : Stack_Trace_Access;
      Id     : Natural;
      Client : access DAP.Clients.DAP_Client'Class);
   --  Select the frame with the ID.
   --  If the frame's location exists, the editor's cursor will be set to it,
   --  opening a new editor if needed.
   --  Do nothing if there is no frame associated with the given ID.

   procedure Set_Frame
     (Self    : Stack_Trace_Access;
      Id      : Natural;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Integer := 0;
      Address : Address_Type);
   --  Set the frame as current. Used from notifications.

   procedure Send_Request
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class);
   --  Send the request to get the traces.

   function Can_Upload
     (Self   : Stack_Trace_Access;
      Client : access DAP.Clients.DAP_Client'Class) return Boolean;
   --  Returns True when we don't load all frames and can send a request to
   --  upload the next chunk of data

   procedure Clear (Self : Stack_Trace_Access);
   --  Clear the information about backtraces

   procedure Register_Module;
   --  Register the functions needed to work properly

private

   type Stack_Trace is tagged record
      Selected_Frame : Frame_Record := No_Frame;
      Frames         : Frames_Vectors.Vector;
      Total_Count    : Natural := 0;
   end record;

   procedure Select_Frame
     (Self   : Stack_Trace_Access;
      Frame  : Frame_Record;
      Client : access DAP.Clients.DAP_Client'Class);

end DAP.Clients.Stack_Trace;
