------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.VFS;
with VSS.Strings.Conversions;

with GPS.Editors;
with DAP.Requests.ConfigurationDone;

package body DAP.Requests.Breakpoints is

   Me : constant Trace_Handle := Create ("DAP.Requests.Breakpoints", On);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Breakpoint_DAP_Request;
      Map  : Breakpoint_Map) is
   begin
      Self.Map     := Map;
      Self.Current := Self.Map.First;
      Self.Prepare_Data;
   end Initialize;

   ------------------
   -- Prepare_Data --
   ------------------

   procedure Prepare_Data (Self : in out Breakpoint_DAP_Request) is
      sb : DAP.Tools.SourceBreakpoint;
   begin
      Self.Parameters.arguments.a_source.name :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Base_Name
             (GPS.Editors.Get_File
                (Self.Map (Self.Current).First_Element.Location)));

      Self.Parameters.arguments.a_source.path :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Full_Name
             (GPS.Editors.Get_File
                (Self.Map (Self.Current).First_Element.Location)));

      Self.Parameters.arguments.sourceModified := False;

      for E in Self.Map (Self.Current).Iterate loop
         sb.line := LSP.Types.LSP_Number
           (GPS.Editors.Get_Line (Self.Map (Self.Current)(E).Location));
         sb.column := 0;
         Self.Parameters.arguments.breakpoints.Append (sb);
      end loop;

      DAP.Breakpoint_Maps.Breakpoint_Hash_Maps.Next (Self.Current);
   end Prepare_Data;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Breakpoint_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      DAP.Tools.SetBreakpointsRequest'Write (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Breakpoint_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.SetBreakpointsResponse;
   begin
      DAP.Tools.SetBreakpointsResponse'Read (Stream, Response);
      Breakpoint_DAP_Request'Class
        (Self).On_Result_Message (Response, New_Request);
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Breakpoint_DAP_Request;
      Result      : DAP.Tools.SetBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Breakpoint_Maps.Breakpoint_Hash_Maps;
   begin
      --  Store breakpoint position

      --  Send new portion
      if Self.Current /= No_Element then
         declare
            R : constant Breakpoint_DAP_Request_Access :=
              new Breakpoint_DAP_Request (Self.Kernel);
         begin
            R.Map := Self.Map;
            R.Current := Self.Current;
            R.Prepare_Data;
            New_Request := DAP_Request_Access (R);
         end;

      else
         declare
            Done : constant DAP.Requests.ConfigurationDone.
              ConfigurationDone_DAP_Request_Access :=
                new DAP.Requests.ConfigurationDone.
                  ConfigurationDone_DAP_Request (Self.Kernel);
         begin
            New_Request := DAP_Request_Access (Done);
         end;
      end if;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Breakpoint_DAP_Request) is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Breakpoint_DAP_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Breakpoint_DAP_Request;
      Id   : LSP.Types.LSP_Number) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Breakpoints;
