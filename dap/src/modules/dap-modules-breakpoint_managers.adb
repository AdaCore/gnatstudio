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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with VSS.Strings.Conversions;

with Gtkada.MDI;              use Gtkada.MDI;

with DAP.Persistent_Breakpoints;
with DAP.Views.Breakpoints;
with DAP.Clients;

with GPS.Default_Styles;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.GtkAda;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;   use GPS.Kernel.Messages.Simple;
with Generic_Views;

package body DAP.Modules.Breakpoint_Managers is

   --  To-Do: remove temporary breakpoints on hit
   --         use Breakpoint.Enabled

   Debugger_Messages_Category : constant String := "debugger-current-line";
   Current_Line_Pixbuf        : constant Unbounded_String :=
     To_Unbounded_String ("gps-emblem-debugger-current");

   -----------------
   -- Break_Sorce --
   -----------------

   procedure Break_Sorce
     (Self      : DAP_Client_Breakpoint_Manager_Access;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False)
   is
      Vector : Breakpoint_Vectors.Vector :=
        Self.Actual.Sources.Get_For_File (File);
   begin
      Vector.Append
        (Breakpoint_Data'
           (Location => Self.Kernel.Get_Buffer_Factory.Create_Marker
                (File   => File,
                 Line   => Line,
                 Column => 1),
            Disposition => (if Temporary then Delete else Keep),
            others      => <>));
      Self.Send_Line (File, Vector);
   end Break_Sorce;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : DAP_Client_Breakpoint_Manager_Access;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      Vector : Breakpoint_Vectors.Vector := Self.Actual.Subprograms;
   begin
      Vector.Append
        (Breakpoint_Data'
        (Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         others      => <>));
      Self.Send_Subprogram (Vector);
   end Break_Subprogram;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : DAP_Client_Breakpoint_Manager_Access) is
   begin
      DAP.Persistent_Breakpoints.Debugger_Initialization;
      for Vector of DAP.Persistent_Breakpoints.
        Get_Persistent_Breakpoints.Sources
      loop
         Self.Requests_Count := Self.Requests_Count + 1;
         Self.Send_Line
           (GPS.Editors.Get_File (Vector.First_Element.Location), Vector);
      end loop;

      if not DAP.Persistent_Breakpoints.Get_Persistent_Breakpoints.
        Subprograms.Is_Empty
      then
         Self.Requests_Count := Self.Requests_Count + 1;
         Self.Send_Subprogram
           (DAP.Persistent_Breakpoints.Get_Persistent_Breakpoints.Subprograms);
      end if;
   end Initialize;

   -----------------
   -- On_Finished --
   -----------------

   procedure On_Finished (Self : DAP_Client_Breakpoint_Manager_Access) is
   begin
      DAP.Persistent_Breakpoints.Synchronize (Self.Kernel, Self.Actual);
   end On_Finished;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Source_Line_Request;
      Result      : in out DAP.Tools.SetBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use type Generic_Views.Abstract_View_Access;

      use DAP.Tools;
      use Breakpoint_Vectors;

      Vector : Breakpoint_Vectors.Vector := Self.Actual;
      Cursor : Breakpoint_Vectors.Cursor := Vector.First;
      Data   : Breakpoint_Data;

   begin
      New_Request := null;

      if Length (Result.a_body.breakpoints) > 0 then
         declare
            Holder : constant GPS.Editors.
              Controlled_Editor_Buffer_Holder :=
                Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);
         begin
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               --  Skip disabled breakpoints
               while Has_Element (Cursor)
                 and then not Element (Cursor).Enabled
               loop
                  Next (Cursor);
               end loop;

               exit when not Has_Element (Cursor);

               declare
                  DAP_Bp : constant Breakpoint :=
                    Result.a_body.breakpoints (Index);
                  Line   : Basic_Types.Editable_Line_Type := 1;
               begin
                  Data := Element (Cursor);
                  if DAP_Bp.id.Is_Set then
                     Data.Num := DAP.Types.Breakpoint_Identifier
                       (DAP_Bp.id.Value);
                  end if;

                  if DAP_Bp.line.Is_Set then
                     Line := Basic_Types.Editable_Line_Type
                       (DAP_Bp.line.Value);
                  end if;

                  Data.Location := Self.Kernel.Get_Buffer_Factory.
                    Create_Marker
                      (File   => Self.File,
                       Line   => Line,
                       Column => Holder.Editor.Expand_Tabs
                         (Line,
                          (if DAP_Bp.column.Is_Set
                           then Basic_Types.Character_Offset_Type
                             (DAP_Bp.column.Value)
                           else 1)));

                  Vector.Replace_Element (Cursor, Data);
                  Next (Cursor);
               end;
            end loop;
         end;
      end if;

      Self.List.Actual.Sources.Set_For_File (Self.File, Vector);
      Self.List.Dec_Response;

      Self.List.Show_Breakpoints;
      if Self.List.Client.Get_Breakpoints_View /= null then
         DAP.Views.View_Access
           (Self.List.Client.Get_Breakpoints_View).Update;
      end if;
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
      Idx : Integer;
   begin
      New_Request := null;
      Self.List.Actual.Subprograms := Self.Actual;
      Idx := Self.List.Actual.Subprograms.First_Index;

      for Index in 1 .. Length (Result.a_body.breakpoints) loop
         while Idx <= Self.List.Actual.Subprograms.Last_Index
           and then not Self.List.Actual.Subprograms.Element (Idx).Enabled
         loop
            --  Skip disabled breakpoints
            Idx := Idx + 1;
         end loop;

         exit when Idx > Self.List.Actual.Subprograms.Last_Index;

         declare
            DAP_Bp : constant Breakpoint :=
              Result.a_body.breakpoints (Index);
         begin
            if DAP_Bp.source.Is_Set
              and then DAP_Bp.line.Is_Set
            then
               declare
                  File   : constant Virtual_File := Create
                    (+(VSS.Strings.Conversions.To_UTF_8_String
                     (DAP_Bp.source.Value.path)));
                  Holder : constant GPS.Editors.
                    Controlled_Editor_Buffer_Holder :=
                      Self.Kernel.Get_Buffer_Factory.Get_Holder (File => File);
                  Line   : constant Basic_Types.Editable_Line_Type :=
                    Basic_Types.Editable_Line_Type
                      (DAP_Bp.line.Value);
                  Bp     : Breakpoint_Data :=
                    Self.List.Actual.Subprograms.Element (Idx);
               begin
                  if DAP_Bp.id.Is_Set then
                     Bp.Num := DAP.Types.Breakpoint_Identifier
                       (DAP_Bp.id.Value);
                  end if;

                  Bp.Location := Self.Kernel.Get_Buffer_Factory.
                    Create_Marker
                      (File   => File,
                       Line   => Line,
                       Column => Holder.Editor.Expand_Tabs
                         (Line,
                          Basic_Types.Character_Offset_Type
                            ((if DAP_Bp.column.Is_Set
                             then DAP_Bp.column.Value
                             else 1))));

                  Self.List.Actual.Subprograms.Replace_Element (Idx, Bp);
                  Idx := Idx + 1;
               end;
            end if;
         end;
      end loop;

      Self.List.Dec_Response;
      Self.List.Show_Breakpoints;
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Source_Line_Request) is
   begin
      Self.List.Dec_Response;
      DAP.Requests.Breakpoints.On_Rejected
        (DAP.Requests.Breakpoints.Breakpoint_DAP_Request (Self));
   end On_Rejected;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Function_Breakpoint_Request) is
   begin
      Self.List.Dec_Response;
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
      Self.List.Dec_Response;
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
      Self.List.Dec_Response;
      DAP.Requests.Function_Breakpoints.On_Error_Message
        (DAP.Requests.Function_Breakpoints.Function_Breakpoint_DAP_Request
           (Self), Message);
   end On_Error_Message;

   ------------------
   -- Dec_Response --
   ------------------

   procedure Dec_Response
     (Self : in out DAP_Client_Breakpoint_Manager) is
   begin
      if Self.Requests_Count = 0 then
         return;
      end if;

      Self.Requests_Count := Self.Requests_Count - 1;

      if Self.Requests_Count = 0 then
         Self.Client.On_Ready;
         Self.Show_Breakpoints;
         DAP.Views.Breakpoints.Attach_View (Self.Kernel, Self.Client);
      end if;
   end Dec_Response;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
      return All_Breakpoints is
   begin
      return Self.Actual;
   end Get_Breakpoints;

   ----------------------
   -- Show_Breakpoints --
   ----------------------

   procedure Show_Breakpoints
     (Self : in out DAP_Client_Breakpoint_Manager) is
   begin
      DAP.Persistent_Breakpoints.Hide_Breakpoints (Self.Kernel);

      for Vector of Self.Actual.Sources loop
         for Data of Vector loop
            DAP.Persistent_Breakpoints.Show_Breakpoint (Self.Kernel, Data);
         end loop;
      end loop;

      for Data of Self.Actual.Subprograms loop
         DAP.Persistent_Breakpoints.Show_Breakpoint (Self.Kernel, Data);
      end loop;
   end Show_Breakpoints;

   ---------------
   -- Send_Line --
   ---------------

   procedure Send_Line
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      File   : GNATCOLL.VFS.Virtual_File;
      Actual : Breakpoint_Vectors.Vector)
   is
      Req : Source_Line_Request_Access :=
        new Source_Line_Request (Self.Kernel);
      Sb  : DAP.Tools.SourceBreakpoint;
   begin
      Req.List   := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.File   := File;
      Req.Actual := Actual;

      Req.Parameters.arguments.source.name :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Base_Name (File));

      Req.Parameters.arguments.source.path :=
        VSS.Strings.Conversions.To_Virtual_String
          (GNATCOLL.VFS.Display_Full_Name (File));

      Req.Parameters.arguments.sourceModified := False;

      for Data of Actual loop
         if Data.Enabled then
            Sb.line   := Integer (GPS.Editors.Get_Line (Data.Location));
            Sb.column :=
              (Is_Set => True,
               Value  => Integer (GPS.Editors.Get_Column (Data.Location)));
            Req.Parameters.arguments.breakpoints.Append (Sb);
         end if;
      end loop;

      Self.Client.Enqueue (DAP_Request_Access (Req));
   end Send_Line;

   ---------------------
   -- Send_Subprogram --
   ---------------------

   procedure Send_Subprogram
     (Self   : not null access DAP_Client_Breakpoint_Manager;
      Actual : Breakpoint_Vectors.Vector)
   is
      Req : Function_Breakpoint_Request_Access :=
        new Function_Breakpoint_Request (Self.Kernel);
      Fb  : DAP.Tools.FunctionBreakpoint;
   begin
      Req.List   := DAP_Client_Breakpoint_Manager_Access (Self);
      Req.Actual := Actual;

      for Data of Req.Actual loop
         if Data.Enabled then
            Fb.name := VSS.Strings.Conversions.To_Virtual_String
              (To_String (Data.Subprogram));

            Req.Parameters.arguments.breakpoints.Append (Fb);
         end if;
      end loop;

      Self.Client.Enqueue (DAP_Request_Access (Req));
   end Send_Subprogram;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self  : DAP_Client_Breakpoint_Manager_Access;
      List  : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      use Breakpoint_Hash_Maps;

      Cursor  : Breakpoint_Hash_Maps.Cursor := Self.Actual.Sources.First;
      Actual  : Breakpoint_Vectors.Vector;
      Data    : Breakpoint_Data;
      Changed : Boolean;

   begin
      while Has_Element (Cursor) loop
         Changed := False;
         Actual  := Element (Cursor);

         for Index in Actual.First_Index .. Actual.Last_Index loop
            for Num of List loop
               Data := Actual.Element (Index);
               if Data.Num = Num then
                  Data.Enabled := State;
                  Actual.Replace_Element (Index, Data);
                  Changed := True;
               end if;
            end loop;
         end loop;

         if Changed then
            Self.Send_Line (Get_File (Actual.First_Element.Location), Actual);
         end if;

         Next (Cursor);
      end loop;

      Changed := False;
      Actual := Self.Actual.Subprograms;
      for Index in Actual.First_Index .. Actual.Last_Index loop
         for Num of List loop
            Data := Actual.Element (Index);
            if Data.Num = Num then
               Data.Enabled := State;
               Actual.Replace_Element (Index, Data);
            end if;
         end loop;
      end loop;

      if Changed then
         Self.Send_Subprogram (Actual);
      end if;
   end Set_Breakpoints_State;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self : DAP_Client_Breakpoint_Manager_Access;
      File : GNATCOLL.VFS.Virtual_File;
      Line : Editable_Line_Type)
   is
      use Breakpoint_Vectors;

      Actual  : Breakpoint_Vectors.Vector :=
        Self.Actual.Sources.Get_For_File (File);
      Changed : Boolean := False;
      Idx     : Integer := Actual.First_Index;

   begin
      while Idx <= Actual.Last_Index loop
         if Get_Line (Actual.Element (Idx).Location) = Line then
            Actual.Delete (Idx);
            Changed := True;
         else
            Idx := Idx + 1;
         end if;
      end loop;

      if Changed then
         Self.Send_Line (File, Actual);
      end if;

      Actual  := Self.Actual.Subprograms;
      Idx     := Self.Actual.Subprograms.First_Index;
      Changed := False;

      while Idx <= Actual.Last_Index loop
         if Get_Line (Actual.Element (Idx).Location) = Line then
            Actual.Delete (Idx);
            Changed := True;
         else
            Idx := Idx + 1;
         end if;
      end loop;

      if Changed then
         Self.Send_Subprogram (Actual);
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
      use Breakpoint_Vectors;

      Actual  : Breakpoint_Vectors.Vector;
      Changed : Boolean;
      Idx     : Integer;
      File    : GNATCOLL.VFS.Virtual_File;

   begin
      for Vector of Self.Actual.Sources loop
         Actual  := Vector;
         Changed := False;
         File    := Get_File (Actual.First_Element.Location);

         for Num of List loop
            Idx := Actual.First_Index;
            while Idx <= Actual.Last_Index loop
               if Actual.Element (Idx).Num = Num then
                  Actual.Delete (Idx);
                  Changed := True;
               else
                  Idx := Idx + 1;
               end if;
            end loop;
         end loop;

         if Changed then
            Self.Send_Line (File, Actual);
         end if;
      end loop;

      Actual  := Self.Actual.Subprograms;
      Changed := False;

      for Num of List loop
         Idx := Actual.First_Index;
         while Idx <= Actual.Last_Index loop
            if Actual.Element (Idx).Num = Num then
               Actual.Delete (Idx);
               Changed := True;
            else
               Idx := Idx + 1;
            end if;
         end loop;
      end loop;

      if Changed then
         Self.Send_Subprogram (Actual);
      end if;
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints
     (Self : DAP_Client_Breakpoint_Manager_Access)
   is
      use Breakpoint_Hash_Maps;
      use Breakpoint_Vectors;

      Empty : Breakpoint_Vectors.Vector;

   begin
      for Vector of Self.Actual.Sources loop
         Self.Send_Line
           (Get_File (Vector.First_Element.Location), Empty);
      end loop;

      Self.Send_Subprogram (Empty);
   end Remove_All_Breakpoints;

   --------------------
   -- Status_Changed --
   --------------------

   procedure Status_Changed
     (Self   : DAP_Client_Breakpoint_Manager;
      Status : Debugger_Status_Kind) is
   begin
      if Status /= Stopped then
         Self.Unhighlight_Current_Line;
      end if;
   end Status_Changed;

   ------------------------------
   -- Unhighlight_Current_Line --
   ------------------------------

   procedure Unhighlight_Current_Line (Self : DAP_Client_Breakpoint_Manager) is
   begin
      GPS.Kernel.Get_Messages_Container (Self.Kernel).Remove_Category
        (Debugger_Messages_Category, GPS.Kernel.Messages.Sides_Only);
   end Unhighlight_Current_Line;

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
            for Vector of Self.Actual.Sources loop
               for Data of Vector loop
                  if Data.Num = Breakpoint_Identifier (Num.Element.all) then
                     Stopped_File := GPS.Editors.Get_File (Data.Location);
                     Stopped_Line := Integer
                       (GPS.Editors.Get_Line (Data.Location));

                     Self.Highlight_Current_File_And_Line
                       (Stopped_File, Stopped_Line);
                     return;
                  end if;
               end loop;
            end loop;
         end;
      end loop;

      for Index in 1 .. Length (Event.a_body.hitBreakpointIds) loop
         declare
            Num : constant Integer_Constant_Reference :=
              Event.a_body.hitBreakpointIds (Index);
         begin
            for Data of Self.Actual.Subprograms loop
               if Data.Num = Breakpoint_Identifier (Num.Element.all) then
                  Stopped_File := GPS.Editors.Get_File (Data.Location);
                  Stopped_Line := Integer
                    (GPS.Editors.Get_Line (Data.Location));

                  Self.Highlight_Current_File_And_Line
                    (Stopped_File, Stopped_Line);
                  return;
               end if;
            end loop;
         end;
      end loop;
   end Stopped;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   procedure On_Location_Changed
     (Self         : DAP_Client_Breakpoint_Manager;
      Stopped_File : GNATCOLL.VFS.Virtual_File;
      Stopped_Line : Integer) is
   begin
      Self.Highlight_Current_File_And_Line (Stopped_File, Stopped_Line);
   end On_Location_Changed;

   -------------------------------------
   -- Highlight_Current_File_And_Line --
   -------------------------------------

   procedure Highlight_Current_File_And_Line
     (Self  : DAP_Client_Breakpoint_Manager;
      File  : Virtual_File;
      Line  : Integer)
   is
      Msg    : Simple_Message_Access;
      Action : GPS.Editors.Line_Information.Line_Information_Access;
   begin
      Self.Unhighlight_Current_Line;

      Msg := Create_Simple_Message
        (Get_Messages_Container (Self.Kernel),
         Category                 =>
           Debugger_Messages_Category,
         File                     => File,
         Line                     => Line,
         Column                   => 1,
         Text                     => "",
         Importance               => Unspecified,
         Flags                    => GPS.Kernel.Messages.Sides_Only,
         Allow_Auto_Jump_To_First => False);

      Msg.Set_Highlighting
        (GPS.Default_Styles.Debugger_Current_Line_Style,
         Highlight_Whole_Line);

      Action := new Line_Information_Record'
        (Text         => Null_Unbounded_String,
         Tooltip_Text =>
           To_Unbounded_String ("Current line in debugger"),
         Image        => Current_Line_Pixbuf,
         others       => <>);
      Msg.Set_Action (Action);

      --  Jump to current location
      declare
         Buffer : constant Editor_Buffer'Class :=
           Self.Kernel.Get_Buffer_Factory.Get
             (File,
              Open_Buffer   => True,
              Focus         => True,
              Unlocked_Only => True);
      begin
         Buffer.Current_View.Cursor_Goto
           (Location   => Buffer.New_Location_At_Line (Line),
            Raise_View => True);

         --  raise the source editor without giving a focus
         declare
            C : constant MDI_Child := GPS.Editors.GtkAda.Get_MDI_Child
              (Buffer.Current_View);
         begin
            if C /= null then
               Raise_Child (C, False);
            end if;
         end;
      end;
   end Highlight_Current_File_And_Line;

end DAP.Modules.Breakpoint_Managers;
