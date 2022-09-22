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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;
with Spawn.String_Vectors;

with Glib.Convert;
with Glib.Object;                use Glib.Object;

with Gtkada.Dialogs;

with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;
with VSS.JSON.Push_Writers;

with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;

with LSP.Types;
with LSP.JSON_Streams;

with DAP.Persistent_Breakpoints;
with DAP.Module;
with DAP.Preferences;
with DAP.Requests.Evaluate;
with DAP.Requests.Initialize;
with DAP.Requests.Disconnects;
with DAP.Requests.StackTraces;
with DAP.Tools;
with DAP.Tools.Inputs;
with DAP.Utils;

with GUI_Utils;
with Remote;
with Language_Handlers;          use Language_Handlers;

package body DAP.Clients is

   Me      : constant Trace_Handle := Create ("GPS.DAP.Clients", On);
   DAP_Log : constant GNATCOLL.Traces.Trace_Handle :=
     Create ("GPS.DAP.IN_OUT", Off);

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager'Class,
      DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager_Access);

   -- StackTrace_Request --

   type StackTrace_Request is
     new DAP.Requests.StackTraces.StackTrace_DAP_Request
   with record
      Client : DAP_Client_Access;
   end record;

   type StackTrace_Request_Access is access all StackTrace_Request;

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   -- Evaluate_Request --
   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with record
      Label : Gtk.Label.Gtk_Label;
   end record;
   type Evaluate_Request_Access is access all Evaluate_Request;
   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);
   overriding procedure On_Rejected (Self : in out Evaluate_Request);
   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String);
   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Sorce (Self.Breakpoints, File, Line, Temporary);
      end if;
   end Break_Source;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : in out DAP_Client;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Subprogram (Self.Breakpoints, Subprogram, Temporary);
      end if;
   end Break_Subprogram;

   ------------------
   -- Current_File --
   ------------------

   function Current_File
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Selected_File;
   end Current_File;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Self : in out DAP_Client) return Integer is
   begin
      return Self.Selected_Line;
   end Current_Line;

   ---------------------
   -- Current_Address --
   ---------------------

   function Current_Address
     (Self : in out DAP_Client) return Address_Type is
   begin
      return Self.Selected_Address;
   end Current_Address;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoint_At (Self.Breakpoints, File, Line);
      end if;
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : in out DAP_Client;
      List : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoints (Self.Breakpoints, List);
      end if;
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints (Self : in out DAP_Client) is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_All_Breakpoints (Self.Breakpoints);
      end if;
   end Remove_All_Breakpoints;

   --------------------
   -- Has_Breakpoint --
   --------------------

   function Has_Breakpoint
     (Self      : DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
      return Boolean is
   begin
      return False;
   end Has_Breakpoint;

   ----------------
   -- Is_Stopped --
   ----------------

   function Is_Stopped (Self : DAP_Client) return Boolean is
   begin
      return Self.Status = Stopped;
   end Is_Stopped;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Self   : in out DAP_Client;
      Status : Debugger_Status_Kind) is
   begin
      if Self.Status /= Terminating then
         Self.Status := Status;

         if Self.Status /= Stopped then
            Self.Selected_File    := GNATCOLL.VFS.No_File;
            Self.Selected_Line    := 0;
            Self.Selected_Address := Invalid_Address;
            Self.Selected_Frame   := 0;

            DAP.Utils.Unhighlight_Current_Line (Self.Kernel);
         end if;

         GPS.Kernel.Hooks.Debugger_State_Changed_Hook.Run
           (Self.Kernel, Self.Visual'Access,
            (if Self.Status /= Stopped
             then GPS.Debuggers.Debug_Busy
             else GPS.Debuggers.Debug_Available));
      end if;
   end Set_Status;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access) is
   begin
      if Self.Status in Initialization .. Stopped then
         Self.Process (Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;

      Request := null;
   end Enqueue;

   -----------------------
   -- Get_Assembly_View --
   -----------------------

   function Get_Assembly_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Assembly_View;
   end Get_Assembly_View;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Breakpoint_Maps.All_Breakpoints
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      Empty : DAP.Breakpoint_Maps.All_Breakpoints;
   begin
      if Self.Breakpoints /= null then
         return DAP.Modules.Breakpoint_Managers.Get_Breakpoints
           (Self.Breakpoints);
      else
         return Empty;
      end if;
   end Get_Breakpoints;

   --------------------------
   -- Get_Breakpoints_View --
   --------------------------

   function Get_Breakpoints_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Breakpoints_View;
   end Get_Breakpoints_View;

   -------------------------
   -- Get_Call_Stack_View --
   -------------------------

   function Get_Call_Stack_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Call_Stack_View;
   end Get_Call_Stack_View;

   ------------------------
   -- Get_Current_Thread --
   ------------------------

   function Get_Current_Thread (Self  : in out DAP_Client) return Integer
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Self.Stopped_Threads.Is_Empty then
         return 0;
      end if;

      if Self.Thread_View /= null
        and then Self.Selected_Thread /= 0
      then
         return Self.Selected_Thread;
      else
         return Self.Stopped_Threads.Element (Self.Stopped_Threads.First);
      end if;
   end Get_Current_Thread;

   ---------------------
   -- Get_Thread_View --
   ---------------------

   function Get_Thread_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Thread_View;
   end Get_Thread_View;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end Get_Executable;

   -----------------------
   -- Set_Assembly_View --
   -----------------------

   procedure Set_Assembly_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Assembly_View := View;
   end Set_Assembly_View;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self  : in out DAP_Client;
      List  : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;
   begin
      if Self.Breakpoints /= null then
         DAP.Modules.Breakpoint_Managers.Set_Breakpoints_State
           (Self.Breakpoints, List, State);
      end if;
   end Set_Breakpoints_State;

   --------------------------
   -- Set_Breakpoints_View --
   --------------------------

   procedure Set_Breakpoints_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Breakpoints_View := View;
   end Set_Breakpoints_View;

   -------------------------
   -- Set_Call_Stack_View --
   -------------------------

   procedure Set_Call_Stack_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Call_Stack_View := View;
   end Set_Call_Stack_View;

   ---------------------
   -- Set_Thread_View --
   ---------------------

   procedure Set_Thread_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Thread_View := View;
   end Set_Thread_View;

   ------------------------
   -- Set_Selected_Frame --
   ------------------------

   procedure Set_Selected_Frame
     (Self    : in out DAP_Client;
      Id      : Integer;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Integer;
      Address : Address_Type) is
   begin
      Self.Selected_Frame   := Id;
      Self.Selected_File    := File;
      Self.Selected_Line    := Line;
      Self.Selected_Address := Address;

      Self.On_Location_Changed;
   end Set_Selected_Frame;

   ------------------------
   -- Get_Selected_Frame --
   ------------------------

   function Get_Selected_Frame
     (Self : in out DAP_Client) return Integer is
   begin
      return Self.Selected_Frame;
   end Get_Selected_Frame;

   -------------------------
   -- Set_Selected_Thread --
   -------------------------

   procedure Set_Selected_Thread (Self : in out DAP_Client; Id : Integer) is
   begin
      Self.Selected_Thread := Id;
   end Set_Selected_Thread;

   ----------------------
   -- Set_Source_Files --
   ----------------------

   procedure Set_Source_Files
     (Self         : in out DAP_Client;
      Source_Files : VSS.String_Vectors.Virtual_String_Vector) is
   begin
      Self.Source_Files := Source_Files;
   end Set_Source_Files;

   --------------------
   -- Get_Request_ID --
   --------------------

   function Get_Request_ID
     (Self : in out DAP_Client) return Integer
   is
      ID : constant Integer := Self.Request_Id;
   begin
      if Self.Request_Id < Integer'Last then
         Self.Request_Id := Self.Request_Id + 1;
      else
         Self.Request_Id := 1;
      end if;

      return ID;
   end Get_Request_ID;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Self : in out DAP_Client) return Debugger_Status_Kind is
   begin
      return Self.Status;
   end Get_Status;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual
     (Self : in out DAP_Client) return Visual_Debugger_Access is
   begin
      return Self.Visual'Unchecked_Access;
   end Get_Visual;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : DAP_Client) return VSS.Strings.Virtual_String is
   begin
      return Self.Error_Msg;
   end Error_Message;

   -------------------
   -- On_Configured --
   -------------------

   procedure On_Configured (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Configured;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Ready);
      GPS.Kernel.Hooks.Debugger_Started_Hook.Run
        (Self.Kernel, Self.Visual'Access);
      Self.Kernel.Refresh_Context;
   end On_Ready;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Continue;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out DAP_Client) is
   begin
      Self.Reject_All_Requests;

      GPS.Kernel.Hooks.Debugger_Process_Terminated_Hook.Run
        (Self.Kernel, Self.Visual'Access);

      DAP.Modules.Breakpoint_Managers.On_Finished (Self.Breakpoints);
      Free (Self.Breakpoints);
      DAP.Persistent_Breakpoints.Show_Breakpoints_In_All_Editors (Self.Kernel);
      DAP.Module.Finished (Self.Id);
   end On_Finished;

   ----------------------------------
   -- Load_Project_From_Executable --
   ----------------------------------

   procedure Load_Project_From_Executable (Self : in out DAP_Client) is
      use GNAT.Strings;
      use GNATCOLL.Projects;
      use GNATCOLL.VFS;
      use Gtkada.Dialogs;
      use GPS.Kernel.Project;

      Project : GNATCOLL.Projects.Project_Type := Get_Project (Self.Kernel);
   begin
      --  Do nothing unless the current project was already generated from an
      --  executable.

      if Get_Registry (Self.Kernel).Tree.Status /= From_Executable
      then
         return;
      end if;

      if Self.File /= No_File and then not Is_Regular_File (Self.File) then
         declare
            Buttons : Message_Dialog_Buttons;
            pragma Unreferenced (Buttons);
         begin
            Buttons := GUI_Utils.GPS_Message_Dialog
              (Msg         =>
                 "The executable specified with" &
                   " --debug does not exist on disk",
               Dialog_Type => Error,
               Buttons     => Button_OK,
               Title       => "Executable not found",
               Parent      => GPS.Kernel.Get_Main_Window (Self.Kernel));
         end;
      end if;

      declare
         List : String_List_Access := Project.Attribute_Value (Main_Attribute);
      begin
         if List /= null then
            for L in List'Range loop
               if Equal (+List (L).all, Full_Name (Self.File)) then
                  Free (List);
                  return;
               end if;
            end loop;
            Free (List);
         end if;
      end;

      --  No handling of desktop is done here, we want to leave all windows
      --  as-is.

      Get_Registry (Self.Kernel).Tree.Unload;

      --  Create an empty project, and we'll add properties to it

      if Self.File /= GNATCOLL.VFS.No_File then
         Get_Registry (Self.Kernel).Tree.Load_Empty_Project
           (Get_Registry (Self.Kernel).Environment,
            Name           => "debugger_" & (+Base_Name (Self.File)),
            Recompute_View => False);
      else
         Get_Registry (Self.Kernel).Tree.Load_Empty_Project
           (Get_Registry (Self.Kernel).Environment,
            Name           => "debugger_no_file",
            Recompute_View => False);
      end if;

      Project := Get_Registry (Self.Kernel).Tree.Root_Project;

      declare
         Bases       : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Bases_Index : Natural := Bases'First;
         Dirs        : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Dirs_Index  : Natural := Dirs'First;
         Main        : GNAT.OS_Lib.Argument_List (1 .. 1);
         Langs       : GNAT.OS_Lib.Argument_List
           (1 .. Self.Source_Files.Length);
         Lang_Index  : Natural := Langs'First;

      begin
         --  Source_Files, Source_Dirs & Languages

         for L in 1 .. Self.Source_Files.Length loop
            declare
               Remote_File : constant Virtual_File :=
                 Create_From_Base
                   (+VSS.Strings.Conversions.To_UTF_8_String
                      (Self.Source_Files.Element (L)),
                    Dir_Name (Self.File),
                    Remote.Get_Nickname (Remote.Debug_Server));
               Local_File  : constant Virtual_File := To_Local (Remote_File);
               Dir         : constant Virtual_File := Local_File.Dir;
               Base        : constant Filesystem_String :=
                 Base_Name (Local_File);
               Lang        : constant String :=
                 Get_Language_From_File
                   (GPS.Kernel.Get_Language_Handler (Self.Kernel),
                    Local_File);
               Found       : Boolean;

            begin
               Found := False;

               if Is_Directory (Dir) then
                  for D in Dirs'First .. Dirs_Index - 1 loop
                     if Equal (+Dirs (D).all, Dir.Full_Name) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Dirs (Dirs_Index) := new String'(+Dir.Full_Name);
                     Dirs_Index := Dirs_Index + 1;
                  end if;

                  Found := False;
                  for J in Bases'First .. Bases_Index - 1 loop
                     if Equal (+Bases (J).all, Base) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Bases (Bases_Index) := new String'
                       (Base_Name
                          (VSS.Strings.Conversions.To_UTF_8_String
                               (Self.Source_Files.Element (L))));
                     Bases_Index := Bases_Index + 1;
                  end if;

                  Found := False;
                  if Lang /= "" then
                     for La in Langs'First .. Lang_Index - 1 loop
                        if Langs (La).all = Lang then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Langs (Lang_Index) := new String'(Lang);
                        Lang_Index := Lang_Index + 1;
                     end if;
                  end if;
               end if;
            end;
         end loop;

         GNATCOLL.Traces.Trace (Me, "Setting Source_Dirs:");
         for D in Dirs'First .. Dirs_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Dirs (D).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Dirs_Attribute,
            Values             => Dirs (Dirs'First .. Dirs_Index - 1));
         GNATCOLL.Utils.Free (Dirs);

         GNATCOLL.Traces.Trace (Me, "Setting Source_Files:");
         for B in Bases'First .. Bases_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Bases (B).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Files_Attribute,
            Values             => Bases (Bases'First .. Bases_Index - 1));
         GNATCOLL.Utils.Free (Bases);

         GNATCOLL.Traces.Trace (Me, "Setting Languages:");
         for L in Langs'First .. Lang_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Langs (L).all);
         end loop;

         if Lang_Index = Langs'First then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Languages_Attribute,
               Values    =>
                 (new String'("ada"), new String'("c"), new String'("c++")));
         else
            Project.Set_Attribute
              (Attribute          => Languages_Attribute,
               Values             => Langs (Langs'First .. Lang_Index - 1));
         end if;

         GNATCOLL.Utils.Free (Langs);

         --  Object_Dir, Exec_Dir, Main

         if Self.File /= GNATCOLL.VFS.No_File then
            Project.Set_Attribute
              (Attribute          => Obj_Dir_Attribute,
               Value              => +Dir_Name (Self.File));
            Project.Set_Attribute
              (Attribute          => Exec_Dir_Attribute,
               Value              => +Dir_Name (Self.File));

            Main (Main'First) := new String'(+Full_Name (Self.File));
            Project.Set_Attribute
              (Attribute          => Main_Attribute,
               Values             => Main);
            GNATCOLL.Utils.Free (Main);
         end if;
      end;

      --  Is the information for this executable already cached? If yes,
      --  we simply reuse it to avoid the need to interact with the debugger.

      Project.Set_Modified (False);
      Get_Registry (Self.Kernel).Tree.Set_Status (From_Executable);
      GPS.Kernel.Hooks.Project_Changed_Hook.Run (Self.Kernel);
      Recompute_View (Self.Kernel);
   end Load_Project_From_Executable;

   -----------------
   -- On_Launched --
   -----------------

   procedure On_Launched (Self : in out DAP_Client) is
   begin
      if Self.Source_Files.Is_Empty then
         Self.Load_Project_From_Executable;
      end if;

      --  Show Main file when 'info line' command is in the protocol

      Self.Breakpoints := new DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager (Self.Kernel, Self.This);
      DAP.Modules.Breakpoint_Managers.Initialize (Self.Breakpoints);
   end On_Launched;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self    : in out DAP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
      use type VSS.Strings.Virtual_String;
      use type DAP.Requests.DAP_Request_Access;

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out Integer;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String);

      Memory : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out Integer;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String)
      is

         Reader : aliased
           VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS     : aliased LSP.JSON_Streams.JSON_Stream
           (False, Reader'Access);

      begin
         Seq         := 0;
         Request_Seq := 0;
         A_Type      := VSS.Strings.Empty_Virtual_String;
         Success     := (Is_Set => False);

         Reader.Set_Stream (Memory'Unchecked_Access);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Document);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);

            begin
               JS.R.Read_Next;

               if Key = "seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Seq := LSP.Types.LSP_Number
                    (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "request_seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Request_Seq := Integer (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "type" then
                  pragma Assert (JS.R.Is_String_Value);

                  A_Type := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "success" then
                  pragma Assert (JS.R.Is_Boolean_Value);

                  Success := (True, JS.R.Boolean_Value);

                  JS.R.Read_Next;

               elsif Key = "message" then
                  pragma Assert (JS.R.Is_String_Value);

                  Message := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "event" then
                  pragma Assert (JS.R.Is_String_Value);

                  Event := JS.R.String_Value;

                  JS.R.Read_Next;

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         Memory.Rewind;
      end Look_Ahead;

      Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => Reader'Access);

      Seq         : LSP.Types.LSP_Number;
      A_Type      : VSS.Strings.Virtual_String;
      Request_Seq : Integer;
      R_Success   : LSP.Types.Optional_Boolean;
      Message     : VSS.Strings.Virtual_String;
      Event       : VSS.Strings.Virtual_String;

      Position    : Requests_Maps.Cursor;
      Request     : DAP.Requests.DAP_Request_Access;
      New_Request : DAP.Requests.DAP_Request_Access := null;

   begin
      if DAP_Log.Is_Active then
         Trace (DAP_Log, "[" & Self.Id'Img & "<-]" & To_String (Data));
      end if;

      Memory.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Seq, Request_Seq, A_Type, R_Success, Message, Event);

      Reader.Set_Stream (Memory'Unchecked_Access);
      Stream.R.Read_Next;
      pragma Assert (Stream.R.Is_Start_Document);
      Stream.R.Read_Next;

      if A_Type = "response" then
         if Request_Seq /= 0 then
            Position := Self.Sent.Find (Request_Seq);

            if Requests_Maps.Has_Element (Position) then
               Request := Requests_Maps.Element (Position);
               Self.Sent.Delete (Position);

               if R_Success.Is_Set
                 and then not R_Success.Value
               then
                  begin
                     Request.On_Error_Message (Message);
                  exception
                     when E : others =>
                        Trace (Me, E);
                  end;

               else
                  begin
                     if Request.Kernel = null
                       or else not Request.Kernel.Is_In_Destruction
                     then
                        Request.On_Result_Message (Reader, New_Request);

                        GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Run
                          (Kernel   => Request.Kernel,
                           Method   => Request.Method);
                     end if;

                  exception
                     when E : others =>
                        Trace (Me, E);
                  end;
               end if;

               DAP.Requests.Destroy (Request);
            end if;
         end if;

         if New_Request /= null then
            Self.Process (New_Request);
         end if;

      elsif A_Type = "event" then
         Self.Process_Event (Reader, Event);
      end if;
   end On_Raw_Message;

   --------------------
   -- Get_StackTrace --
   --------------------

   procedure Get_StackTrace
     (Self      : in out DAP_Client;
      Thread_Id : Integer)
   is
      Req : StackTrace_Request_Access :=
        new StackTrace_Request (Self.Kernel);
   begin
      Req.Client := Self.This;
      Req.Parameters.arguments.threadId := Thread_Id;
      Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Get_StackTrace;

   -------------------
   -- Process_Event --
   -------------------

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Event  : VSS.Strings.Virtual_String)
   is
      use VSS.Strings;
      use DAP.Tools.Enum;

      Success : Boolean := True;
   begin
      if Event = "output" then
         declare
            output : DAP.Tools.OutputEvent;
         begin
            DAP.Tools.Inputs.Input_OutputEvent (Stream, output, Success);
            if not Success then
               return;
            end if;

            if output.a_body.category.Is_Set
              and then output.a_body.category.Value = console
            then
               Self.Kernel.Get_Messages_Window.Insert_Text
                 ("Debugger adapter" & Self.Id'Img & ":"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.a_body.output));

            elsif output.a_body.category.Is_Set
              and then output.a_body.category.Value = stdout
            then
               --  Adapter writes debuggee output in a log file
               null;

            elsif output.a_body.category.Is_Set
              and then output.a_body.category.Value = stderr
            then
               Self.Kernel.Get_Messages_Window.Insert_Error
                 ("Debugger" & Self.Id'Img & " [stderr]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.a_body.output));
               Trace (Me, "Debugger" & Self.Id'Img & " [stderr]:" &
                        VSS.Strings.Conversions.To_UTF_8_String
                        (output.a_body.output));
            end if;
         end;

      elsif Event = "initialized" then
         Self.Set_Status (Initialized);

      elsif Event = "stopped" then
         declare
            stop : DAP.Tools.StoppedEvent;
         begin
            Self.Set_Status (Stopped);
            DAP.Tools.Inputs.Input_StoppedEvent (Stream, stop, Success);
            if not Success then
               return;
            end if;

            if stop.a_body.threadId.Is_Set then
               Integer_Sets.Include
                 (Self.Stopped_Threads, stop.a_body.threadId.Value);
            end if;
            Self.All_Threads_Stopped := stop.a_body.allThreadsStopped;

            if stop.a_body.reason = breakpoint then
               Self.Breakpoints.Stopped
                 (stop, Self.Selected_File, Self.Selected_Line);

            elsif stop.a_body.reason = step
              and then stop.a_body.threadId.Is_Set
            then
               null;

            else
               Self.Kernel.Get_Messages_Window.Insert_Error
                 ("Debugger" & Self.Id'Img & " stopped:"  &
                    stop.a_body.reason'Img);

               Trace (Me, "Debugger" & Self.Id'Img & " stopped:" &
                        stop.a_body.reason'Img);
            end if;

            --  Get stopped frameId/file/line/address
            if stop.a_body.threadId.Is_Set then
               Self.Get_StackTrace (stop.a_body.threadId.Value);
            end if;
         end;

      elsif Event = "continued" then
         declare
            Continued : DAP.Tools.ContinuedEvent;
         begin
            DAP.Tools.Inputs.Input_ContinuedEvent (Stream, Continued, Success);
            if Continued.a_body.allThreadsContinued then
               Self.All_Threads_Stopped := False;
               Integer_Sets.Clear (Self.Stopped_Threads);
            else
               Integer_Sets.Exclude
                 (Self.Stopped_Threads, Continued.a_body.threadId);
            end if;
         end;

      elsif Event = "breakpoint" then
         --  We process breakpoint responses so do nothing with event for now.
         null;

      elsif Event = "thread" then
         --  The tread view uses requests to get the list of threads.
         null;

      elsif Event = "exited" then
         --  Do not handle, at least for now.
         null;

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           ("Event:" & VSS.Strings.Conversions.To_UTF_8_String (Event));

         Trace (Me, "Event:" &
                  VSS.Strings.Conversions.To_UTF_8_String (Event));
      end if;
   end Process_Event;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use GNATCOLL.VFS;
      use DAP.Tools;
      pragma Unreferenced (New_Request);

   begin
      if Length (Result.a_body.stackFrames) > 0 then
         declare
            Frame : constant StackFrame_Variable_Reference :=
              Get_StackFrame_Variable_Reference
                (Result.a_body.stackFrames, 1);
         begin
            Self.Client.Selected_Frame := Frame.id;

            Self.Client.Selected_Address := String_To_Address
              (VSS.Strings.Conversions.To_UTF_8_String
                 (Frame.instructionPointerReference));

            if Frame.source.Is_Set then
               Self.Client.Selected_File := Create
                 (+(VSS.Strings.Conversions.To_UTF_8_String
                  (Frame.source.Value.path)));
               Self.Client.Selected_Line := Frame.line;

               Self.Client.On_Location_Changed;
            end if;
         end;
      end if;
   end On_Result_Message;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   procedure On_Location_Changed
     (Self : in out DAP_Client) is
   begin
      DAP.Utils.Highlight_Current_File_And_Line
        (Self.Kernel, Self.Selected_File, Self.Selected_Line);

      GPS.Kernel.Hooks.Debugger_Location_Changed_Hook.Run
        (Self.Kernel, Self.Visual'Access);
   end On_Location_Changed;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out DAP_Client) is
      Init : DAP.Requests.Initialize.Initialize_DAP_Request_Access :=
        new DAP.Requests.Initialize.Initialize_DAP_Request (Self.Kernel);
   begin
      Init.Initialize
        (Self.Project, Self.File,
         Ada.Strings.Unbounded.To_String (Self.Args));

      Self.Process (DAP.Requests.DAP_Request_Access (Init));
   end On_Started;

   -------------------
   -- On_Terminated --
   -------------------

   procedure On_Terminated (Self : in out DAP_Client) is
   begin
      GPS.Kernel.Hooks.Debugger_Terminated_Hook.Run
        (Self.Kernel, Self.Visual'Access);
      Self.Stop;
   end On_Terminated;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access)
   is
      Id     : constant Integer :=
        Self.Get_Request_ID;
      Writer : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Stream : aliased VSS.Text_Streams.Memory_UTF8_Output.
        Memory_UTF8_Output_Stream;

   begin
      if Self.Status /= Terminating then
         Request.Set_Seq (Id);
         Request.Set_Client (Self.This);
         Writer.Set_Stream (Stream'Unchecked_Access);
         Writer.Start_Document;
         Request.Write (Writer);
         Writer.End_Document;

         --  Send request's message

         Self.Send_Buffer (Stream.Buffer);

         if DAP_Log.Is_Active then
            Trace (DAP_Log,
                   "[" & Self.Id'Img & "->]"
                   & VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String
                     (Stream.Buffer));
         end if;

         --  Add request to the map

         Self.Sent.Insert (Id, Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;
   end Process;

   -------------------------
   -- Reject_All_Requests --
   -------------------------

   procedure Reject_All_Requests (Self : in out DAP_Client) is
   begin
      --  Reject all queued requests. Clean commands queue.

      for Request of Self.Sent loop
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end loop;

      Self.Sent.Clear;
   end Reject_All_Requests;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self    : in out DAP_Client;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Node_Args : Spawn.String_Vectors.UTF_8_String_Vector;
      Adapter   : constant String := DAP.Preferences.DAP_Adapter.Get_Pref;

   begin
      Trace (Me, "Launching the debug adapter: " & Adapter);

      Self.Project := Project;
      Self.File    := File;
      Self.Args    := Ada.Strings.Unbounded.To_Unbounded_String (Args);

      declare
         Vals : GNAT.Strings.String_List_Access := GNATCOLL.Utils.Split
           (Adapter, On => ' ');
      begin
         Self.Set_Program (Vals (Vals'First).all);
         for Index in Vals'First + 1 .. Vals'Last loop
            Node_Args.Append (Vals (Index).all);
         end loop;
         GNAT.Strings.Free (Vals);
      end;

      Self.Set_Arguments (Node_Args);
      Self.Start;
   end Start;

   ----------
   -- Quit --
   ----------

   procedure Quit (Self : in out DAP_Client) is
      Disconnect : DAP.Requests.Disconnects.
        Disconnect_DAP_Request_Access := new DAP.Requests.Disconnects.
          Disconnect_DAP_Request (Self.Kernel);
   begin
      if Self.Status /= Terminating then
         Self.Process (DAP.Requests.DAP_Request_Access (Disconnect));
         Self.Set_Status (Terminating);
      end if;
   end Quit;

   --------------
   -- Value_Of --
   --------------

   procedure Value_Of
     (Self   : in out DAP_Client;
      Entity : String;
      Label  : Gtk.Label.Gtk_Label)
   is
      Req   : Evaluate_Request_Access := new Evaluate_Request (Self.Kernel);
      Frame : constant Integer := Self.Get_Selected_Frame;
   begin
      Req.Label := Label;
      Ref (GObject (Label));
      Req.Parameters.arguments.expression :=
        VSS.Strings.Conversions.To_Virtual_String (Entity);
      if Frame /= 0 then
         Req.Parameters.arguments.frameId := (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.hover);
      Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Value_Of;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Self.Label.Set_Markup
        ("<b>Debugger value :</b> " & Glib.Convert.Escape_Text
           (VSS.Strings.Conversions.To_UTF_8_String (Result.a_body.result)));
      Unref (GObject (Self.Label));
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Evaluate_Request) is
   begin
      Unref (GObject (Self.Label));
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Unref (GObject (Self.Label));
   end On_Error_Message;

end DAP.Clients;
