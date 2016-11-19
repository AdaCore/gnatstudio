------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
with GPS.VCS;               use GPS.VCS;
with GPS.VCS_Engines;       use GPS.VCS_Engines;

package body VCS2.Scripts is
   Me : constant Trace_Handle := Create ("VCS2.SCRIPT") with Unreferenced;

   type Script_Engine_Factory is new VCS_Engine_Factory with record
      Kernel         : access Kernel_Handle_Record'Class;
      Construct      : Subprogram_Type;
      Find_Repo      : Subprogram_Type;
      Default_Status : VCS_File_Status;
   end record;
   overriding function Create_Engine
     (Self        : not null access Script_Engine_Factory;
      Working_Dir : Virtual_File)
     return not null VCS_Engine_Access;
   overriding function Find_Working_Directory
     (Self  : not null access Script_Engine_Factory;
      File  : Virtual_File) return Virtual_File;

   type Script_Engine is new VCS_Engine with record
      Factory : access Script_Engine_Factory'Class;
      Script  : Scripting_Language;
   end record;
   overriding function Name
     (Self : not null access Script_Engine) return String;
   overriding procedure Async_Fetch_Status_For_Files
     (Self    : not null access Script_Engine;
      Files   : File_Array);
   overriding procedure Async_Fetch_Status_For_Project
     (Self    : not null access Script_Engine;
      Project : Project_Type);
   overriding procedure Async_Fetch_Status_For_All_Files
     (Self    : not null access Script_Engine);
   overriding function Default_File_Status
     (Self    : not null access Script_Engine)
      return VCS_File_Status is (Self.Factory.Default_Status);
   overriding procedure Stage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array);
   overriding procedure Unstage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array);
   overriding procedure Commit_Staged_Files
     (Self    : not null access Script_Engine;
      Message : String);

   procedure Static_VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all script functions

   procedure Call_Method
     (Self   : not null access Script_Engine'Class;
      Method : String) with Inline;
   procedure Call_Method
     (Self   : not null access Script_Engine'Class;
      Method : String;
      Data   : in out Callback_Data'Class);
   --  Call a python method of self.
   --  Free Data

   procedure Set_Nth_Arg
     (Data  : in out Callback_Data'Class;
      Nth   : Integer;
      Files : File_Array);
   --  Store a list of files as the nth argument

   -----------------
   -- Call_Method --
   -----------------

   procedure Call_Method
     (Self   : not null access Script_Engine'Class;
      Method : String)
   is
      D : Callback_Data'Class := Create (Self.Script, 0);
   begin
      Call_Method (Self, Method, D);
   end Call_Method;

   -----------------
   -- Call_Method --
   -----------------

   procedure Call_Method
     (Self   : not null access Script_Engine'Class;
      Method : String;
      Data   : in out Callback_Data'Class)
   is
      Inst  : constant Class_Instance :=
        Create_VCS_Instance (Self.Script, Self);
      F     : Subprogram_Type := Get_Method (Inst, Method);
      Dummy : Boolean;
   begin
      if F /= null then
         Dummy := F.Execute (Data);
         Free (F);
      end if;
      Free (Data);
   end Call_Method;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : not null access Script_Engine) return String is
   begin
      return Self.Factory.Name;
   end Name;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data  : in out Callback_Data'Class;
      Nth   : Integer;
      Files : File_Array)
   is
      L : List_Instance'Class := New_List (Data.Get_Script);
      Index : Positive := 1;
   begin
      for F of Files loop
         if F /= No_File then
            L.Set_Nth_Arg (Index, Create_File (Data.Get_Script, F));
            Index := Index + 1;
         end if;
      end loop;

      Data.Set_Nth_Arg (Nth, L);
      Free (L);   --  now owned by Data
   end Set_Nth_Arg;

   ----------------------------------
   -- Async_Fetch_Status_For_Files --
   ----------------------------------

   overriding procedure Async_Fetch_Status_For_Files
     (Self    : not null access Script_Engine;
      Files   : File_Array)
   is
      D : Callback_Data'Class := Create (Self.Script, 1);
   begin
      Set_Nth_Arg (D, 1, Files);
      Call_Method (Self, "async_fetch_status_for_files", D);
   end Async_Fetch_Status_For_Files;

   ------------------------------------
   -- Async_Fetch_Status_For_Project --
   ------------------------------------

   overriding procedure Async_Fetch_Status_For_Project
     (Self    : not null access Script_Engine;
      Project : Project_Type)
   is
      D : Callback_Data'Class := Create (Self.Script, 1);
   begin
      D.Set_Nth_Arg (1, Create_Project (Self.Script, Project));
      Call_Method (Self, "async_fetch_status_for_project", D);
   end Async_Fetch_Status_For_Project;

   --------------------------------------
   -- Async_Fetch_Status_For_All_Files --
   --------------------------------------

   overriding procedure Async_Fetch_Status_For_All_Files
     (Self    : not null access Script_Engine) is
   begin
      Call_Method (Self, "async_fetch_status_for_all_files");
   end Async_Fetch_Status_For_All_Files;

   -------------------
   -- Create_Engine --
   -------------------

   overriding function Create_Engine
     (Self        : not null access Script_Engine_Factory;
      Working_Dir : Virtual_File)
      return not null VCS_Engine_Access
   is
      R : constant access Script_Engine := new Script_Engine;
      Script : constant Scripting_Language := Self.Construct.Get_Script;
      Data   : Callback_Data'Class := Script.Create (1);
      Inst   : Class_Instance;

   begin
      Ensure_Directory (Working_Dir);
      Data.Set_Nth_Arg (1, Create_File (Script, Working_Dir));
      Inst := Self.Construct.Execute (Data);  -- create the pyton instance
      Free (Data);

      Set_VCS_Instance (R, Inst);  --  set binding with Ada

      R.Factory := Self;
      R.Script  := Script;

      Call_Method (R, "setup");
      return R;
   end Create_Engine;

   ----------------------------
   -- Find_Working_Directory --
   ----------------------------

   overriding function Find_Working_Directory
     (Self  : not null access Script_Engine_Factory;
      File  : Virtual_File) return Virtual_File
   is
      Script : constant Scripting_Language := Self.Find_Repo.Get_Script;
      Data   : Callback_Data'Class := Script.Create (1);
   begin
      Data.Set_Nth_Arg (1, Create_File (Script, File));

      declare
         S : constant String := Self.Find_Repo.Execute (Data);
      begin
         Free (Data);
         if S = "" then
            return No_File;
         else
            return Create (+S);
         end if;
      end;
   end Find_Working_Directory;

   -----------------
   -- Stage_Files --
   -----------------

   overriding procedure Stage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array)
   is
      Data : Callback_Data'Class := Create (Self.Script, 1);
   begin
      Set_Nth_Arg (Data, 1, Files);
      Call_Method (Self, "stage_files", Data);
   end Stage_Files;

   -------------------
   -- Unstage_Files --
   -------------------

   overriding procedure Unstage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array)
   is
      Data : Callback_Data'Class := Create (Self.Script, 1);
   begin
      Set_Nth_Arg (Data, 1, Files);
      Call_Method (Self, "unstage_files", Data);
   end Unstage_Files;

   -------------------------
   -- Commit_Staged_Files --
   -------------------------

   overriding procedure Commit_Staged_Files
     (Self    : not null access Script_Engine;
      Message : String)
   is
      Data : Callback_Data'Class := Create (Self.Script, 1);
   begin
      Set_Nth_Arg (Data, 1, Message);
      Call_Method (Self, "commit_staged_files", Data);
   end Commit_Staged_Files;

   -----------------
   -- VCS_Handler --
   -----------------

   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : constant Class_Instance := Data.Nth_Arg (1);
      VCS  : constant not null VCS_Engine_Access :=
        VCS_Engine_Access (Get_VCS (Inst));
   begin
      if Command = "name" then
         Data.Set_Return_Value (VCS.Name);

      elsif Command = "ensure_status_for_files" then
         declare
            List : constant List_Instance := Data.Nth_Arg (2);
            Files : File_Array (1 .. List.Number_Of_Arguments);
         begin
            for F in Files'Range loop
               Files (F) := Nth_Arg (List, F);
            end loop;

            VCS.Ensure_Status_For_Files (Files);
         end;

      elsif Command = "ensure_status_for_project" then
         VCS.Ensure_Status_For_Project (Get_Data (Data, 2));

      elsif Command = "ensure_status_for_all_source_files" then
         VCS.Ensure_Status_For_All_Source_Files;

      elsif Command = "set_run_in_background" then
         VCS.Set_Run_In_Background (Data.Nth_Arg (2));

      elsif Command = "get_file_status" then
         declare
            Props : constant VCS_File_Properties :=
              VCS.File_Properties_From_Cache (Nth_Arg (Data, 2));
         begin
            Data.Set_Return_Value_As_List (Size => 3);
            Data.Set_Return_Value (VCS_File_Status'Pos (Props.Status));
            Data.Set_Return_Value (To_String (Props.Version));
            Data.Set_Return_Value (To_String (Props.Repo_Version));
         end;

      elsif Command = "_set_file_status" then
         declare
            Status : constant VCS_File_Status := VCS_File_Status
               (Integer'(Data.Nth_Arg (3, Integer (Status_Unmodified))));
            Version : constant Unbounded_String :=
               To_Unbounded_String (Data.Nth_Arg (4, ""));
            Repo_Version : constant Unbounded_String :=
               To_Unbounded_String (Data.Nth_Arg (5, ""));
         begin

            --  Did we have a single file ?
            declare
               F : constant Virtual_File := Nth_Arg (Data, 2);
            begin
               VCS.Set_File_Status_In_Cache
                 (File  => F,
                  Props =>
                    (Status       => Status,
                     Version      => Version,
                     Repo_Version => Repo_Version));
            end;

         exception
            when Invalid_Parameter =>
               --  Or maybe a list of files ?
               declare
                  List : constant List_Instance := Data.Nth_Arg (2);
               begin
                  for Idx in 1 .. List.Number_Of_Arguments loop
                     VCS.Set_File_Status_In_Cache
                       (File  => Nth_Arg (List, Idx),
                        Props =>
                          (Status       => Status,
                           Version      => Version,
                           Repo_Version => Repo_Version));
                  end loop;
               end;
         end;

      elsif Command = "invalidate_status_cache" then
         VCS.Invalidate_File_Status_Cache;

      elsif Command = "_override_status_display" then
         VCS.Override_Display
           (Status    => VCS_File_Status (Integer'(Data.Nth_Arg (2))),
            Display   =>
              (Label     => To_Unbounded_String (Data.Nth_Arg (3, "")),
               Icon_Name => To_Unbounded_String (Data.Nth_Arg (4, ""))));
      end if;
   end VCS_Handler;

   ------------------------
   -- Static_VCS_Handler --
   ------------------------

   procedure Static_VCS_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "_register" then
         declare
            F : constant access Script_Engine_Factory :=
              new Script_Engine_Factory;
         begin
            F.Kernel    := Kernel;
            F.Construct := Data.Nth_Arg (2);
            F.Default_Status := VCS_File_Status (Integer'(Data.Nth_Arg (3)));
            F.Find_Repo := Data.Nth_Arg (4);
            Register_Factory (Get_Kernel (Data), Data.Nth_Arg (1), F);
         end;

      elsif Command = "get" then
         declare
            P : constant Project_Type := Get_Data (Data, 1);
            F : constant not null VCS_Engine_Access := Get_VCS (Kernel, P);
         begin
            Data.Set_Return_Value
              (Create_VCS_Instance (Get_Script (Data), F));
         end;

      elsif Command = "active_vcs" then
         declare
            V : constant VCS_Engine_Access := Active_VCS (Kernel);
         begin
            if V /= null then
               Data.Set_Return_Value
                 (Create_VCS_Instance (Get_Script (Data), V));
            end if;
         end;

      elsif Command = "vcs_in_use" then
         declare
            procedure On_VCS (VCS : not null access VCS_Engine'Class);
            procedure On_VCS (VCS : not null access VCS_Engine'Class) is
            begin
               Data.Set_Return_Value
                 (Create_VCS_Instance (Get_Script (Data), VCS));
            end On_VCS;
         begin
            Data.Set_Return_Value_As_List;
            For_Each_VCS (Kernel, On_VCS'Access);
         end;
      end if;
   end Static_VCS_Handler;

   ----------------------
   -- Register_Scripts --
   ----------------------

   procedure Register_Scripts
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      VCS : constant Class_Type := Kernel.Scripts.New_Class (VCS_Class_Name);
   begin
      Kernel.Scripts.Register_Command
        ("_register",
         Params        => (1 => Param ("name"),
                           2 => Param ("construct"),
                           3 => Param ("default_status"),
                           4 => Param ("discover_working_dir")),
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get",
         Params        => (1 => Param ("project")),
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("vcs_in_use",
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("active_vcs",
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);

      Kernel.Scripts.Register_Property
        ("name",
         Class         => VCS,
         Getter        => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_files",
         Params        => (1 => Param ("files")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_project",
         Params        => (1 => Param ("project")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_all_source_files",
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_run_in_background",
         Params        => (1 => Param ("background")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("invalidate_status_cache",
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_file_status",
         Params        => (1 => Param ("file")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("_set_file_status",
         Params        => (1 => Param ("file"),
                           2 => Param ("status", Optional => True),
                           3 => Param ("version",    Optional => True),
                           4 => Param ("repo_version", Optional => True)),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("_override_status_display",
         Params        => (1 => Param ("status"),
                           2 => Param ("label"),
                           3 => Param ("icon_name")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
   end Register_Scripts;

end VCS2.Scripts;
