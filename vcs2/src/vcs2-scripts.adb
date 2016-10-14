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
with GPS.Scripts;           use GPS.Scripts;
with GPS.VCS;               use GPS.VCS;
with GPS.VCS_Engines;       use GPS.VCS_Engines;

package body VCS2.Scripts is
   Me : constant Trace_Handle := Create ("VCS2.SCRIPT") with Unreferenced;

   type Script_Engine_Factory is new VCS_Engine_Factory with record
      Kernel    : access Kernel_Handle_Record'Class;
      Construct : Subprogram_Type;
      Find_Repo : Subprogram_Type;
   end record;
   overriding function Create_Engine
     (Self : not null access Script_Engine_Factory;
      Repo : String)
     return not null VCS_Engine_Access;
   overriding function Find_Repo
     (Self  : not null access Script_Engine_Factory;
      File  : Virtual_File) return String;

   VCS_Class_Name        : constant String := "VCS2";

   type Engine_Proxy is new Script_Proxy with null record;
   overriding function Class_Name
     (Self : Engine_Proxy) return String is (VCS_Class_Name);

   package Engine_Proxies is new Script_Proxies
     (Element_Type => VCS_Engine_Access,
      Proxy        => Engine_Proxy);

   type Script_Engine is new VCS_Engine with record
      Factory : access VCS_Engine_Factory'Class;
      Script  : Scripting_Language;
      Inst    : Engine_Proxy;    --  instance of GPS.VCS
   end record;
   overriding procedure Free (Self : in out Script_Engine);
   overriding function Name
     (Self : not null access Script_Engine) return String;
   overriding procedure Async_Fetch_Status_For_File
     (Self    : not null access Script_Engine;
      File    : Virtual_File);
   overriding procedure Async_Fetch_Status_For_Project
     (Self    : not null access Script_Engine;
      Project : Project_Type);
   overriding procedure Async_Fetch_Status_For_All_Files
     (Self    : not null access Script_Engine);

   procedure Static_VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all script functions

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Script_Engine) is
   begin
      Free (Self.Inst);
      Free (VCS_Engine (Self));   --  inherited
   end Free;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : not null access Script_Engine) return String is
   begin
      return Self.Factory.Name;
   end Name;

   ---------------------------------
   -- Async_Fetch_Status_For_File --
   ---------------------------------

   overriding procedure Async_Fetch_Status_For_File
     (Self    : not null access Script_Engine;
      File    : Virtual_File)
   is
      Inst  : constant Class_Instance :=
        Engine_Proxies.Get_Or_Create_Instance
          (Self.Inst, VCS_Engine_Access (Self), Self.Script);
      F : Subprogram_Type := Get_Method (Inst, "async_fetch_status_for_file");
      D : Callback_Data'Class := Create (F.Get_Script, 1);
      Dummy : Boolean;
   begin
      D.Set_Nth_Arg (1, Create_File (F.Get_Script, File));
      Dummy := F.Execute (D);
      Free (F);
      Free (D);
   end Async_Fetch_Status_For_File;

   ------------------------------------
   -- Async_Fetch_Status_For_Project --
   ------------------------------------

   overriding procedure Async_Fetch_Status_For_Project
     (Self    : not null access Script_Engine;
      Project : Project_Type)
   is
      Inst  : constant Class_Instance :=
        Engine_Proxies.Get_Or_Create_Instance
          (Self.Inst, VCS_Engine_Access (Self), Self.Script);
      F     : Subprogram_Type :=
        Get_Method (Inst, "async_fetch_status_for_project");
      D     : Callback_Data'Class := Create (F.Get_Script, 1);
      Dummy : Boolean;
   begin
      D.Set_Nth_Arg (1, Create_Project (F.Get_Script, Project));
      Dummy := F.Execute (D);
      Free (F);
      Free (D);
   end Async_Fetch_Status_For_Project;

   --------------------------------------
   -- Async_Fetch_Status_For_All_Files --
   --------------------------------------

   overriding procedure Async_Fetch_Status_For_All_Files
     (Self    : not null access Script_Engine)
   is
      Inst  : constant Class_Instance :=
        Engine_Proxies.Get_Or_Create_Instance
          (Self.Inst, VCS_Engine_Access (Self), Self.Script);
      F     : Subprogram_Type :=
        Get_Method (Inst, "async_fetch_status_for_all_files");
      D     : Callback_Data'Class := Create (F.Get_Script, 0);
      Dummy : Boolean;
   begin
      Dummy := F.Execute (D);
      Free (D);
      Free (F);
   end Async_Fetch_Status_For_All_Files;

   -------------------
   -- Create_Engine --
   -------------------

   overriding function Create_Engine
     (Self : not null access Script_Engine_Factory;
      Repo : String)
     return not null VCS_Engine_Access
   is
      R : constant access Script_Engine := new Script_Engine;
      Script : constant Scripting_Language := Self.Construct.Get_Script;
      Data : Callback_Data'Class := Script.Create (1);
   begin
      Data.Set_Nth_Arg (1, Repo);
      Engine_Proxies.Store_In_Instance
        (R.Inst,
         Inst => Self.Construct.Execute (Data),
         Obj  => VCS_Engine_Access (R));
      Free (Data);

      R.Factory := Self;
      R.Script  := Script;
      return R;
   end Create_Engine;

   ---------------
   -- Find_Repo --
   ---------------

   overriding function Find_Repo
     (Self  : not null access Script_Engine_Factory;
      File  : Virtual_File) return String
   is
      Script : constant Scripting_Language := Self.Find_Repo.Get_Script;
      Data : Callback_Data'Class := Script.Create (1);
   begin
      Data.Set_Nth_Arg (1, Create_File (Script, File));

      return S : constant String := Self.Find_Repo.Execute (Data) do
         Free (Data);
      end return;
   end Find_Repo;

   -----------------
   -- VCS_Handler --
   -----------------

   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : constant Class_Instance := Data.Nth_Arg (1);
      VCS  : constant VCS_Engine_Access := Engine_Proxies.From_Instance (Inst);
   begin
      if Command = "name" then
         Data.Set_Return_Value (VCS.Name);

      elsif Command = "ensure_status_for_file" then
         VCS.Ensure_Status_For_File (Nth_Arg (Data, 2));

      elsif Command = "ensure_status_for_project" then
         VCS.Ensure_Status_For_Project (Get_Data (Data, 2));

      elsif Command = "ensure_status_for_all_files" then
         VCS.Ensure_Status_For_All_Files;

      elsif Command = "get_file_status" then
         declare
            Props : constant VCS_File_Properties :=
              VCS.File_Properties_From_Cache (Nth_Arg (Data, 2));
         begin
            Data.Set_Return_Value
              (VCS_File_Status'Pos (Props.Status));
         end;

      elsif Command = "set_file_status" then
         VCS.Set_File_Status_In_Cache
           (File  => Nth_Arg (Data, 2),
            Props =>
              (Status  => VCS_File_Status'Val
                  (Data.Nth_Arg (3, VCS_File_Status'Pos (Status_Unmodified))),
               Version      => To_Unbounded_String (Data.Nth_Arg (4, "")),
               Repo_Version => To_Unbounded_String (Data.Nth_Arg (5, ""))));

      elsif Command = "invalidate_status_cache" then
         VCS.Invalidate_File_Status_Cache;
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
            F.Find_Repo := Data.Nth_Arg (3);
            Register_Factory (Get_Kernel (Data), Data.Nth_Arg (1), F);
         end;

      elsif Command = "get" then
         declare
            P : constant Project_Type := Get_Data (Data, 1);
            F : constant not null VCS_Engine_Access :=
              Get_VCS (Kernel, P.Project_Path);
         begin
            Data.Set_Return_Value
              (Engine_Proxies.Get_Or_Create_Instance
                 (Script_Engine (F.all).Inst,
                  Obj    => F,
                  Script => Get_Script (Data)));
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
                           3 => Param ("find_repo")),
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get",
         Params        => (1 => Param ("project")),
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);

      Kernel.Scripts.Register_Property
        ("name",
         Class         => VCS,
         Getter        => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_file",
         Params        => (1 => Param ("file")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_project",
         Params        => (1 => Param ("project")),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ensure_status_for_all_files",
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
        ("set_file_status",
         Params        => (1 => Param ("file"),
                           2 => Param ("status", Optional => True),
                           3 => Param ("version",    Optional => True),
                           4 => Param ("repo_version", Optional => True)),
         Class         => VCS,
         Handler       => VCS_Handler'Access);
   end Register_Scripts;

end VCS2.Scripts;
