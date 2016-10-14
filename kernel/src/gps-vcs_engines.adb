------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Project;          use GPS.Kernel.Project;

package body GPS.VCS_Engines is
   Me : constant Trace_Handle := Create ("VCS");

   use Name_To_Factory;

   package Project_To_Engine is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => VCS_Engine_Access,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => "=");
   use Project_To_Engine;

   type Kernel_Data is record
      Factories     : aliased Name_To_Factory.Map;
      VCS_Engines   : Project_To_Engine.Map;
      No_VCS_Engine : VCS_Engine_Access := new Dummy_VCS_Engine;
   end record;
   Global_Data : Kernel_Data;
   --  Data that will be stored in the kernel, once VCS2 is integrated.
   --  Not done yet to limit the amount of recompiling

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (VCS_Engine'Class, VCS_Engine_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (VCS_Engine_Factory'Class, VCS_Engine_Factory_Access);

   function Need_Update_For_Files
     (Self    : not null access VCS_Engine'Class;
      Sources : File_Array)
      return Boolean;
   --  Return True if any of the files in Sources needs an update of its status
   --  in the cache.
   --  Also mark all files as not needing update, so that multiple calls to
   --  Ensure_Status_* do not result in multiple parallel computation of the
   --  status.

   ----------------------
   -- Register_Factory --
   ----------------------

   procedure Register_Factory
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String;
      Factory : not null access VCS_Engine_Factory'Class)
   is
      pragma Unreferenced (Kernel);
      N : constant String := To_Lower (Name);
   begin
      Trace (Me, "Register VCS factory " & N);
      Factory.Name := To_Unbounded_String (N);
      Global_Data.Factories.Include (N, VCS_Engine_Factory_Access (Factory));
   end Register_Factory;

   -----------------------
   -- All_VCS_Factories --
   -----------------------

   function All_VCS_Factories
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return access Name_To_Factory.Map
   is
      pragma Unreferenced (Kernel);
   begin
      return Global_Data.Factories'Access;
   end All_VCS_Factories;

   ---------------------
   -- Get_VCS_Factory --
   ---------------------

   function Get_VCS_Factory
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String)
      return access VCS_Engine_Factory'Class
   is
      pragma Unreferenced (Kernel);
      C : constant Name_To_Factory.Cursor :=
        Global_Data.Factories.Find (To_Lower (Name));
   begin
      if Has_Element (C) then
         return Element (C);
      else
         return null;
      end if;
   end Get_VCS_Factory;

   -------------------
   -- No_VCS_Engine --
   -------------------

   function No_VCS_Engine
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return not null access VCS_Engine'Class
   is
      pragma Unreferenced (Kernel);
   begin
      return Global_Data.No_VCS_Engine;
   end No_VCS_Engine;

   -------------
   -- Get_VCS --
   -------------

   function Get_VCS
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Location : Virtual_File)
      return not null VCS_Engine_Access
   is
      pragma Unreferenced (Kernel);
      C : constant Project_To_Engine.Cursor :=
        Global_Data.VCS_Engines.Find (Location);
   begin
      if Has_Element (C) then
         return Element (C);
      else
         return Global_Data.No_VCS_Engine;
      end if;
   end Get_VCS;

   -------------
   -- Set_VCS --
   -------------

   procedure Set_VCS
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Location : Virtual_File;
      Engine   : not null VCS_Engine_Access)
   is
      Old : VCS_Engine_Access := Get_VCS (Kernel, Location);
      Old_No_Longer_Used : Boolean := True;
   begin
      Engine.Kernel := Kernel_Handle (Kernel);
      Global_Data.VCS_Engines.Include (Location, Engine);

      --  Should we free the old engine ?
      if Old /= Global_Data.No_VCS_Engine then
         for E of Global_Data.VCS_Engines loop
            if E = Old then
               Old_No_Longer_Used := False;
               exit;
            end if;
         end loop;

         if Old_No_Longer_Used then
            Free (Old.all);
            Unchecked_Free (Old);
         end if;
      end if;
   end Set_VCS;

   ----------
   -- Name --
   ----------

   function Name
     (Self : not null access VCS_Engine_Factory'Class) return String is
   begin
      return To_String (Self.Name);
   end Name;

   ---------------------------
   -- Need_Update_For_Files --
   ---------------------------

   function Need_Update_For_Files
     (Self    : not null access VCS_Engine'Class;
      Sources : File_Array)
     return Boolean
   is
      C : VCS_File_Cache.Cursor;
      Need_Update : Boolean := False;
   begin
      --  Set temporary entry to prevent unneeded parallel computation.
      --  Do not call the hook though, this will be done by Async_Fetch

      for F of Sources loop
         C := Self.Cache.Find (F);
         if not Has_Element (C) then
            Self.Cache.Include
              (F, (Need_Update => False, Props => Default_Properties));
            Need_Update := True;

         elsif Element (C).Need_Update then
            Self.Cache.Include
              (F, (Need_Update => False, Props => Element (C).Props));
            Need_Update := True;
         end if;
      end loop;

      return Need_Update;
   end Need_Update_For_Files;

   ----------------------------
   -- Ensure_Status_For_File --
   ----------------------------

   procedure Ensure_Status_For_File
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File) is
   begin
      if Need_Update_For_Files (Self, (1 => File)) then
         Self.Async_Fetch_Status_For_File (File);
      end if;
   end Ensure_Status_For_File;

   -------------------------------
   -- Ensure_Status_For_Project --
   -------------------------------

   procedure Ensure_Status_For_Project
     (Self    : not null access VCS_Engine'Class;
      Project : Project_Type)
   is
      S : File_Array_Access := Project.Source_Files (Recursive => False);
   begin
      if Need_Update_For_Files (Self, S.all) then
         Self.Async_Fetch_Status_For_Project (Project);
      end if;
      Unchecked_Free (S);
   end Ensure_Status_For_Project;

   ---------------------------------
   -- Ensure_Status_For_All_Files --
   ---------------------------------

   procedure Ensure_Status_For_All_Files
     (Self    : not null access VCS_Engine'Class)
   is
      S : File_Array_Access :=
        Get_Project (Self.Kernel).Source_Files (Recursive => True);
   begin
      if Need_Update_For_Files (Self, S.all) then
         Self.Async_Fetch_Status_For_All_Files;
      end if;
      Unchecked_Free (S);
   end Ensure_Status_For_All_Files;

   --------------------------------
   -- File_Properties_From_Cache --
   --------------------------------

   function File_Properties_From_Cache
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File)
      return VCS_File_Properties
   is
      C : constant VCS_File_Cache.Cursor := Self.Cache.Find (File);
   begin
      if Has_Element (C) then
         return Element (C).Props;
      else
         return Default_Properties;
      end if;
   end File_Properties_From_Cache;

   ----------------------------------
   -- Invalidate_File_Status_Cache --
   ----------------------------------

   procedure Invalidate_File_Status_Cache
     (Self    : not null access VCS_Engine'Class) is
   begin
      for F of Self.Cache loop
         F.Need_Update := True;
      end loop;
   end Invalidate_File_Status_Cache;

   ------------------------------
   -- Set_File_Status_In_Cache --
   ------------------------------

   procedure Set_File_Status_In_Cache
     (Self         : not null access VCS_Engine'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties := Default_Properties)
   is
      C : constant VCS_File_Cache.Cursor := Self.Cache.Find (File);
      Need_Update : constant Boolean :=
        not Has_Element (C) or else Props /= Element (C).Props;
   begin
      if Need_Update then
         Self.Cache.Include
           (File,
            (Need_Update  => False,
             Props        => Props));

         Vcs_File_Status_Update_Hook.Run
           (Self.Kernel,
            File          => File,
            Status        => Props.Status,
            Revision      => To_String (Props.Version),
            Repo_Revision => To_String (Props.Repo_Version));
      end if;
   end Set_File_Status_In_Cache;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Kernel : not null access Kernel_Handle_Record'Class) is
      pragma Unreferenced (Kernel);
      C1, C2 : Project_To_Engine.Cursor;
      E      : VCS_Engine_Access;
      F2     : VCS_Engine_Factory_Access;
      Should_Free : Boolean;
   begin
      C1 := Global_Data.VCS_Engines.First;
      while Has_Element (C1) loop
         E := Element (C1);

         C2 := Next (C1);
         Should_Free := True;
         while Has_Element (C2) loop
            if Element (C2) = E then
               Should_Free := False;
               exit;
            end if;
            Next (C2);
         end loop;

         if Should_Free then
            Free (E.all);
            Unchecked_Free (E);
         end if;

         Next (C1);
      end loop;
      Global_Data.VCS_Engines.Clear;

      Free (Global_Data.No_VCS_Engine.all);
      Unchecked_Free (Global_Data.No_VCS_Engine);

      for F of Global_Data.Factories loop
         F2 := F;
         Unchecked_Free (F2);
      end loop;
      Global_Data.Factories.Clear;
   end Finalize;

end GPS.VCS_Engines;
