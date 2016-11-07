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

   Default_Display_Unmodified : constant Status_Display :=
     (Label     => To_Unbounded_String ("Up to date"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-up-to-date"));
   Default_Display_Modified : constant Status_Display :=
     (Label     => To_Unbounded_String ("Modified"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-modified"));
   Default_Display_Deleted  : constant Status_Display :=
     (Label     => To_Unbounded_String ("Removed"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-removed"));
   Default_Display_Untracked : constant Status_Display :=
     (Label     => To_Unbounded_String ("Untracked"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-not-registered"));
   Default_Display_Ignored : constant Status_Display :=
     (Label     => To_Unbounded_String ("Ignored"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-unknown"));  --  ???
   Default_Display_Added : constant Status_Display :=
     (Label     => To_Unbounded_String ("Added"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-added"));
   Default_Display_Staged : constant Status_Display :=
     (Label     => To_Unbounded_String ("Staged"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-needs-merge")); --  ???
   Default_Display_Conflict : constant Status_Display :=
     (Label     => To_Unbounded_String ("Conflict"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-has-conflicts"));
   Default_Display_Needs_Update : constant Status_Display :=
     (Label     => To_Unbounded_String ("Needs update"),
      Icon_Name => To_Unbounded_String ("gps-emblem-vcs-needs-update"));

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
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (VCS_Command'Class, VCS_Command_Access);

   function Need_Update_For_Files
     (Self    : not null access VCS_Engine'Class;
      Sources : File_Array)
      return Boolean;
   --  Return True if any of the files in Sources needs an update of its status
   --  in the cache.
   --  Also mark all files as not needing update, so that multiple calls to
   --  Ensure_Status_* do not result in multiple parallel computation of the
   --  status.

   type Cmd_Ensure_Status_For_Files (Size : Natural) is
      new VCS_Command with record
         Files : File_Array (1 .. Size);
      end record;
   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_Files;
       VCS  : not null access VCS_Engine'Class);
   --  Implementation for Ensure_Status_For_Files

   type Cmd_Ensure_Status_For_Project is new VCS_Command with record
      Project : Project_Type;
   end record;
   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_Project;
       VCS  : not null access VCS_Engine'Class);
   --  Implementation for Ensure_Status_For_Project

   type Cmd_Ensure_Status_For_All_Files is new VCS_Command with
      null record;
   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_All_Files;
       VCS  : not null access VCS_Engine'Class);
   --  Implementation for Ensure_Status_For_All_Source_Files

   procedure Queue
      (Self    : not null access VCS_Engine'Class;
       Command : VCS_Command_Access);
   --  Queue a new command for VCS.
   --  Free Command eventually.

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
      return not null access VCS_Engine'Class is
   begin
      --  In case it wasn't initialized before
      Global_Data.No_VCS_Engine.Kernel := Kernel_Handle (Kernel);
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

   -----------------------------
   -- Guess_VCS_For_Directory --
   -----------------------------

   function Guess_VCS_For_Directory
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Directory : Virtual_File) return not null VCS_Engine_Access
   is
      VCS : VCS_Engine_Access;
      D   : Virtual_File;
      Dir : Virtual_File := Directory;
   begin
      if Directory /= No_File then
         loop
            VCS := Get_VCS (Kernel, Dir);
            if VCS /= Global_Data.No_VCS_Engine then
               return VCS;
            end if;
            D := Dir.Get_Parent;

            --  Avoid corner cases, for instance when Dir only contains
            --  file information with no directory
            if D = No_File or else D = Dir then
               exit;
            end if;
            Dir := D;
         end loop;
      end if;

      return Global_Data.No_VCS_Engine;
   end Guess_VCS_For_Directory;

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
         if F = No_File then
            null;
         elsif not Has_Element (C) then
            Self.Cache.Include
              (F, (Need_Update => False, Props => Default_Properties));
            if not Need_Update and then Active (Me) then
               Trace
                  (Me, "Will fetch status because " & F.Display_Full_Name
                   & " not in cache");
            end if;
            Need_Update := True;

         elsif Element (C).Need_Update then
            Self.Cache.Include
              (F, (Need_Update => False, Props => Element (C).Props));
            if not Need_Update and then Active (Me) then
               Trace
                  (Me, "Will fetch status because " & F.Display_Full_Name
                   & " needs update");
            end if;
            Need_Update := True;
         end if;
      end loop;

      return Need_Update;
   end Need_Update_For_Files;

   ----------------------------
   -- Ensure_Status_For_File --
   ----------------------------

   procedure Ensure_Status_For_Files
     (Self    : not null access VCS_Engine;
      Files   : File_Array) is
   begin
      Queue (Self, new Cmd_Ensure_Status_For_Files'
         (Size => Files'Length, Files => Files));
   end Ensure_Status_For_Files;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_Files;
       VCS  : not null access VCS_Engine'Class) is
   begin
      Trace (Me, "Ensure status for a set of files");
      if Need_Update_For_Files (VCS, Self.Files) then
         VCS.Async_Fetch_Status_For_Files (Self.Files);
      end if;
   end Execute;

   -------------------------------
   -- Ensure_Status_For_Project --
   -------------------------------

   procedure Ensure_Status_For_Project
     (Self    : not null access VCS_Engine;
      Project : Project_Type) is
   begin
      Queue (Self, new Cmd_Ensure_Status_For_Project'(Project => Project));
   end Ensure_Status_For_Project;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_Project;
       VCS  : not null access VCS_Engine'Class)
   is
      S : File_Array_Access := Self.Project.Source_Files (Recursive => False);
      N : constant Boolean := Need_Update_For_Files (VCS, S.all);
   begin
      if Active (Me) then
         Trace (Me, "Ensure status for project " & Self.Project.Name
                & " => " & N'Img);
      end if;
      if N then
         VCS.Async_Fetch_Status_For_Project (Self.Project);
      end if;
      Unchecked_Free (S);
   end Execute;

   ----------------------------------------
   -- Ensure_Status_For_All_Source_Files --
   ----------------------------------------

   procedure Ensure_Status_For_All_Source_Files
     (Self    : not null access VCS_Engine) is
   begin
      Queue (Self, new Cmd_Ensure_Status_For_All_Files);
   end Ensure_Status_For_All_Source_Files;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self : not null access Cmd_Ensure_Status_For_All_Files;
       VCS  : not null access VCS_Engine'Class)
   is
      pragma Unreferenced (Self);
      Iter : Project_Iterator :=
        Get_Project (VCS.Kernel).Start (Recursive => True);
      N    : Boolean := False;
      P    : Project_Type;
      F    : File_Array_Access;
   begin
      Trace (Me, "Ensure status for all source files");
      loop
         P := Current (Iter);
         exit when P = No_Project;

         if Get_VCS (VCS.Kernel, P) = VCS then
            --  Need to call this for all projects to initialize table
            F := P.Source_Files (Recursive => False);
            N := Need_Update_For_Files (VCS, F.all) or N;
            Unchecked_Free (F);
         end if;

         Next (Iter);
      end loop;

      if N then
         VCS.Async_Fetch_Status_For_All_Files;
      end if;
   end Execute;

   -----------
   -- Queue --
   -----------

   procedure Queue
      (Self    : not null access VCS_Engine'Class;
       Command : VCS_Command_Access)
   is
      Cmd : VCS_Command_Access;
   begin
      if Self.Run_In_Background > 0 then
         --  Allow users to directly pass a "new " as parameter
         Self.Queue.Append (Command.all'Unchecked_Access);
      else
         Command.Execute (Self);
         Command.Free;
         Cmd := Command;
         Unchecked_Free (Cmd);
      end if;
   end Queue;

   ---------------------------
   -- Set_Run_In_Background --
   ---------------------------

   procedure Set_Run_In_Background
      (Self       : not null access VCS_Engine'Class;
       Background : Boolean)
   is
      Cmd : VCS_Command_Access;
   begin
      if Background then
         Self.Run_In_Background := Self.Run_In_Background + 1;
      else
         Self.Run_In_Background := Self.Run_In_Background - 1;
      end if;

      if Active (Me) then
         Trace (Me, Self.Name & " in background" & Self.Run_In_Background'Img);
      end if;

      if Self.Run_In_Background <= 0 then
         Self.Run_In_Background := 0;  --  just in case

         --  Execute next command in queue

         if not Self.Queue.Is_Empty then
            Cmd := Self.Queue.First_Element;
            Self.Queue.Delete_First;

            Cmd.Execute (Self);
            Cmd.Free;
            Unchecked_Free (Cmd);
         end if;
      end if;
   end Set_Run_In_Background;

   --------------------------------
   -- File_Properties_From_Cache --
   --------------------------------

   function File_Properties_From_Cache
     (Self    : not null access VCS_Engine;
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
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File := No_File)
   is
      C : VCS_File_Cache.Cursor;
   begin
      if File = No_File then
         for F of Self.Cache loop
            F.Need_Update := True;
         end loop;

         --  ??? Would be nice to refresh, but we don't know what info
         --  is needed.

      else
         C := Self.Cache.Find (File);
         if Has_Element (C) then
            Self.Cache.Reference (C).Need_Update := True;

            --  Force a refresh immediately in this case since we
            --  know what needs refreshing
            Self.Ensure_Status_For_Files ((1 => File));
         end if;
      end if;
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
      Need_Update : Boolean;
      Need_Hook   : Boolean;
   begin
      if Has_Element (C) then
         Need_Update := Props /= Element (C).Props;
         Need_Hook := Need_Update;
      else
         Need_Update := True;
         Need_Hook := Props /= Default_Properties;
      end if;

      if Need_Update then
         Self.Cache.Include
           (File,
            (Need_Update  => False,
             Props        => Props));

         if Need_Hook then
            Vcs_File_Status_Changed_Hook.Run
              (Self.Kernel,
               Vcs    => Self,
               File   => File,
               Props  => Props);
         end if;
      end if;
   end Set_File_Status_In_Cache;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
     (Self   : not null access VCS_Engine'Class;
      Status : VCS_File_Status) return Status_Display
   is
      C : constant VCS_Status_Displays.Cursor := Self.Displays.Find (Status);
   begin
      --  Has the VCS defined specific display for this combination of flags ?
      if Has_Element (C) then
         return Element (C);
      else
         --  Fallbacks by looking at a subset of the flags
         if (Status and Status_Modified) /= 0 then
            return Default_Display_Modified;
         elsif (Status and (Status_Deleted or Status_Staged_Deleted)) /= 0 then
            return Default_Display_Deleted;
         elsif (Status and Status_Untracked) /= 0 then
            return Default_Display_Untracked;
         elsif (Status and Status_Ignored) /= 0 then
            return Default_Display_Ignored;
         elsif (Status and Status_Staged_Added) /= 0 then
            return Default_Display_Added;
         elsif (Status and (Status_Staged_Modified
                           or Status_Staged_Renamed
                           or Status_Staged_Copied)) /= 0
         then
            return Default_Display_Staged;
         elsif (Status and Status_Conflict) /= 0 then
            return Default_Display_Conflict;
         elsif (Status and Status_Needs_Update) /= 0 then
            return Default_Display_Needs_Update;
         else
            return Default_Display_Unmodified;
         end if;
      end if;
   end Get_Display;

   ----------------------
   -- Override_Display --
   ----------------------

   procedure Override_Display
     (Self    : not null access VCS_Engine'Class;
      Status  : VCS_File_Status;
      Display : Status_Display) is
   begin
      Self.Displays.Include (Status, Display);
   end Override_Display;

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

   --------------------------
   -- Get_Tooltip_For_File --
   --------------------------

   function Get_Tooltip_For_File
     (VCS     : not null access VCS_Engine'Class;
      File    : GNATCOLL.VFS.Virtual_File)
     return String
   is
      Props : constant VCS_File_Properties :=
         VCS.File_Properties_From_Cache (File);
   begin
      if Props.Status /= Status_Untracked then
         return "<b>" & VCS.Name & " status</b>: "
           & To_String (VCS.Get_Display (Props.Status).Label)
           & (if Props.Version /= ""
              then ASCII.LF & "<b>" & VCS.Label_Version & "</b>: "
                 & To_String (Props.Version)
              else "")
           & (if Props.Repo_Version /= ""
              then ASCII.LF & "<b>" & VCS.Label_Repo_Version & "</b>: "
                 & To_String (Props.Repo_Version)
              else "");
      else
         return "";
      end if;
   end Get_Tooltip_For_File;

end GPS.VCS_Engines;
