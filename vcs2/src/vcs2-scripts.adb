------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with GNAT.Strings;          use GNAT.Strings;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
with GPS.VCS;               use GPS.VCS;
with GPS_Unbounded_String_Vectors;
with Informational_Popups;  use Informational_Popups;
with VCS2.Engines;          use VCS2.Engines;

package body VCS2.Scripts is
   Me : constant Trace_Handle := Create ("GPS.VCS.SCRIPT");

   VCS2_Task_Visitor_Class_Name : constant String :=
     "VCS2_Task_Visitor";

   type Task_Properties_Record is new Instance_Property_Record with record
      Visitor : access Task_Visitor'Class;
   end record;

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
     (Self      : not null access Script_Engine;
      From_User : Boolean);
   overriding function Default_File_Status
     (Self    : not null access Script_Engine)
      return VCS_File_Status is (Self.Factory.Default_Status);
   overriding procedure Stage_Or_Unstage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array;
      Stage   : Boolean);
   overriding procedure Async_Commit_Staged_Files
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Message : String);
   overriding procedure Async_Fetch_History
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Filter  : History_Filter);
   overriding procedure Async_Fetch_Commit_Details
     (Self        : not null access Script_Engine;
      Ids         : not null GNAT.Strings.String_List_Access;
      Visitor     : not null access Task_Visitor'Class);
   overriding procedure Async_Diff
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File := No_File);
   overriding procedure Async_View_File
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File);
   overriding procedure Async_Annotations
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      File        : Virtual_File);
   overriding procedure Async_Branches
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class);
   overriding procedure Async_Action_On_Branch
     (Self         : not null access Script_Engine;
      Visitor      : not null access Task_Visitor'Class;
      Action       : Branch_Action;
      Category, Id, Text : String);
   overriding procedure Async_Discard_Local_Changes
     (Self        : not null access Script_Engine;
      Files       : GNATCOLL.VFS.File_Array);
   overriding procedure Async_Checkout
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String);
   overriding procedure Async_Checkout_File
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String;
      File    : Virtual_File);
   overriding procedure Make_File_Writable
     (Self       : not null access Script_Engine;
      File       : GNATCOLL.VFS.Virtual_File;
      Writable   : Boolean);

   procedure Static_VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure VCS_Task_Handler
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
   procedure Set_Nth_Arg
     (Data    : in out Callback_Data'Class;
      Nth     : Integer;
      Visitor : not null access Task_Visitor'Class);
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

   ------------------------
   -- Make_File_Writable --
   ------------------------

   overriding procedure Make_File_Writable
     (Self       : not null access Script_Engine;
      File       : GNATCOLL.VFS.Virtual_File;
      Writable   : Boolean)
   is
      Inst  : constant Class_Instance :=
        Create_VCS_Instance (Self.Script, Self);
      F     : Subprogram_Type := Get_Method (Inst, "make_file_writable");
   begin
      if F /= null then
         declare
            Result : Boolean;
            Data  : Callback_Data'Class := Self.Script.Create (2);
         begin
            Data.Set_Nth_Arg (1, Create_File (Self.Script, File));
            Data.Set_Nth_Arg (2, Writable);
            Result := F.Execute (Data);
            Free (Data);
            Free (F);

            if not Result then
               VCS_Engine (Self.all).Make_File_Writable (File, Writable);
            end if;
         end;

      else
         --  Inherited
         VCS_Engine (Self.all).Make_File_Writable (File, Writable);
      end if;
   end Make_File_Writable;

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

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data    : in out Callback_Data'Class;
      Nth     : Integer;
      Visitor : not null access Task_Visitor'Class)
   is
      Script : constant Scripting_Language := Data.Get_Script;
      Inst : Class_Instance;
   begin
      --  First arg is the visitor
      Inst := Script.New_Instance
        (Script.Get_Repository.New_Class (VCS2_Task_Visitor_Class_Name));
      Set_Data (Inst, VCS2_Task_Visitor_Class_Name,
                Task_Properties_Record'(Visitor => Visitor));
      Data.Set_Nth_Arg (Nth, Inst);
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
     (Self      : not null access Script_Engine;
      From_User : Boolean)
   is
      D : Callback_Data'Class := Create (Self.Script, 1);
   begin
      D.Set_Nth_Arg (1, From_User);
      Call_Method (Self, "async_fetch_status_for_all_files", D);
   end Async_Fetch_Status_For_All_Files;

   -------------------------
   -- Async_Fetch_History --
   -------------------------

   overriding procedure Async_Fetch_History
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Filter  : History_Filter)
   is
      D    : Callback_Data'Class := Self.Script.Create (2);
      L    : List_Instance'Class := Self.Script.New_List;
   begin
      Set_Nth_Arg (D, 1, Visitor);

      --  Second arg and others are the filters
      L.Set_Nth_Arg (1, Filter.Up_To_Lines);
      if Filter.For_File = No_File then
         L.Set_Nth_Arg (2, No_Class_Instance);
      else
         L.Set_Nth_Arg (2, Create_File (Self.Script, Filter.For_File));
      end if;
      L.Set_Nth_Arg (3, To_String (Filter.Filter));
      L.Set_Nth_Arg (4, Filter.Current_Branch_Only);
      L.Set_Nth_Arg (5, Filter.Branch_Commits_Only);
      D.Set_Nth_Arg (2, L);
      Free (L);  --  now owned by D

      Call_Method (Self, "async_fetch_history", D);
   end Async_Fetch_History;

   --------------------------------
   -- Async_Fetch_Commit_Details --
   --------------------------------

   overriding procedure Async_Fetch_Commit_Details
     (Self        : not null access Script_Engine;
      Ids         : not null GNAT.Strings.String_List_Access;
      Visitor     : not null access Task_Visitor'Class)
   is
      D    : Callback_Data'Class := Create (Self.Script, 2);
      L    : List_Instance'Class := New_List (Self.Script);
   begin
      for Id in Ids'Range loop
         L.Set_Nth_Arg (Id - Ids'First + 1, Ids (Id).all);
      end loop;
      Set_Nth_Arg (D, 1, L);
      Free (L);   --  adopted by D

      Set_Nth_Arg (D, 2, Visitor);

      Call_Method (Self, "async_fetch_commit_details", D);
   end Async_Fetch_Commit_Details;

   -----------------------
   -- Async_Annotations --
   -----------------------

   overriding procedure Async_Annotations
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      File        : Virtual_File)
   is
      D    : Callback_Data'Class := Self.Script.Create (2);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Create_File (Self.Script, File));
      Call_Method (Self, "async_annotations", D);
   end Async_Annotations;

   --------------------
   -- Async_Branches --
   --------------------

   overriding procedure Async_Branches
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class)
   is
      D    : Callback_Data'Class := Self.Script.Create (1);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      Call_Method (Self, "async_branches", D);
   end Async_Branches;

   ----------------------------
   -- Async_Action_On_Branch --
   ----------------------------

   overriding procedure Async_Action_On_Branch
     (Self         : not null access Script_Engine;
      Visitor      : not null access Task_Visitor'Class;
      Action       : Branch_Action;
      Category, Id, Text : String)
   is
      D : Callback_Data'Class := Self.Script.Create (5);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Branch_Action'Pos (Action));
      D.Set_Nth_Arg (3, Category);
      D.Set_Nth_Arg (4, Id);
      D.Set_Nth_Arg (5, Text);
      Call_Method (Self, "async_action_on_branch", D);
   end Async_Action_On_Branch;

   ---------------------------------
   -- Async_Discard_Local_Changes --
   ---------------------------------

   overriding procedure Async_Discard_Local_Changes
     (Self        : not null access Script_Engine;
      Files       : GNATCOLL.VFS.File_Array)
   is
      D : Callback_Data'Class := Self.Script.Create (1);
   begin
      Set_Nth_Arg (D, 1, Files);
      Call_Method (Self, "async_discard_local_changes", D);
   end Async_Discard_Local_Changes;

   ----------------
   -- Async_Diff --
   ----------------

   overriding procedure Async_Diff
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File := No_File)
   is
      D    : Callback_Data'Class := Self.Script.Create (3);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Ref);

      if File = No_File then
         D.Set_Nth_Arg (3, No_Class_Instance);
      else
         D.Set_Nth_Arg (3, Create_File (Self.Script, File));
      end if;

      Call_Method (Self, "async_diff", D);
   end Async_Diff;

   ---------------------
   -- Async_View_File --
   ---------------------

   overriding procedure Async_View_File
     (Self        : not null access Script_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File)
   is
      D    : Callback_Data'Class := Self.Script.Create (3);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Ref);
      D.Set_Nth_Arg (3, Create_File (Self.Script, File));
      Call_Method (Self, "async_view_file", D);
   end Async_View_File;

   -------------------
   -- Create_Engine --
   -------------------

   overriding function Create_Engine
     (Self        : not null access Script_Engine_Factory;
      Working_Dir : Virtual_File)
      return not null VCS_Engine_Access
   is
      Script : constant Scripting_Language := Self.Construct.Get_Script;
      R      : constant not null VCS_Engine_Access :=
                 new Script_Engine'
                   (VCS_Engine with Factory => Self, Script => Script);
      Data   : Callback_Data'Class := Script.Create (1);
      Inst   : Class_Instance;

   begin
      Ensure_Directory (Working_Dir);
      Data.Set_Nth_Arg (1, Create_File (Script, Working_Dir));
      Inst := Self.Construct.Execute (Data);  -- create the pyton instance
      Free (Data);

      Set_VCS_Instance (R, Inst);  --  set binding with Ada

      Script_Engine'Class (R.all).Call_Method ("setup");

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

   ----------------------------
   -- Stage_Or_Unstage_Files --
   ----------------------------

   overriding procedure Stage_Or_Unstage_Files
     (Self    : not null access Script_Engine;
      Files   : GNATCOLL.VFS.File_Array;
      Stage   : Boolean)
   is
      Data  : Callback_Data'Class := Create (Self.Script, 2);
   begin
      Set_Nth_Arg (Data, 1, Files);
      Data.Set_Nth_Arg (2, Stage);
      Call_Method (Self, "stage_or_unstage_files", Data);
   end Stage_Or_Unstage_Files;

   -------------------------------
   -- Async_Commit_Staged_Files --
   -------------------------------

   overriding procedure Async_Commit_Staged_Files
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Message : String)
   is
      Data : Callback_Data'Class := Create (Self.Script, 2);
   begin
      Set_Nth_Arg (Data, 1, Visitor);
      Set_Nth_Arg (Data, 2, Message);
      Call_Method (Self, "async_commit_staged_files", Data);
   end Async_Commit_Staged_Files;

   --------------------
   -- Async_Checkout --
   --------------------

   overriding procedure Async_Checkout
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String)
   is
      D : Callback_Data'Class := Self.Script.Create (2);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Commit);
      Call_Method (Self, "async_checkout", D);
   end Async_Checkout;

   -------------------------
   -- Async_Checkout_File --
   -------------------------

   overriding procedure Async_Checkout_File
     (Self    : not null access Script_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String;
      File    : Virtual_File)
   is
      D : Callback_Data'Class := Self.Script.Create (3);
   begin
      Set_Nth_Arg (D, 1, Visitor);
      D.Set_Nth_Arg (2, Commit);
      D.Set_Nth_Arg (3, Create_File (Self.Script, File));
      Call_Method (Self, "async_checkout_file", D);
   end Async_Checkout_File;

   -----------------
   -- VCS_Handler --
   -----------------

   procedure VCS_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : constant Class_Instance := Data.Nth_Arg (1);
      VCS  : VCS_Engine_Access;
   begin
      if not Has_VCS (Inst) then
         --  the VCS engine has been freed, likely because the user
         --  has loaded another project
         --  ??? A better approach would be to stop background processing
         --  when we kill a VCS engine, but this is tricky to do.
         return;
      end if;

      VCS := VCS_Engine_Access (Get_VCS (Inst));

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
         VCS.Ensure_Status_For_All_Source_Files (From_User => False);

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
            List : constant List_Instance := Data.Nth_Arg (2);
            Count : constant Integer := List.Number_Of_Arguments;
            Files : GNATCOLL.VFS.File_Array (1 .. Count);
         begin
            Trace (Me, "Creating file_set from python");
            for Idx in 1 .. Count loop
               Files (Idx) := Nth_Arg (List, Idx);
            end loop;

            Trace (Me, "Calling Set_Files_Status_In_Cache "
               & Files'Length'Img);
            VCS.Set_Files_Status_In_Cache
              (Files => Files,
               Props =>
                 (Status       => Status,
                  Version      => Version,
                  Repo_Version => Repo_Version));
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

   ----------------------
   -- VCS_Task_Handler --
   ----------------------

   procedure VCS_Task_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst    : constant Class_Instance := Data.Nth_Arg (1);
      Prop    : Instance_Property;
      Visitor : access Task_Visitor'Class;
   begin
      Prop := Get_Data (Inst, VCS2_Task_Visitor_Class_Name);
      if Prop = null then
         return;
      end if;

      Visitor := Task_Properties_Record (Prop.all).Visitor;

      if Command = "success" then
         declare
            Kernel : constant Kernel_Handle := Get_Kernel (Data);
            Msg : constant String := Data.Nth_Arg (2);
         begin
            if Msg /= "" then
               Display_Informational_Popup
                  (Parent   => Get_Main_Window (Kernel),
                   Icon_Name => "github-commit-symbolic",
                   Text      => Msg);
            end if;
            Visitor.On_Success (Kernel);
         end;

      elsif Command = "history_line" then
         declare
            Line : constant List_Instance'Class := Data.Nth_Arg (2);
         begin
            declare
               Parents : constant List_Instance'Class := Line.Nth_Arg (5);
               Names   : constant List_Instance'Class := Line.Nth_Arg (6);
               P_Count : constant Natural := Parents.Number_Of_Arguments;
               N_Count : constant Natural := Names.Number_Of_Arguments;
               P       : GPS_Unbounded_String_Vectors.Vector;
               N       : Commit_Names_Access;
            begin
               if P_Count /= 0 then
                  for A in 1 .. P_Count loop
                     P.Append (Parents.Nth_Arg (A));
                  end loop;
               end if;

               if N_Count /= 0 then
                  N := new Commit_Names (1 .. N_Count);
                  for A in 1 .. N_Count loop
                     declare
                        B : constant List_Instance'Class :=
                          Names.Nth_Arg (A);
                     begin
                        N (A).Name := B.Nth_Arg (1);
                        N (A).Kind := Name_Kind'Val (B.Nth_Arg (2));
                     end;
                  end loop;
               end if;

               Visitor.On_History_Line
                 (ID       => Line.Nth_Arg (1),
                  Author   => Line.Nth_Arg (2),
                  Date     => Line.Nth_Arg (3),
                  Subject  => Line.Nth_Arg (4),
                  Parents  => P,
                  Names    => N,
                  Flags    => Commit_Flags'Val (Line.Nth_Arg (7, 0)));
               Free (N);
            end;
         end;

      elsif Command = "set_details" then
         Visitor.On_Commit_Details
           (ID      => Data.Nth_Arg (2),
            Header  => Data.Nth_Arg (3),
            Message => Data.Nth_Arg (4));

      elsif Command = "diff_computed" then
         Visitor.On_Diff_Computed
           (Diff    => Data.Nth_Arg (2));

      elsif Command = "file_computed" then
         Visitor.On_File_Computed
           (Contents => Data.Nth_Arg (2));

      elsif Command = "annotations" then
         declare
            Ids   : constant List_Instance'Class := Data.Nth_Arg (4);
            Texts : constant List_Instance'Class := Data.Nth_Arg (5);

            Text  : String_List_Access :=
              new String_List (1 .. Texts.Number_Of_Arguments);
            Id    : String_List_Access :=
              new String_List (1 .. Texts.Number_Of_Arguments);
            Non_Null : Natural := 0;
         begin
            for T in Text'Range loop
               declare
                  Commit : constant String := Ids.Nth_Arg (T);
               begin
                  if T = Text'First or else Commit /= Id (Non_Null).all then
                     Id (T)   := new String'(Commit);
                     Text (T) := new String'(Texts.Nth_Arg (T));
                     Non_Null := T;
                  end if;
               end;
            end loop;

            Visitor.On_Annotation
              (File       => Nth_Arg (Data, 2),
               First_Line => Data.Nth_Arg (3),
               Ids        => Id.all,
               Text       => Text.all);

            Free (Text);
            Free (Id);
         end;

      elsif Command = "branches" then
         declare
            List     : constant List_Instance'Class := Data.Nth_Arg (5);
            Branches : Branches_Array (1 .. List.Number_Of_Arguments);
         begin
            for B in Branches'Range loop
               declare
                  Current : constant List_Instance'Class := List.Nth_Arg (B);
               begin
                  Branches (B) :=
                    (Name       => new String'(Current.Nth_Arg (1)),
                     Is_Current => Current.Nth_Arg (2),
                     Emblem     => new String'(Current.Nth_Arg (3, "")),
                     Id         => new String'(Current.Nth_Arg (4, "")));
               end;
            end loop;

            Visitor.On_Branches
              (Category   => Data.Nth_Arg (2),
               Iconname   => Data.Nth_Arg (3),
               Can_Rename => Data.Nth_Arg (4),
               Branches   => Branches);

            for B of Branches loop
               Free (B.Name);
               Free (B.Emblem);
               Free (B.Id);
            end loop;
         end;

      elsif Command = "tooltip" then
         Visitor.On_Tooltip (Text => Data.Nth_Arg (2));
      end if;
   end VCS_Task_Handler;

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
            F : constant not null VCS_Engine_Factory_Access :=
                  new Script_Engine_Factory'
                    (VCS_Engine_Factory with
                     Kernel         => Kernel,
                     Construct      => Data.Nth_Arg (2),
                     Default_Status =>
                       VCS_File_Status (Integer'(Data.Nth_Arg (3))),
                     Find_Repo      => Data.Nth_Arg (4));

         begin
            Register_Factory (Get_Kernel (Data), Data.Nth_Arg (1), F);
         end;

      elsif Command = "get" then
         declare
            P : constant Project_Type := Get_Data (Data, 1);
            F : constant not null VCS_Engine_Access :=
               VCS_Engine_Access (Kernel.VCS.Get_VCS (P));
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

      elsif Command = "supported_systems" then
         declare
            Choices : Unbounded_String := To_Unbounded_String ("Auto");

            procedure On_Name (Name : String);
            procedure On_Name (Name : String) is
            begin
               Choices := Choices & ASCII.LF & Name;
            end On_Name;
         begin
            For_Each_Registered_Factory (Kernel, On_Name'Access);
            Data.Set_Return_Value (To_String (Choices));
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
      Task_Visitor : constant Class_Type :=
        Kernel.Scripts.New_Class (VCS2_Task_Visitor_Class_Name);
   begin
      Kernel.Scripts.Register_Command
        ("supported_systems",
         Static_Method => True,
         Class         => VCS,
         Handler       => Static_VCS_Handler'Access);
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

      Kernel.Scripts.Register_Command
        ("success",
         Params        => (1 => (Param ("msg", Optional => True))),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("history_line",
         Params        => (2 => Param ("line")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_details",
         Params        => (2 => Param ("id"),
                           3 => Param ("header"),
                           4 => Param ("message")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("diff_computed",
         Params        => (2 => Param ("diff")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("file_computed",
         Params        => (2 => Param ("contents")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("annotations",
         Params        => (2 => Param ("file"),
                           3 => Param ("first_line"),
                           4 => Param ("ids"),
                           5 => Param ("annotations")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("branches",
         Params        => (2 => Param ("category"),
                           3 => Param ("iconname"),
                           4 => Param ("can_rename"),
                           5 => Param ("branches")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
      Kernel.Scripts.Register_Command
        ("tooltip",
         Params        => (2 => Param ("text")),
         Class         => Task_Visitor,
         Handler       => VCS_Task_Handler'Access);
   end Register_Scripts;

end VCS2.Scripts;
