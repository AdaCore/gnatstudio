------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with GNAT.Strings;

with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Projects;            use GNATCOLL.Projects;

with Gtkada.MDI;                   use Gtkada.MDI;

with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.VCS;
with Log_Utils;                    use Log_Utils;

package body VCS_Utils is
   Me : constant Trace_Handle := Create ("GPS.VCS.UTILS");

   use type GNAT.Strings.String_Access;

   ---------------------------
   -- Display_Editor_Status --
   ---------------------------

   procedure Display_Editor_Status
     (Kernel : access Kernel_Handle_Record'Class;
      Ref    : VCS_Access;
      Status : File_Status_Record)
   is
      use type GPS.VCS.VCS_File_Status;
      Props : GPS.VCS.VCS_File_Properties;
      Stat  : GPS.VCS.VCS_File_Status;
   begin
      if Ref /= null then
         if Status.Status.Icon_Name.all = Unknown_Stock then
            Stat := GPS.VCS.Status_No_VCS;
         elsif Status.Status.Icon_Name.all = Added_Stock then
            Stat := GPS.VCS.Status_Staged_Added;
         elsif Status.Status.Icon_Name.all = Removed_Stock then
            Stat := GPS.VCS.Status_Staged_Deleted or GPS.VCS.Status_Deleted;
         elsif Status.Status.Icon_Name.all = Modified_Stock then
            Stat := GPS.VCS.Status_Staged_Modified or GPS.VCS.Status_Modified;
         elsif Status.Status.Icon_Name.all = Needs_Merge_Stock then
            Stat := GPS.VCS.Status_Conflict;
         elsif Status.Status.Icon_Name.all = Needs_Update_Stock then
            Stat := GPS.VCS.Status_Needs_Update;
         elsif Status.Status.Icon_Name.all = Up_To_Date_Stock then
            Stat := GPS.VCS.Status_Unmodified;
         else
            Stat := GPS.VCS.Status_Untracked;
         end if;

         Props :=
            (Status   => Stat,
             Version  =>
                (if Status.Working_Revision = null
                    or else Status.Working_Revision.all = "n/a"
                 then Null_Unbounded_String
                 else To_Unbounded_String (Status.Working_Revision.all)),
             Repo_Version =>
                (if Status.Repository_Revision = null
                    or else Status.Repository_Revision.all = "n/a"
                 then Null_Unbounded_String
                 else To_Unbounded_String (Status.Repository_Revision.all)));

         declare
            T : constant Project_Tree_Access := Get_Project_Tree (Kernel.all);
            Info : constant File_Info'Class :=
              File_Info'Class (T.Info_Set (Status.File).First_Element);
         begin
            Kernel.VCS.Get_VCS (Project (Info, Root_If_Not_Found => True))
              .Set_Files_Status_In_Cache ((1 => Status.File), Props);
         end;
      end if;
   end Display_Editor_Status;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Context : Selection_Context) return Virtual_File is
   begin
      if Has_Directory_Information (Context) then
         return Directory_Information (Context);
      end if;
      return Get_Current_Dir;
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Context : Selection_Context) return Virtual_File is
   begin
      if Has_File_Information (Context) then
         return File_Information (Context);
      end if;
      return GNATCOLL.VFS.No_File;
   end Get_Current_File;

   ----------------
   -- Save_Files --
   ----------------

   function Save_Files
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Files     : File_Array;
      Activity  : Activity_Id := No_Activity;
      Save_Logs : Boolean     := False) return Boolean
   is
      Children     : MDI_Child_Array (1 .. Files'Length);
      Logs         : MDI_Child_Array (Children'Range);
      Activity_Log : MDI_Child_Array (1 .. 1);
      File         : Virtual_File;

   begin
      for C in Children'Range loop
         File := Files (C - Children'First + Files'First);
         Children (C) := Get_File_Editor (Kernel, File);

         if Save_Logs then
            Logs (C) := Get_File_Editor
              (Kernel, Get_Log_From_File (Kernel, File, False));
         end if;
      end loop;

      if Save_Logs then
         if Activity /= No_Activity then
            Activity_Log (1) := Get_File_Editor
              (Kernel, Get_Log_File (Kernel, Activity));
            return Save_MDI_Children
              (Kernel, Children & Logs & Activity_Log,
               Force => Auto_Save.Get_Pref);

         else
            return Save_MDI_Children
              (Kernel, Children & Logs, Force => Auto_Save.Get_Pref);
         end if;

      else
         return Save_MDI_Children
           (Kernel, Children, Force => Auto_Save.Get_Pref);
      end if;
   end Save_Files;

   -------------------------
   -- Update_Files_Status --
   -------------------------

   procedure Update_Files_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Status         : File_Status_List.Vector;
      VCS_Identifier : VCS_Access;
      Clear_Logs     : Boolean;
      Up_To_Date     : VCS_File_Status) is
   begin
      for S of Status loop
         declare
            File    : constant Virtual_File := S.File;
            Success : Boolean;
         begin
            --  Clear the logs

            if Clear_Logs and then S.Status = Up_To_Date then
               declare
                  Log : constant Virtual_File :=
                          Get_Log_From_File (Kernel, File, False);
               begin
                  if Log /= No_File and then Is_Regular_File (Log) then
                     Delete (Log, Success);
                     Open_File_Action_Hook.Run
                         (Kernel,
                          File      => Log,
                          Project   => No_Project,
                          Line      => -1);  --  close all editors
                  end if;

                  Remove_File_From_Mapping (Kernel, File);
               end;
            end if;

            --  Display the editor status

            if Is_Open (Kernel, File) and then VCS_Identifier /= null then
               Display_Editor_Status (Kernel, VCS_Identifier, S);
            end if;

         exception
            when E : others => Trace (Me, E);
         end;
      end loop;
   end Update_Files_Status;

   --------------------
   -- Revision_Lower --
   --------------------

   function Revision_Lower (Rev1, Rev2 : String) return Boolean is

      N_Size : constant := 6;
      --  Number of digits used for normalized numbers

      procedure Normalize (Source : String; Dest : out String);
      --  Normalize revision number in Source and store it in Dest

      ---------------
      -- Normalize --
      ---------------

      procedure Normalize (Source : String; Dest : out String) is
         J, K : Natural := Source'First;
         D    : Natural := Dest'First;
      begin
         if Source = "" then
            return;
         end if;

         loop
            J := Index (Source (K .. Source'Last), ".");
            if J = 0 then
               J := Source'Last;
            else
               J := J - 1;
            end if;

            Dest (D .. D + N_Size - 1) :=
              ((N_Size - (J - K + 1)) * '0') & Source (K .. J);
            D := D + N_Size;

            exit when J = Source'Last;
            K := J + 2;
         end loop;
      end Normalize;

      R1 : String (1 .. Rev1'Length * N_Size);
      R2 : String (1 .. Rev2'Length * N_Size);
   begin
      Normalize (Rev1, R1);
      Normalize (Rev2, R2);
      return R1 < R2;
   end Revision_Lower;

end VCS_Utils;
