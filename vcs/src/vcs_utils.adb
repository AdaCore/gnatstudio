------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with GNAT.Strings;

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with VCS_Module;                use VCS_Module;
with Log_Utils;                 use Log_Utils;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;

package body VCS_Utils is
   Me : constant Trace_Handle := Create ("VCS_UTILS");

   use type GNAT.Strings.String_Access;

   Max_Rev_Length : constant := 10;
   --  The maximum length of a revision string, in characters. Revisions longer
   --  than this will be krunched when displayed in the editors.

   ---------------------------
   -- Display_Editor_Status --
   ---------------------------

   procedure Display_Editor_Status
     (Kernel : access Kernel_Handle_Record'Class;
      Ref    : VCS_Access;
      Status : File_Status_Record)
   is
      function Short_Revision return String;
      --  If R is too long, return only the last digits

      --------------------
      -- Short_Revision --
      --------------------

      function Short_Revision return String is
      begin
         if Status.Working_Revision = null
           or else Status.Working_Revision.all = "n/a"
         then
            return "";
         elsif Status.Working_Revision'Length <= Max_Rev_Length then
            return Status.Working_Revision.all;
         else
            return "[...]" & Status.Working_Revision
              (Status.Working_Revision'Last - Max_Rev_Length ..
                 Status.Working_Revision'Last);
         end if;
      end Short_Revision;

      Label   : GNAT.Strings.String_Access;

   begin
      if Ref = null then
         return;
      end if;

      if Status.Status = VCS.Unknown
        or else Status.Status.Label = null
      then
         Label := new String'("");
      else
         declare
            R : constant String := Short_Revision;
         begin
            if R = "" then
               Label := new String'(Status.Status.Label.all);
            else
               Label := new String'(R & " (" & Status.Status.Label.all & ")");
            end if;
         end;
      end if;

      Add_Editor_Label
        (Kernel, Status.File, VCS_Module_Name, Label.all,
         Tooltip => "Status for <b>" & Name (Ref) & "</b>: " & Label.all,
         Icon    => Status.Status.Stock_Id.all);

      GNAT.Strings.Free (Label);
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
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Clear_Logs     : Boolean;
      Up_To_Date     : VCS.File_Status)
   is
      use type File_Status_List.List_Node;
      Iter : File_Status_List.List_Node := File_Status_List.First (Status);
   begin
      while Iter /= File_Status_List.Null_Node loop
         declare
            S       : constant File_Status_Record :=
                        File_Status_List.Data (Iter);
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
                     Close_File_Editors (Kernel, Log);
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

         Iter := File_Status_List.Next (Iter);
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
