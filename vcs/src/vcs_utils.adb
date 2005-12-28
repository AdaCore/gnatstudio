-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;            use Ada.Exceptions;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with VCS_Module;                use VCS_Module;
with Basic_Types;               use Basic_Types;
with Log_Utils;                 use Log_Utils;
with Traces;                    use Traces;

package body VCS_Utils is

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
      Status_Label   : String_Access;
      Revision_Label : String_Access;

      function Short_Revision (R : String) return String;
      --  If R is too long, return only the last digits.

      --------------------
      -- Short_Revision --
      --------------------

      function Short_Revision (R : String) return String is
      begin
         if R'Length <= Max_Rev_Length then
            return R;

         else
            return "[...]" & R (R'Last - Max_Rev_Length .. R'Last);
         end if;
      end Short_Revision;

      use String_List_Utils.String_List;
   begin
      if Ref = null then
         return;
      end if;

      if Status.Status = VCS.Unknown
        or else Status.Status.Label = null
      then
         Status_Label := new String'("");
      else
         Status_Label := new String'
           (" (" & Status.Status.Label.all & ")");
      end if;

      if not Is_Empty (Status.Working_Revision) then
         Revision_Label := new String'
           (Name (Ref) & ":"
            & Short_Revision (Head (Status.Working_Revision)));
      else
         Revision_Label := new String'(Name (Ref));
      end if;

      Add_Editor_Label
        (Kernel, Status.File, VCS_Module_Name,
         Revision_Label.all & Status_Label.all);

      Free (Status_Label);
      Free (Revision_Label);
   end Display_Editor_Status;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Context : Selection_Context_Access) return String
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File) then
            return Directory_Information (File);
         end if;
      end if;

      return Get_Current_Dir;
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Context : Selection_Context_Access) return Virtual_File
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         return File_Information (File);
      end if;

      return VFS.No_File;
   end Get_Current_File;

   ----------------
   -- Save_Files --
   ----------------

   function Save_Files
     (Kernel    : Kernel_Handle;
      Files     : String_List.List;
      Save_Logs : Boolean := False) return Boolean
   is
      use String_List;
      Children   : MDI_Child_Array (1 .. Length (Files));
      Logs       : MDI_Child_Array (Children'Range);
      Files_Temp : List_Node := First (Files);
      File       : Virtual_File;
   begin
      for C in Children'Range loop
         File := Create (Full_Filename => Head (Files));
         Children (C) := Get_File_Editor (Kernel, File);

         if Save_Logs then
            Logs (C) := Get_File_Editor
              (Kernel, Get_Log_From_File (Kernel, File, False));
         end if;

         Files_Temp := Next (Files_Temp);
      end loop;

      if Save_Logs then
         return Save_MDI_Children
           (Kernel, Children & Logs, Force => Get_Pref (Auto_Save));
      else
         return Save_MDI_Children
           (Kernel, Children, Force => Get_Pref (Auto_Save));
      end if;
   end Save_Files;

   -------------------------
   -- Update_Files_Status --
   -------------------------

   procedure Update_Files_Status
     (Kernel         : Kernel_Handle;
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
                  if Log /= VFS.No_File and then Is_Regular_File (Log) then
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
            when E : others =>
               Trace (Exception_Handle, "Unexpected exception: "
                      & Exception_Information (E));
         end;

         Iter := File_Status_List.Next (Iter);
      end loop;
   end Update_Files_Status;

end VCS_Utils;
