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

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with VCS_Module;                use VCS_Module;
with Basic_Types;               use Basic_Types;
with VFS;                       use VFS;
with Log_Utils;                 use Log_Utils;

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

end VCS_Utils;
