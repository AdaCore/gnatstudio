-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Prj;         use Prj;
with Prj.Com;     use Prj.Com;
with Prj.Part;    use Prj.Part;
with Prj.Proc;    use Prj.Proc;
with Prj.Env;     use Prj.Env;
with Prj.Tree;    use Prj.Tree;
with Errout;      use Errout;
with Namet;       use Namet;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Text_IO;     use Text_IO;

with Project_Viewers;   use Project_Viewers;
with Glide_Main_Window; use Glide_Main_Window;
with Gtkada.MDI;        use Gtkada.MDI;
with Glide_Page;        use Glide_Page;
with GVD.Process;       use GVD.Process;

package body Glide_Kernel.Project is

   Project_Editor_Window_Name : constant String := "Project editor";

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (Kernel : access Kernel_Handle_Record'Class; Short_File_Name : String)
      return String
   is
      Path : String_Access;
   begin
      Path := Locate_Regular_File
        (Short_File_Name,
         Ada_Include_Path (Kernel.Project_View).all);

      if Path = null then
         return "";

      else
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;
   end Find_Source_File;

   ---------------------------
   -- Get_Project_File_Name --
   ---------------------------

   function Get_Project_File_Name
     (Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      return Get_Name_String (Name_Of (Kernel.Project));
   end Get_Project_File_Name;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'class; Project : String) is
   begin
      Prj.Part.Parse (Kernel.Project, Project, True);
      Kernel.Project_View := No_Project;
      Project_Changed (Kernel);
      Recompute_View (Kernel);
   end Load_Project;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Prj.Tree.Project_Node_Id is
   begin
      return Handle.Project;
   end Get_Project;

   ----------------------
   -- Get_Project_View --
   ----------------------

   function Get_Project_View
     (Handle : access Kernel_Handle_Record'Class) return Prj.Project_Id is
   begin
      return Handle.Project_View;
   end Get_Project_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Handle  : access Kernel_Handle_Record'Class)
   is
      procedure Report_Error (S : String);

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         --  ??? Errors should be reported in the Glide console, rather than
         --  ??? simply printed on the standard output.
         Put_Line ("Error: " & S);
      end Report_Error;

   begin
      Prj.Reset;
      Prj.Proc.Process (Handle.Project_View, Handle.Project,
                        Report_Error'Unrestricted_Access);
      pragma Assert (Handle.Project_View /= No_Project);

      Project_View_Changed (Handle);
   end Recompute_View;

   -------------------------
   -- Open_Project_Editor --
   -------------------------

   procedure Open_Project_Editor
     (Handle : access Kernel_Handle_Record'Class)
   is
      Top        : constant Glide_Window := Glide_Window (Handle.Main_Window);
      Page       : Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      MDI        : constant MDI_Window := Page.Process_Mdi;
      Child      : MDI_Child;
      Viewer     : Project_Viewer;
   begin
      if Get_Project (Handle) = Empty_Node then
         return;
      end if;

      Child := Find_MDI_Child (MDI, Project_Editor_Window_Name);
      if Child /= null then
         Raise_Child (Child);
         return;
      end if;

      Gtk_New (Viewer, Handle, Page.Explorer);
      Child := Put (MDI, Viewer);
      Set_Title (Child, Project_Editor_Window_Name);
   end Open_Project_Editor;

end Glide_Kernel.Project;

