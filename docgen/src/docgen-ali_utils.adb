-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Ada.Text_IO;          use Ada.Text_IO;
with Output;               use Output;
with Prj;                  use Prj;
with Prj.Part;             use Prj.Part;
with Prj.Proc;             use Prj.Proc;
with Prj.Tree;             use Prj.Tree;
with Prj_API;              use Prj_API;
with Src_Info;             use Src_Info;
with Src_Info.ALI;         use Src_Info.ALI;
with Language_Handlers;       use Language_Handlers;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Docgen.ALI_Utils is

   ----------------------------
   -- Predefined_Source_Path --
   ----------------------------

   function Predefined_Source_Path return String is
   begin
      return "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adainclude";
   end Predefined_Source_Path;

   ----------------------------
   -- Predefined_Object_Path --
   ----------------------------

   function Predefined_Object_Path return String is
   begin
      return "/usr/local/gnat/lib/gcc-lib/i686-pc-linux-gnu/2.8.1/adalib";
   end Predefined_Object_Path;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Name : String;
      Handler      : access Language_Handlers.Language_Handler_Record'Class;
      Project_Tree : out Project_Node_Id;
      Project_View : out Project_Id)
   is
      procedure Report_Error (S : String);
      procedure Report_Error_Project
        (S       : String;
         Project : Project_Id);
      --  Output error messages from the project parser to the glide console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Put_Line ("**Error**: " & S);
      end Report_Error;

      procedure Report_Error_Project
        (S       : String;
         Project : Project_Id)
      is
         pragma Unreferenced (Project);
      begin
         Put_Line ("**Error**: " & S);
      end Report_Error_Project;

   begin
      Output.Set_Special_Output (Report_Error'Unrestricted_Access);
      Prj.Part.Parse (Project_Tree, Name, True);
      Prj.Proc.Process (Project_View, Project_Tree,
                        Report_Error_Project'Unrestricted_Access);

      if Project_View = Prj.No_Project then
         Put_Line ("*** Error loading project file '" & Name & "'");
         return;
      end if;

      Set_Project_View (Glide_Language_Handler (Handler), Project_View);
   end Load_Project;

   ------------------
   -- Load_LI_File --
   ------------------

   procedure Load_LI_File
     (Source_Info_List : in out Src_Info.LI_File_List;
      Handler          : Language_Handlers.Language_Handler;
      Project_View     : Prj.Project_Id;
      Source_Filename  : String;
      LI               : out Src_Info.LI_File_Ptr)
   is
      File_Project : Project_Id;
   begin
      --  This code is extracted from Locate_From_Source_And_Complete

      LI := Locate_From_Source (Source_Info_List, Source_Filename);
      File_Project := Get_Project_From_File
        (Project_View, Base_Name (Source_Filename));

      if File_Project = Prj.No_Project then
         return;
      end if;

      --  Create and parse the LI file
      Create_Or_Complete_LI
        (Handler                => Get_LI_Handler_From_File
           (Glide_Language_Handler (Handler), Source_Filename, File_Project),
         File                   => LI,
         Source_Filename        => Source_Filename,
         List                   => Source_Info_List,
         Project                => File_Project,
         Predefined_Source_Path => Predefined_Source_Path,
         Predefined_Object_Path => Predefined_Object_Path);
   end Load_LI_File;

   -------------------------
   -- Create_Lang_Handler --
   -------------------------

   function Create_Lang_Handler return Language_Handlers.Language_Handler is
      Handler : Glide_Language_Handler;
   begin
      Gtk_New (Handler);

      Register_LI_Handler
        (Handler, "Ada", new Src_Info.ALI.ALI_Handler_Record);

      Register_Language (Handler, "ada", null);
      Add_Language_Info
        (Handler, "ada",
         LI                  => Get_LI_Handler_By_Name (Handler, "Ada"),
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb");
      return Language_Handler (Handler);
   end Create_Lang_Handler;

end Docgen.ALI_Utils;
