-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

package body Src_Info.Dummy is

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   function Case_Insensitive_Identifiers
     (Handler : access Dummy_LI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Case_Insensitive_Identifiers;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler         : access Dummy_LI_Handler_Record;
      File            : in out LI_File_Ptr;
      Source_Filename : String;
      List            : LI_File_List;
      Project         : Projects.Project_Type)
   is
      pragma Unreferenced (Handler, File, Source_Filename, List, Project);
   begin
      null;
   end Create_Or_Complete_LI;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler       : access Dummy_LI_Handler_Record;
      Root_Project  : Projects.Project_Type;
      Project       : Projects.Project_Type;
      Recursive     : Boolean := False)
      return LI_Handler_Iterator'Class
   is
   begin
      raise Program_Error;
      return Generate_LI_For_Project
        (Handler, Root_Project, Project, Recursive);
   end Generate_LI_For_Project;

   ----------------------------
   -- Generate_LI_For_Source --
   ----------------------------

   function Generate_LI_For_Source
     (Handler       : access Dummy_LI_Handler_Record;
      Root_Project  : Projects.Project_Type;
      File_Project  : Projects.Project_Type;
      Full_Filename : String)
      return LI_Handler_Iterator'Class
   is
   begin
      raise Program_Error;
      return Generate_LI_For_Source
        (Handler, Root_Project, File_Project, Full_Filename);
   end Generate_LI_For_Source;

   -----------------------------
   -- LI_Filename_From_Source --
   -----------------------------

   function LI_Filename_From_Source
     (Handler         : access Dummy_LI_Handler_Record;
      Source_Filename : String;
      Project         : Projects.Project_Type) return String
   is
      pragma Unreferenced (Handler, Source_Filename, Project);
   begin
      return "";
   end LI_Filename_From_Source;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Handler      : access Dummy_LI_Handler_Record;
      List         : LI_File_List;
      In_Directory : String;
      Project      : Projects.Project_Type)
   is
      pragma Unreferenced (Handler, List, In_Directory, Project);
   begin
      null;
   end Parse_All_LI_Information;

end Src_Info.Dummy;
