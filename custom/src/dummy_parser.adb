------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Entities;           use Entities;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;
with Language.Tree.Database; use Language.Tree.Database;

package body Dummy_Parser is

   type Dummy_LI_Handler_Record is new LI_Handler_Record with null record;

   overriding function Get_Name
     (LI : access Dummy_LI_Handler_Record) return String;
   overriding function Case_Insensitive_Identifiers
     (Handler : access Dummy_LI_Handler_Record) return Boolean;
   overriding function Get_Source_Info
     (Handler               : access Dummy_LI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Source_File;
   overriding function Parse_All_LI_Information
     (Handler   : access Dummy_LI_Handler_Record;
      Project   : Project_Type) return LI_Information_Iterator'Class;
   overriding function Generate_LI_For_Project
     (Handler   : access Dummy_LI_Handler_Record;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project   : Project_Type;
      Errors    : Error_Report;
      Recursive : Boolean := False)
      return LI_Handler_Iterator'Class;
   --  See doc for inherited subprograms

   type Dummy_LI_Handler_Iterator is new LI_Handler_Iterator with null record;
   overriding procedure Destroy (Iterator : in out Dummy_LI_Handler_Iterator);
   overriding procedure Continue
     (Iterator : in out Dummy_LI_Handler_Iterator;
      Errors   : Error_Report;
      Finished : out Boolean);
   --  See doc for inherited subprograms

   type Dummy_LI_Information_Iterator
     is new LI_Information_Iterator with null record;
   overriding procedure Next
     (Iter  : in out Dummy_LI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural);
   --  See doc for inherited subprograms

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Iterator : in out Dummy_LI_Handler_Iterator)
   is
      pragma Unreferenced (Iterator);
   begin
      null;
   end Destroy;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Iterator : in out Dummy_LI_Handler_Iterator;
      Errors   : Error_Report;
      Finished : out Boolean)
   is
      pragma Unreferenced (Iterator, Errors);
   begin
      Finished := True;
   end Continue;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   overriding function Case_Insensitive_Identifiers
     (Handler : access Dummy_LI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Case_Insensitive_Identifiers;

   ---------------------
   -- Get_Source_Info --
   ---------------------

   overriding function Get_Source_Info
     (Handler               : access Dummy_LI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Source_File
   is
      pragma Unreferenced (Handler, Source_Filename, File_Has_No_LI_Report);
   begin
      return null;
   end Get_Source_Info;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   overriding function Parse_All_LI_Information
     (Handler   : access Dummy_LI_Handler_Record;
      Project   : Project_Type) return LI_Information_Iterator'Class
   is
      pragma Unreferenced (Handler, Project);
      Iter : Dummy_LI_Information_Iterator;
   begin
      return Iter;
   end Parse_All_LI_Information;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   overriding function Generate_LI_For_Project
     (Handler   : access Dummy_LI_Handler_Record;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project   : Project_Type;
      Errors    : Error_Report;
      Recursive : Boolean := False)
      return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Handler, Project, Recursive, Lang_Handler, Errors);
      Iter : Dummy_LI_Handler_Iterator;
   begin
      return Iter;
   end Generate_LI_For_Project;

   -----------------------------
   -- Create_Dummy_LI_Handler --
   -----------------------------

   function Create_Dummy_LI_Handler return Entities.LI_Handler is
   begin
      return new Dummy_LI_Handler_Record;
   end Create_Dummy_LI_Handler;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (LI : access Dummy_LI_Handler_Record) return String
   is
      pragma Unreferenced (LI);
   begin
      return "";
   end Get_Name;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Iter  : in out Dummy_LI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural)
   is
      pragma Unreferenced (Iter, Steps);
   begin
      Count := 0;
      Total := 0;
   end Next;

end Dummy_Parser;
