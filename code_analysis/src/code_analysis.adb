-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

package body Code_Analysis is

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node : Code_Analysis.File_Access;
      Line_Num  : Line_Id) return Line_Access
   is
      Line_Node : Line_Access;
   begin
      if File_Node.Lines (Integer (Line_Num)) /= null then
         return File_Node.Lines (Natural (Line_Num));
      end if;

      Line_Node := new Line;
      Line_Node.Number := Integer (Line_Num);
      File_Node.Lines (Integer (Line_Num)) := Line_Node;
      return Line_Node;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node  : File_Access;
      Sub_Id     : Subprogram_Id) return Subprogram_Access
   is
      Sub_Node : Subprogram_Access;
   begin
      if File_Node.Subprograms.Contains (String (Sub_Id.all)) then
         return File_Node.Subprograms.Element (Sub_Id.all);
      end if;

      Sub_Node := new Subprogram;
      Sub_Node.Name := Sub_Id;
      File_Node.Subprograms.Insert (String (Sub_Id.all), Sub_Node);
      return Sub_Node;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Project_Node : Project_Access;
      File_Id      : VFS.Virtual_File) return File_Access
   is
      File_Node : File_Access;
   begin
      if Project_Node.Files.Contains (File_Id) then
         return Project_Node.all.Files.Element (File_Id);
      end if;

      File_Node := new File;
      File_Node.Name := File_Id;
      Project_Node.Files.Insert (File_Id, File_Node);
      return File_Node;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Project_Id : VFS.Virtual_File) return Project_Access
   is
      Project_Node : Project_Access;
   begin
      if Projects.Contains (Project_Id) then
         return Projects.Element (Project_Id);
      end if;

      Project_Node := new Project;
      Project_Node.Name := Project_Id;
      Projects.Insert (Project_Id, Project_Node);
      return Project_Node;
   end Get_Or_Create;

   function Get_Tree return Code_Analysis_Tree is
      Tree : constant Code_Analysis_Tree := Projects'Access;
   begin
      return Tree;
   end Get_Tree;

   -------------------
   -- Free_Analysis --
   -------------------

   procedure Free_Analysis (Analysis_Id : in out Analysis) is
   begin
      if Analysis_Id.Coverage_Data /= null then
         Unchecked_Free (Analysis_Id.Coverage_Data);
      end if;
   end Free_Analysis;

   ---------------
   -- Free_Line --
   ---------------

   procedure Free_Line (Line_Node : in out Line_Access) is
   begin
      Free_Analysis (Line_Node.Analysis_Data);
      Unchecked_Free (Line_Node);
   end Free_Line;

   ---------------------
   -- Free_Subprogram --
   ---------------------

   procedure Free_Subprogram (Sub_Node : in out Subprogram_Access) is
   begin
      Free_Analysis (Sub_Node.Analysis_Data);
      Unchecked_Free (Sub_Node.Name);
      Unchecked_Free (Sub_Node);
   end Free_Subprogram;

   ---------------
   -- Free_File --
   ---------------

   procedure Free_File (File_Node : in out File_Access) is

      procedure Free_From_Cursor (Cursor : Subprogram_Maps.Cursor);
      --  To be used by Idefinite_Hashed_Maps.Iterate subprogram

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (Cursor : Subprogram_Maps.Cursor) is
         Sub_Node : Subprogram_Access := Subprogram_Maps.Element (Cursor);
      begin
         Free_Subprogram (Sub_Node);
      end Free_From_Cursor;

   begin

      for J in 1 .. File_Node.Lines'Length loop
         Free_Line (File_Node.Lines (J));
      end loop;

      File_Node.Subprograms.Iterate (Free_From_Cursor'Access);
      Free_Analysis (File_Node.Analysis_Data);
      Unchecked_Free (File_Node);
   end Free_File;

   ------------------
   -- Free_Project --
   ------------------

   procedure Free_Project (Project_Node : in out Project_Access) is

      procedure Free_From_Cursor (Cursor : File_Maps.Cursor);
      --  To be used by Idefinite_Hashed_Maps.Iterate subprogram

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (Cursor : File_Maps.Cursor) is
         File_Node : File_Access := File_Maps.Element (Cursor);
      begin
         Free_File (File_Node);
      end Free_From_Cursor;

   begin
      Project_Node.Files.Iterate (Free_From_Cursor'Access);
      Free_Analysis (Project_Node.Analysis_Data);
      Project_Maps.Delete (Projects, Project_Node.Name);
      Unchecked_Free (Project_Node);
   end Free_Project;

   ------------------------
   -- Free_Code_Analysis --
   ------------------------

   procedure Free_Code_Analysis is
      use Project_Maps;
      Cur          : Cursor := Projects.First;
      Project_Node : Project_Access;
   begin
      loop
         exit when Cur = No_Element;
         Project_Node := Element (Cur);
         Next (Cur);
         Free_Project (Project_Node);
      end loop;
   end Free_Code_Analysis;
end Code_Analysis;
