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

   -----------------------
   -- Get_Analysis_Data --
   -----------------------

   function Get_Analysis (L : Line_Access) return Analysis_Access is
   begin
      return L.Analysis_Data;
   end Get_Analysis;

   -----------------------
   -- Get_Analysis_Data --
   -----------------------

   function Get_Analysis (S : Subprogram_Access) return Analysis_Access is
   begin
      return S.Analysis_Data;
   end Get_Analysis;

   -----------------------
   -- Get_Analysis_Data --
   -----------------------

   function Get_Analysis (F : File_Access) return Analysis_Access is
   begin
      return F.Analysis_Data;
   end Get_Analysis;

   -----------------------
   -- Get_Analysis_Data --
   -----------------------

   function Get_Analysis (P : Project_Access) return Analysis_Access is
   begin
      return P.Analysis_Data;
   end Get_Analysis;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (L : Line_Access) return Integer is
   begin
      return L.all.Number;
   end Get_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (S : Subprogram_Access) return Subprogram_Id is
   begin
      return S.all.Name;
   end Get_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (F : File_Access) return Subprogram_Id is
   begin
      return F.all.Name;
   end Get_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (P : Project_Access) return Subprogram_Id is
   begin
      return P.all.Name;
   end Get_Id;

   -------------
   -- Get_Map --
   -------------

   function Get_Map (S : Subprogram_Access) return Line_Maps.Map is
   begin
      return S.all.Lines;
   end Get_Map;

   -------------
   -- Get_Map --
   -------------

   function Get_Map (F : File_Access) return Subprogram_Maps.Map is
   begin
      return F.all.Subprograms;
   end Get_Map;

   -------------
   -- Get_Map --
   -------------

   function Get_Map (P : Project_Access) return File_Maps.Map is
   begin
      return P.all.Files;
   end Get_Map;

   ------------------------
   -- Get_Or_Create_Line --
   ------------------------

   function Get_Or_Create_Line
     (S_A : Subprogram_Access;
      L_I : Integer) return Line_Access is
      L_A : Line_Access;
   begin
      if S_A.all.Lines.Contains (L_I) then
         return S_A.all.Lines.Element (L_I);
      end if;
      L_A := new Line;
      L_A.Number := L_I;
      L_A.Analysis_Data := new Analysis;
      S_A.all.Lines.Insert (L_I, L_A);
      return L_A;
   end Get_Or_Create_Line;

   ------------------------------
   -- Get_Or_Create_Subprogram --
   ------------------------------

   function Get_Or_Create_Subprogram
     (F_A : File_Access;
      S_I : Subprogram_Id) return Subprogram_Access is
      S_A : Subprogram_Access;
   begin
      if F_A.all.Subprograms.Contains (S_I) then
         return F_A.all.Subprograms.Element (S_I);
      end if;
      S_A := new Subprogram;
      S_A.Name := S_I;
      S_A.Analysis_Data := new Analysis;
      F_A.all.Subprograms.Insert (String (S_I), S_A);
      return S_A;
   end Get_Or_Create_Subprogram;

   ------------------------
   -- Get_Or_Create_File --
   ------------------------

   function Get_Or_Create_File
     (P_A : Project_Access;
      S_I : Subprogram_Id) return File_Access is
      F_A : File_Access;
   begin
      if P_A.all.Files.Contains (S_I) then
         return P_A.all.Files.Element (S_I);
      end if;
      F_A := new File;
      F_A.Name := S_I;
      F_A.Analysis_Data := new Analysis;
      P_A.all.Files.Insert (String (S_I), F_A);
      return F_A;
   end Get_Or_Create_File;

   ---------------------------
   -- Get_Or_Create_Project --
   ---------------------------

   function Get_Or_Create_Project
     (S_I : Subprogram_Id) return Project_Access is
      P_A : Project_Access;
   begin
      if Projects.Contains (S_I) then
         return Projects.Element (S_I);
      end if;
      P_A := new Project;
      P_A.Name := S_I;
      P_A.Analysis_Data := new Analysis;
      Projects.Insert (String (S_I), P_A);
      return P_A;
   end Get_Or_Create_Project;

   -------------------
   -- Free_Analysis --
   -------------------

   procedure Free_Analysis (A : Analysis_Access) is
   begin
      if A.Coverage_Data /= null then
         Unchecked_Free_Coverage (A.Coverage_Data);
      end if;
   end Free_Analysis;

   ---------------
   -- Free_Line --
   ---------------

   procedure Free_Line (L_A : Line_Access) is
   begin
      if L_A.Analysis_Data /= null then
         Free_Analysis (L_A.Analysis_Data);
         Unchecked_Free_Analysis (L_A.Analysis_Data);
      end if;
   end Free_Line;

   ---------------------
   -- Free_Subprogram --
   ---------------------

   procedure Free_Subprogram (S_A : Subprogram_Access) is

      procedure Free_From_Cursor (C : Line_Maps.Cursor);
      --  Subprogram that Free the element pointed by the access
      --  itself pointed by the cursor

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (C : Line_Maps.Cursor) is
         L_A : Line_Access := Line_Maps.Element (C);
      begin
         Free_Line (L_A);
         Unchecked_Free_Line (L_A);
      end Free_From_Cursor;

   begin
      S_A.Lines.Iterate (Free_From_Cursor'Access);
      if S_A.Analysis_Data /= null then
         Free_Analysis (S_A.Analysis_Data);
         Unchecked_Free_Analysis (S_A.Analysis_Data);
      end if;
   end Free_Subprogram;

   ---------------
   -- Free_File --
   ---------------

   procedure Free_File (F_A : File_Access) is

      procedure Free_From_Cursor (C : Subprogram_Maps.Cursor);
      --  Subprogram that Free the element pointed by the access
      --  itself pointed by the cursor

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (C : Subprogram_Maps.Cursor) is
         S_A : Subprogram_Access := Subprogram_Maps.Element (C);
      begin
         Free_Subprogram (S_A);
         Unchecked_Free_Subprogram (S_A);
      end Free_From_Cursor;

   begin
      F_A.Subprograms.Iterate (Free_From_Cursor'Access);
      if F_A.Analysis_Data /= null then
         Free_Analysis (F_A.Analysis_Data);
         Unchecked_Free_Analysis (F_A.Analysis_Data);
      end if;
   end Free_File;

   ------------------
   -- Free_Project --
   ------------------

   procedure Free_Project (P_A : Project_Access) is

      procedure Free_From_Cursor (C : File_Maps.Cursor);
      --  Subprogram that Free the element pointed by the access
      --  itself pointed by the cursor

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (C : File_Maps.Cursor) is
         F_A : File_Access := File_Maps.Element (C);
      begin
         Free_File (F_A);
         Unchecked_Free_File (F_A);
      end Free_From_Cursor;

   begin
      P_A.Files.Iterate (Free_From_Cursor'Access);
      if P_A.Analysis_Data /= null then
         Free_Analysis (P_A.Analysis_Data);
         Unchecked_Free_Analysis (P_A.Analysis_Data);
      end if;
   end Free_Project;

end Code_Analysis;
