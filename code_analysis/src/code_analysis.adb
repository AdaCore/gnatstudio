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
     (S_A : Subprogram_Access;
      L_I : Line_Id) return Line_Access is
      L_A : Line_Access;
   begin
      if S_A.Lines.Contains (L_I) then
         return S_A.Lines.Element (L_I);
      end if;
      L_A := new Line;
      L_A.Number := L_I;
      S_A.Lines.Insert (L_I, L_A);
      return L_A;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (F_A : File_Access;
      S_I : Subprogram_Id) return Subprogram_Access is
      S_A : Subprogram_Access;
   begin
      if F_A.Subprograms.Contains (String (S_I.all)) then
         return F_A.Subprograms.Element (S_I.all);
      end if;
      S_A := new Subprogram;
      S_A.Name := S_I;
      F_A.Subprograms.Insert (String (S_I.all), S_A);
      return S_A;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (P_A : Project_Access;
      F_I : File_Id) return File_Access is
      F_A : File_Access;
   begin
      if P_A.Files.Contains (F_I.all) then
         return P_A.all.Files.Element (F_I.all);
      end if;
      F_A := new File;
      F_A.Name := F_I;
      P_A.Files.Insert (String (F_I.all), F_A);
      return F_A;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (P_I : Project_Id) return Project_Access is
      P_A : Project_Access;
   begin
      if Projects.Contains (P_I.all) then
         return Projects.Element (P_I.all);
      end if;
      P_A := new Project;
      P_A.Name := P_I;
      Projects.Insert (String (P_I.all), P_A);
      return P_A;
   end Get_Or_Create;

   -------------------
   -- Free_Analysis --
   -------------------

   procedure Free_Analysis (A : in out Analysis) is
   begin
      if A.Coverage_Data /= null then
         Unchecked_Free (A.Coverage_Data);
      end if;
   end Free_Analysis;

   ---------------
   -- Free_Line --
   ---------------

   procedure Free_Line (L_A : in out Line_Access) is
   begin
      Free_Analysis (L_A.Analysis_Data);
      Unchecked_Free (L_A);
   end Free_Line;

   ---------------------
   -- Free_Subprogram --
   ---------------------

   procedure Free_Subprogram (S_A : in out Subprogram_Access) is

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
      end Free_From_Cursor;

   begin
      S_A.Lines.Iterate (Free_From_Cursor'Access);
      Free_Analysis (S_A.Analysis_Data);
      Unchecked_Free (S_A.Name);
      Unchecked_Free (S_A);
   end Free_Subprogram;

   ---------------
   -- Free_File --
   ---------------

   procedure Free_File (F_A : in out File_Access) is

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
      end Free_From_Cursor;

   begin
      F_A.Subprograms.Iterate (Free_From_Cursor'Access);
      Free_Analysis (F_A.Analysis_Data);
      Unchecked_Free (F_A.Name);
      Unchecked_Free (F_A);
   end Free_File;

   ------------------
   -- Free_Project --
   ------------------

   procedure Free_Project (P_A : in out Project_Access) is

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
      end Free_From_Cursor;

   begin
      P_A.Files.Iterate (Free_From_Cursor'Access);
      Free_Analysis (P_A.Analysis_Data);
      Project_Maps.Delete (Projects, String (P_A.Name.all));
      Unchecked_Free (P_A.Name);
      Unchecked_Free (P_A);
   end Free_Project;

end Code_Analysis;
