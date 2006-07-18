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

--  <description>
--  This package provides a low level structure design to store code
--  analysis informations such as code coverage (provided by gcov),
--  Metrics, SSAT, GNATmem, (Valgrind).
--  Information are stored in a tree structure with the following
--  levels : Project, File, Subprogram, Line.
--  </description>

with Ada.Containers.Hashed_Maps;            use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Unchecked_Deallocation;

with Integer_Hash;                           use Integer_Hash;

package Code_Analysis is

   -----------------------------
   -- Tree decoration records --
   -----------------------------

   type Coverage is record
      Covered_Child_Count : Integer;
      Total_Child_Count   : Integer;
   end record;
   --  Code Coverage specific information

   type Coverage_Access is access Coverage;

   type Analysis is record
      Coverage_Data : Coverage_Access;
      --  Metrics_Data : Metrics_Record_Access;
      --  SSAT_Data    : SSAT_Record_Access;
   end record;
   --  Record to store the various code analysis information

   type Analysis_Access is access all Analysis;

   ----------------
   -- Tree types --
   ----------------

   type Line is private;
   type Subprogram is private;
   type File is private;
   type Project is private;

   type Line_Access is access all Line;
   type Subprogram_Access is access all Subprogram;
   type File_Access is access all File;
   type Project_Access is access all Project;

   subtype Subprogram_Id is String (1 .. 12); --  8.3 format...

   -------------------
   -- Maps packages --
   -------------------

   package Line_Maps is
     new  Hashed_Maps
       (Key_Type        => Integer,
        Element_Type    => Line_Access,
        Hash            => Mersenne_Prime,
        Equivalent_Keys => "=");

   package Subprogram_Maps is
     new Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Subprogram_Access,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   package File_Maps is
     new Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => File_Access,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   package Project_Maps is
     new Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Project_Access,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --  Instanciation of Indefinite_Hashed_Maps, for every tree levels
   --  Used to stored the children of every nodes

   ------------
   -- Getter --
   ------------

   function Get_Analysis (L : Line_Access) return Analysis_Access;
   function Get_Analysis (S : Subprogram_Access) return Analysis_Access;
   function Get_Analysis (F : File_Access) return Analysis_Access;
   function Get_Analysis (P : Project_Access) return Analysis_Access;

   function Get_Id (L : Line_Access) return Integer;
   function Get_Id (S : Subprogram_Access) return Subprogram_Id;
   function Get_Id (F : File_Access) return Subprogram_Id;
   function Get_Id (P : Project_Access) return Subprogram_Id;
   --  Getter of the identifing field of every tree node types
   --  Could be an Integer for Lines, or
   --  to be fixed

   function Get_Map (S : Subprogram_Access) return Line_Maps.Map;
   function Get_Map (F : File_Access) return Subprogram_Maps.Map;
   function Get_Map (P : Project_Access) return File_Maps.Map;

   function Get_Or_Create_Line
     (S_A : Subprogram_Access;
      L_I : Integer) return Line_Access;

   function Get_Or_Create_Subprogram
     (F_A : File_Access;
      S_I : Subprogram_Id) return Subprogram_Access;

   function Get_Or_Create_File
     (P_A : Project_Access;
      S_I : Subprogram_Id) return File_Access;

   function Get_Or_Create_Project
     (S_I : Subprogram_Id) return Project_Access;
   --  Functions that allow to get an access over an identified tree node
   --  if the node doesn't exists, it is created by this function

   ----------------------
   -- Root of the tree --
   ----------------------

   Projects : Project_Maps.Map;

   -------------
   -- Free-er --
   -------------

   procedure Free_Project (P_A : Project_Access);
   --  Free every children and himself

private

   procedure Free_Line (L_A : Line_Access);
   --  Free every children
   procedure Free_Subprogram (S_A : Subprogram_Access);
   --  Free every children
   procedure Free_File (F_A : File_Access);
   --  Free every children

   procedure Free_Analysis (A : Analysis_Access);
   --  Free an Analysis record, so also a
   --  Coverage record
   --  to be continued

   procedure Unchecked_Free_Coverage is new
     Ada.Unchecked_Deallocation (Coverage, Coverage_Access);

   procedure Unchecked_Free_Analysis is new
     Ada.Unchecked_Deallocation (Analysis, Analysis_Access);

   ----------------
   -- Line level --
   ----------------

   type Line is record
      Number : Integer;
      Analysis_Data : Analysis_Access;
   end record;
   --  A Line is identified in the Lines' maps of every Subprogram record by an
   --  Integer

   procedure Unchecked_Free_Line is new
     Ada.Unchecked_Deallocation (Line, Line_Access);

   ----------------------
   -- Subprogram level --
   ----------------------

   type Subprogram is record
      Name : Subprogram_Id;
      Lines : Line_Maps.Map;

      Analysis_Data : Analysis_Access;
   end record;
   --  A Subprogram is identified in the Subprograms' maps of every File record
   --  by a string's subtype

   procedure Unchecked_Free_Subprogram is new
     Ada.Unchecked_Deallocation (Subprogram, Subprogram_Access);

   ----------------
   -- File level --
   ----------------

   type File is record
      Name        : Subprogram_Id;
      Subprograms : Subprogram_Maps.Map;

      Analysis_Data : Analysis_Access;
   end record;
   --  A File is identified in the Files' maps of every File record
   --  by a string's subtype
   --  Will be replaced by the apropriate field of VFS objects

   procedure Unchecked_Free_File is new
     Ada.Unchecked_Deallocation (File, File_Access);

   -------------------
   -- Project level --
   -------------------

   type Project is record
      Name        : Subprogram_Id;
      Files       : File_Maps.Map;

      Analysis_Data : Analysis_Access;
   end record;
   --  A Project is identified in the Projects' maps of every Project record
   --  by a string's subtype
   --  Will be replaced by the apropriate field of VFS objects

   procedure Unchecked_Free_Project is new
     Ada.Unchecked_Deallocation (Project, Project_Access);

end Code_Analysis;
