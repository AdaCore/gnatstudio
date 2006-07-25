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

with Ada.Containers.Indefinite_Hashed_Maps;
use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Unchecked_Deallocation;

--  with Integer_Hash;                          use Integer_Hash;

package Code_Analysis is

   -----------------------------
   -- Tree decoration records --
   -----------------------------

   type Coverage is tagged record
      Covered : Natural;
   end record;
   --  Basic code coverage information
   --  Intended for line usage. Will record the number of line execution

   type Node_Coverage is new Coverage with record
      Children : Natural;
   end record;
   --  Node coverage information, including an extra Natural compared to
   --  basic coverage info.
   --  The 1st value will be covered child count, and the added will be the
   --  total child count.

   type Subprogram_Coverage is new Node_Coverage with record
      Called : Natural;
   end record;
   --  Specific subprogram coverage extra info

   type Coverage_Access is access all Coverage'Class;

   type Analysis is record
      Coverage_Data : Coverage_Access;
      --  Metrics_Data : Metrics_Record_Access;
      --  SSAT_Data    : SSAT_Record_Access;
   end record;
   --  Record to store the various code analysis information

   ----------------
   -- Tree types --
   ----------------

   subtype Line_Id is Natural;
   type Subprogram_Id is access all String;
   type File_Id is access all String;
   type Project_Id is access all String;

   type Node;
   type Line;
   type Subprogram;
   type File;
   type Project;

   type Line_Access is access all Line;
   type Subprogram_Access is access all Subprogram;
   type File_Access is access all File;
   type Project_Access is access all Project;

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
   --  Instanciation of Indefinite_Hashed_Maps
   --  Used to stored the children of Project nodes

   type Node is abstract tagged record
      Analysis_Data : Analysis;
   end record;
   --  Abstract father type of all the following specific node types

   type Line is new Node with record
      Number        : Line_Id;
   end record;
   --  A Line is identified in the Lines' maps of every Subprogram record by an
   --  Integer

   type Line_Array is array (Positive range <>) of Line_Access;
   --  Choosen "container" type for Lines

   type Line_Array_Access is access Line_Array;

   type Subprogram is new Node with record
      Name : Subprogram_Id;
   end record;
   --  A Subprogram is identified in the Subprograms' maps of every File record
   --  by a string's subtype

   type File is new Node with record
         Name        : File_Id;
      --      Name : VFS.Virtual_File;
      --  Will be the next modification
      Subprograms : Subprogram_Maps.Map;
      Lines       : Line_Array_Access;
   end record;
   --  A File is identified in the Files' maps of every Project records
   --  by a string's subtype
   --  Will be replaced by the apropriate VFS objects

   type Project is new Node with record
      Name        : Project_Id;
      Files       : File_Maps.Map;
   end record;
   --  A Project is identified in the Projects' maps of every Project record
   --  by a string's subtype
   --  Will be replaced by the apropriate field of VFS objects

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create (F_A : Code_Analysis.File_Access; L_I : Line_Id)
                           return Line_Access;

   function Get_Or_Create (F_A : File_Access; S_I : Subprogram_Id)
                           return Subprogram_Access;

   function Get_Or_Create (P_A : Project_Access; F_I : File_Id)
                           return File_Access;

   function Get_Or_Create (P_I : Project_Id) return Project_Access;
   --  Functions that allow to get an access over an identified tree node
   --  if the node doesn't exists, it is created by this function

   ----------------------
   -- Root of the tree --
   ----------------------

   Projects : Project_Maps.Map;

   -------------
   -- Free-er --
   -------------

   procedure Free_Project (P_A : in out Project_Access);
   --  Free every children and himself

private

   procedure Free_Line (L_A : in out Line_Access);
   --  Free every children
   procedure Free_Subprogram (S_A : in out Subprogram_Access);
   --  Free every children
   procedure Free_File (F_A : in out File_Access);
   --  Free every children

   procedure Free_Analysis (A : in out Analysis);
   --  Free an Analysis record, so also a
   --  Coverage record
   --  to be continued

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, Subprogram_Id);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, File_Id);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, Project_Id);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Coverage'Class, Coverage_Access);

   ----------------
   -- Line level --
   ----------------

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Line, Line_Access);

   ----------------------
   -- Subprogram level --
   ----------------------

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Subprogram, Subprogram_Access);

   ----------------
   -- File level --
   ----------------

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (File, File_Access);

   -------------------
   -- Project level --
   -------------------

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Project, Project_Access);

end Code_Analysis;
