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
--  levels: Project, File, Subprogram, Line.
--  </description>

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Unchecked_Deallocation;

with VFS;      use VFS;
with VFS_Hash; use VFS_Hash;

package Code_Analysis is

   -----------------------------
   -- Tree decoration records --
   -----------------------------

   type Coverage is tagged record
      Covered : Natural;
   end record;
   --  Basic code coverage information
   --  Record the Line's execution counts

   type Node_Coverage is new Coverage with record
      Children : Natural;
   end record;
   --  Extra node coverage information
   --  The Covered value will be covered child count, and the Children value
   --  will be the total children count.

   type Subprogram_Coverage is new Node_Coverage with record
      Called : Natural;
   end record;
   --  Specific Subprogram extra info

   type Coverage_Access is access all Coverage'Class;

   type Analysis is record
      Coverage_Data : Coverage_Access;
      --  futur other specific analysis records should be added here, such as
      --  Metrics_Data : Metrics_Record_Access;
      --  SSAT_Data    : SSAT_Record_Access;
   end record;
   --  Store the various code analysis information
   --  As this stage, we don't want to elaborate something more complicated
   --  We need to have visibility on all structures within a same module.

   ----------------
   -- Tree types --
   ----------------

   subtype Line_Id is Natural;
   type Subprogram_Id is access all String;

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
   --  Used to stored the Subprogram nodes of every Files

   package File_Maps is
     new Indefinite_Hashed_Maps
       (Key_Type        => VFS.Virtual_File,
        Element_Type    => File_Access,
        Hash            => VFS_Hash.VFS_Hash,
        Equivalent_Keys => VFS."=");
   --  Used to stored the File nodes of every Projects

   package Project_Maps is
     new Indefinite_Hashed_Maps
       (Key_Type        => VFS.Virtual_File,
        Element_Type    => Project_Access,
        Hash            => VFS_Hash.VFS_Hash,
        Equivalent_Keys => VFS."=");
   --  Used to stored the Project nodes

   type Code_Analysis_Tree is access all Project_Maps.Map;

   type Node is abstract tagged record
      Analysis_Data : Analysis;
   end record;
   --  Abstract father type of all the following specific node types
   --  Line, Subprogram, File, Project

   type Line is new Node with record
      Number : Line_Id;
   end record;

   type Line_Array is array (Positive range <>) of Line_Access;

   type Line_Array_Access is access Line_Array;

   type Subprogram is new Node with record
      Name : Subprogram_Id;
   end record;
   --  A Subprogram is identified in the Subprograms' maps of every File record
   --  by a string type

   type File is new Node with record
      Name        : VFS.Virtual_File;
      Subprograms : Subprogram_Maps.Map;
      Lines       : Line_Array_Access;
   end record;

   type Project is new Node with record
      Name  : VFS.Virtual_File;
      Files : File_Maps.Map;
   end record;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node : Code_Analysis.File_Access;
      Line_Num  : Line_Id) return Line_Access;
   function Get_Or_Create
     (File_Node : File_Access;
      Sub_Id    : Subprogram_Id) return Subprogram_Access;
   function Get_Or_Create
     (Project_Node : Project_Access;
      File_Id      : VFS.Virtual_File) return File_Access;
   function Get_Or_Create
     (Project_Id : VFS.Virtual_File) return Project_Access;
   --  allow to get an access pointing on an identified tree node
   --  if the node doesn't exists, it is created

   function Get_Tree return Code_Analysis_Tree;
   --  Getter of the root tree map defined private bellow

   -------------
   -- Free-er --
   -------------

   procedure Free_Project (Project_Node : in out Project_Access);
   --  Free every children and himself

private

   procedure Free_Line (Line_Node : in out Line_Access);
   --  Free every children
   procedure Free_Subprogram (Sub_Node : in out Subprogram_Access);
   --  Free every children
   procedure Free_File (File_Node : in out File_Access);
   --  Free every children

   procedure Free_Analysis (Analysis_Id : in out Analysis);
   --  Free an Analysis record, so also a
   --  Coverage record if allocated
   --  and futur other specific analysis records should be added here

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, Subprogram_Id);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Coverage'Class, Coverage_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Line, Line_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Subprogram, Subprogram_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (File, File_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Project, Project_Access);

   ------------------------------------
   -- Root of the code analysis tree --
   ------------------------------------

   Projects : aliased Project_Maps.Map;
   --  ??? this is still a global variable that we want to get rid of

end Code_Analysis;
