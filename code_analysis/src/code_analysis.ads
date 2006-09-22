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
--  This package provides a low level structure designed to store code
--  analysis information such as code coverage (provided by gcov), metrics,
--  stack usage, memory usage, (Valgrind).
--  Information are stored in a tree structure with the following
--  levels: Project, File, Subprogram, Line.
--  </description>

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;                          use GNAT.Strings;

with Projects;                              use Projects;
with Projects_Hash;                         use Projects_Hash;
with VFS;                                   use VFS;
with VFS_Hash;                              use VFS_Hash;

package Code_Analysis is

   -----------------------------
   -- Tree decoration records --
   -----------------------------

   type Coverage is tagged record
      Coverage : Natural := 0;
   end record;
   --  Basic code coverage information
   --  Record the Line's execution counts

   type Node_Coverage is new Coverage with record
      Children : Natural := 0;
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
      --  Future other specific analysis records might be added here, such as
      --  Metrics_Data : Metrics_Record_Access;
      --  SSAT_Data    : SSAT_Record_Access;
   end record;
   --  Store the various code analysis information
   --  ??? In the future, we want a more flexible data structure where each
   --  module can store their data without having visibility on all these
   --  modules in code_analysis.
   --  As this stage, we don't want to elaborate something more complicated.
   --  Furthermore we will need to have visibility on all structures within a
   --  same module when writing advanced tools which will cross information
   --  coming from different code analysis tools.

   ----------------
   -- Tree types --
   ----------------

   type Node;
   type Line;
   type Subprogram;
   type File;
   type Project;

   type Subprogram_Access is access all Subprogram;
   type File_Access       is access all File;
   type Project_Access    is access all Project;

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
       (Key_Type        => Project_Type,
        Element_Type    => Project_Access,
        Hash            => Projects_Hash.Projects_Hash,
        Equivalent_Keys => Projects."=");
   --  Used to stored the Project nodes

   type Code_Analysis_Tree is access all Project_Maps.Map;

   type Node is abstract tagged record
      Analysis_Data : Analysis;
   end record;
   --  Abstract father type of all the following specific node types
   --  Line, Subprogram, File, Project

   type Line is new Node with record
      Number   : Natural;
      Contents : String_Access;
   end record;

   Null_Line : constant Line := (Node with Number => 0, Contents => null);

   type Line_Array is array (Positive range <>) of Line;

   type Line_Array_Access is access Line_Array;

   type Subprogram is new Node with record
      Name      : String_Access;
      Body_Line : Natural := 1;
   end record;
   --  A Subprogram is identified in the Subprograms' maps of every File record
   --  by a string type

   type File is new Node with record
      Name        : VFS.Virtual_File;
      Subprograms : Subprogram_Maps.Map;
      Lines       : Line_Array_Access;
   end record;

   type Project is new Node with record
      Name  : Project_Type;
      Files : File_Maps.Map;
   end record;

   Pix_Col  : constant := 0;
   --  Gtk_Tree_Model column number dedicated to the icons associated with each
   --  node of code_analysis data structure
   Name_Col : constant := 1;
   --  Gtk_Tree_Model column number dedicated to the name of the nodes of
   --  code_analysis structure
   Node_Col : constant := 2;
   --  Gtk_Tree_Model column number dedicated to the nodes of code_analysis
   --  structure
   Cov_Col  : constant := 3;
   --  Gtk_Tree_Model column number dedicated to the coverage information
   --  contained in the node coverage records
   Sort_Col : constant := 4;
   --  Gtk_Tree_Model column number dedicated to some raw coverage information
   --  used to sort rows by not covered lines order

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node : File_Access;
      Sub_Name  : String_Access) return Subprogram_Access;

   function Get_Or_Create
     (Project_Node : Project_Access;
      File_Name    : VFS.Virtual_File) return File_Access;

   function Get_Or_Create
     (Projects     : Code_Analysis_Tree;
      Project_Name : Project_Type) return Project_Access;
   --  allow to get an access pointing on an identified tree node
   --  if the node doesn't exists, it is created

   ------------------------------------
   -- Determinist Ordered Provisions --
   ------------------------------------

   type Subprogram_Array is array (Positive range <>) of Subprogram_Access;
   --  Used to sort Subprogram nodes in a determinist way before to process a
   --  whole hashed map, that can't be iterate in a determinist order

   type File_Array is array (Positive range <>) of File_Access;
   --  Used to sort File nodes in a determinist way before to process a
   --  whole hashed map, that can't be iterate in a determinist order

   type Project_Array is array (Positive range <>) of Project_Access;
   --  Used to sort Project nodes in a determinist way before to process a
   --  whole hashed map, that can't be iterate in a determinist order

   procedure Sort_Subprograms (Nodes : in out Subprogram_Array);
   --  Heap sorts the given Subprogram_Array in alphabetical order based on the
   --  Subprogram.Name

   procedure Sort_Files (Nodes : in out File_Array);
   --  Heap sorts the given File_Array in alphabetical order based on the
   --  VFS.Base_Name (File.Name)

   procedure Sort_Projects (Nodes : in out Project_Array);
   --  Heap sorts the given Project_Array in alphabetical order based on the
   --  String'(Project_Name (Project.Name))

   -------------
   -- Free-er --
   -------------

   procedure Free_Code_Analysis (Projects : Code_Analysis_Tree);
   --  Free a whole code analysis structure

private

   -------------
   -- Free-er --
   -------------

   procedure Free_Line (Line_Node : in out Line'Class);
   --  Free the data associated to a Line

   procedure Free_Subprogram (Sub_Node : in out Subprogram_Access);
   --  Free every children and himself

   procedure Free_File (File_Node : in out File_Access);
   --  Free every children and himself

   procedure Free_Project (Project_Node : in out Project_Access);
   --  Free every children and himself

   procedure Free_Analysis (Analysis_Id : in out Analysis);
   --  Free an Analysis record, so also a
   --  Coverage record if allocated
   --  and futur other specific analysis records should be added here

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, String_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Coverage'Class, Coverage_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Subprogram, Subprogram_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (File, File_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Project, Project_Access);

end Code_Analysis;
