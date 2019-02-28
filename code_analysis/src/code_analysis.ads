------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  <description>
--  This package provides a low level structure designed to store code
--  analysis information such as code coverage (provided by gcov), metrics,
--  stack usage, memory usage, (Valgrind).
--  Information are stored in a tree structure with the following
--  levels: Project, File, Subprogram, Line.
--  </description>

with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;                          use GNAT.Strings;

with GNATCOLL.Projects;                     use GNATCOLL.Projects;
with GNATCOLL.VFS;                          use GNATCOLL.VFS;
with GPS.Kernel;
with GPS.Editors.Line_Information;
with Commands;

package Code_Analysis is

   -----------------------------
   -- Tree decoration records --
   -----------------------------

   type Coverage_Status is
     (Valid,
      Undetermined);

   type File_Coverage_Status is
     (Valid,
      File_Not_Found,
      --  Error status obtained if no Gcov file was found associated to a
      --  source file when trying to load Gcov info.
      File_Out_Of_Date,
      --  Error status obtained when the Gcov file that is attempted to be
      --  parsed is older than the source file associated to.
      File_Empty,
      --  gcov file found but empty.
      File_Corrupted,
      --  The gcov file could not be parsed.
      Undetermined);
      --  The status is undetermined.

   type Coverage is abstract tagged record
      Coverage   : Natural := 0;
   end record;
   --  Basic code coverage information
   --  Record the Line's execution counts and the Subprogram, File and Project
   --  number of not covered lines

   function Is_Valid
     (Self : Coverage) return Boolean is abstract;

   function Is_Exempted (Self : Coverage) return Boolean
   is
     (False);
   --  Should return True if this coverage information is related to exempted
   --  code.
   --
   --  This is used in order to know if the related code should be taken
   --  into account when calculating coverage percentages (e.g: percentage
   --  of covered lines in a subprogram).
   --
   --  Override this subprogram if your coverage analysis tool distinguishes
   --  exempted code (e.g: GNATcov).

   type Line_Coverage is abstract new Coverage with null record;

   function Line_Coverage_Info
     (Coverage : access Line_Coverage;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record is abstract;
   --  Return a String_Access pointing on a message describing the coverage
   --  state of the line from which the Coverage record had been extracted
   --  If Bin_Mode is True, then the returned messages can only be between
   --  (covered | not covered)

   procedure Add_Location_If_Uncovered
     (Coverage    : Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : String_Access;
      Added       : in out Boolean;
      Allow_Auto_Jump_To_First : Boolean) is abstract;
   --  Adds location of the uncovered line to the location window. Set Added to
   --  True if line has been added; otherwise preserve Added value.

   type Node_Coverage is abstract new Coverage with record
      Children : Natural := 0;
   end record;
   --  Extra node coverage information
   --  Children is the Subprogram, File or Project children count

   type File_Coverage is new Node_Coverage with record
      Status : File_Coverage_Status := Undetermined;
   end record;

   overriding function Is_Valid (Self : File_Coverage) return Boolean;

   type Subprogram_Coverage is new Node_Coverage with record
      Called : Natural;
      Status : Coverage_Status := Undetermined;
   end record;
   --  Specific Subprogram extra info
   --  The number of time the subprogram has been called

   overriding function Is_Valid (Self : Subprogram_Coverage) return Boolean;

   type Project_Coverage is new Node_Coverage with record
      Status    : Coverage_Status := Undetermined;
      Have_Runs : Boolean         := False;
      Runs      : Natural;
   end record;
   --  Store project number of call if this info is available
   --  Older gcov than gcov (GCC) 4.1.3 20070620 were fitted with a runs field
   --  in their header, reporting the number of executions of the produced
   --  executable file

   overriding function Is_Valid (Self : Project_Coverage) return Boolean;

   type Coverage_Access is access all Coverage'Class;

   type CodePeer_Data_Root is abstract tagged null record;
   type CodePeer_Data_Access is access all CodePeer_Data_Root'Class;

   procedure Finalize (Self : access CodePeer_Data_Root) is null;

   type Analysis is record
      Coverage_Data  : Coverage_Access;
      --  Future other specific analysis records might be added here, such as
      --  Metrics_Data : Metrics_Record_Access;
      --  SSAT_Data    : SSAT_Record_Access;
      CodePeer_Data : CodePeer_Data_Access;
      --  Additional information for the CodePeer plugin.
   end record;
   --  Store the various code analysis information
   --  ??? In the future, we want a more flexible data structure where each
   --  module can store their data without having visibility on all these
   --  modules in code_analysis.
   --  At this stage, we don't want to elaborate something more complicated.
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

   type Subprogram_Access is access all Subprogram'Class;
   type File_Access       is access all File'Class;
   type Project_Access    is access all Project'Class;
   type Node_Access       is access all Node'Class;

   function Less (V1, V2 : String) return Boolean;
   function Less (V1, V2 : Virtual_File) return Boolean;
   function Less (V1, V2 : Project_Type) return Boolean;
   function Equ  (V1, V2 : Subprogram_Access) return Boolean;
   function Equ  (V1, V2 : File_Access) return Boolean;
   function Equ  (V1, V2 : Project_Access) return Boolean;

   package Subprogram_Maps is
     new Indefinite_Ordered_Maps
       (Key_Type        => String,
        Element_Type    => Subprogram_Access,
        "="             => Equ,
        "<"             => Less);
   --  Used to stored the Subprogram nodes of every Files

   package File_Maps is
     new Ordered_Maps
       (Key_Type        => GNATCOLL.VFS.Virtual_File,
        Element_Type    => File_Access,
        "="             => Equ,
        "<"             => Less);
   --  Used to stored the File nodes of every Projects

   package Project_Maps is
     new Ordered_Maps
       (Key_Type        => Project_Type,
        Element_Type    => Project_Access,
        "="             => Equ,
        "<"             => Less);
   --  Used to stored the Project nodes

   type Code_Analysis_Tree is access all Project_Maps.Map;

   type Node is tagged record
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
      Name   : String_Access;
      Line   : Natural := 1;
      Column : Natural := 1;
      Start  : Natural := 0;
      Stop   : Natural := 0;
   end record;
   --  A Subprogram is identified in the Subprograms' maps of every File record
   --  by a string type
   --  Line is the declaration line within the encapsulating file
   --  Column is the declaration column within the encapsulation file
   --  Start is the starting line of the definition of the subprogram
   --  Stop is the ending line of the definition of the subprogram

   type File is new Node with record
      Name          : GNATCOLL.VFS.Virtual_File;
      Subprograms   : Subprogram_Maps.Map;
      Lines         : Line_Array_Access;
      Line_Commands : Commands.Command_Lists.List;
      --  List used to track the coverage commands in the editor
   end record;

   type Project is new Node with record
      Name  : Project_Type;
      Files : File_Maps.Map;
   end record;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node : File_Access;
      Key       : String) return Subprogram_Access;

   function Get_Or_Create
     (Project_Node : Project_Access;
      File_Name    : GNATCOLL.VFS.Virtual_File) return File_Access;

   function Get_Or_Create
     (Projects     : Code_Analysis_Tree;
      Project_Name : Project_Type) return Project_Access;
   --  allow to get an access pointing on an identified tree node
   --  if the node doesn't exists, it is created

   --------------------------------------
   -- Deterministic Ordered Provisions --
   --------------------------------------

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

   procedure Free_Code_Analysis (Projects : in out Code_Analysis_Tree);
   --  Free a whole code analysis structure

   procedure Clear_Code_Analysis (Projects : Code_Analysis_Tree);
   --  Clear all information in the code analysis structure

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Coverage'Class, Coverage_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (CodePeer_Data_Root'Class, CodePeer_Data_Access);

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

private

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String, String_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Subprogram'Class, Subprogram_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (File'Class, File_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Line_Array, Line_Array_Access);

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Project'Class, Project_Access);

end Code_Analysis;
