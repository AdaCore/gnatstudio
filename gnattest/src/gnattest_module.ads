------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

--  This package defines the module for GNATTest integration.

with Basic_Types;
with GNATCOLL.Projects;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GPS.Kernel;

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

package GNATTest_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Find_Tested
     (File_Name       : Virtual_File;
      File            : out Virtual_File;
      Subprogram_Name : out Ada.Strings.Unbounded.Unbounded_String;
      Line            : out Natural;
      Column          : out Basic_Types.Visible_Column_Type);
   --  Find tested subprogram for given test unit

   procedure Open_File
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : GNATCOLL.Projects.Project_Type;
      File            : Virtual_File;
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Subprogram_Name : String := "");
   --  Open unit in editor and place cursor to given Line and Column.
   --  Project is recommended for cross-references in the case of aggregate
   --  projects.

   type Source_Entity is record
      Source_File      : Virtual_File;
      Subprogram_Name  : Unbounded_String;
      Line             : Natural := 0;
      Column           : Natural := 0;
      Test_Case_Name   : Unbounded_String;
   end record;

   function "<" (Left, Right : Source_Entity) return Boolean;

   type Row_Index is array (1 .. 2) of Natural;
   --  This type used in Tree_Model only

   type Test_Entity is record
      File_Name        : Virtual_File;
      Subprogram_Name  : Unbounded_String;
      Line             : Natural;
      Column           : Natural;
      Stamp            : Ada.Calendar.Time;
      Row              : Row_Index;
   end record;

   package Source_Entity_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_Entity,
      Element_Type => Test_Entity);

   Test_Setup : constant Unbounded_String :=
     To_Unbounded_String ("test setup");

   Test_Teardown : constant Unbounded_String :=
     To_Unbounded_String ("test teardown");

end GNATTest_Module;
