------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

--  This package handles creation of project from templates.
--  NOTE: this should remain independent from the GPS Kernel, so that
--  it can be reused in GNATbench or in a stand-alone executable.

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with Ada.Strings.Unbounded.Hash;

with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;

package Project_Templates is

   Template_File_Extension : constant Filesystem_String := ".gpt";
   --  Extension for recognized template files
   --  (".gpt" stands for GNAT Project Template).

   type Variable (Nb_Choices : Integer) is record
      Label         : Unbounded_String;
      --  The label of the variable

      Default_Value : Unbounded_String;
      --  The default value

      Description   : Unbounded_String;
      --  A one-line description

      Choices       : Unbounded_String_Array (1 .. Nb_Choices);
      --  An optional choices' list
   end record;

   package Variables_List is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Variable);

   type Project_Template is record
      Label    : Unbounded_String;
      --  The label of the template

      Category : Unbounded_String;
      --  The category of the template, for sorting/displaying purposes.
      --  This can be a number of fields separated by '/'.

      Description : Unbounded_String;
      --  A short (one or two sentences) description for the template

      Source_Dir : Virtual_File;
      --  The root directory that contains the template

      Variables : Variables_List.List;
      --  A list of variables which need to be defined for this project

      Project : Unbounded_String;
      --  The original name of the project file to load after installing
      --  the template.

      Python_Script : Virtual_File;
      --  The source file containing the hooks
      --  to run during and after deployment.

   end record;

   Null_Project_Template : constant Project_Template :=
     (Null_Unbounded_String,
      Null_Unbounded_String,
      Null_Unbounded_String,
      No_File,
      Variables_List.Empty_List,
      Null_Unbounded_String,
      No_File);

   package Project_Templates_List is new Ada.Containers.Doubly_Linked_Lists
     (Project_Template);

   procedure Read_Templates_File
     (File      : Virtual_File;
      Errors    : out Unbounded_String;
      Templates : in out Project_Templates_List.List);
   --  Return a list of project templates read from description file File.
   --  The format of File is as follows:
   --
   --  name: <short name of the template>
   --  category: <category for organizing the template>
   --  project: <name of the project file>
   --  post_hook: <name of the python file>  (optional)
   --
   --  <variable_1_label>: <default_value_1> : <description_1>
   --  <variable_2_label>: <default_value_2> : <description_2>
   --
   --  [description]
   --  <the multi-line description of the project template>
   --
   --  and so forth.
   --  Errors contains error messages in case of errors.

   procedure Read_Templates_Dir
     (Dir       : Virtual_File;
      Errors    : out Unbounded_String;
      Templates : in out Project_Templates_List.List);
   --  Look in all subdirectories of Dir and attempt to find all template files
   --  in those directories, and reads the results in Templates.

   package Variable_Assignments is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Unbounded_String,
      Ada.Strings.Unbounded.Hash,
      Ada.Strings.Unbounded."=");

   procedure Instantiate_Template
     (Template    : Project_Template;
      Target_Dir  : Virtual_File;
      Assignments : Variable_Assignments.Map;
      Project     : out Virtual_File;
      Errors      : out Unbounded_String);
   --  Create Template in target directory Target_Dir, with given variable
   --  assignments.
   --  Target_Dir is created if necessary.
   --  Project is set to the project file defined in the template.
   --  In case of errors, they are listed in Errors.

   function Default_Assignments
     (Variables : Variables_List.List) return Variable_Assignments.Map;
   --  Convenience function, return the default assignment map for the given
   --  variables list
end Project_Templates;
