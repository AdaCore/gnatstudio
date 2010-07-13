-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package Toolchains is

   Tool_AAMP                    : aliased constant String := "aamp";
   Tool_E500V2_WRS_VXWORKS      : aliased constant String :=
     "e500v2-wrs-vxworks";
   Tool_E500V2_WRS_VXWORKSMILS  : aliased constant String :=
     "e500v2-wrs-vxworksmils";
   Tool_I586_WRS_VXWORKS        : aliased constant String :=
     "i586-wrs-vxworks";
   Tool_JVM                     : aliased constant String := "jvm";
   Tool_POWERPC_WRS_VXWORKS     : aliased constant String :=
     "powerpc-wrs-vxworks";
   Tool_POWERPC_WRS_VXWORKSAE   : aliased constant String :=
     "powerpc-wrs-vxworksae";
   Tool_POWERPC_WRS_VXWORKSMILS : aliased constant String :=
     "powerpc-wrs-vxworksmils";

   Known_Toolchains : constant String_List :=
     ( --  Bareboards
      new String'("erc32-elf"),
      new String'("leon-elf"),
      new String'("powerpc-eabispe"),
      new String'("powerpc-elf"),

      --  VxWorks platforms
      new String'(Tool_E500V2_WRS_VXWORKS),
      new String'(Tool_E500V2_WRS_VXWORKSMILS),
      new String'(Tool_I586_WRS_VXWORKS),
      new String'(Tool_POWERPC_WRS_VXWORKS),
      new String'(Tool_POWERPC_WRS_VXWORKSAE),
      new String'(Tool_POWERPC_WRS_VXWORKSMILS),

      --  Other cross
      new String'(Tool_AAMP),
      new String'(Tool_JVM),
      new String'("powerpc-elf-lynxos"),
      new String'("powerpc-elf-pikeos"),
      new String'("powerpc-xcoff-lynxos"));

   Target_Exception : exception;

   type Toolchain_Manager_Record is abstract tagged private;
   type Toolchain_Manager is access all Toolchain_Manager_Record'Class;

   function Execute
     (This : Toolchain_Manager_Record; Command : String) return String
      is abstract;
   --  Executes the command and returns the result. The implementation of this
   --  subprogram typically differs between GNATbench and GPS.

   ----------------------
   -- Ada_Library_Info --
   ----------------------

   type Ada_Library_Info is private;
   --  This type stores the information as returned by GNATLS, e.g. various
   --  paths, gnat version...
   --  ??? how do we free instances of this object?

   function Get_Source_Path (This : Ada_Library_Info) return File_Array;
   --  Return the source path associated to this library information

   function Get_Objects_Path (This : Ada_Library_Info) return File_Array;
   --  Return the objects path associated to this library information

   function Get_Project_Path (This : Ada_Library_Info) return File_Array;
   --  Return the project path associated to this library information

   function Get_Version (This : Ada_Library_Info) return String;
   --  Return the GNAT version associated to this library information

   function Get_Error (This : Ada_Library_Info) return String;
   --  If the library information has not been correctly computed, return
   --  the error message explaining the problem.

   function Get_Install_Path (This : Ada_Library_Info) return Virtual_File;
   --  Return the path where the toolchain associated with this library
   --  information is installed

   function Has_Errors (This : Ada_Library_Info) return Boolean;
   --  Return true if the library information contains errors and is not
   --  correctly loaded, false otherwise.

   ---------------
   -- Toolchain --
   ---------------

   type Tool_Names is
     (Unknown,
      C_Compiler,
      Ada_Compiler,
      GNAT_Driver,
      GNAT_List,
      Debugger,
      CPP_Filt);
   --  This enumeration represents the various tools that can be set in a
   --  toolchain.

   type Toolchain is private;
   --  This type represent a toolchain. It can either be generated
   --  automatically and points to tools as known by GPS/GNAT, or setup
   --  through customization, either manually or from the contents of a project
   --  file.

   Null_Toolchain : aliased constant Toolchain;

   procedure Compute_Predefined_Paths
     (This : Toolchain; Manager : Toolchain_Manager);
   --  Retreives the predefined path if needed.

   function Get_Command (This : Toolchain; Name : Tool_Names) return String;
   --  Return the command to use in order to call the tool given in parameter.

   function Is_Simple_Cross (This : Toolchain) return Boolean;
   --  Return true if the toolchain is a "simple" cross toolchain, that is
   --  to say all the tools are the for prefix-tool, false otherwise.

   function Get_Name (This : Toolchain) return String;
   --  Return the name of this toolchain, as used for the properties deduction

   function Copy (This : Toolchain) return Toolchain;
   --  Copy all the data for the toolchain given in parameter.

   -----------------------
   -- Toolchain_Manager --
   -----------------------

   function Create_Known_Toolchain (Name : String) return Toolchain;
   --  Create a new toolchain based on a known toolchain description. Will
   --  return Null_Toolchain if no such name is known. Note that the returned
   --  object needs to be added to the toolchain manager manually if it has to
   --  be stored.

   function Is_Known_Toolchain_Name (Name : String) return Boolean;
   --  Return true if this is the name of a known toolchain.

   function Get_Native_Toolchain (This : Toolchain_Manager) return Toolchain;
   --  Returns the native toolchain associated to this manager - tries to
   --  create one if none has already been created

   function Get_Toolchain
     (This : Toolchain_Manager; Name : String) return Toolchain;
   --  Return a toolchain according to its name. If no such toolchain exist,
   --  but the name is the name of a known toolchain, then it will be
   --  automatically created. Otherwise, will return Null_Toolchain.

   function Compute_Toolchain
     (This : Toolchain_Manager; Project : Project_Type) return Toolchain;
   --  Retreives the toolchain based on the contents of a project. This
   --  toolchain may not be stored in the manager if the project specifies
   --  dedicated tool commands. The caller is always responsible for freeing
   --  the returned value.

   function Compute_Toolchain_From_Tool
     (This : Toolchain_Manager;
      Name : String;
      Tool : Tool_Names) return Toolchain;
   --  Retreives the toolchain of the given project based (in order) on the
   --  gnatlist and compiler_command attributes of the ide package. Create
   --  one if needed. Return null if the heuristics can't determine any
   --  reasonable toolchain.

   procedure Add_Toolchain
     (This          : Toolchain_Manager;
      Ada_Toolchain : Toolchain);
   --  Add a toolchain in the toolchain manager - raise an exception if the
   --  toolchain already exsits.

   function Create_Anonymous_Name (This : Toolchain_Manager) return String;
   --  Return a unique anonymous name that's not already registered in the
   --  manager

   function Get_Library_Information
     (This           : Toolchain_Manager;
      GNATls_Command : String) return Ada_Library_Info;
   --  Returns the library information corresponding to the gnatls executable
   --  given in parameter - caches the result so that no extra computation has
   --  to be done the second time the same information is requested.

   type Toolchain_Array is array (Integer range <>) of Toolchain;

   function Get_Toolchains (This : Toolchain_Manager) return Toolchain_Array;
   --  Return the toolchains contained in this manager.

   procedure Scan_Toolchains
     (This     : Toolchain_Manager;
      Progress : access procedure
        (Name    : String;
         Current : Integer;
         Total   : Integer));
   --  Scans the toolchains installed on the system using gprbuild, and run
   --  each gnat list on the whole list (scanned toolchains + already loaded
   --  toolchains

private

   type Ada_Library_Info_Record is record
      Source_Path  : File_Array_Access;
      Objects_Path : File_Array_Access;
      Project_Path : File_Array_Access;
      Version      : String_Access;
      Error        : String_Access;
      Install_Path : Virtual_File;
   end record;

   type Ada_Library_Info is access all Ada_Library_Info_Record;

   type Tool_Name_Array is array (Tool_Names) of String_Access;

   type Toolchain_Record is record
      Name : String_Access;
      --  The triplet used as the name of the toolchain, or "native" if it's
      --  a native toolchain

      Label : String_Access;
      --  The name of the toolchain as displayed by the user

      Is_Native : Boolean;
      --  Is this a native toolchain?

      Is_Custom : Boolean;
      --  Are the contents of this toolchain coming from standard description,
      --  or are fields been manually set by the user?

      Tool_Commands : Tool_Name_Array;
      --  The name of the tools for this toolchain.

      Is_Computed : Boolean;
      --  Have the toolchain properties already been computed from gnatls?

      Is_Valid : Boolean;
      --  Is this toolchain accessible from the environment ?

      Library : Ada_Library_Info;
   end record;

   type Toolchain is access all Toolchain_Record;

   Null_Toolchain : aliased constant Toolchain := null;

   package Toolchain_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Toolchain);

   package Library_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Ada_Library_Info);

   type Toolchain_Manager_Record is abstract tagged record
      Toolchains          : Toolchain_Maps.Map;
      No_Native_Toolchain : Boolean := False;
      Computed_Libraries  : Library_Maps.Map;
   end record;

end Toolchains;
