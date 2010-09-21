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

--  This package provides a way to analyze & modify the toolchain definition
--  decribed in a GNAT project file.

with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Basic_Types; use Basic_Types;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;

package Toolchains is

   --  ??? Should get rid of all these hard coded values and replace them
   --  by an XML file.
   --  Also, missing some toolchains, e.g. dotnet

   Tool_AAMP                    : aliased constant String := "aamp";
   Tool_E500V2_WRS_VXWORKS      : aliased constant String :=
     "e500v2-wrs-vxworks";
   Tool_E500V2_WRS_VXWORKSMILS  : aliased constant String :=
     "e500v2-wrs-vxworksmils";
   Tool_I586_WRS_VXWORKS        : aliased constant String :=
     "i586-wrs-vxworks";
   Tool_JVM                     : aliased constant String := "jvm";
   Tool_Dotnet                  : aliased constant String := "dotnet";
   Tool_POWERPC_WRS_VXWORKS     : aliased constant String :=
     "powerpc-wrs-vxworks";
   Tool_POWERPC_WRS_VXWORKSAE   : aliased constant String :=
     "powerpc-wrs-vxworksae";
   Tool_POWERPC_WRS_VXWORKSMILS : aliased constant String :=
     "powerpc-wrs-vxworksmils";

   Known_Toolchains : aliased constant String_List :=
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
      new String'(Tool_Dotnet),
      new String'("powerpc-elf-lynxos"),
      new String'("powerpc-elf-pikeos"),
      new String'("powerpc-xcoff-lynxos"));

   function Is_Known_Toolchain_Name (Name : String) return Boolean;
   --  Tell if the name is a known toolchain description

   Toolchain_Exception : exception;

   --------------
   -- Compiler --
   --------------

   type Compiler (<>) is private;
   No_Compiler : constant Compiler;

   function Get_Exe (C : Compiler) return String;
   function Is_Valid (C : Compiler) return Boolean;

   ----------------------
   -- Ada_Library_Info --
   ----------------------

   type Ada_Library_Info (<>) is limited private;
   type Ada_Library_Info_Access is access all Ada_Library_Info;
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

   function Get_Date (This : Ada_Library_Info) return Date_Type;
   --  Return the date of this library information. This is directly deduced
   --  from the version string.

   function Get_Error (This : Ada_Library_Info) return String;
   --  If the library information has not been correctly computed, return
   --  the error message explaining the problem.

   function Get_Install_Path (This : Ada_Library_Info) return Virtual_File;
   --  Return the path where the toolchain associated with this library
   --  information is installed

   function Has_Errors (This : Ada_Library_Info) return Boolean;
   --  Return true if the library information contains errors and is not
   --  correctly loaded, false otherwise.

   procedure Set_Source_Path
     (This : in out Ada_Library_Info; Val : File_Array);
   --  Set the source path associated to this library information

   procedure Set_Objects_Path
     (This : in out Ada_Library_Info; Val : File_Array);
   --  Set the objects path associated to this library information

   procedure Set_Project_Path
     (This : in out Ada_Library_Info; Val : File_Array);
   --  Set the project path associated to this library information

   procedure Set_Version (This : in out Ada_Library_Info; Val : String);
   --  Set the GNAT version associated to this library information

   procedure Set_Error (This : in out Ada_Library_Info; Val : String);
   --  If the library information has not been correctly computed, set
   --  the error message explaining the problem.

   procedure Set_Install_Path
     (This : in out Ada_Library_Info; Val : Virtual_File);
   --  Set the path where the toolchain associated with this library
   --  information is installed

   ---------------
   -- Toolchain --
   ---------------

   type Tools is
     (Unknown,
      GNAT_Driver,
      GNAT_List,
      Debugger,
      CPP_Filt);
   --  This enumeration represents the various tools that can be set in a
   --  toolchain.
   --  ??? Remove Gnatmake and Gnatlist are duplicates of the gnat driver

   type Toolchain is private;
   --  This type represent a toolchain. It can either be generated
   --  automatically and points to tools as known by GPS/GNAT, or setup
   --  through customization, either manually or from the contents of a project
   --  file.

   Null_Toolchain : aliased constant Toolchain;

   procedure Compute_Predefined_Paths (This : Toolchain);
   --  Retreives the predefined path if needed.

   function Get_Command (This : Toolchain; Name : Tools) return String;
   --  Return the command to use in order to call the tool given in parameter.

   procedure Set_Command
     (This       : Toolchain;
      Name       : Tools;
      Value      : String;
      Is_Default : Boolean := False);
   --  Set the command for this tool on this toolchain

   function Is_Valid (This : Toolchain; Name : Tools) return Boolean;
   --  Tell if the tool could be found on the system

   function Is_Default (This : Toolchain; Name : Tools) return Boolean;
   --  Tell if the tool is the default one for the toolchain

   function Is_Default (This : Toolchain; Lang : String) return Boolean;
   --  Tell if the compiler for Lang is the default one for the This toolchain

   procedure Reset_To_Default (This : Toolchain; Name : Tools);
   procedure Reset_To_Default (This : Toolchain; Lang : String);
   --  Reset the specified tool or compiler to its default value

   function Get_Compiler (This : Toolchain; Lang : String) return Compiler;
   --  Return the command to use in order to call the compiler for the language
   --  given in parameter.

   procedure Set_Compiler
     (This    : Toolchain;
      Lang    : String;
      Value   : String;
      Default : Boolean := False);
   --  Set the command for the compiler for Lang on this toolchain

   function Is_Simple_Cross (This : Toolchain) return Boolean;
   --  Return true if the toolchain is a "simple" cross toolchain, that is
   --  to say all the tools are the for prefix-tool, false otherwise.

   function Get_Name (This : Toolchain) return String;
   --  Return the name of this toolchain, as used for the properties deduction

   procedure Set_Name (This : Toolchain; Name : String);
   --  Changes the name of the toolchain given in parameter

   function Get_Label (This : Toolchain) return String;
   --  Return the label of the toolchain, for display purposes. Usually equals
   --  to the name execpt in certain cases (e.g. native)

   function Copy (This : Toolchain) return Toolchain;
   --  Copy all the data for the toolchain given in parameter.

   function Is_Custom (This : Toolchain) return Boolean;
   --  Return true if this toolchain is a custom toolchain, that is to say it's
   --  not one of the common toolchains known by GPS and its properties have
   --  been manually set by the user.

   function Is_Native (This : Toolchain) return Boolean;
   --  Return true if this toolchain is a native toolchain.

   procedure Set_Custom (This : Toolchain; Value : Boolean);
   --  Set wether this toolchain is a custom toolchain

   procedure Set_Native (This : Toolchain; Value : Boolean);
   --  Set wether this toolchain is a native toolchain

   procedure Free (This : in out Toolchain);
   --  Free the memory associated to this toolchain. Removal from the manager
   --  has to be done separately, if needed. Note that on the other end,
   --  removing a toolchain from the manager will free it, so this should
   --  only be used for toolchains outside of the toolchain manager.

   function Get_Library_Information
     (This : Toolchain) return Ada_Library_Info_Access;
   --  Return the library information, as computed by gnatls. The library
   --  information needs to have been computed beforehands.

   procedure Set_Library_Information
     (This : Toolchain;
      Info : Ada_Library_Info_Access);
   --  Modifies the library information stored in this toolchain.

   -----------------------
   -- Toolchain_Manager --
   -----------------------

   type Toolchain_Manager_Record is abstract tagged private;
   type Toolchain_Manager is access all Toolchain_Manager_Record'Class;

   function Execute
     (This       : Toolchain_Manager_Record;
      Command    : String;
      Timeout_MS : Integer) return String
      is abstract;
   --  Executes the command and returns the result. The implementation of this
   --  subprogram typically differs between GNATbench and GPS. If the process
   --  didn't return until timeout miliseconds, then the call has to be
   --  aborted.

   type Language_Id is private;

   Null_Language_Id : constant Language_Id;

   procedure Add_Language
     (Manager : access Toolchain_Manager_Record;
      Lang    : String;
      Project : Project_Type);
   --  Add a new supported language to the toolchain manager
   --  If the language is already present, then nothing happens.
   --  ??? what is this project parameter for?

   function Get_Or_Create_Language
     (Manager : access Toolchain_Manager_Record;
      Lang    : String) return Language_Id;
   --  Return the language id of the given name, create one if it doesn't exist
   --  yet.

   function Create_Empty_Toolchain
     (Manager : access Toolchain_Manager_Record) return Toolchain;
   --  Create an empty toolchain. The result has either to be added to the
   --  manager or manually freed. The result is an empty toolchain that can
   --  be modified, different from Null_Toolchain which can't.

   function Get_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Name    : String) return Toolchain;
   --  Return a toolchain according to its name. If no such toolchain exist,
   --  but the name is the name of a known toolchain, then it will be
   --  automatically created. Otherwise, will return Null_Toolchain.

   function Get_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Project : Project_Type) return Toolchain;
   --  Retreives the toolchain based on the contents of a project. This
   --  toolchain is always stored in the Manager.

   function Get_Toolchain
     (Manager    : access Toolchain_Manager_Record;
      Project    : Project_Type;
      Languages : GNAT.Strings.String_List) return Toolchain;
   --  Retreives the toolchain based on the contents of a project and a list of
   --  languages. This toolchain is always stored in the Manager.

   procedure Add_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc      : Toolchain);
   --  Add a toolchain in the toolchain manager - raise an exception if the
   --  toolchain already exsits.

   procedure Remove_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc_Name : String);
   --  Remove an existing toolchain in the toolchain manager - raise an
   --  exception if the toolchain is not found.

   type Toolchain_Array is array (Integer range <>) of aliased Toolchain;

   function Get_Toolchains
     (Manager : access Toolchain_Manager_Record) return Toolchain_Array;
   --  Return the toolchains contained in this manager.

   procedure Scan_Toolchains (Manager : access Toolchain_Manager_Record);
   procedure Scan_Toolchains
     (Manager : access Toolchain_Manager_Record;
      Progress : access procedure
        (Name    : String;
         Current : Integer;
         Total   : Integer));
   --  Scans the toolchains installed on the system using gprbuild, and run
   --  each gnat list on the whole list (scanned toolchains + already loaded
   --  toolchains

   function Get_Native_Toolchain
     (Manager : access Toolchain_Manager_Record) return Toolchain;
   --  Returns the native toolchain associated to this manager - tries to
   --  create one if none has already been created

   procedure Compute_If_Needed
     (Manager : access Toolchain_Manager_Record;
      This    : in out Ada_Library_Info);
   --  Computes this library info using gnatls if needed.

   -------------------------------
   -- Toolchain_Change_Listener --
   -------------------------------

   type Toolchain_Change_Listener_Record is abstract tagged null record;
   type Toolchain_Change_Listener is access all
     Toolchain_Change_Listener_Record'Class;

   procedure Toolchain_Changed
     (This    : Toolchain_Change_Listener_Record;
      Manager : Toolchain_Manager) is abstract;
   --  Reacts to changes made in the manager.

   procedure Add_Listener
     (Manager  : access Toolchain_Manager_Record;
      Listener : Toolchain_Change_Listener);
   --  Adds a listener to the toolchain change event - does nothing if the
   --  listener is already registered.

   procedure Remove_Listener
     (Manager  : access Toolchain_Manager_Record;
      Listener : Toolchain_Change_Listener);
   --  Removes the listener from the toolchain change event - does nothing if
   --  the listener doesn't exist.

   function Get_Or_Create_Library_Information
     (Manager        : access Toolchain_Manager_Record;
      GNATls_Command : String) return Ada_Library_Info_Access;
   --  Return the library info for this gnatls command. The resulting object
   --  is not computed through gnatls, and the information may be inaccurate.
   --  This is flagged in the internal state of the object, which will do
   --  a gnatls query the first time up to date data is needed. If there's
   --  already a library for this gnatls command, it will get returned.

private

   type Ada_Library_Info is limited record
      GNATls_Command : String_Access;

      Source_Path  : File_Array_Access;
      Objects_Path : File_Array_Access;
      Project_Path : File_Array_Access;
      Version      : String_Access;
      Error        : String_Access;
      Install_Path : Virtual_File := No_File;

      Is_Computed : Boolean := False;
      --  Have the toolchain properties already been computed from gnatls? If
      --  not, the values stored in this object may be just buffered values
      --  that will need to be updated as soon as this is actually used.
   end record;

   type Tool_Name_Array is array (Tools) of String_Access;
   type Boolean_Tool_Array is array (Tools) of Boolean;

   type Compiler (Exe_Length : Natural) is
      record
         Exe        : String (1 .. Exe_Length);
         Is_Valid   : Boolean;
      end record;
   No_Compiler : constant Compiler :=
                   (Exe_Length => 0,
                    Exe        => "",
                    Is_Valid   => False);

   package Compiler_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Compiler,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Toolchain_Record is record
      Name : String_Access;
      --  The triplet used as the name of the toolchain, or "native" if it's
      --  a native toolchain

      Label : String_Access;
      --  The name of the toolchain as displayed by the user

      Is_Native : Boolean := False;
      --  Is this a native toolchain?

      Is_Custom : Boolean := False;
      --  Are the contents of this toolchain coming from standard description,
      --  or are fields been manually set by the user?

      Tool_Commands : Tool_Name_Array;
      --  The name of the tools for this toolchain.

      Default_Tools : Tool_Name_Array;
      --  List of default tools for the target

      Is_Valid_Tool : Boolean_Tool_Array := (others => False);
      --  Wether those tools could be found.

      Compiler_Commands : Compiler_Maps.Map;
      --  The compiler commands indexed by language

      Default_Compilers : Compiler_Maps.Map;
      --  The default compiler commands indexed by language

      Compilers_Scanned : Boolean := False;

      Is_Valid : Boolean;
      --  Is this toolchain accessible from the environment ?

      Library : Ada_Library_Info_Access;

      Manager : Toolchain_Manager;
      --  The manager this toolchain is attached to
   end record;

   type Toolchain is access all Toolchain_Record;

   Null_Toolchain : aliased constant Toolchain := null;

   package Toolchain_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Toolchain);

   package Library_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Ada_Library_Info_Access);

   package Listener_List is new Ada.Containers.Doubly_Linked_Lists
     (Toolchain_Change_Listener);

   package Language_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String,
      Hash                => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Elements => Ada.Strings.Equal_Case_Insensitive);

   type Language_Id is new Language_Sets.Cursor;

   Null_Language_Id : constant Language_Id :=
     Language_Id (Language_Sets.No_Element);

   type Toolchain_Manager_Record is abstract tagged record
      Toolchains          : Toolchain_Maps.Map;
      No_Native_Toolchain : Boolean := False;
      Computed_Libraries  : Library_Maps.Map;
      Listeners           : Listener_List.List;
      Languages           : Language_Sets.Set;
   end record;

   function Create_Known_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Name    : String) return Toolchain;
   --  Create a toolchain from a known description

   function Create_Anonymous_Name
     (Manager : access Toolchain_Manager_Record;
      Prefix  : String) return String;
   --  Return a unique anonymous name from a prefix, that's not already
   --  registered in the manager

   procedure Fire_Change_Event (This : access Toolchain_Manager_Record);
   --  Calls the Toolchain_Changed event on all listeners.

end Toolchains;
