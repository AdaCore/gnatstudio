------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

--  This package provides a way to analyze & modify the toolchain definition
--  decribed in a GNAT project file.

with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Basic_Types; use Basic_Types;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;

package Toolchains is

   Toolchain_Exception : exception;

   --------------
   -- Compiler --
   --------------

   type Compiler_Origin is
     (From_Default,
      From_Gprconfig,
      From_Project,
      From_Project_Driver,
      From_User);

   subtype Default_Compiler_Origin is Compiler_Origin
     range From_Default .. From_Gprconfig;

   type Compiler is private;
   type Compiler_Array is array (Natural range <>) of Compiler;
   No_Compiler : constant Compiler;

   function Get_Exe (C : Compiler) return String;
   function Is_Valid (C : Compiler) return Boolean;
   function Get_Origin (C : Compiler) return Compiler_Origin;

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

   subtype Valid_Tools is Tools range GNAT_Driver .. CPP_Filt;

   type Toolchain is private;
   --  This type represent a toolchain. It can either be generated
   --  automatically and points to tools as known by GPS/GNAT, or setup
   --  through customization, either manually or from the contents of a project
   --  file.

   Null_Toolchain : aliased constant Toolchain;

   procedure Ref (This : Toolchain);
   procedure Unref (This : in out Toolchain);
   --  Manage number of references made to a toolchain - usefull when
   --  toolchains are stored for a long time in order to protect their data
   --  against calls to Remove or Clear.

   procedure Compute_Predefined_Paths (This : Toolchain);
   --  Retreives the predefined path if needed.

   function Get_Command (This : Toolchain; Name : Valid_Tools) return String;
   --  Return the command to use in order to call the tool given in parameter.

   procedure Set_Command
     (This         : Toolchain;
      Name         : Valid_Tools;
      Value        : String;
      Origin       : Compiler_Origin;
      Is_Base_Name : Boolean);
   --  Set the command for this tool on this toolchain
   --  Origin is the origin of the command that we are setting,
   --  Is_Base_Name tells wether the command is a base name or a full path

   function Is_Valid (This : Toolchain; Name : Tools) return Boolean;
   --  Tell if the tool could be found on the system

   function Is_Default (This : Toolchain; Name : Tools) return Boolean;
   --  Tell if the tool is the default one for the toolchain

   function Is_Defined (This : Toolchain; Lang : String) return Boolean;
   --  Tell if a compiler is defined for this toolchain

   function Is_Default (This : Toolchain; Lang : String) return Boolean;
   --  Tell if the compiler for Lang is the default one for the This toolchain

   function Is_Base_Name (This : Toolchain; Name : Tools) return Boolean;
   --  Tell if the tool is the system default (e.g. is the one discovered in
   --  the path when typing its base name from a console)

   function Is_Base_Name (This : Toolchain; Lang : String) return Boolean;
   --  Same as above for a compiler

   procedure Reset_Tool_To_Default (This : Toolchain; Name : Tools);
   procedure Reset_Compiler_To_Default (This : Toolchain; Lang : String);
   procedure Reset_Runtime_To_Default (This : Toolchain; Lang : String);
   --  Reset the specified tool, compiler or runtime to its default value

   procedure Add_Compiler
     (This   : Toolchain;
      Lang   : String;
      Value  : String;
      Origin : Compiler_Origin);
   --  Add command for the compiler for Lang on this toolchain

   procedure Set_Compiler
     (This  : Toolchain;
      Lang  : String;
      Value : String);
   --  Sets the compiler command to use for this toolchain. If such a compiler
   --  has already been defined using Add_Compiler above, then this compiler
   --  is selected as the compiler to use. Else, a compiler is created using
   --  'From_User' as Origin. Only one compiler 'From_User' can exist for a
   --  given tuple Toolchain/Lang, so it will be overwritten if one is already
   --  present.

   function Get_Compiler (This : Toolchain; Lang : String) return Compiler;
   --  Return the command to use in order to call the compiler for the language
   --  given in parameter.

   function Get_Compilers
     (This : Toolchain; Lang : String) return Compiler_Array;
   --  Get all compilers defined in this toolchain for Lang.

   function Get_Compiler_Is_Used
     (This : Toolchain; Lang : String) return Boolean;
   --  Tell wether Lang requires a compiler

   procedure Set_Compiler_Is_Used
     (This : Toolchain; Lang : String; Value : Boolean);
   --  Set whether Lang needs to be compiled

   function Get_Name (This : Toolchain) return String;
   --  Return the name of this toolchain, as used for the properties
   --  deduction.

   function Get_Target_Name (This : Toolchain) return String;
   --  Return the target as known by gprbuild of this toolchain.

   function Native_Target_Name return String;
   --  Return the target as known by gprbuild for the native toolchain.

   procedure Set_Name (This : Toolchain; Name : String);
   --  Changes the name of the toolchain given in parameter

   function Get_Label (This : Toolchain) return String;
   --  Return the label of the toolchain, for display purposes. Usually equals
   --  to the name execpt in certain cases (e.g. native). Each label is unique
   --  in a toolchain manager.
   --  ??? Label should probably be renamed Id

   procedure Set_Label (This : Toolchain; Label : String);
   --  Modifies the label of the toolchain given in parameter.

   function Copy (This : Toolchain) return Toolchain;
   --  Copy all the data for the toolchain given in parameter.

   function Is_Custom (This : Toolchain) return Boolean;
   --  Return true if this toolchain is a custom toolchain, that is to say it's
   --  not one of the common toolchains known by GPS and its properties have
   --  been manually set by the user.

   function Is_Native (This : Toolchain) return Boolean;
   --  Return true if this toolchain is a native toolchain.

   function Is_Valid (This : Toolchain) return Boolean;
   --  Tell if the toolchain is valid (is found in the PATH)

   procedure Set_Custom (This : Toolchain; Value : Boolean);
   --  Set whether this toolchain is a custom toolchain

   procedure Set_Native (This : Toolchain; Value : Boolean);
   --  Set whether this toolchain is a native toolchain

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

   procedure Initialize_Known_Toolchain (This : Toolchain; Name : String);
   --  Sets the toolchain values according to the known toolchain name
   --  given in parameter. This doesn't add the toolchain in the manager.

   function Get_Defined_Runtimes
     (Tc   : Toolchain;
      Lang : String) return GNAT.Strings.String_List;
   --  Return the list of all the runtimes defined for this toolchain and the
   --  given language.

   function Get_Used_Runtime
     (Tc   : Toolchain;
      Lang : String) return String;
   --  Return the runtime currently used by this toolchain for the given
   --  language.

   procedure Set_Used_Runtime
     (Tc      : Toolchain;
      Lang    : String;
      Runtime : String);
   --  Set the runtime used by this toolchain for the given language.

   function Is_Runtime_Defined
     (Tc      : Toolchain;
      Lang    : String;
      Runtime : String) return Boolean;
   --  Return True if the given runtime is valid for this toolchain and this
   --  language, False otherwise.

   function Is_Default_Runtime_Used
     (Tc   : Toolchain;
      Lang : String) return Boolean;
   --  Return True if the runtime currently used by this toolchain and this
   --  language is the default one, False otherwise.

   -----------------------
   -- Toolchain_Manager --
   -----------------------

   type Toolchain_Manager_Record is abstract tagged private;
   type Toolchain_Manager is access all Toolchain_Manager_Record'Class;

   function Execute
     (This              : Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String
      is abstract;
   --  Executes the command and returns the result. The implementation of this
   --  subprogram typically differs between GNATbench and GPS. If the process
   --  didn't return until timeout miliseconds, then the call has to be
   --  aborted.
   --  Handle_GUI_Events is used to manually handle GUI events if needed (
   --  so that GUI is responsive while Execute is called)

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
      Label   : String) return Toolchain;
   --  Return a toolchain according to its name. If no such toolchain exist,
   --  but the name is the name of a known toolchain, then it will be
   --  automatically created. Otherwise, will return Null_Toolchain.

   function Get_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Project : Project_Type) return Toolchain;
   --  Retreives the toolchain based on the contents of a project. This
   --  toolchain is always stored in the Manager.

   procedure Add_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc      : Toolchain);
   --  Add a toolchain in the toolchain manager - raise an exception if the
   --  toolchain already exsits. If the toolchain added has no ada library
   --  information available, it will be automatically computed.

   procedure Remove_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc_Name : String);
   --  Remove an existing toolchain in the toolchain manager - raise an
   --  exception if the toolchain is not found.

   type Toolchain_Array is array (Integer range <>) of aliased Toolchain;

   function Get_Toolchains
     (Manager : access Toolchain_Manager_Record) return Toolchain_Array;
   --  Return the toolchains contained in this manager.

   procedure Compute_Gprconfig_Compilers
     (Mgr     : access Toolchain_Manager_Record;
      Success : out Boolean);
   --  Retrieve all compilers found on the host.
   --  Status is set to false if gprconfig could not be found or is too olds

   procedure Compute_Gprconfig_Compilers
     (Mgr      : access Toolchain_Manager_Record;
      Callback : access procedure
        (Toolchain : String; Num, Total : Natural);
      Success  : out Boolean);
   --  Same as above, with callback called when analyzing toolchains

   function Get_Native_Toolchain
     (Manager : access Toolchain_Manager_Record) return Toolchain;
   --  Returns the native toolchain associated to this manager - tries to
   --  create one if none has already been created

   procedure Compute_If_Needed
     (Manager : access Toolchain_Manager_Record;
      This    : in out Ada_Library_Info);
   --  Computes this library info using gnatls if needed.

   procedure Compute_Always
     (Manager : access Toolchain_Manager_Record;
      This    : in out Ada_Library_Info);
   --  Computes this library info using gnatls in any case.

   procedure Clear_Toolchains (Manager : in out Toolchain_Manager_Record);
   --  Remove and free all the toolchains contained in this manager

   procedure Do_Snapshot (Manager : in out Toolchain_Manager_Record);
   --  Saves the current Manager state.

   procedure Do_Commit (Manager : in out Toolchain_Manager_Record);
   --  Commits the changes done to the Manager since last Snapshot

   procedure Do_Rollback (Manager : in out Toolchain_Manager_Record);
   --  Rollback changes done to the manager since last snapshot

   procedure Free (Manager : in out Toolchain_Manager);
   --  Free memory associated with the manager

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

   type Tool_Record is record
      Command   : Ada.Strings.Unbounded.Unbounded_String;
      --  The actual command used to launch the tool

      Is_Valid  : Boolean;
      --  Wether the command was found

      Origin    : Compiler_Origin;
      --  Where the tool definition comes from

      Base_Name : Boolean;
      --  Tells if this tool's command is a base name or a full path name
   end record;

   No_Tool : constant Tool_Record :=
               (Command     => Ada.Strings.Unbounded.Null_Unbounded_String,
                Is_Valid    => False,
                Origin      => From_Default,
                Base_Name => False);

   type Tool_Array is array (Tools) of Tool_Record;

   type Compiler is record
      Exe         : Ada.Strings.Unbounded.Unbounded_String;
      --  The base or full name of the compiler

      Is_Valid    : Boolean;
      --  Wether the Exe command could be resolved to an actual file

      Origin      : Compiler_Origin;
      --  Where this compiler description comes from

      Toolchain   : Ada.Strings.Unbounded.Unbounded_String;
      --  The toolchain the compiler belongs to

      Lang        : Ada.Strings.Unbounded.Unbounded_String;
      --  The language compiled by this compiler

      Base_Name : Boolean;
      --  Tells if this Exe field is a base name or a full path
   end record;

   No_Compiler : constant Compiler :=
                   (Exe         => Ada.Strings.Unbounded.Null_Unbounded_String,
                    Is_Valid    => False,
                    Origin      => From_Default,
                    Toolchain   => Ada.Strings.Unbounded.Null_Unbounded_String,
                    Lang        => Ada.Strings.Unbounded.Null_Unbounded_String,
                    Base_Name => False);

   package Compiler_Vector is new Ada.Containers.Vectors (Positive, Compiler);
   package Compiler_Ref_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Natural,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   package Runtime_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   package Runtime_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => "=");

   package Runtime_Lists_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Runtime_Lists.List,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
      "="             => Runtime_Lists."=");

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

      Tools         : Tool_Array := (others => No_Tool);
      --  The tools for this toolchain.

      Default_Tools : Tool_Array := (others => No_Tool);
      --  The default tools for this toolchain, as defined in the known
      --  toolchains database

      Full_Compiler_List : Compiler_Vector.Vector;
      --  All compilers defined for this toolchain.

      Used_Compiler_List : Compiler_Ref_Maps.Map;
      --  The compilers currently in use, indexed by language.

      Defined_Runtimes : Runtime_Lists_Maps.Map;
      --  The runtimes defined for this toolchain, indexed by language.

      Used_Runtimes : Runtime_Maps.Map;
      --  The runtimes currently used by this toolchain, indexed by language.

      Compilers_Scanned : Boolean := False;

      Is_Valid : Boolean;
      --  Is this toolchain accessible from the environment ?

      Library : Ada_Library_Info_Access;

      Manager : Toolchain_Manager;
      --  The manager this toolchain is attached to

      Refs    : Integer := 0;
      --  Number of references to that toolchain. The toolchain is never
      --  freed before this reaches 0.
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
      Saved_Toolchains    : Toolchain_Maps.Map;
      No_Native_Toolchain : Boolean := False;
      Computed_Libraries  : Library_Maps.Map;
      Listeners           : Listener_List.List;
      Languages           : Language_Sets.Set;
      Gprconfig_Compilers : Compiler_Vector.Vector;
      Compilers_Scanned   : Boolean := False;
      Gprconfig_Success   : Boolean := True;
   end record;

   function Get_Known_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Name    : String) return Toolchain;
   --  Gets a toolchain from a known description - creates it if it's not yet
   --  stored in the manager.

   function Create_Anonymous_Name
     (Manager : access Toolchain_Manager_Record;
      Prefix  : String) return String;
   --  Return a unique anonymous name from a prefix, that's not already
   --  registered in the manager

   procedure Fire_Change_Event (This : access Toolchain_Manager_Record);
   --  Calls the Toolchain_Changed event on all listeners.

end Toolchains;
