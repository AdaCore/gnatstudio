------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

--  This package defines the abstract root type for GPS kernel.

with Ada.Tags;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Commands;
with Language_Handlers;
with Projects;
with Xref;
with GPS.Messages_Windows;
with GPS.Process_Launchers;
with GPS.Editors;                     use GPS.Editors;
with GNATCOLL.Projects;
with GNATCOLL.Scripts.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;
with Toolchains;
with Ada.Finalization;                use Ada.Finalization;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language;                        use Language;
with Ada.Containers;                  use Ada.Containers;
with Ada.Strings.Hash;
with Language.Tree;
with Language.Tree.Database;

package GPS.Core_Kernels is

   type Core_Kernel_Record is abstract
     new GNATCOLL.Scripts.Projects.Project_Tree_Retriever with private;

   type Core_Kernel is access all Core_Kernel_Record'Class;

   function Scripts
     (Kernel : access Core_Kernel_Record'Class)
      return GNATCOLL.Scripts.Scripts_Repository;
   --  Data used to store information for the scripting languages

   function Symbols
     (Kernel : access Core_Kernel_Record'Class)
      return GNATCOLL.Symbols.Symbol_Table_Access;
   --  Return the symbol table used to store various shared strings, in
   --  particular storing the name of all entities found in the source files

   function Lang_Handler
     (Kernel : access Core_Kernel_Record'Class)
      return Language_Handlers.Language_Handler;
   --  The type used to convert from file names to languages

   function Get_Construct_Database
     (Kernel : not null access Core_Kernel_Record)
      return Language.Tree.Database.Construct_Database_Access;
   --  Return the database storing the construct information

   function Get_Buffer_Factory
     (Kernel : not null access Core_Kernel_Record)
      return GPS.Editors.Editor_Buffer_Factory_Access is abstract;
   --  Get the buffer factory for this kernel, which will in turn allow you to
   --  get buffer objects

   function Default_Language_Tree_Provider
     (Kernel : not null access Core_Kernel_Record)
      return Semantic_Tree_Provider_Access;
   --  Will return the default language tree provider. At the time being, this
   --  is the abstract constructs tree provider.

   procedure Register_Tree_Provider
     (Kernel   : not null access Core_Kernel_Record;
      Lang     : Language_Access;
      Provider : Semantic_Tree_Provider_Access);
   --  Register a new tree provider for the given language. This is the entry
   --  point for implementors of the Abstract_Language_Tree API, which will
   --  register their implementation via this function.

   function Get_Abstract_Tree_For_File
     (Kernel  : not null access Core_Kernel_Record;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class;
   --  Returns the abstract tree for the given file. This is the entry point
   --  for clients of the Abstract_Language_Tree API. You get the tree for a
   --  specific file with this function, and the kernel will take care of
   --  dispatching on the correct underlying implementation of the tree API.
   --  See Abstract_Language_Tree for Context description.

   procedure Semantic_Tree_Updated
     (Kernel  : not null access Core_Kernel_Record;
      File    : GNATCOLL.VFS.Virtual_File) is null;
   --  This procedure is called each time when a semantic tree finishes
   --  Update_Async routine.

   function Registry
     (Kernel : access Core_Kernel_Record'Class)
      return Projects.Project_Registry_Access;
   --  The project registry

   overriding function Get_Project_Tree
     (Self : Core_Kernel_Record)
      return GNATCOLL.Projects.Project_Tree_Access;

   function Databases
     (Kernel : access Core_Kernel_Record'Class)
      return Xref.General_Xref_Database;
   --  Return the entity databases

   function Messages_Window
     (Self : not null access Core_Kernel_Record)
      return GPS.Messages_Windows.Abstract_Messages_Window_Access
      is abstract;
   --  Return console window

   function Process_Launcher
     (Self : not null access Core_Kernel_Record)
     return GPS.Process_Launchers.Process_Launcher is abstract;
   --  Process launcher service

   function Get_Share_Dir
     (Self : not null access Core_Kernel_Record)
     return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return share/gps/ in installation directory for GPS.
   --  This always ends up with a directory separator.

   function Get_System_Dir
     (Handle : not null access Core_Kernel_Record)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the installation directory for GPS. This always ends up with a
   --  directory separator

   function Create_From_Base
     (Kernel : access Core_Kernel_Record'Class;
      Name   : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Virtual_File;
   --  Create a new file. First try to resolve Base_Name (Name) to an absolute
   --  path based on the source and object paths. If no file is found,
   --  use Name instead.

   procedure Create_Registry
     (Self   : not null access Core_Kernel_Record;
      Result : out Projects.Project_Registry_Access) is abstract;
   --  Initialize Registry with kernel specific version

   procedure Create_Database
     (Self   : not null access Core_Kernel_Record;
      Result : out Xref.General_Xref_Database) is abstract;
   --  Initialize Database with kernel specific version

   procedure Create_Scripts_Repository
     (Self   : not null access Core_Kernel_Record;
      Result : out GNATCOLL.Scripts.Scripts_Repository);
   --  Initialize Scripts_Repository with kernel specific version

   function Get_Build_Mode
     (Self : not null access Core_Kernel_Record) return String;
   --  Returns the current build mode.

   function Get_Target
     (Self : not null access Core_Kernel_Record) return String is abstract;
   --  Returns the current target

   function Get_Runtime
     (Self : not null access Core_Kernel_Record) return String is abstract;
   --  Return the current runtime

   function Get_Toolchains_Manager
     (Self : not null access Core_Kernel_Record)
      return Toolchains.Toolchain_Manager;

   procedure Set_Toolchains_Manager
     (Self    : not null access Core_Kernel_Record;
      Manager : Toolchains.Toolchain_Manager);

   procedure Initialize
     (Self    : not null access Core_Kernel_Record'Class;
      Symbols : GNATCOLL.Symbols.Symbol_Table_Access := null);

   procedure Destroy (Self : not null access Core_Kernel_Record'Class);

   type Abstract_Module_Record is abstract tagged limited null record;
   type Abstract_Module is access all Abstract_Module_Record'Class;

   procedure Destroy (Id : in out Abstract_Module_Record) is null;
   --  Free the memory associated with the module. By default, this does
   --  nothing.

   procedure Register_Module
     (Kernel : access Core_Kernel_Record'Class;
      Module : not null Abstract_Module);
   --  Register Module in Kernel. Module is registered as implementation of
   --  any service defined by each of its parents.

   function Module
     (Kernel : access Core_Kernel_Record'Class;
      Tag    : Ada.Tags.Tag) return Abstract_Module;
   --  Return last module that implement service with given Tag

   package Abstract_Module_List is new Ada.Containers.Doubly_Linked_Lists
     (Abstract_Module, "=");

   function Module_List
     (Kernel : access Core_Kernel_Record'Class;
      Tag    : Ada.Tags.Tag) return Abstract_Module_List.List;
   --  Return list of modules that implement service with given Tag

   type Editor_Listener_Factory is abstract new Controlled with null record;
   type Editor_Listener_Factory_Access is
     access all Editor_Listener_Factory'Class;

   function Get_Scheduled_Command
     (Kernel        : not null access Core_Kernel_Record;
      Dummy_Command : access Commands.Root_Command'Class)
      return Commands.Command_Access
     is (null);
   --  Return the command that wraps Command in the task manager. Such a
   --  wrapper is used when commands are run in the background.
   --  This always returns null for the CLI kernel, since it does not include
   --  a task manager for background commands.

   -----------------------------
   -- Editor_Listener_Factory --
   -----------------------------

   function Create
     (This : Editor_Listener_Factory;
      Editor : Editor_Buffer'Class;
      Factory : Editor_Buffer_Factory'Class;
      Kernel : Core_Kernel) return Editor_Listener_Access
      is abstract;

private

   function Hash (Tag : Ada.Tags.Tag) return Ada.Containers.Hash_Type;

   package Module_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Tags.Tag,
      Element_Type    => Abstract_Module_List.List,
      Hash            => Hash,
      Equivalent_Keys => Ada.Tags."=",
      "="             => Abstract_Module_List."=");

   function Language_Hash (L : Language_Access) return Hash_Type is
     (Ada.Strings.Hash (L.Get_Name));

   package Sem_Tree_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type     => Language_Access,
      Element_Type => Semantic_Tree_Provider_Access,
      Hash         => Language_Hash,
      Equivalent_Keys => "=");

   type Core_Kernel_Record is abstract
     new GNATCOLL.Scripts.Projects.Project_Tree_Retriever with
   record
      Symbols : GNATCOLL.Symbols.Symbol_Table_Access;
      --  The symbol used to store common strings read from sources

      Lang_Handler : Language_Handlers.Language_Handler;
      --  The type used to convert from file names to languages

      Registry : Projects.Project_Registry_Access;
      --  The project registry

      Database : Xref.General_Xref_Database;
      --  The cross-reference information

      Scripts : GNATCOLL.Scripts.Scripts_Repository;
      --  Data used to store information for the scripting languages

      Toolchains_Manager : Toolchains.Toolchain_Manager;
      --  Current toolchain manager

      Semantic_Tree_Providers : Sem_Tree_Maps.Map;
      --  Map of Language indices -> Semantic tree provider

      Modules : Module_Maps.Map;
   end record;

end GPS.Core_Kernels;
