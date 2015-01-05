------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2015, AdaCore                  --
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

with Language_Handlers;
with Projects;
with Xref;
with GPS.Messages_Windows;
with GPS.Process_Launchers;
with GPS.Editors;
use GPS.Editors;

with GNATCOLL.Projects;
with GNATCOLL.Scripts.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;
with Toolchains;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization; use Ada.Finalization;

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

   function Get_Buffer_Factory
     (Kernel : not null access Core_Kernel_Record)
      return GPS.Editors.Editor_Buffer_Factory_Access is abstract;

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
     (Self : not null access Core_Kernel_Record'Class;
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

      Modules : Module_Maps.Map;
   end record;

end GPS.Core_Kernels;
