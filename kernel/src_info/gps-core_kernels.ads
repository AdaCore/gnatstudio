------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Language_Handlers;
with Projects;
with Xref;

with GNATCOLL.Scripts;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;

package GPS.Core_Kernels is

   type Core_Kernel_Record is abstract tagged private;

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

   function Registry
     (Kernel : access Core_Kernel_Record'Class)
      return Projects.Project_Registry_Access;
   --  The project registry

   function Databases
     (Kernel : access Core_Kernel_Record'Class)
      return Xref.General_Xref_Database;
   --  Return the entity databases

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
      Result : out GNATCOLL.Scripts.Scripts_Repository) is abstract;
   --  Initialize Scripts_Repository with kernel specific version

   procedure Initialize (Self : not null access Core_Kernel_Record'Class);

   procedure Destroy (Self : not null access Core_Kernel_Record'Class);

private

   type Core_Kernel_Record is abstract tagged record
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
   end record;

end GPS.Core_Kernels;
