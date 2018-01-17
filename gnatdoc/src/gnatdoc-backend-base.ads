------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

with GNATdoc.Atree; use GNATdoc.Atree;

package GNATdoc.Backend.Base is

   package Virtual_File_Vectors is
     new Ada.Containers.Vectors (Positive, GNATCOLL.VFS.Virtual_File);

   type Collected_Entities is record
      Access_Types      : EInfo_List.Vector;
      CPP_Classes       : EInfo_List.Vector;
      CPP_Constructors  : EInfo_List.Vector;
      Generic_Formals   : EInfo_List.Vector;
      Interface_Types   : EInfo_List.Vector;
      Methods           : EInfo_List.Vector;
      Pkgs              : aliased EInfo_List.Vector;
      --  Ordinary and generic packages.
      Pkgs_Instances    : EInfo_List.Vector;
      --  Generic packages instantiations.
      Record_Types      : EInfo_List.Vector;
      Simple_Types      : EInfo_List.Vector;
      Subprgs           : aliased EInfo_List.Vector;
      Tagged_Types      : EInfo_List.Vector;
      Variables         : EInfo_List.Vector;
      Tasks             : EInfo_List.Vector;
      Protected_Objects : EInfo_List.Vector;
      Entries           : EInfo_List.Vector;
   end record;

   type Base_Backend is
     abstract new GNATdoc.Backend.GNATdoc_Backend with record
      Context       : access constant Docgen_Context;
      Src_Files     : Files_List.Vector;
      Entities      : Collected_Entities;
      Resource_Dirs : Virtual_File_Vectors.Vector;
      --  List of directories to lookup for resource files.
   end record;

   function Name (Self : Base_Backend) return String is abstract;
   --  Returns name of the backend in lowercase. It is used to construct paths.

   procedure Generate_Lang_Documentation
     (Self        : in out Base_Backend;
      Tree        : access Tree_Type;
      Entity      : Entity_Id;
      Entities    : Collected_Entities;
      Scope_Level : Natural) is abstract;
   --  Generates documentation for single <lang> file.

   function Get_Resource_File
     (Self      : Base_Backend'Class;
      File_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Virtual_File;
   --  Returns full name of the specified resource file. This subprogram do
   --  lookup in all resource directories and take in sense name of backend.

   overriding procedure Initialize
     (Self    : in out Base_Backend;
      Context : access constant Docgen_Context);
   --  Initialize backend.

   overriding procedure Process_File
     (Self : in out Base_Backend;
      Tree : access Tree_Type);
   --  Process one file. Depending from the file's language it calls
   --  corresponding Generate_<language>_Documentation subprogram. This
   --  subprogram should not be overridden by derived backends.

end GNATdoc.Backend.Base;
