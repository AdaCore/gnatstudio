------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

--  This package implements the reStructured text backend of Docgen. Given
--  that we do not need to implement several backends (because sphynx takes
--  care of processing reStructured Text and generate html, Latex, PDF, etc.)
--  the routines in this package have not been designed using tagged types.

--  For details on the reStructured Text Markup language read:
--    http://docutils.sourceforge.net/docs/user/rst/quickref.html
--    http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html

--  For details on sphynx read:
--    http://sphinx-doc.org

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPS.Kernel;        use GPS.Kernel;
with Docgen3.Atree;     use Docgen3.Atree;
with Docgen3.Files;     use Docgen3.Files;
with Docgen3.Frontend;  use Docgen3.Frontend;

private package Docgen3.Backend is

   type Backend_Info is private;

   function Get_Doc_Directory
     (Kernel : Kernel_Handle) return Virtual_File;
   --  If the Directory_Dir attribute is defined in the project, then use the
   --  value; otherwise use the default directory (that is, a subdirectory
   --  'doc' in the object directory, or in the project directory if no
   --  object dir is defined).

   procedure Initialize
     (Context : access constant Docgen_Context;
      Info    : out Backend_Info);
   --  Initialize the backend and create the destination directory with
   --  support files. Returns the backend structure used to collect
   --  information of all the processed files (used to generate the
   --  global indexes).

   procedure Process_File
     (Context : access constant Docgen_Context;
      Tree    : access Tree_Type;
      Info    : Backend_Info);
   --  Generate documentation of a single file

   procedure Finalize
     (Context             : access constant Docgen_Context;
      Src_Files           : in out Files_List.Vector;
      Info                : Backend_Info;
      Update_Global_Index : Boolean);
   --  Destroy the backend structure used to colledt information of all the
   --  processed files. If Update_Global_Index is true then update the global
   --  indexes.

private
   type Collected_Entities is record
      Pkgs         : EInfo_List.Vector;
      Variables    : EInfo_List.Vector;
      Types        : EInfo_List.Vector;
      Record_Types : EInfo_List.Vector;
      Tagged_Types : EInfo_List.Vector;
      Subprgs      : EInfo_List.Vector;
      Methods      : EInfo_List.Vector;
   end record;

   type Backend_Info is access Collected_Entities;
end Docgen3.Backend;
