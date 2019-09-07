------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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

--  This package provides buffer-related facilities to the Clang integration:
--    - resolution of a line/column from an offset
--    - list of 'Unsaved_Files'

with GNATCOLL.VFS;   use GNATCOLL.VFS;

with GPS.Core_Kernels; use GPS.Core_Kernels;

with Libclang.Index; use Libclang.Index;
with Basic_Types;    use Basic_Types;

package Clang_Buffer_Facilities is

   function Get_Unsaved_Files (K : Core_Kernel) return Unsaved_File_Array;
   --  Return the currently unsaved files

   procedure Offset_To_Line_Column
     (K      : Core_Kernel;
      File   : Virtual_File;
      Offset : Integer;
      Line   : out Integer;
      Column : out Visible_Column_Type);
   --  Return the line/column pair corresponding to the offset in the given
   --  file.
   --
   --  This can also be done by libclang from a given Translation_Unit, but
   --  obtaining a Translation_Unit can be very slow, so prefer to call this
   --  function if you do not already have a Translation_Unit at hand.
   --
   --  Note; this assumes offset comes from libclang, so it can have different
   --  interpretations: if it comes from a libclang 'unsaved_file', this means
   --  the text comes from an editor, and therefore the offset needs to be
   --  interpreted from this editor. In other cases, this means that libclang
   --  has read the offset from file on disk, and therefore the line/column
   --  need to be re-extracted from this file on disk, so it can take into
   --  account non-normalized line terminators.

end Clang_Buffer_Facilities;
