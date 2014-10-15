------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with Libclang.Index; use Libclang.Index;
with GPS.Core_Kernels; use GPS.Core_Kernels;

package Language.Libclang is
   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files)
      return Clang_Translation_Unit;
end Language.Libclang;
