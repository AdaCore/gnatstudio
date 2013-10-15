------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

package body GNATdoc.Backend.Base is

   -----------------------
   -- Get_Resource_File --
   -----------------------

   function Get_Resource_File
     (Self      : Base_Backend'Class;
      File_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Virtual_File
   is
      Backend : constant Filesystem_String := Filesystem_String (Self.Name);
      Dir     : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.Kernel.Get_Share_Dir.Create_From_Dir
          ("gnatdoc").Create_From_Dir (Backend);
      Dev_Dir : constant GNATCOLL.VFS.Virtual_File :=
        Self.Context.Kernel.Get_Share_Dir.Create_From_Dir
          ("gnatdoc").Create_From_Dir ("resources").Create_From_Dir (Backend);

   begin
      --  Special case: check for this in order to be able to work
      --  in the development environment

      if Dir.Is_Directory then
         return Dir.Create_From_Dir (File_Name);

      else
         return Dev_Dir.Create_From_Dir (File_Name);
      end if;
   end Get_Resource_File;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Backend : in out Base_Backend;
      Context : access constant Docgen_Context) is
   begin
      Backend.Context := Context;
   end Initialize;

end GNATdoc.Backend.Base;
