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

with GNATCOLL.VFS;
with GPS.Kernel;
with Language;         use Language;
with Xref;

package Docgen2.Utils is

   procedure Warning
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Loc    : Xref.General_Location;
      Msg    : String);
   --  Prints a warning

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether File is a spec file

   function Get_Entity
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Construct : String;
      Loc       : Xref.General_Location;
      Lang      : Language.Language_Access)
      return Xref.General_Entity;
   --  Retrieve the entity corresponding to construct at location Loc/File.

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : Xref.General_Location;
      Db        : access Xref.General_Xref_Database_Record'Class;
      Lang      : Language.Language_Access)
      return Xref.General_Entity;
   --  Retrieve the entity declaration corresponding to construct.

   function Filter (S : String) return String;
   --  Returns an string without the spaces and line terminators at the
   --  beginning/end of the string.

   function Spaces_Only (Text : String) return Boolean;
   --  Returns true if the string is not empty and all its characters are ' '

end Docgen2.Utils;
