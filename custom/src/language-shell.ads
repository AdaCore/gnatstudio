------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS.Kernel;

package Language.Shell is
   procedure Register_Shell_Language
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Instance    : Class_Instance;
      Lang_Name   : String;
      Body_Suffix : String;
      Spec_Suffix : String := "";
      Obj_Suffix  : String := "";
      Indent      : Indentation_Kind := Simple);

   procedure Setup (Kernel : GPS.Kernel.Kernel_Handle);
   --  ??? Missing documentation

end Language.Shell;
