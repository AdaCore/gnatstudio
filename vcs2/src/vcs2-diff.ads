------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with GPS.Kernel; use GPS.Kernel;

package VCS2.Diff is

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Create actions for this module

   procedure Create_Or_Reuse_Diff_Editor
     (Kernel              : Kernel_Handle;
      Patch               : String;
      Title               : String := "";
      Header              : String := "";
      Give_Focus_On_Reuse : Boolean := False);
   --  Get or reuse an editor using the diff language. Patch will replace the
   --  buffer content. If Title is set then renames the editor.
   --  If Header is set then prefix the Patch with Header.
   --  If Give_Focus_On_Reuse is True, the focus is given to the editior, even
   --  when reusing an already existing one.

   procedure Clear_Diff_Editor (Kernel : Kernel_Handle);
   --  Empty the diff editor.

end VCS2.Diff;
