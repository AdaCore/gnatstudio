-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package contains subprograms related to the Projects registry which
--  require to spawn external executables.

package Projects.Registry.Queries is

   procedure Compute_Predefined_Paths
     (Registry     : Project_Registry_Access;
      GNAT_Version : out GNAT.Strings.String_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      E_Handler    : Error_Handler := Null_E_Handler);
   --  Compute the predefined paths for the GNAT runtime, and return the
   --  GNAT version that is used.

end Projects.Registry.Queries;
