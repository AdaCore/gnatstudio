-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

with GPS.Kernel;
with Interactive_Consoles;

package Python_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Initialize_IO;
   --  Initialize the redirection of stdin, stdout and stderr.

   procedure Override_Default_IO
     (Console : Interactive_Consoles.Interactive_Console);
   --  Override the console to which Python should write through "print"

   procedure Load_Python_Startup_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Load all the user's startup file of the user.
   --  This procedure should be called only after all standard modules
   --  have been registered, so that if the user's startup files depend
   --  on standard GPS functions these are already loaded.
   --  This procedure does nothing if the python module hasn't been
   --  registered.
   --
   --  ??? Ideally, we should have a hook run after all modules have been
   --  registered

end Python_Module;

