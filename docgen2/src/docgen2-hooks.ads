-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

package Docgen2.Hooks is

   procedure Register_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Need to be called at GPS startup: this registers the user_tag_hook

   procedure Documentation_Generation_Start_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Doc_Path : String);
   --  Notification for documentation generation start.

   procedure Documentation_Generation_Finish_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Doc_Path : String);
   --  Notification for documentation generation start.

   function User_Tag_Action
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Tag, Attrs, Value : String;
      Entity_Name, Href : String) return String;
   --  Execute the Docgen_User_Tag_Action hook and return the result

end Docgen2.Hooks;
