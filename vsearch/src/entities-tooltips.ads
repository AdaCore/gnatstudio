-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides functions for drawing tooltips representing entity
--  informations.

with Gdk.Pixmap;       use Gdk.Pixmap;

with GPS.Kernel;       use GPS.Kernel;

with Entities;         use Entities;
with Entities.Queries; use Entities.Queries;

package Entities.Tooltips is

   function Draw_Tooltip
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Ref    : Entity_Reference;
      Status : Find_Decl_Or_Body_Query_Status) return Gdk_Pixmap;
   --  Return a tooltip representing Entity.

end Entities.Tooltips;
