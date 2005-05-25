-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

--  This package provides support for displaying tooltips in the editors.
--  These tooltips can be overriden by any module in GPS, but they also have
--  a default value which is extracted from the cross-references information.

with Tooltips;

private package Src_Editor_Box.Tooltips is

   function Create_Tooltips
     (Box : access Source_Editor_Box_Record'Class)
      return Standard.Tooltips.Tooltips_Access;
   --  Create a new tooltips handler for Box

end Src_Editor_Box.Tooltips;
