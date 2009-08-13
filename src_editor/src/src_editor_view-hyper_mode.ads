-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009, AdaCore                        --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package describes the behavior of the Hyper Mode on source editor
--  views.

package Src_Editor_View.Hyper_Mode is

   procedure Activate_Hyper_Mode (View : access Source_View_Record);
   --  Activate the Hyper mode on View.
   --  This does not enter hyper mode, but connects the necessary handlers
   --  so that the view becomes able to enter hyper mode.

end Src_Editor_View.Hyper_Mode;
