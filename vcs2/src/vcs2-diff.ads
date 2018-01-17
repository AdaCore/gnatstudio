------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

with GPS.Kernel;    use GPS.Kernel;
with Gtk.Text_View; use Gtk.Text_View;

package VCS2.Diff is

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Create actions for this module

   -----------------
   -- Diff viewer --
   -----------------

   type Diff_Viewer_Record is new Gtk_Text_View_Record with private;
   type Diff_Viewer is access all Diff_Viewer_Record'Class;
   --  This widget can be used to display a diff/patch with syntax highlighting

   procedure Gtk_New (Self  : out Diff_Viewer);
   --  Create a new widget

   procedure Add_Diff
     (Self  : not null access Diff_Viewer_Record;
      Patch : String);
   --  Display some diff in Self.

private

   type Diff_Viewer_Record is new Gtk_Text_View_Record with null record;

end VCS2.Diff;
