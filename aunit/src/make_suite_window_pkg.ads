------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk.Button;           use Gtk.Button;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;

with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;

with GPS.Kernel;

package Make_Suite_Window_Pkg is

   type Make_Suite_Window_Record is new Gtk_Dialog_Record with record
      Kernel           : GPS.Kernel.Kernel_Handle;
      Directory_Entry  : Gtk_Entry;
      Name_Entry       : Gtk_Entry;
      Test_View        : Gtk_Tree_View;
      Test_Model       : Gtk_Tree_Store;
      Browse_Directory : Gtk_Button;
      Add              : Gtk_Button;
      Remove           : Gtk_Button;
      Label            : Gtk_Label;
   end record;
   type Make_Suite_Window_Access is access all Make_Suite_Window_Record'Class;

   procedure Gtk_New
     (Make_Suite_Window : out Make_Suite_Window_Access;
      Handle            : GPS.Kernel.Kernel_Handle);

   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class;
      Handle            : GPS.Kernel.Kernel_Handle);

end Make_Suite_Window_Pkg;
