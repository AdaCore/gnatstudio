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

--  This package groups a tree (that shows projects, directories, files, and
--  entities in the files), and the display of the scenario variables that the
--  user can modify.
--  This widget also knows how to save its state to an Ada stream, and re-read
--  a previously saved configuration.

with GPS.Kernel;
with Gtk.Handlers;
with Gtk.Box;
with Gtkada.Tree_View;

package Project_Explorers is

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Project_Explorer is access all Project_Explorer_Record'Class;

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new explorer.
   --  On each update, and since the list of withed projects can not changed,
   --  the open/close status of all the project nodes is kept.

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  You should connect to the "context_changed" signal in the kernel to get
   --  report on selection changes.
   --  </signals>

private
   type Project_Explorer_Access is access all Project_Explorer_Record;

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with record
      Tree      : Gtkada.Tree_View.Tree_View;

      Kernel    : GPS.Kernel.Kernel_Handle;
      Expand_Id : Gtk.Handlers.Handler_Id;
      --  The signal for the expansion of nodes in the project view

      Expanding : Boolean := False;
   end record;

end Project_Explorers;
