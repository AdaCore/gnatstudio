------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
--  Backtraces view for CodePeer's messages

with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View;  use Gtk.Tree_View;

with Generic_Views;
with GPS.Kernel;     use GPS.Kernel;

package CodePeer.Backtrace_View is

   type Backtrace_View_Record is new Generic_Views.View_Record with private;

   procedure Display_Backtraces
     (Kernel           : access Kernel_Handle_Record'Class;
      Output_Directory : GNATCOLL.VFS.Virtual_File;
      File             : GNATCOLL.VFS.Virtual_File;
      Message          : GPS.Kernel.Messages.Message_Access;
      Subprogram       : String;
      Set              : Natural_Sets.Set);
   --  Fill backtraces view by backtraces of specified VNs

   procedure Close_Backtraces_View
     (Kernel : access Kernel_Handle_Record'Class);
   --  Close Backtraces view

private

   type Backtrace_View_Record is new Generic_Views.View_Record with record
      Store : Gtk_Tree_Store;
      View  : Gtk_Tree_View;
   end record;

end CodePeer.Backtrace_View;
