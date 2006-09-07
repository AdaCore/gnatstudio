-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package provides the subprograms needed by the construction of a
--  Gtk_Tree_View for Code_Analysis structure
--  </description>

with Code_Analysis;   use Code_Analysis;
with Gtk.Tree_Store;  use Gtk.Tree_Store;
with Gtk.Tree_Model;  use Gtk.Tree_Model;

package Code_Analysis_Tree_Model is

   package GType_Subprogram is new Generic_Set (Code_Analysis.Subprogram);
   package GType_File is new Generic_Set (Code_Analysis.File);
   package GType_Project is new Generic_Set (Code_Analysis.Project);

   ------------
   -- Filler --
   ------------

   procedure Fill_Iter
     (Model      : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Cur        : Project_Maps.Cursor);
   --  Fill the Gtk_Tree_Store

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Subp_Node : Subprogram_Access);
   --  Fill the Gtk_Tree_Store with the given Subprogram node

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      File_Node : File_Access);
   --  Fill the Gtk_Tree_Store with the given File node

   procedure Fill_Iter
     (Model        : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Project_Node : Project_Access);
   --  Fill the Gtk_Tree_Store with the given Project node

   procedure Fill_Iter
     (Model        : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Analysis_Id  : Analysis);
   --  Fill the Gtk_Tree_Store with the given Analysis record

end Code_Analysis_Tree_Model;
