-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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

with Code_Analysis;     use Code_Analysis;
with Gtk.Tree_Store;    use Gtk.Tree_Store;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Code_Analysis_GUI; use Code_Analysis_GUI;

package Code_Analysis_Tree_Model is

   package Subprogram_Set is new Generic_Set (Code_Analysis.Subprogram);
   package File_Set is new Generic_Set (Code_Analysis.File);
   package Project_Set is new Generic_Set (Code_Analysis.Project);
   package Node_Set is new Generic_Set (Code_Analysis.Node);

   ------------
   -- Filler --
   ------------

   procedure Fill_Iter
     (Model       : in out Gtk_Tree_Store;
      Iter        : in out Gtk_Tree_Iter;
      Analysis_Id : Analysis;
      Bin_Mode    : Boolean := False);
   --  Fill the Gtk_Tree_Store with the given Analysis record
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Subp_Node : Subprogram_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the given Subprogram node
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Subprograms
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the given parentless Subprogram node
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Sibling   : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the given File node and recurse on its
   --  children
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Files
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Sibling   : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the given File node and stop
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Subprograms
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the children of the given File node
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Sibling  : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the given Project node and recurse on its
   --  children
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Files
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Sibling  : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the children of the given Project node
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Subprograms
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with the subprograms of the file children of the
   --  given Project node
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with every nodes
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Files
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with file rows only
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

   procedure Fill_Iter_With_Subprograms
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons);
   --  Fill the Gtk_Tree_Store with subprogram rows only
   --  If Bin_Mode is True, then the coverage messages will only be between
   --  (covered | not covered)

end Code_Analysis_Tree_Model;
