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

with Glib;                     use Glib;
with Projects;                 use Projects;
with Code_Analysis_Tree_Model; use Code_Analysis_Tree_Model;
with Code_Analysis_Module;     use Code_Analysis_Module;
with Code_Coverage;            use Code_Coverage;
with VFS;

package body Code_Analysis_Tree_Model is

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model       : in out Gtk_Tree_Store;
      Iter        : in out Gtk_Tree_Iter;
      Analysis_Id : Analysis) is
   begin
      if Analysis_Id.Coverage_Data /= null then
         Fill_Iter (Model, Iter, Analysis_Id.Coverage_Data);
      end if;
   end Fill_Iter;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Subp_Node : Subprogram_Access)
   is
   begin
      Append (Model, Iter, Parent);
      Set (Model, Iter, Pix_Col, C_Proxy
           (Code_Analysis_Module_ID.Subp_Pixbuf));
      Set (Model, Iter, Name_Col, String (Subp_Node.Name.all));
      GType_Subprogram.Set (Model, Iter, Node_Col, Subp_Node.all'Access);
      Fill_Iter (Model, Iter, Subp_Node.Analysis_Data);
   end Fill_Iter;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      File_Node : File_Access)
   is
      use Subprogram_Maps;
      Map_Cur   : Subprogram_Maps.Cursor := File_Node.Subprograms.First;
      Self_Iter : Gtk_Tree_Iter;
      Sort_Arr  : Subprogram_Array
        (1 .. Integer (File_Node.Subprograms.Length));
   begin
      Append (Model, Iter, Parent);
      Self_Iter := Iter;
      Gtk.Tree_Store.Set (Model, Iter, Pix_Col, C_Proxy
           (Code_Analysis_Module_ID.File_Pixbuf));
      Gtk.Tree_Store.Set
        (Model, Iter, Name_Col, VFS.Base_Name (File_Node.Name));
      GType_File.Set (Model, Iter, Node_Col, File_Node.all'Access);
      Fill_Iter (Model, Iter, File_Node.Analysis_Data);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Subprograms (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Self_Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      File_Node : File_Access) is
   begin
      Append (Model, Iter, Null_Iter);
      Gtk.Tree_Store.Set (Model, Iter, Pix_Col, C_Proxy
           (Code_Analysis_Module_ID.File_Pixbuf));
      Gtk.Tree_Store.Set
        (Model, Iter, Name_Col, VFS.Base_Name (File_Node.Name));
      GType_File.Set (Model, Iter, Node_Col, File_Node.all'Access);
      Fill_Iter (Model, Iter, File_Node.Analysis_Data);
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model     : in out Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      File_Node : File_Access)
   is
      use Subprogram_Maps;
      Map_Cur   : Subprogram_Maps.Cursor := File_Node.Subprograms.First;
      Sort_Arr  : Subprogram_Array
        (1 .. Integer (File_Node.Subprograms.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Subprograms (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Null_Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model        : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur   : File_Maps.Cursor := Project_Node.Files.First;
      Self_Iter : Gtk_Tree_Iter;
      Sort_Arr  : File_Array (1 .. Integer (Project_Node.Files.Length));
   begin
      Append (Model, Iter, Null_Iter);
      Self_Iter := Iter;
      Gtk.Tree_Store.Set (Model, Iter, Pix_Col, C_Proxy
           (Code_Analysis_Module_ID.Project_Pixbuf));
      Gtk.Tree_Store.Set (Model, Iter, Name_Col,
           UTF8_String (String'(Project_Name (Project_Node.Name))));
      GType_Project.Set (Model, Iter, Node_Col, Project_Node.all'Access);
      Fill_Iter (Model, Iter, Project_Node.Analysis_Data);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Self_Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model        : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur   : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr  : File_Array (1 .. Integer (Project_Node.Files.Length));
   begin

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Files (Model, Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model        : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur   : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr  : File_Array (1 .. Integer (Project_Node.Files.Length));
   begin

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Subprograms (Model, Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Files (Model, Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model    : in out Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Subprograms (Model, Iter, Sort_Arr (J));
      end loop;
   end Fill_Iter_With_Subprograms;
end Code_Analysis_Tree_Model;
