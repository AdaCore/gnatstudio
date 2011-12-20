------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  <description>
--  This package provides the subprograms needed by the construction of a
--  Gtk_Tree_View for Code_Analysis structure
--  </description>

with Glib;                     use Glib;
with Glib.Object;

with GPS.Intl;                 use GPS.Intl;
with Code_Analysis_Tree_Model; use Code_Analysis_Tree_Model;
with Code_Coverage;            use Code_Coverage;
with GNATCOLL.VFS;

package body Code_Analysis_Tree_Model is

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model       : Gtk_Tree_Store;
      Iter        : in out Gtk_Tree_Iter;
      Analysis_Id : Analysis;
      Bin_Mode    : Boolean := False) is
   begin
      if Analysis_Id.Coverage_Data /= null then
         Fill_Iter (Model, Iter, Analysis_Id.Coverage_Data, Bin_Mode);
      else
         Set (Model, Iter, Cov_Col, -"n/a");
         Set (Model, Iter, Cov_Sort, Glib.Gint (0));
         Set (Model, Iter, Cov_Bar_Val, Glib.Gint (0));
         Set (Model, Iter, Cov_Bar_Txt, -"n/a");
      end if;
   end Fill_Iter;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Subp_Node : Subprogram_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons) is
   begin
      Append (Model, Iter, Parent);
      Set (Model, Iter, Pix_Col, Glib.Object.GObject (Icons.Subp_Pixbuf));
      Set (Model, Iter, Name_Col, Subp_Node.Name.all);
      Subprogram_Set.Set (Model, Iter, Node_Col, Subp_Node.all'Access);
      File_Set.Set (Model, Iter, File_Col, File_Node.all'Access);
      Project_Set.Set (Model, Iter, Prj_Col, Prj_Node.all'Access);
      Fill_Iter (Model, Iter, Subp_Node.Analysis_Data, Bin_Mode);
   end Fill_Iter;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Subp_Node : Subprogram_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons) is
   begin
      Append (Model, Iter, Null_Iter);
      Set (Model, Iter, Pix_Col, Glib.Object.GObject (Icons.Subp_Pixbuf));
      Set (Model, Iter, Name_Col, Subp_Node.Name.all);
      Subprogram_Set.Set (Model, Iter, Node_Col, Subp_Node.all'Access);
      File_Set.Set (Model, Iter, File_Col, File_Node.all'Access);
      Project_Set.Set (Model, Iter, Prj_Col, Prj_Node.all'Access);
      Fill_Iter (Model, Iter, Subp_Node.Analysis_Data, Bin_Mode);
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Sibling   : in out Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons)
   is
      use Subprogram_Maps;
      Map_Cur   : Subprogram_Maps.Cursor := File_Node.Subprograms.First;
      Self_Iter : Gtk_Tree_Iter;
      Sort_Arr  : Subprogram_Array
        (1 .. Integer (File_Node.Subprograms.Length));
   begin
      if File_Node.Analysis_Data.Coverage_Data /= null
        and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
      then
         Insert_After (Model, Iter, Parent, Sibling);
         Sibling := Iter;
      else
         Append (Model, Iter, Parent);
      end if;

      Self_Iter := Iter;
      Gtk.Tree_Store.Set
        (Model, Iter, Pix_Col, Glib.Object.GObject (Icons.File_Pixbuf));
      Gtk.Tree_Store.Set
        (Model, Iter, Name_Col,
         GNATCOLL.VFS.Display_Base_Name (File_Node.Name));
      File_Set.Set (Model, Iter, Node_Col, File_Node.all'Access);
      File_Set.Set (Model, Iter, File_Col, File_Node.all'Access);
      Project_Set.Set (Model, Iter, Prj_Col, Prj_Node.all'Access);
      Fill_Iter (Model, Iter, File_Node.Analysis_Data, Bin_Mode);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Subprograms (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Self_Iter, Prj_Node, File_Node, Sort_Arr (J),
                    Bin_Mode, Icons);
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Sibling   : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons) is
   begin
      if File_Node.Analysis_Data.Coverage_Data /= null
        and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
      then
         Insert_After (Model, Iter, Null_Iter, Sibling);
         Sibling := Iter;
      else
         Append (Model, Iter, Null_Iter);
      end if;

      Gtk.Tree_Store.Set
        (Model, Iter, Pix_Col, Glib.Object.GObject (Icons.File_Pixbuf));
      Gtk.Tree_Store.Set
        (Model, Iter, Name_Col,
         GNATCOLL.VFS.Display_Base_Name (File_Node.Name));
      File_Set.Set (Model, Iter, Node_Col, File_Node.all'Access);
      File_Set.Set (Model, Iter, File_Col, File_Node.all'Access);
      Project_Set.Set (Model, Iter, Prj_Col, Prj_Node.all'Access);
      Fill_Iter (Model, Iter, File_Node.Analysis_Data, Bin_Mode);
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      File_Node : File_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons)
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
         Fill_Iter_With_Subprograms
           (Model, Iter, Prj_Node, File_Node, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Sibling  : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons)
   is
      use File_Maps;
      Map_Cur    : File_Maps.Cursor := Prj_Node.Files.First;
      Self_Iter  : Gtk_Tree_Iter;
      Sort_Arr   : File_Array (1 .. Integer (Prj_Node.Files.Length));
      Child_Sibl : Gtk_Tree_Iter := Null_Iter;
   begin
      if Prj_Node.Analysis_Data.Coverage_Data /= null
        and then Prj_Node.Analysis_Data.Coverage_Data.Is_Valid
      then
         Insert_After (Model, Iter, Null_Iter, Sibling);
         Sibling := Iter;
      else
         Append (Model, Iter, Null_Iter);
      end if;

      Self_Iter := Iter;
      Gtk.Tree_Store.Set
        (Model, Iter, Pix_Col, Glib.Object.GObject (Icons.Prj_Pixbuf));
      Gtk.Tree_Store.Set (Model, Iter, Name_Col,
                          UTF8_String (Prj_Node.Name.Name));
      Project_Set.Set (Model, Iter, Node_Col, Prj_Node.all'Access);
      Fill_Iter (Model, Iter, Prj_Node.Analysis_Data, Bin_Mode);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Child_Sibl, Self_Iter, Prj_Node, Sort_Arr (J),
                    Bin_Mode, Icons);
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model     : Gtk_Tree_Store;
      Iter      : in out Gtk_Tree_Iter;
      Sibling   : in out Gtk_Tree_Iter;
      Prj_Node  : Project_Access;
      Bin_Mode  : Boolean := False;
      Icons     : Code_Analysis_Icons)
   is
      use File_Maps;
      Map_Cur   : File_Maps.Cursor := Prj_Node.Files.First;
      Sort_Arr  : File_Array (1 .. Integer (Prj_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Files
           (Model, Iter, Sibling, Prj_Node, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons)
   is
      use File_Maps;
      Map_Cur   : File_Maps.Cursor := Prj_Node.Files.First;
      Sort_Arr  : File_Array (1 .. Integer (Prj_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Subprograms
           (Model, Iter, Prj_Node, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
      Sibling  : Gtk_Tree_Iter := Null_Iter;
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Sibling, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
      Sibling  : Gtk_Tree_Iter := Null_Iter;
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter_With_Files
           (Model, Iter, Sibling, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False;
      Icons    : Code_Analysis_Icons)
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
         Fill_Iter_With_Subprograms
           (Model, Iter, Sort_Arr (J), Bin_Mode, Icons);
      end loop;
   end Fill_Iter_With_Subprograms;
end Code_Analysis_Tree_Model;
