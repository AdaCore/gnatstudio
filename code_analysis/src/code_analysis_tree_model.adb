------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Glib;                     use Glib;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Code_Analysis_Tree_Model; use Code_Analysis_Tree_Model;
with Code_Analysis_GUI;        use Code_Analysis_GUI;
with Code_Coverage;            use Code_Coverage;
with GNATCOLL.VFS;
with System;

package body Code_Analysis_Tree_Model is

   procedure Set_Columns_Values
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Icon     : String;
      Name     : String;
      Node     : System.Address;
      File     : System.Address;
      Prjoject : System.Address);
   --  Set model's values.

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
         Set_And_Clear
           (Model, Iter, (Cov_Col, Cov_Sort, Cov_Bar_Txt, Cov_Bar_Val),
            (1 => As_String ("n/a"),
             2 => As_Int    (0),
             3 => As_String ("n/a"),
             4 => As_Int    (0)));
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
      Bin_Mode  : Boolean := False) is
   begin
      Append (Model, Iter, Parent);
      Set_Columns_Values
        (Model, Iter,
         Icon     => Subp_Pixbuf_Cst,
         Name     => Subp_Node.Name.all,
         Node     => Subp_Node.all'Address,
         File     => File_Node.all'Address,
         Prjoject => Prj_Node.all'Address);

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
      Bin_Mode  : Boolean := False) is
   begin
      Append (Model, Iter, Null_Iter);
      Set_Columns_Values
        (Model, Iter,
         Icon     => Subp_Pixbuf_Cst,
         Name     => Subp_Node.Name.all,
         Node     => Subp_Node.all'Address,
         File     => File_Node.all'Address,
         Prjoject => Prj_Node.all'Address);

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
      Bin_Mode  : Boolean := False)
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

      Set_Columns_Values
        (Model, Iter,
         Icon     => File_Pixbuf_Cst,
         Name     => GNATCOLL.VFS.Display_Base_Name (File_Node.Name),
         Node     => File_Node.all'Address,
         File     => File_Node.all'Address,
         Prjoject => Prj_Node.all'Address);

      Fill_Iter (Model, Iter, File_Node.Analysis_Data, Bin_Mode);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := (Element (Map_Cur));
         Next (Map_Cur);
      end loop;

      Sort_Subprograms (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Self_Iter, Prj_Node, File_Node, Sort_Arr (J),
                    Bin_Mode);
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
      Bin_Mode  : Boolean := False) is
   begin
      if File_Node.Analysis_Data.Coverage_Data /= null
        and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
      then
         Insert_After (Model, Iter, Null_Iter, Sibling);
         Sibling := Iter;
      else
         Append (Model, Iter, Null_Iter);
      end if;

      Set_Columns_Values
        (Model, Iter,
         Icon     => File_Pixbuf_Cst,
         Name     => GNATCOLL.VFS.Display_Base_Name (File_Node.Name),
         Node     => File_Node.all'Address,
         File     => File_Node.all'Address,
         Prjoject => Prj_Node.all'Address);

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
      Bin_Mode  : Boolean := False)
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
           (Model, Iter, Prj_Node, File_Node, Sort_Arr (J), Bin_Mode);
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
      Bin_Mode : Boolean := False)
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

      Set_And_Clear
        (Model, Iter, (Icon_Name_Col, Name_Col, Node_Col),
         (1 => As_String  (Prj_Pixbuf_Cst),
          2 => As_String  (Prj_Node.View.Name),
          3 => As_Pointer (Prj_Node.all'Address)));

      Fill_Iter (Model, Iter, Prj_Node.Analysis_Data, Bin_Mode);

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         Fill_Iter (Model, Iter, Child_Sibl, Self_Iter, Prj_Node, Sort_Arr (J),
                    Bin_Mode);
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
      Bin_Mode  : Boolean := False)
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
           (Model, Iter, Sibling, Prj_Node, Sort_Arr (J), Bin_Mode);
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Prj_Node : Project_Access;
      Bin_Mode : Boolean := False)
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
           (Model, Iter, Prj_Node, Sort_Arr (J), Bin_Mode);
      end loop;
   end Fill_Iter_With_Subprograms;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False)
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
         Fill_Iter (Model, Iter, Sibling, Sort_Arr (J), Bin_Mode);
      end loop;
   end Fill_Iter;

   --------------------------
   -- Fill_Iter_With_Files --
   --------------------------

   procedure Fill_Iter_With_Files
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False)
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
         Fill_Iter_With_Files (Model, Iter, Sibling, Sort_Arr (J), Bin_Mode);
      end loop;
   end Fill_Iter_With_Files;

   --------------------------------
   -- Fill_Iter_With_Subprograms --
   --------------------------------

   procedure Fill_Iter_With_Subprograms
     (Model    : Gtk_Tree_Store;
      Iter     : in out Gtk_Tree_Iter;
      Projects : Code_Analysis_Tree;
      Bin_Mode : Boolean := False)
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
           (Model, Iter, Sort_Arr (J), Bin_Mode);
      end loop;
   end Fill_Iter_With_Subprograms;

   ------------------------
   -- Set_Columns_Values --
   ------------------------

   procedure Set_Columns_Values
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Icon     : String;
      Name     : String;
      Node     : System.Address;
      File     : System.Address;
      Prjoject : System.Address) is
   begin
      Set_And_Clear
        (Model, Iter, (Icon_Name_Col, Name_Col, Node_Col, File_Col, Prj_Col),
         (1 => As_String  (Icon),
          2 => As_String  (Name),
          3 => As_Pointer (Node),
          4 => As_Pointer (File),
          5 => As_Pointer (Prjoject)));
   end Set_Columns_Values;

end Code_Analysis_Tree_Model;
