-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
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

with System.Address_To_Access_Conversions;

with Gtk.Tree_Model.Utils;

package body Code_Analysis.Tree_Models is

   package Node_Conversion is
     new System.Address_To_Access_Conversions (Code_Analysis.Node);

   package Utilities is

      function Project_At
        (Tree  : Code_Analysis.Code_Analysis_Tree;
         Index : Positive) return Code_Analysis.Project_Access;

      function Project_Index
        (Tree    : Code_Analysis.Code_Analysis_Tree;
         Project : Code_Analysis.Project_Access) return Natural;

      function Next_Project
        (Tree    : Code_Analysis.Code_Analysis_Tree;
         Project : Code_Analysis.Project_Access)
         return Code_Analysis.Project_Access;

      function File_At
        (Project : Code_Analysis.Project_Access;
         Index   : Positive) return Code_Analysis.File_Access;

      function File_Index
        (Project : Code_Analysis.Project_Access;
         File    : Code_Analysis.File_Access) return Natural;

      function Next_File
        (Project : Code_Analysis.Project_Access;
         File    : Code_Analysis.File_Access) return Code_Analysis.File_Access;

      function Subprogram_At
        (File  : Code_Analysis.File_Access;
         Index : Positive) return Code_Analysis.Subprogram_Access;

      function Subprogram_Index
        (File       : Code_Analysis.File_Access;
         Subprogram : Code_Analysis.Subprogram_Access) return Natural;

      function Next_Subprogram
        (File       : Code_Analysis.File_Access;
         Subprogram : Code_Analysis.Subprogram_Access)
         return Code_Analysis.Subprogram_Access;

   end Utilities;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Simple_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Parent);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Parent);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Parent);

   begin
      if Subprogram_Node /= null then
         return Gtk.Tree_Model.Null_Iter;

      elsif File_Node /= null then
         return
           Self.Create_Tree_Iter
             (Project_Node, File_Node, Utilities.Subprogram_At (File_Node, 1));

      elsif Project_Node /= null then
         return
           Self.Create_Tree_Iter
             (Project_Node, Utilities.File_At (Project_Node, 1));

      elsif Gtk.Tree_Model.Utils.Is_Null (Parent) then
         return Self.Create_Tree_Iter (Utilities.Project_At (Self.Tree, 1));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Children;

   ----------------------
   -- Create_Tree_Iter --
   ----------------------

   function Create_Tree_Iter
     (Self            : access Simple_Tree_Model_Record'Class;
      Project_Node    : Code_Analysis.Project_Access    := null;
      File_Node       : Code_Analysis.File_Access       := null;
      Subprogram_Node : Code_Analysis.Subprogram_Access := null)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);

   begin
      return
        Gtk.Tree_Model.Utils.Init_Tree_Iter
          (1,
           Node_Conversion.To_Address
             (Node_Conversion.Object_Pointer (Project_Node)),
           Node_Conversion.To_Address
             (Node_Conversion.Object_Pointer (File_Node)),
           Node_Conversion.To_Address
             (Node_Conversion.Object_Pointer (Subprogram_Node)));
   end Create_Tree_Iter;

   -------------
   -- File_At --
   -------------

   function File_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.File_Access
   is
      pragma Unreferenced (Self);
      --  Not used because all related information is stored directly in the
      --  GtkTreeIter.

   begin
      return
        Code_Analysis.File_Access
          (Code_Analysis.Node_Access
               (Node_Conversion.To_Pointer
                    (Gtk.Tree_Model.Utils.Get_User_Data_2 (Iter))));
   end File_At;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Simple_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);

      Index_1 : constant Integer := Indices'First;
      Index_2 : constant Integer := Indices'First + 1;
      Index_3 : constant Integer := Indices'First + 2;

      Project_Node    : Code_Analysis.Project_Access;
      File_Node       : Code_Analysis.File_Access;
      Subprogram_Node : Code_Analysis.Subprogram_Access;

   begin
      if Indices'Length = 1 then
         Project_Node :=
           Utilities.Project_At (Self.Tree, Integer (Indices (Index_1)) + 1);

         return Self.Create_Tree_Iter (Project_Node);

      elsif Indices'Length = 2 then
         Project_Node :=
           Utilities.Project_At (Self.Tree, Integer (Indices (Index_1)) + 1);
         File_Node :=
           Utilities.File_At (Project_Node, Integer (Indices (Index_2)) + 1);

         return Self.Create_Tree_Iter (Project_Node, File_Node);

      elsif Indices'Length = 3 then
         Project_Node :=
           Utilities.Project_At (Self.Tree, Integer (Indices (Index_1)) + 1);
         File_Node :=
           Utilities.File_At (Project_Node, Integer (Indices (Index_2)) + 1);
         Subprogram_Node :=
           Utilities.Subprogram_At
             (File_Node, Integer (Indices (Index_3)) + 1);

         return
           Self.Create_Tree_Iter (Project_Node, File_Node, Subprogram_Node);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Iter);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Iter);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Iter);
      Result          : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                          Gtk.Tree_Model.Gtk_New;

   begin
      if Project_Node /= null then
         Gtk.Tree_Model.Append_Index
           (Result,
            Glib.Gint (Utilities.Project_Index (Self.Tree, Project_Node) - 1));

         if File_Node /= null then
            Gtk.Tree_Model.Append_Index
              (Result,
               Glib.Gint (Utilities.File_Index (Project_Node, File_Node) - 1));

            if Subprogram_Node /= null then
               Gtk.Tree_Model.Append_Index
                 (Result,
                  Glib.Gint
                    (Utilities.Subprogram_Index
                       (File_Node, Subprogram_Node) - 1));
            end if;
         end if;
      end if;

      return Result;
   end Get_Path;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Iter);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Iter);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Iter);

   begin
      if Gtk.Tree_Model.Utils.Is_Null (Iter) then
         return not Self.Tree.Is_Empty;

      elsif Subprogram_Node /= null then
         return False;
         --  Subprogram cann't have child.

      elsif File_Node /= null then
         return not File_Node.Subprograms.Is_Empty;

      elsif Project_Node /= null then
         return not Project_Node.Files.Is_Empty;

      else
         --  "Total" line never has child

         return False;
      end if;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : access Simple_Tree_Model_Record'Class;
      Tree : Code_Analysis.Code_Analysis_Tree)
   is
   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      Self.Tree := Tree;
   end Initialize;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Iter);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Iter);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Iter);

   begin
      if Subprogram_Node /= null then
         return 0;

      elsif File_Node /= null then
         return Glib.Gint (File_Node.Subprograms.Length);

      elsif Project_Node /= null then
         return Glib.Gint (Project_Node.Files.Length);

      elsif Gtk.Tree_Model.Utils.Is_Null (Iter) then
         return Glib.Gint (Self.Tree.Length + 1);
         --  Additional child here is a "Totals" line

      else
         return 0;
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Simple_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Project_Node    : Code_Analysis.Project_Access := Self.Project_At (Iter);
      File_Node       : Code_Analysis.File_Access    := Self.File_At (Iter);
      Subprogram_Node : Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Iter);

   begin
      if Subprogram_Node /= null then
         Subprogram_Node :=
           Utilities.Next_Subprogram (File_Node, Subprogram_Node);

         if Subprogram_Node /= null then
            Iter :=
              Self.Create_Tree_Iter (Project_Node, File_Node, Subprogram_Node);

         else
            Iter := Gtk.Tree_Model.Null_Iter;
         end if;

      elsif File_Node /= null then
         File_Node := Utilities.Next_File (Project_Node, File_Node);

         if File_Node /= null then
            Iter := Self.Create_Tree_Iter (Project_Node, File_Node);

         else
            Iter := Gtk.Tree_Model.Null_Iter;
         end if;

      elsif Project_Node /= null then
         Project_Node := Utilities.Next_Project (Self.Tree, Project_Node);

         Iter := Self.Create_Tree_Iter (Project_Node);

      else
         --  "Total" line is a last line in the report

         Iter := Gtk.Tree_Model.Null_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Simple_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project_Node    : Code_Analysis.Project_Access :=
                          Self.Project_At (Parent);
      File_Node       : Code_Analysis.File_Access :=
                          Self.File_At (Parent);
      Subprogram_Node : Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Parent);

   begin
      if Subprogram_Node /= null then
         return Gtk.Tree_Model.Null_Iter;

      elsif File_Node /= null then
         Subprogram_Node :=
           Utilities.Subprogram_At (File_Node, Integer (N) + 1);

         if Subprogram_Node /= null then
            return
              Self.Create_Tree_Iter (Project_Node, File_Node, Subprogram_Node);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;

      elsif Project_Node /= null then
         File_Node := Utilities.File_At (Project_Node, Integer (N) + 1);

         if File_Node /= null then
            return Self.Create_Tree_Iter (Project_Node, File_Node);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;

      elsif Gtk.Tree_Model.Utils.Is_Null (Parent) then
         Project_Node := Utilities.Project_At (Self.Tree, Integer (N) + 1);

         --  Project_Node = null means the child is a "Total" line.

         return Self.Create_Tree_Iter (Project_Node);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Simple_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Child);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Child);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Child);

   begin
      if Subprogram_Node /= null then
         return Self.Create_Tree_Iter (Project_Node, File_Node);

      elsif File_Node /= null then
         return Self.Create_Tree_Iter (Project_Node);

      elsif Project_Node /= null then
         return Gtk.Tree_Model.Null_Iter;

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Parent;

   ----------------
   -- Project_At --
   ----------------

   function Project_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Project_Access
   is
      pragma Unreferenced (Self);
      --  Not used because all related information is stored directly in the
      --  GtkTreeIter.

   begin
      return
        Code_Analysis.Project_Access
          (Code_Analysis.Node_Access
               (Node_Conversion.To_Pointer
                    (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter))));
   end Project_At;

   -------------------
   -- Subprogram_At --
   -------------------

   function Subprogram_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Subprogram_Access
   is
      pragma Unreferenced (Self);
      --  Not used because all related information is stored directly in the
      --  GtkTreeIter.

   begin
      return
        Code_Analysis.Subprogram_Access
          (Code_Analysis.Node_Access
               (Node_Conversion.To_Pointer
                    (Gtk.Tree_Model.Utils.Get_User_Data_3 (Iter))));
   end Subprogram_At;

   ---------------
   -- Utilities --
   ---------------

   package body Utilities is

      -------------
      -- File_At --
      -------------

      function File_At
        (Project : Code_Analysis.Project_Access;
         Index   : Positive) return Code_Analysis.File_Access
      is
         Map_Cur  : Code_Analysis.File_Maps.Cursor := Project.Files.First;
         Sort_Arr : Code_Analysis.File_Array
           (1 .. Integer (Project.Files.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.File_Maps.Element (Map_Cur);
            Code_Analysis.File_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Files (Sort_Arr);

         if Index in Sort_Arr'Range then
            return Sort_Arr (Index);

         else
            return null;
         end if;
      end File_At;

      ----------------
      -- File_Index --
      ----------------

      function File_Index
        (Project : Code_Analysis.Project_Access;
         File    : Code_Analysis.File_Access) return Natural
      is
         Map_Cur  : Code_Analysis.File_Maps.Cursor := Project.Files.First;
         Sort_Arr : Code_Analysis.File_Array
           (1 .. Integer (Project.Files.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.File_Maps.Element (Map_Cur);
            Code_Analysis.File_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Files (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = File then
               return J;
            end if;
         end loop;

         return 0;
      end File_Index;

      ---------------
      -- Next_File --
      ---------------

      function Next_File
        (Project : Code_Analysis.Project_Access;
         File    : Code_Analysis.File_Access) return Code_Analysis.File_Access
      is
         Map_Cur  : Code_Analysis.File_Maps.Cursor := Project.Files.First;
         Sort_Arr : Code_Analysis.File_Array
           (1 .. Integer (Project.Files.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.File_Maps.Element (Map_Cur);
            Code_Analysis.File_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Files (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = File then
               if J + 1 in Sort_Arr'Range then
                  return Sort_Arr (J + 1);

               else
                  return null;
               end if;
            end if;
         end loop;

         return null;
      end Next_File;

      ------------------
      -- Next_Project --
      ------------------

      function Next_Project
        (Tree    : Code_Analysis.Code_Analysis_Tree;
         Project : Code_Analysis.Project_Access)
         return Code_Analysis.Project_Access
      is
         Map_Cur  : Code_Analysis.Project_Maps.Cursor := Tree.First;
         Sort_Arr : Code_Analysis.Project_Array (1 .. Integer (Tree.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Project_Maps.Element (Map_Cur);
            Code_Analysis.Project_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Projects (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = Project then
               if J + 1 in Sort_Arr'Range then
                  return Sort_Arr (J + 1);

               else
                  return null;
               end if;
            end if;
         end loop;

         return null;
      end Next_Project;

      ---------------------
      -- Next_Subprogram --
      ---------------------

      function Next_Subprogram
        (File       : Code_Analysis.File_Access;
         Subprogram : Code_Analysis.Subprogram_Access)
         return Code_Analysis.Subprogram_Access
      is
         Map_Cur  : Code_Analysis.Subprogram_Maps.Cursor
           := File.Subprograms.First;
         Sort_Arr : Code_Analysis.Subprogram_Array
           (1 .. Integer (File.Subprograms.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Subprogram_Maps.Element (Map_Cur);
            Code_Analysis.Subprogram_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Subprograms (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = Subprogram then
               if J + 1 in Sort_Arr'Range then
                  return Sort_Arr (J + 1);

               else
                  return null;
               end if;
            end if;
         end loop;

         return null;
      end Next_Subprogram;

      ----------------
      -- Project_At --
      ----------------

      function Project_At
        (Tree  : Code_Analysis.Code_Analysis_Tree;
         Index : Positive) return Code_Analysis.Project_Access
      is
         Map_Cur  : Code_Analysis.Project_Maps.Cursor := Tree.First;
         Sort_Arr : Code_Analysis.Project_Array (1 .. Integer (Tree.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Project_Maps.Element (Map_Cur);
            Code_Analysis.Project_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Projects (Sort_Arr);

         if Index in Sort_Arr'Range then
            return Sort_Arr (Index);

         else
            return null;
         end if;
      end Project_At;

      -------------------
      -- Project_Index --
      -------------------

      function Project_Index
        (Tree    : Code_Analysis.Code_Analysis_Tree;
         Project : Code_Analysis.Project_Access) return Natural
      is
         Map_Cur  : Code_Analysis.Project_Maps.Cursor := Tree.First;
         Sort_Arr : Code_Analysis.Project_Array (1 .. Integer (Tree.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Project_Maps.Element (Map_Cur);
            Code_Analysis.Project_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Projects (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = Project then
               return J;
            end if;
         end loop;

         return 0;
      end Project_Index;

      -------------------
      -- Subprogram_At --
      -------------------

      function Subprogram_At
        (File  : Code_Analysis.File_Access;
         Index : Positive) return Code_Analysis.Subprogram_Access
      is
         Map_Cur  : Code_Analysis.Subprogram_Maps.Cursor :=
           File.Subprograms.First;
         Sort_Arr : Code_Analysis.Subprogram_Array
           (1 .. Integer (File.Subprograms.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Subprogram_Maps.Element (Map_Cur);
            Code_Analysis.Subprogram_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Subprograms (Sort_Arr);

         if Index in Sort_Arr'Range then
            return Sort_Arr (Index);

         else
            return null;
         end if;
      end Subprogram_At;

      ----------------------
      -- Subprogram_Index --
      ----------------------

      function Subprogram_Index
        (File       : Code_Analysis.File_Access;
         Subprogram : Code_Analysis.Subprogram_Access) return Natural
      is
         Map_Cur  : Code_Analysis.Subprogram_Maps.Cursor :=
           File.Subprograms.First;
         Sort_Arr : Code_Analysis.Subprogram_Array
           (1 .. Integer (File.Subprograms.Length));

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Subprogram_Maps.Element (Map_Cur);
            Code_Analysis.Subprogram_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Subprograms (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Sort_Arr (J) = Subprogram then
               return J;
            end if;
         end loop;

         return 0;
      end Subprogram_Index;

   end Utilities;

end Code_Analysis.Tree_Models;
