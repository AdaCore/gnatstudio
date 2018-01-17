------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;

package body Code_Analysis.Tree_Models is

   package Node_Conversion is
     new System.Address_To_Access_Conversions (Code_Analysis.Node);

   package Project_Conversions is
     new System.Address_To_Access_Conversions (Project_Item'Class);

   package File_Conversions is
     new System.Address_To_Access_Conversions (File_Item'Class);

   package Subprogram_Conversions is
     new System.Address_To_Access_Conversions (Subprogram_Item'Class);

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

      elsif Parent = Null_Iter then
         return Self.Create_Tree_Iter (Utilities.Project_At (Self.Tree, 1));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Children;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Filterable_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project    : constant Project_Item_Access    := Self.Project (Parent);
      File       : constant File_Item_Access       := Self.File (Parent);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Parent);

   begin
      if Subprogram /= null then
         return Gtk.Tree_Model.Null_Iter;

      elsif File /= null then
         if not File.Subprograms.Is_Empty then
            return
              Self.Create_Tree_Iter
                (Project, File, File.Subprograms.First_Element);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;

      elsif Project /= null then
         if not Project.Files.Is_Empty then
            return
              Self.Create_Tree_Iter (Project, Project.Files.First_Element);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;

      elsif Parent = Null_Iter then
         return Self.Create_Tree_Iter (Self.Projects.First_Element);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Children;

   ------------
   -- Create --
   ------------

   function Create
     (Self    : access Filterable_Tree_Model_Record;
      Project : Code_Analysis.Project_Access) return Project_Item_Access
   is
      pragma Unreferenced (Self);
      --  Used for call dispatching only

   begin
      return new Project_Item (Project);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Self : access Filterable_Tree_Model_Record;
      File : Code_Analysis.File_Access) return File_Item_Access
   is
      pragma Unreferenced (Self);
      --  Used for call dispatching only

   begin
      return new File_Item (File);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Self       : access Filterable_Tree_Model_Record;
      Subprogram : Code_Analysis.Subprogram_Access)
      return Subprogram_Item_Access
   is
      pragma Unreferenced (Self);
      --  Used for call dispatching only

   begin
      return new Subprogram_Item (Subprogram);
   end Create;

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

   ----------------------
   -- Create_Tree_Iter --
   ----------------------

   function Create_Tree_Iter
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access    := null;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);

   begin
      return
        Gtk.Tree_Model.Utils.Init_Tree_Iter
          (2,
           Project_Conversions.To_Address
             (Project_Conversions.Object_Pointer (Project)),
           File_Conversions.To_Address
             (File_Conversions.Object_Pointer (File)),
           Subprogram_Conversions.To_Address
             (Subprogram_Conversions.Object_Pointer (Subprogram)));
   end Create_Tree_Iter;

   ----------
   -- File --
   ----------

   function File
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return File_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return
        File_Item_Access
          (File_Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_2 (Iter)));
   end File;

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

   -------------
   -- File_At --
   -------------

   function File_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.File_Access
   is
      File : constant File_Item_Access := Self.File (Iter);

   begin
      if File /= null then
         return File.Node;

      else
         return null;
      end if;
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
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Filterable_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);

      Index_1 : constant Integer := Indices'First;
      Index_2 : constant Integer := Indices'First + 1;
      Index_3 : constant Integer := Indices'First + 2;

      Project    : Project_Item_Access;
      File       : File_Item_Access;
      Subprogram : Subprogram_Item_Access;

   begin
      if Indices'Length >= 1 then
         if Natural (Indices (Index_1)) < Natural (Self.Projects.Length) then
            Project := Self.Projects.Element (Natural (Indices (Index_1)) + 1);

         elsif Natural (Indices (Index_1))
                 = Natural (Self.Projects.Length)
         then
            --  "Totals" line

            return Self.Create_Tree_Iter;

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;
      end if;

      if Indices'Length >= 2 then
         if Natural (Indices (Index_2)) < Natural (Project.Files.Length) then
            File := Project.Files.Element (Natural (Indices (Index_2)) + 1);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;
      end if;

      if Indices'Length >= 3 then
         if Natural (Indices (Index_3))
              < Natural (File.Subprograms.Length)
         then
            Subprogram :=
              File.Subprograms.Element (Natural (Indices (Index_3)) + 1);

         else
            return Gtk.Tree_Model.Null_Iter;
         end if;
      end if;

      if Indices'Length >= 4 then
         return Gtk.Tree_Model.Null_Iter;
      end if;

      return Self.Create_Tree_Iter (Project, File, Subprogram);
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
      Result          : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      Gtk_New (Result);
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

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Project    : constant Project_Item_Access    := Self.Project (Iter);
      File       : constant File_Item_Access       := Self.File (Iter);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);
      Result     : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      Gtk_New (Result);
      if Project /= null then
         Gtk.Tree_Model.Append_Index
           (Result, Glib.Gint (Self.Projects.Find_Index (Project) - 1));

         if File /= null then
            Gtk.Tree_Model.Append_Index
              (Result, Glib.Gint (Project.Files.Find_Index (File) - 1));

            if Subprogram /= null then
               Gtk.Tree_Model.Append_Index
                 (Result,
                  Glib.Gint
                    (File.Subprograms.Find_Index (Subprogram) - 1));
            end if;
         end if;

      elsif Iter /= Null_Iter then
         Gtk.Tree_Model.Append_Index
           (Result, Glib.Gint (Self.Projects.Length));
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
      if Iter = Null_Iter then
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

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Project    : constant Project_Item_Access    := Self.Project (Iter);
      File       : constant File_Item_Access       := Self.File (Iter);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);

   begin
      if Iter = Null_Iter then
         return not Self.Projects.Is_Empty;

      elsif Subprogram /= null then
         return False;
         --  Subprogram cann't have child.

      elsif File /= null then
         return not File.Subprograms.Is_Empty;

      elsif Project /= null then
         return not Project.Files.Is_Empty;

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
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : access Filterable_Tree_Model_Record'Class;
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

      elsif Iter = Null_Iter then
         return Glib.Gint (Self.Tree.Length + 1);
         --  Additional child here is a "Totals" line

      else
         return 0;
      end if;
   end N_Children;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Project    : constant Project_Item_Access    := Self.Project (Iter);
      File       : constant File_Item_Access       := Self.File (Iter);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);

   begin
      if Subprogram /= null then
         return 0;

      elsif File /= null then
         return Glib.Gint (File.Subprograms.Length);

      elsif Project /= null then
         return Glib.Gint (Project.Files.Length);

      elsif Iter = Null_Iter then
         return Glib.Gint (Self.Projects.Length + 1);
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

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Filterable_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Project    : constant Project_Item_Access    := Self.Project (Iter);
      File       : constant File_Item_Access       := Self.File (Iter);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);

   begin
      if Subprogram /= null then
         declare
            Next : constant Subprogram_Vectors.Cursor :=
                     Subprogram_Vectors.Next
                       (File.Subprograms.Find (Subprogram));

         begin
            if Subprogram_Vectors.Has_Element (Next) then
               Iter :=
                 Self.Create_Tree_Iter
                   (Project, File, Subprogram_Vectors.Element (Next));

               return;
            end if;
         end;

      elsif File /= null then
         declare
            Next : constant File_Vectors.Cursor :=
                     File_Vectors.Next (Project.Files.Find (File));

         begin
            if File_Vectors.Has_Element (Next) then
               Iter :=
                 Self.Create_Tree_Iter (Project, File_Vectors.Element (Next));

               return;
            end if;
         end;

      elsif Project /= null then
         declare
            Next : constant Project_Vectors.Cursor :=
                     Project_Vectors.Next (Self.Projects.Find (Project));

         begin
            if Project_Vectors.Has_Element (Next) then
               Iter := Self.Create_Tree_Iter (Project_Vectors.Element (Next));

            else
               --  "Totals" line is a last line at the project level

               Iter := Self.Create_Tree_Iter;
            end if;

            return;
         end;
      end if;

      Iter := Gtk.Tree_Model.Null_Iter;
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

      elsif Parent = Null_Iter then
         Project_Node := Utilities.Project_At (Self.Tree, Integer (N) + 1);

         --  Project_Node = null means the child is a "Total" line.

         return Self.Create_Tree_Iter (Project_Node);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Filterable_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project    : constant Project_Item_Access    := Self.Project (Parent);
      File       : constant File_Item_Access       := Self.File (Parent);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Parent);

   begin
      if Subprogram /= null then
         return Gtk.Tree_Model.Null_Iter;

      elsif File /= null then
         if Natural (N) < Natural (File.Subprograms.Length) then
            return
              Self.Create_Tree_Iter
                (Project, File, File.Subprograms.Element (Natural (N) + 1));
         end if;

      elsif Project /= null then
         if Natural (N) < Natural (Project.Files.Length) then
            return
              Self.Create_Tree_Iter
                (Project, Project.Files.Element (Natural (N) + 1));
         end if;

      elsif Parent = Null_Iter then
         if Natural (N) < Natural (Self.Projects.Length) then
            return
              Self.Create_Tree_Iter (Self.Projects.Element (Natural (N) + 1));

         elsif Natural (N) = Natural (Self.Projects.Length) then
            --  "Totals" line

            return Self.Create_Tree_Iter;
         end if;
      end if;

      return Gtk.Tree_Model.Null_Iter;
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

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Filterable_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Project    : constant Project_Item_Access    := Self.Project (Child);
      File       : constant File_Item_Access       := Self.File (Child);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Child);

   begin
      if Subprogram /= null then
         return Self.Create_Tree_Iter (Project, File);

      elsif File /= null then
         return Self.Create_Tree_Iter (Project);

      elsif Project /= null then
         return Gtk.Tree_Model.Null_Iter;

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Parent;

   -------------
   -- Project --
   -------------

   function Project
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Project_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return
        Project_Item_Access
          (Project_Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));
   end Project;

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

   ----------------
   -- Project_At --
   ----------------

   function Project_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Project_Access
   is
      Project : constant Project_Item_Access := Self.Project (Iter);

   begin
      if Project /= null then
         return Project.Node;

      else
         return null;
      end if;
   end Project_At;

   -----------------
   -- Reconstruct --
   -----------------

   procedure Reconstruct (Self : access Filterable_Tree_Model_Record'Class) is

      procedure Reconstruct_Tree (Tree : Code_Analysis.Code_Analysis_Tree);

      procedure Reconstruct_Project (Project : Project_Item_Access);

      procedure Reconstruct_File
        (Project : Project_Item_Access;
         File    : File_Item_Access);

      ----------------------
      -- Reconstruct_File --
      ----------------------

      procedure Reconstruct_File
        (Project : Project_Item_Access;
         File    : File_Item_Access)
      is

         procedure Free is new Ada.Unchecked_Deallocation
           (Subprogram_Item'Class, Subprogram_Item_Access);

         Map_Cur    : Code_Analysis.Subprogram_Maps.Cursor :=
                        File.Node.Subprograms.First;
         Sort_Arr   : Code_Analysis.Subprogram_Array
                        (1 .. Integer (File.Node.Subprograms.Length));
         Subprogram : Subprogram_Item_Access;
         Index      : Positive := 1;
         Hidden     : Boolean;

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Subprogram_Maps.Element (Map_Cur);
            Code_Analysis.Subprogram_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Subprograms (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Index <= Natural (File.Subprograms.Length)
              and then File.Subprograms.Element (Index).Node = Sort_Arr (J)
            then
               Hidden := False;
               Subprogram := File.Subprograms.Element (Index);

            else
               Hidden := True;
               Subprogram := Self.Create (Sort_Arr (J));
            end if;

            if Self.Is_Visible (Project, File, Subprogram) then
               if Hidden then
                  File.Subprograms.Insert (Index, Subprogram);
                  Self.Row_Inserted (Project, File, Subprogram);

                  if File.Subprograms.Length = 1 then
                     Self.Row_Has_Child_Toggled (Project, File);
                  end if;

               elsif Self.Is_Changed (Project, File, Subprogram) then
                  Self.Row_Changed (Project, File, Subprogram);
               end if;

               Index := Index + 1;

            else
               if not Hidden then
                  Self.Row_Deleted (Project, File, Subprogram);
                  File.Subprograms.Delete (Index);

                  if File.Subprograms.Is_Empty then
                     Self.Row_Has_Child_Toggled (Project, File);
                  end if;
               end if;

               Free (Subprogram);
            end if;
         end loop;
      end Reconstruct_File;

      -------------------------
      -- Reconstruct_Project --
      -------------------------

      procedure Reconstruct_Project (Project : Project_Item_Access) is

         procedure Free is new Ada.Unchecked_Deallocation
           (File_Item'Class, File_Item_Access);

         Map_Cur  : Code_Analysis.File_Maps.Cursor := Project.Node.Files.First;
         Sort_Arr : Code_Analysis.File_Array
                      (1 .. Integer (Project.Node.Files.Length));
         File     : File_Item_Access;
         Index    : Positive := 1;
         Hidden   : Boolean;

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.File_Maps.Element (Map_Cur);
            Code_Analysis.File_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Files (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Index <= Natural (Project.Files.Length)
              and then Project.Files.Element (Index).Node = Sort_Arr (J)
            then
               Hidden := False;
               File := Project.Files.Element (Index);

            else
               Hidden := True;
               File := Self.Create (Sort_Arr (J));
            end if;

            if Self.Is_Visible (Project, File) then
               if Hidden then
                  Project.Files.Insert (Index, File);
                  Self.Row_Inserted (Project, File);

                  if Project.Files.Length = 1 then
                     Self.Row_Has_Child_Toggled (Project);
                  end if;

               elsif Self.Is_Changed (Project, File) then
                  Self.Row_Changed (Project, File);
               end if;

               Index := Index + 1;
               Reconstruct_File (Project, File);

            else
               if not Hidden then
                  Self.Row_Deleted (Project, File);
                  Project.Files.Delete (Index);

                  if Project.Files.Is_Empty then
                     Self.Row_Has_Child_Toggled (Project);
                  end if;
               end if;

               Free (File);
            end if;
         end loop;
      end Reconstruct_Project;

      ----------------------
      -- Reconstruct_Tree --
      ----------------------

      procedure Reconstruct_Tree (Tree : Code_Analysis.Code_Analysis_Tree) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Project_Item'Class, Project_Item_Access);

         Map_Cur  : Code_Analysis.Project_Maps.Cursor := Tree.First;
         Sort_Arr : Code_Analysis.Project_Array (1 .. Integer (Tree.Length));
         Project  : Project_Item_Access;
         Index    : Positive := 1;
         Hidden   : Boolean;
         Path     : Gtk_Tree_Path;

      begin
         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Code_Analysis.Project_Maps.Element (Map_Cur);
            Code_Analysis.Project_Maps.Next (Map_Cur);
         end loop;

         Code_Analysis.Sort_Projects (Sort_Arr);

         for J in Sort_Arr'Range loop
            if Index <= Natural (Self.Projects.Length)
              and then Self.Projects.Element (Index).Node = Sort_Arr (J)
            then
               Hidden := False;
               Project := Self.Projects.Element (Index);

            else
               Hidden := True;
               Project := Self.Create (Sort_Arr (J));
            end if;

            if Self.Is_Visible (Project) then
               if Hidden then
                  Self.Projects.Insert (Index, Project);
                  Self.Row_Inserted (Project);

                  if Self.Projects.Length = 1 then
                     Path := Gtk_Tree_Path_New;
                     Row_Has_Child_Toggled
                       (To_Interface (Self), Path, Null_Iter);
                     Path_Free (Path);
                  end if;

               elsif Self.Is_Changed (Project) then
                  Self.Row_Changed (Project);
               end if;

               Index := Index + 1;
               Reconstruct_Project (Project);

            else
               if not Hidden then
                  Self.Row_Deleted (Project);
                  Self.Projects.Delete (Index);

                  if Self.Projects.Is_Empty then
                     Path := Gtk_Tree_Path_New;
                     Row_Has_Child_Toggled
                       (To_Interface (Self), Path, Null_Iter);
                     Path_Free (Path);
                  end if;
               end if;

               Free (Project);
            end if;
         end loop;
      end Reconstruct_Tree;

   begin
      Reconstruct_Tree (Self.Tree);
      Self.Row_Changed (null);
   end Reconstruct;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Self.Create_Tree_Iter (Project, File, Subprogram);
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path := Self.Get_Path (Iter);

   begin
      Row_Changed (To_Interface (Self), Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Row_Changed;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Self.Create_Tree_Iter (Project, File, Subprogram);
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path := Self.Get_Path (Iter);

   begin
      Row_Deleted (To_Interface (Self), Path);
      Gtk.Tree_Model.Path_Free (Path);
   end Row_Deleted;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Self.Create_Tree_Iter (Project, File, Subprogram);
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path := Self.Get_Path (Iter);

   begin
      Row_Has_Child_Toggled (To_Interface (Self), Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Row_Has_Child_Toggled;

   ------------------
   -- Row_Inserted --
   ------------------

   procedure Row_Inserted
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Self.Create_Tree_Iter (Project, File, Subprogram);
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path := Self.Get_Path (Iter);

   begin
      Row_Inserted (To_Interface (Self), Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Row_Inserted;

   ----------------
   -- Subprogram --
   ----------------

   function Subprogram
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Subprogram_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return
        Subprogram_Item_Access
          (Subprogram_Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_3 (Iter)));
   end Subprogram;

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

   -------------------
   -- Subprogram_At --
   -------------------

   function Subprogram_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Subprogram_Access
   is
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);

   begin
      if Subprogram /= null then
         return Subprogram.Node;

      else
         return null;
      end if;
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
