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
with Code_Coverage;            use Code_Coverage;
with VFS;

package body Code_Analysis_Tree_Model is

   ----------------
   -- Fill_Store --
   ----------------

   procedure Fill_Store
     (Tree_Store  : in out Gtk_Tree_Store;
      Iter        : in out Gtk_Tree_Iter;
      Analysis_Id : Analysis) is
   begin
      if Analysis_Id.Coverage_Data /= null then
         Fill_Store (Tree_Store, Iter, Analysis_Id.Coverage_Data);
      end if;
   end Fill_Store;

   ----------------
   -- Fill_Store --
   ----------------

   procedure Fill_Store
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      Sub_Node   : Subprogram_Access) is
   begin
      Append (Tree_Store, Iter, Parent);
      Set (Tree_Store, Iter, Node_Col, String (Sub_Node.Name.all));
      Fill_Store (Tree_Store, Iter, Sub_Node.Analysis_Data);
   end Fill_Store;

   ----------------
   -- Fill_Store --
   ----------------

   procedure Fill_Store
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter;
      File_Node  : File_Access)
   is
      use Subprogram_Maps;
      Self_Iter : Gtk_Tree_Iter;
      Cur       : Cursor;
      Sub_Node  : Subprogram_Access;
   begin
      Append (Tree_Store, Iter, Parent);
      Self_Iter := Iter;
      Set (Tree_Store, Iter, Node_Col, VFS.Base_Name (File_Node.Name));
      Fill_Store (Tree_Store, Iter, File_Node.Analysis_Data);
      Cur := File_Node.Subprograms.First;

      loop
         exit when Cur = No_Element;
         Sub_Node := Element (Cur);
         Fill_Store (Tree_Store, Iter, Self_Iter, Sub_Node);
         Next (Cur);
      end loop;
   end Fill_Store;

   ----------------
   -- Fill_Store --
   ----------------

   procedure Fill_Store
     (Tree_Store   : in out Gtk_Tree_Store;
      Iter         : in out Gtk_Tree_Iter;
      Project_Node : Project_Access)
   is
      use File_Maps;
      Self_Iter    : Gtk_Tree_Iter;
      Cur          : Cursor;
      File_Node    : File_Access;
   begin
      Append (Tree_Store, Iter, Null_Iter);
      Self_Iter := Iter;
      Set (Tree_Store,
        Iter,
        Node_Col,
        UTF8_String (String'(Project_Name (Project_Node.Name))));
      Fill_Store (Tree_Store, Iter, Project_Node.Analysis_Data);
      Cur := Project_Node.Files.First;

      loop
         exit when Cur = No_Element;
         File_Node := Element (Cur);
         Fill_Store (Tree_Store, Iter, Self_Iter, File_Node);
         Next (Cur);
      end loop;
   end Fill_Store;

   ----------------
   -- Fill_Store --
   ----------------

   procedure Fill_Store
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Cur        : Project_Maps.Cursor)
   is
      use Project_Maps;
      Variable_Cur : Cursor := Cur;
      Project_Node : Project_Access;
   begin
      loop
         exit when Variable_Cur = No_Element;
         Project_Node := Element (Variable_Cur);
         Fill_Store (Tree_Store, Iter, Project_Node);
         Next (Variable_Cur);
      end loop;
   end Fill_Store;
end Code_Analysis_Tree_Model;
