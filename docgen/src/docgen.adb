-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Generic_List;
with VFS;                       use VFS;
with Src_Info.Queries;          use Src_Info.Queries;
with Glide_Kernel.Project;      use Glide_Kernel, Glide_Kernel.Project;
with Projects.Registry;         use Projects, Projects.Registry;

package body Docgen is

   ----------------------
   -- Compare_Elements --
   ----------------------

   function Compare_Elements (X, Y : Source_File_Information) return Boolean is
   begin
      if X.File_Name = Y.File_Name then
         --  ??? Strange test: if one is the spec, the other is also the spec
         --  return Is_Spec_File (Kernel, X.File_Name);
         return True;
      else
         return Full_Name (X.File_Name).all < Full_Name (Y.File_Name).all;
      end if;
   end Compare_Elements;

   ---------------------------
   -- Compare_Elements_Name --
   ---------------------------

   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean is
   begin
      if X.Is_Private and not Y.Is_Private then
         return False;
      elsif not X.Is_Private and Y.Is_Private then
         return True;
      else
         return Get_Name (X.Entity) < Get_Name (Y.Entity);
      end if;
   end Compare_Elements_Name;

   ---------------------------
   -- Compare_Elements_Line --
   ---------------------------

   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Declaration_Line_Of (X.Entity) <
        Get_Declaration_Line_Of (Y.Entity);
   end Compare_Elements_Line;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Declaration_Column_Of (X.Entity) <
        Get_Declaration_Column_Of (Y.Entity);
   end Compare_Elements_Column;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Declaration_Column_Of (X.Entity) <
        Get_Declaration_Column_Of (Y.Entity);
   end Compare_Elements_Column;

   ---------------------------
   -- Compare_Elements_Name --
   ---------------------------

   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Name (X.Entity) < Get_Name (Y.Entity);
   end Compare_Elements_Name;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Source_File_Information) is
   begin
      Free (X.Package_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_List_Information) is
   begin
      Free (X.Name);
      Destroy (X.Entity);

      if not Type_Reference_List.Is_Empty (X.Calls_List) then
         Type_Reference_List.Free (X.Calls_List);
      end if;

      if not Type_Reference_List.Is_Empty (X.Called_List) then
         Type_Reference_List.Free (X.Called_List);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_List_Information) is
   begin
      Destroy (X.Entity);
   end Free;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines (Line : String) return Natural is
      Line_Nr : Natural := 1;
   begin
      for J in Line'Range loop
         if Line (J) = ASCII.LF then
            Line_Nr := Line_Nr + 1;
         end if;
      end loop;

      return Line_Nr;
   end Count_Lines;

   -----------------------
   -- Get_Doc_File_Name --
   -----------------------

   function Get_Doc_File_Name
     (Source_Filename : VFS.Virtual_File;
      Source_Path     : String;
      Doc_Suffix      : String) return String
   is
      Ext      : constant String := File_Extension (Source_Filename);
   begin
      return Source_Path &
        Base_Name (Source_Filename, Ext)
        & "_"
        & Ext (Ext'First + 1 .. Ext'Last)
        & Doc_Suffix;
   end Get_Doc_File_Name;

   -----------
   -- Clone --
   -----------

   function Clone
     (Entity : Entity_List_Information) return Entity_List_Information is
   begin
      return
        (Kind         => Entity.Kind,
         Name         => new String'(Entity.Name.all),
         Entity       => Copy (Entity.Entity),
         Is_Private   => Entity.Is_Private,
         Line_In_Body => Entity.Line_In_Body,
         Calls_list   => Type_Reference_List.Null_List,    --  ???
         Called_List  => Type_Reference_List.Null_List);   --  ???
   end Clone;

   -------------------------
   -- Source_File_In_List --
   -------------------------

   function Source_File_In_List
     (Source_File_List : Type_Source_File_List.List;
      Name             : Virtual_File) return Boolean
   is
      package TSFL renames Type_Source_File_List;
      use type TSFL.List_Node;
      Source_File_Node : TSFL.List_Node := TSFL.First (Source_File_List);
   begin
      while Source_File_Node /= TSFL.Null_Node loop
         if TSFL.Data (Source_File_Node).File_Name = Name then
            return True;
         end if;

         Source_File_Node := TSFL.Next (Source_File_Node);
      end loop;

      return False;
   end Source_File_In_List;

   ------------------
   -- Count_Points --
   ------------------

   function Count_Points (Text : String) return Natural is
      Counter : Natural := 0;
   begin
      for J in Text'First .. Text'Last loop
         if Text (J) = '.' then
            Counter := Counter + 1;
         end if;
      end loop;

      return Counter;
   end Count_Points;

   -----------------
   -- Spec_Suffix --
   -----------------

   function Spec_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String is
   begin
      if Is_Spec_File (Kernel, File) then
         return File_Extension (File);
      else
         return File_Extension (Other_File_Name (Kernel, File));
      end if;
   end Spec_Suffix;

   -----------------
   -- Body_Suffix --
   -----------------

   function Body_Suffix
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return String is
   begin
      if Is_Spec_File (Kernel, File) then
         return File_Extension (Other_File_Name (Kernel, File));
      else
         return File_Extension (File);
      end if;
   end Body_Suffix;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Boolean is
   begin
      return Get_Unit_Part_From_Filename
        (Get_Project_From_File (Get_Registry (Kernel), File), File) =
        Unit_Spec;
   end Is_Spec_File;

end Docgen;
