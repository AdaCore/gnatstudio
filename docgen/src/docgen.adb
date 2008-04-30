-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with Projects.Registry;  use Projects, Projects.Registry;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GPS.Intl;           use GPS.Intl;

package body Docgen is

   function String_Hash is new HTables.Hash (HTable_Header);

   -----------
   -- Error --
   -----------

   procedure Error
     (Report : in out Docgen_Error_Reporter_Record;
      File   : Source_File)
   is
      Message : constant String := -"No cross references found for " &
                  Full_Name (Get_Filename (File)).all;
   begin
      Report.Called := True;
      Insert (Report.Kernel, Message, Add_LF => True, Mode => Error);
   end Error;

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
         return Get_Name (X.Entity).all < Get_Name (Y.Entity).all;
      end if;
   end Compare_Elements_Name;

   ---------------------------
   -- Compare_Elements_Line --
   ---------------------------

   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return Get_Line (Get_Declaration_Of (X.Entity)) <
        Get_Line (Get_Declaration_Of (Y.Entity));
   end Compare_Elements_Line;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean
   is
      use Basic_Types;
   begin
      return Get_Column (Get_Declaration_Of (X.Entity)) <
        Get_Column (Get_Declaration_Of (Y.Entity));
   end Compare_Elements_Column;

   -----------------------------
   -- Compare_Elements_Column --
   -----------------------------

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean
   is
      use Basic_Types;
   begin
      return Get_Column (Get_Declaration_Of (X.Entity)) <
        Get_Column (Get_Declaration_Of (Y.Entity));
   end Compare_Elements_Column;

   ---------------------------
   -- Compare_Elements_Name --
   ---------------------------

   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return Get_Name (X.Entity).all < Get_Name (Y.Entity).all;
   end Compare_Elements_Name;

   -----------------
   --  Duplicate  --
   -----------------

   procedure Duplicate
     (List_Out : in out Type_Reference_List.List;
      List_In  : in Type_Reference_List.List)
   is
      use Type_Reference_List;
      Node : Type_Reference_List.List_Node;
      Data : Type_Reference_List.Data_Access;

   begin
      if not Type_Reference_List.Is_Empty (List_In) then
         Node := Type_Reference_List.First (List_In);

         while Node /= Type_Reference_List.Null_Node loop
            Data := Type_Reference_List.Data_Ref (Node);
            Ref (Data.Entity);
            Type_Reference_List.Append
              (List_Out, (Data.Entity, Data.Set_Link));
            Node := Type_Reference_List.Next (Node);
         end loop;
      end if;
   end Duplicate;

   -----------------------------------------
   -- Compare_Elements_By_Line_And_Column --
   -----------------------------------------

   function Compare_Elements_By_Line_And_Column
     (X, Y : Reference_In_File) return Boolean
   is
      use Basic_Types;
   begin
      return X.Line < Y.Line or else
      (X.Line = Y.Line and then X.Column < Y.Column);
   end Compare_Elements_By_Line_And_Column;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Source_File_Information) is
   begin
      GNAT.Strings.Free (X.Unit_Name);
      GNAT.Strings.Free (X.Doc_File_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_List_Information) is
   begin
      Unref (X.Entity);
      if X.Public_Declaration /= null then
         Unref (X.Public_Declaration);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_List_Information) is
   begin
      Unref (X.Entity);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Reference_In_File) is
      pragma Unreferenced (X);
   begin
      null;
      --  Memory accessed by the field Entity is freed separately: those
      --  records are saved in a list. When we destroy this list, it calls
      --  subprogram Free (Entity_Information).
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_Information) is
      pragma Unreferenced (X);
   begin
      null;
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
     (Source_Filename : GNATCOLL.VFS.Virtual_File;
      Doc_Suffix      : String) return String
   is
      Ext : constant String := File_Extension (Source_Filename);
   begin
      return Base_Name (Source_Filename, Ext)
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
      if Entity.Public_Declaration /= null then
         Ref (Entity.Public_Declaration);
      end if;

      Ref (Entity.Entity);
      return
        (Kind               => Entity.Kind,
         Entity             => Entity.Entity,
         Is_Private         => Entity.Is_Private,
         Line_In_Body       => Entity.Line_In_Body,
         Public_Declaration => Entity.Public_Declaration,
         Processed          => Entity.Processed);
   end Clone;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Entities.Source_File) return HTable_Header is
   begin
      return String_Hash (Full_Name (Get_Filename (Key)).all);
   end Hash;

   -------------------------
   -- Source_File_In_List --
   -------------------------

   function Source_File_In_List
     (Source_File_List : Type_Source_File_Table.HTable;
      File             : Source_File) return Boolean is
   begin
      return Type_Source_File_Table.Get (Source_File_List, File) /=
        No_Source_File_Information;
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

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Get_Unit_Part_From_Filename
        (Get_Project_From_File (Get_Registry (Kernel).all, File), File) =
        Unit_Spec;
   end Is_Spec_File;

end Docgen;
