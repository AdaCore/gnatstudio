-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Generic_List;

package body Docgen is

   -------------
   -- Compare --
   -------------

   function Compare_Elements (X, Y : Source_File_Information) return Boolean
   is
   begin
      if File_Extension (X.File_Name.all) = ".ads" then
         if File_Extension (Y.File_Name.all) = ".adb" then
            return True;
         else
            return X.File_Name.all < Y.File_Name.all;
         end if;
      else
         if File_Extension (Y.File_Name.all) = ".adb" then
            return X.File_Name.all < Y.File_Name.all;
         else
            return False;
         end if;
      end if;
   end Compare_Elements;


   function Compare_Elements_Name
     (X, Y : Entity_List_Information) return Boolean is
   begin
      if X.Is_Private and not Y.Is_Private then
         return False;
      elsif not X.Is_Private and Y.Is_Private then
         return True;
      else
         return X.Short_Name.all < Y.Short_Name.all;
      end if;
   end Compare_Elements_Name;

   function Compare_Elements_Line
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return X.Line < Y.Line;
   end Compare_Elements_Line;

   function Compare_Elements_Column
     (X, Y : Entity_List_Information) return Boolean is
   begin
      return X.Column < Y.Column;
   end Compare_Elements_Column;

   function Compare_Elements_Column
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return X.Column < Y.Column;
   end Compare_Elements_Column;

   function Compare_Elements_Name
     (X, Y : Reference_List_Information) return Boolean is
   begin
      return X.Subprogram_Name.all < Y.Subprogram_Name.all;
   end Compare_Elements_Name;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Source_File_Information) is
   begin
      Free (X.File_Name);
      Free (X.Prj_File_Name);
      Free (X.Package_Name);
   end Free;


   procedure Free (X : in out Entity_List_Information) is
   begin
      Free (X.Name);
      Free (X.Short_Name);
      Free (X.File_Name);
      if not Type_Reference_List.Is_Empty (X.Calls_List) then
         Type_Reference_List.Free (X.Calls_List);
      end if;
      if not Type_Reference_List.Is_Empty (X.Called_List) then
         Type_Reference_List.Free (X.Called_List);
      end if;
   end Free;

   procedure Free (X : in out Reference_List_Information) is
   begin
      Free (X.File_Name);
      Free (X.Subprogram_Name);
   end Free;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines
     (Line    : String) return Natural is
      Line_Nr : Natural;
   begin
      Line_Nr := 1;
      for J in Line'First .. Line'Last loop
         if Line (J) = ASCII.LF then
            Line_Nr := Line_Nr + 1;
         end if;
      end loop;
      return Line_Nr;
   end Count_Lines;

   ----------------------
   -- Get_String_Index --
   ----------------------

   function Get_String_Index
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Natural
   is
      L : constant Natural := Substring'Length - 1;
      New_Index : Natural;
   begin
      if Type_Str'Length = 1 then
         if Type_Str = Substring then
            return 1;
         else
            return 0;
         end if;
      end if;
      New_Index := Index;
      while New_Index + L <= Type_Str'Last
        and then Type_Str (New_Index .. New_Index + L) /= Substring
      loop
         New_Index := New_Index + 1;
      end loop;
      if New_Index + L > Type_Str'Last then return 0;
      else return New_Index;
      end if;
   end Get_String_Index;

end Docgen;
