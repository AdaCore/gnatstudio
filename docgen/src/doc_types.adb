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

package body Doc_Types is

   -------------
   -- Compare --
   -------------

   --  the .ads file is always "smaller" than an .adb!
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
      if not Type_Reference_List.Is_Empty (X.Ref_List) then
         Type_Reference_List.Free (X.Ref_List);
      end if;
   end Free;

   procedure Free (X : in out Reference_List_Information) is
   begin
      Free (X.File_Name);
   end Free;

   -----------------
   -- Count_Lines --
   -----------------

   --  returns the number of lines in the String
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


   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : Src_Info.E_Kind) return String is
   begin
      --  ??? Would be nice to do it as a primitive subprogram of the
      --  LI_Handlers, unfortunately they currently don't have access to
      --  Glide_Intl for proper translations.

      case Kind is
         when Overloaded_Entity            => return "???";
         when Unresolved_Entity            => return "unknown";
         when Access_Object                =>
            return "access variable / pointer";
         when Access_Type                  => return "access type / pointer";
         when Array_Object                 => return "array";
         when Array_Type                   => return "array type";
         when Boolean_Object               => return "boolean";
         when Boolean_Type                 => return "boolean type";
         when Class_Wide_Object            => return "class wide";
         when Class_Wide_Type              => return "class wide type";
         when Decimal_Fixed_Point_Object   => return "decimal fixed point";
         when Decimal_Fixed_Point_Type     =>
            return "decimal fixed point type";
         when Entry_Or_Entry_Family        => return "entry or entry family";
            --  later
         when Enumeration_Literal          => return "enumeration literal";
            --  not used
         when Enumeration_Object           => return "enumeration";
         when Enumeration_Type             => return "enumeration type";
         when Exception_Entity             => return "exception";
         when Floating_Point_Object        => return "floating point";
         when Floating_Point_Type          => return "floating point type";
         when Generic_Class                => return "generic class";
            --  later
         when Generic_Function_Or_Operator => return "generic function";
         when Generic_Package              => return "generic package";
            --  later
         when Generic_Procedure            => return "generic procedure";
         when Label_On_Block               => return "label on block";
            --  not used
         when Label_On_Loop                => return "label on loop";
            --  not used
         when Label_On_Statement           => return "label on statement";
            --  not used
         when Modular_Integer_Object       => return "modular integer";
         when Modular_Integer_Type         => return "modular integer type";
         when Named_Number                 => return "named number";
         when Non_Generic_Function_Or_Operator => return "function";
         when Non_Generic_Package          => return "package";
            --  later
         when Non_Generic_Procedure        => return "procedure";
         when Ordinary_Fixed_Point_Object  => return "fixed point";
         when Ordinary_Fixed_Point_Type    => return "fixed point type";
         when Private_Type                 => return "private type";
         when Protected_Object             => return "protected object";
         when Protected_Type               => return "protected type";
         when Record_Object                => return "record / struct";
         when Record_Type                  => return "record type / struct";
         when Signed_Integer_Object        => return "signed integer";
         when Signed_Integer_Type          => return "signed integer type";
         when String_Object                => return "string";
         when String_Type                  => return "string type";
         when Task_Object                  => return "task";
         when Task_Type                    => return "task type";
      end case;
   end Kind_To_String;

end Doc_Types;
