-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Casing;                  use Casing;
with Namet;                   use Namet;
with Prj.Com;                 use Prj.Com;
with Stringt;                 use Stringt;
with Types;                   use Types;
with Snames;
with Prj.Util;                use Prj.Util;
with Prj_API;                 use Prj_API;
with Prj;                     use Prj;

package body Src_Info.Prj_Utils is

   function Get_Filename
     (Unit_Name_Id       : Name_Id;
      Dot_Replacement_Id : Name_Id;
      Casing             : Casing_Type;
      Extension_Id       : Name_Id)
      return File_Name_Type;
   --  Return the filename for the given Unit_Name using the Dot_Replacement,
   --  Casing and Extension rules.

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Unit_Name_Id       : Name_Id;
      Dot_Replacement_Id : Name_Id;
      Casing             : Casing_Type;
      Extension_Id       : Name_Id)
      return File_Name_Type
   is
      Unit_Name : constant String := Get_Name_String (Unit_Name_Id);
      Dot_Repl  : constant String := Get_Name_String (Dot_Replacement_Id);
      Extension : constant String := Get_Name_String (Extension_Id);
      Dot       : constant Character := '.';

      Index : Positive;
   begin
      --  Since we will need to use the Namet buffer to apply the casing,
      --  we use it for the entire conversion. This avoids maintaining
      --  a local string that we will copy into the Namet Name Buffer anyway.

      --  Copy the Unit_Name, changing the Dot character into Dot_Repl...
      Index := 1;
      for C in Unit_Name'Range loop
         if Unit_Name (C) = Dot then
            Name_Buffer (Index .. Index + Dot_Repl'Length - 1) := Dot_Repl;
            Index := Index + Dot_Repl'Length;
         else
            Namet.Name_Buffer (Index) := Unit_Name (C);
            Index := Index + 1;
         end if;
      end loop;

      --  Add the extension
      Namet.Name_Buffer (Index .. Index + Extension'Length - 1) := Extension;
      Index := Index + Extension'Length;

      --  Apply the casing
      Namet.Name_Len := Index - 1;
      Set_Casing (Casing);

      return Namet.Name_Find;
   end Get_Filename;

   -----------------------
   -- Get_Spec_Filename --
   -----------------------

   function Get_Spec_Filename (U : Prj.Com.Unit_Id) return File_Name_Type is
   begin
      if U = Prj.Com.No_Unit then
         return No_File;
      end if;
      return Units.Table (U).File_Names (Specification).Name;
   end Get_Spec_Filename;

   -----------------------
   -- Get_Body_Filename --
   -----------------------

   function Get_Body_Filename (U : Prj.Com.Unit_Id) return File_Name_Type is
   begin
      if U = Prj.Com.No_Unit then
         return No_File;
      end if;
      return Units.Table (U).File_Names (Body_Part).Name;
   end Get_Body_Filename;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (Id : Array_Element_Id) return Unit_Name_Type is
   begin
      return Prj.Array_Elements.Table (Id).Index;
   end Get_Unit_Name;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Id : Array_Element_Id) return File_Name_Type is
   begin
      String_To_Name_Buffer (Prj.Array_Elements.Table (Id).Value.Value);
      return Namet.Name_Find;
   end Get_Filename;

   ----------------------
   -- Search_Unit_Name --
   ----------------------

   function Search_Unit_Name
     (Exception_List : Prj.Array_Element_Id;
      Unit_Name      : Unit_Name_Type)
      return Prj.Array_Element_Id
   is
      Id   : Array_Element_Id := Exception_List;
      Elmt : Array_Element;
   begin
      while Id /= No_Array_Element loop
         Elmt := Array_Elements.Table (Id);
         exit when Elmt.Index = Unit_Name;
         Id := Elmt.Next;
      end loop;
      return Id;
   end Search_Unit_Name;

   ---------------------
   -- Search_Filename --
   ---------------------

   function Search_Filename
     (Exception_List : Prj.Array_Element_Id;
      Filename       : File_Name_Type)
      return Prj.Array_Element_Id
   is
      Id   : Array_Element_Id := Exception_List;
      Elmt : Array_Element;
   begin
      while Id /= No_Array_Element loop
         Elmt := Array_Elements.Table (Id);
         String_To_Name_Buffer (Elmt.Value.Value);
         exit when Namet.Name_Find = Filename;
         Id := Elmt.Next;
      end loop;
      return Id;
   end Search_Filename;

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename
     (Unit_Name : Unit_Name_Type; Project : Prj.Project_Id)
      return String
   is
      Part        : Unit_Part;
      Short_Uname : Name_Id;

      procedure Check_Project
        (View : Project_Id; Result : in out Name_Id);
      --  Check into the specific View

      -------------------
      -- Check_Project --
      -------------------

      procedure Check_Project
        (View : Project_Id; Result : in out Name_Id)
      is
         Candidate : Name_Id;
         Except_Id : Array_Element_Id;
      begin
         if Result /= No_Name then
            return;
         end if;

         case Part is
            when Unit_Body =>
               Except_Id := Search_Unit_Name
                 (Projects.Table (View).Naming.Bodies, Short_Uname);
            when Unit_Spec =>
               Except_Id := Search_Unit_Name
                 (Projects.Table (View).Naming.Specifications, Short_Uname);
            when others =>
               null;
         end case;

         --  If found, then return the associated filename
         if Except_Id /= No_Array_Element then
            Candidate := Get_Filename (Except_Id);

         else
            --  If not found, then return the filename computed using the
            --  regular naming scheme. As a safety precaution, put back the
            --  stripped unit name in the name buffer, because we need it and
            --  it might have been overwritten during previous calls.
            case Part is
               when Unit_Spec =>
                  Candidate := Get_Spec_Filename
                    (Short_Uname, Projects.Table (View).Naming);
               when Unit_Body =>
                  Candidate := Get_Body_Filename
                    (Short_Uname, Projects.Table (View).Naming);
               when others =>
                  --  Impossible or would be an error.
                  Candidate := No_Name;
            end case;
         end if;

         if Candidate /= No_Name
           and then Is_Direct_Source (Get_Name_String (Candidate), View)
         then
            Result := Candidate;
         end if;
      end Check_Project;

      procedure For_All_Projects is new For_Every_Project_Imported
        (Name_Id, Check_Project);

      Part_Marker_Len : constant := 2; --  It is either '%s' or '%b'
      Result : Name_Id := No_Name;
      D, S : Name_Id;

   begin
      --  ??? This should be implemented with mapping files instead. See
      --  ??? fname.ad[bs] in the GNAT sources

      Namet.Get_Name_String (Unit_Name);

      --  Check that the '%' marker is there
      if Namet.Name_Len <= Part_Marker_Len + 1
        or else Namet.Name_Buffer (Namet.Name_Len - 1) /= '%'
      then
         return "";
      end if;

      --  Compute the Unit_Part, strip the part marker from the Unit_Name
      --  in the Name_Buffer, and search the unit name in the associated
      --  exception list
      case Namet.Name_Buffer (Namet.Name_Len) is
         when 'b'    => Part := Unit_Body;
         when 's'    => Part := Unit_Spec;
         when others => return "";  --  Incorrect unit name
      end case;

      Namet.Name_Len := Namet.Name_Len - Part_Marker_Len;
      Short_Uname := Namet.Name_Find;

      For_All_Projects (Project, Result);

      if Result = No_Name then
         --  Special handling for the runtime files
         --  ??? Could be simplified if we have direct access to the default
         --  ??? naming scheme.

         Name_Len := 1;
         Name_Buffer (1 .. Name_Len) := "-";
         D := Name_Find;

         Name_Len := 4;

         if Part = Unit_Body then
            Name_Buffer (1 .. Name_Len) := ".adb";
         else
            Name_Buffer (1 .. Name_Len) := ".ads";
         end if;

         S := Name_Find;
         return Get_Name_String
           (Get_Filename (Short_Uname, D, All_Lower_Case, S));
      else
         return Get_Name_String (Result);
      end if;
   end Get_Source_Filename;

   -----------------------
   -- Get_Spec_Filename --
   -----------------------

   function Get_Spec_Filename
     (Unit_Name : Unit_Name_Type;
      Naming    : Prj.Naming_Data)
      return File_Name_Type is
   begin
      --  ??? This currently assumes we are using Ada
      return
        Get_Filename
          (Unit_Name, Naming.Dot_Replacement,
           Naming.Casing,
           Value_Of (Snames.Name_Ada, Naming.Specification_Suffix));
   end Get_Spec_Filename;

   -----------------------
   -- Get_Body_Filename --
   -----------------------

   function Get_Body_Filename
     (Unit_Name : Unit_Name_Type;
      Naming    : Prj.Naming_Data)
      return File_Name_Type is
   begin
      --  ??? This currently assumes we are using Ada
      return
        Get_Filename
          (Unit_Name, Naming.Dot_Replacement,
           Naming.Casing,
           Value_Of (Snames.Name_Ada, Naming.Implementation_Suffix));
   end Get_Body_Filename;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name
     (Filename : File_Name_Type;
      Project  : Prj.Project_Id)
      return Name_Id
   is
      Fname    : constant String := Get_Name_String (Filename);
      Dot_Repl : constant String := Get_Name_String
        (Projects.Table (Project).Naming.Dot_Replacement);
      Dot      : constant Character := '.';

      Index       : Integer;
      Namet_Index : Natural;
      Last        : Integer;
   begin
      Last := Delete_File_Suffix (Fname, Project);

      --  According to the naming scheme, this file is neither a body, nor a
      --  spec. So we can not extract a Unit Name from it.
      if Last = Fname'Last then
         return No_Name;
      end if;

      Index := Fname'First;
      Namet_Index := Namet.Name_Buffer'First;
      while Index <= Last loop
         if Index + Dot_Repl'Length - 1 <= Last
           and then Fname (Index .. Index + Dot_Repl'Length - 1) = Dot_Repl
         then
            Name_Buffer (Namet_Index) := Dot;
            Index := Index + Dot_Repl'Length;
            Namet_Index := Namet_Index + 1;
         else
            Name_Buffer (Namet_Index) := To_Lower (Fname (Index));
            Index := Index + 1;
            Namet_Index := Namet_Index + 1;
         end if;
      end loop;

      Namet.Name_Len := Namet_Index - 1;
      return Namet.Name_Find;
   end Get_Unit_Name;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Short_File_Name, Path, Predefined_Path : String) return String
   is
      File : String_Access;
   begin
      --  First, try on the project object path
      File := Locate_Regular_File (Short_File_Name, Path);

      if File /= null then
         declare
            Full_Path : constant String := File.all;
         begin
            Free (File);
            return Full_Path;
         end;
      end if;

      --  Fallback, try on the Predefined_Object_Path if set
      if Predefined_Path /= "" then
         File := Locate_Regular_File (Short_File_Name, Predefined_Path);
         if File /= null then
            declare
               Full_Path : constant String := File.all;
            begin
               Free (File);
               return Full_Path;
            end;
         end if;
      end if;

      --  Object file not found anywhere, return the empty string
      return "";
   end Find_File;

end Src_Info.Prj_Utils;
