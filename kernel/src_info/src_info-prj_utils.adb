-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Casing;                  use Casing;
with Namet;                   use Namet;
with Prj.Com;                 use Prj.Com;
with Stringt;                 use Stringt;
with Types;                   use Types;
with Snames;
with Prj.Env;                 use Prj.Env;
with Prj.Util;                use Prj.Util;
with Prj_API;                 use Prj_API;

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

   --------------------------
   -- Get_Source_Filenames --
   --------------------------

   --  procedure Get_Source_Filenames
   --     (Project       : Prj.Project_Id;
   --      Unit_Name     : String;
   --      Spec_Filename : out Types.Name_Id;
   --      Body_Filename : out Types.Name_Id)
   --  is
   --     Unit_Name_Id : Name_Id;
   --     Unit  : Prj.Com.Unit_Id;
   --  begin
   --     Name_Buffer (1 .. Unit_Name'Length) := Unit_Name;
   --     Name_Len := Unit_Name'Length;
   --     Unit_Name_Id := Name_Find;

   --     Unit  := Units_Htable.Get (Unit_Name_Id);
   --     if Unit = Prj.Com.No_Unit then
   --        Put_Line ("*** Warning: Failed to locate " & Unit_Name &
   --                  " project file tables.");
   --        Spec_Filename := No_Name;
   --        Body_Filename := No_Name;
   --        return;
   --     end if;

   --     Spec_Filename := Units.Table (Unit).File_Names (Specification).Name;
   --     Body_Filename := Units.Table (Unit).File_Names (Body_Part).Name;
   --  end Get_Source_Filenames;

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
     (Unit_Name : Unit_Name_Type;
      Naming    : Prj.Naming_Data)
      return String
   is
      Part_Marker_Len : constant := 2; --  It is either '%s' or '%b'
      Part            : Unit_Part;
      Except_Id       : Array_Element_Id;
   begin
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
         when 'b' =>
            Part := Unit_Body;
            Namet.Name_Len := Namet.Name_Len - Part_Marker_Len;
            Except_Id := Search_Unit_Name (Naming.Bodies, Namet.Name_Find);
         when 's' =>
            Part := Unit_Spec;
            Namet.Name_Len := Namet.Name_Len - Part_Marker_Len;
            Except_Id :=
              Search_Unit_Name (Naming.Specifications, Namet.Name_Find);
         when others =>
            --  Incorrect unit name
            return "";
      end case;

      --  If found, then return the associated filename
      if Except_Id /= No_Array_Element then
         return Get_Name_String (Get_Filename (Except_Id));
      end if;

      --  If not found, then return the filename computed using the regular
      --  naming scheme. As a safety precaution, put back the stripped unit
      --  name in the name buffer, because we need it and it might have been
      --  overwritten during previous calls.
      Namet.Get_Name_String (Unit_Name);
      Namet.Name_Len := Namet.Name_Len - Part_Marker_Len;

      case Part is
         when Unit_Spec =>
            return Get_Name_String
              (Get_Spec_Filename (Name_Find, Naming));
         when Unit_Body =>
            return Get_Name_String
              (Get_Body_Filename (Name_Find, Naming));
         when others =>
            --  Impossible or would be an error.
            return "";
      end case;
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

   ----------------------
   -- Find_Object_File --
   ----------------------

   function Find_Object_File
     (Project_View    : Prj.Project_Id;
      Short_File_Name : String;
      Object_Path     : String)
      return String
   is
      Path : String_Access;
   begin
      --  First, try on the project object path
      Path := Locate_Regular_File
        (Short_File_Name, Ada_Objects_Path (Project_View).all);

      if Path /= null then
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;

      --  Fallback, try on the Object_Path (only if Use_Object_Path is set)
      if Object_Path /= "" then
         Path := Locate_Regular_File (Short_File_Name, Object_Path);
         if Path /= null then
            declare
               Full_Path : constant String := Path.all;
            begin
               Free (Path);
               return Full_Path;
            end;
         end if;
      end if;

      --  Object file not found anywhere, return the empty string
      return "";
   end Find_Object_File;

end Src_Info.Prj_Utils;
