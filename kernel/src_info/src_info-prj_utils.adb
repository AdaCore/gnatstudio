-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is  free software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Namet;                   use Namet;
with Types;                   use Types;
with Projects;                use Projects;

package body Src_Info.Prj_Utils is

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename
     (Unit_Name : Unit_Name_Type; Project : Project_Type)
      return String
   is
      N : constant String := Get_String (Unit_Name);
      --  Need to do a copy, since Name_Buffer is modified afterward
   begin
      return Get_Source_Filename (N, Project);
   end Get_Source_Filename;

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename
     (Unit_Name : String;
      Project   : Project_Type) return String
   is
      Part_Marker_Len : constant := 2; --  It is either '%s' or '%b'
      Part        : Unit_Part;
      Iter : Imported_Project_Iterator := Start (Project, Recursive => True);
   begin
      --  Check that the '%' marker is there
      if Unit_Name'Length < Part_Marker_Len + 1
        or else Unit_Name (Unit_Name'Last - 1) /= '%'
      then
         return "";
      end if;

      --  Compute the Unit_Part, strip the part marker from the Unit_Name
      --  in the Name_Buffer, and search the unit name in the associated
      --  exception list
      case Unit_Name (Unit_Name'Last) is
         when 'b'    => Part := Unit_Body;
         when 's'    => Part := Unit_Spec;
         when others => return "";  --  Incorrect unit name
      end case;

      while Current (Iter) /= No_Project loop
         declare
            N : constant String := Get_Filename_From_Unit
              (Current (Iter),
               Unit_Name (1 .. Unit_Name'Last - Part_Marker_Len),
               Part);
         begin
            if N /= "" then
               return N;
            end if;
         end;
         Next (Iter);
      end loop;

      --  We end up here for the runtime files, so we just try to append the
      --  standard GNAT extensions. The name will be krunched appropriately by
      --  Process_With anyway

      return Get_Filename_From_Unit
        (No_Project, Unit_Name (1 .. Unit_Name'Last - Part_Marker_Len), Part);
   end Get_Source_Filename;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name
     (Filename : File_Name_Type;
      Project  : Project_Type)
      return Name_Id
   is
      Fname    : constant String := Get_String (Filename);
      Dot_Repl : constant String := Get_Attribute_Value
        (Project, Dot_Replacement_Attribute, Default => "-");
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

end Src_Info.Prj_Utils;
