------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

package body Docgen3.Utils is

   ------------
   -- Filter --
   ------------

   function Filter (S : String) return String is
      F : Natural := S'First;
      L : Natural := S'Last;
   begin
      while F <= S'Last and then S (F) = ASCII.LF loop
         F := F + 1;
      end loop;

      while L >= F and then S (L) = ASCII.LF loop
         L := L - 1;
      end loop;

      return S (F .. L);
   end Filter;

   --------------------
   -- Get_Short_Name --
   --------------------

   function Get_Short_Name (Ada_Expanded_Name : String) return String is
      Index : Integer := Ada_Expanded_Name'Last;
   begin
      while Index > Ada_Expanded_Name'First
        and then Ada_Expanded_Name (Index) /= '.'
      loop
         Index := Index - 1;
      end loop;

      if Ada_Expanded_Name (Index) = '.' then
         return Ada_Expanded_Name (Index + 1 .. Ada_Expanded_Name'Last);
      else
         return Ada_Expanded_Name;
      end if;
   end Get_Short_Name;

   -----------
   -- Image --
   -----------

   function Image
     (Db     : General_Xref_Database;
      Entity : General_Entity) return String
   is
      Decl : constant General_Entity_Declaration :=
               Get_Declaration (Db, Entity);
   begin
      return Image (Decl.Loc);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Loc           : General_Location;
      With_Filename : Boolean := True) return String
   is
   begin
      if Loc = No_Location
        or else Loc.File = No_File
      then
         return "";
      end if;

      if not With_Filename then
         return
           To_String (Loc.Line) & ":" &
           To_String (Integer (Loc.Column));
      else
         declare
            F_Name : constant String := +Loc.File.Base_Name;
         begin
            if F_Name = "<case_insensitive_predefined>"
              or else F_Name = "<case_sensitive_predefined>"
            then
               return "standard";
            end if;

            return F_Name & ":" &
              To_String (Loc.Line) & ":" &
              To_String (Integer (Loc.Column));
         end;
      end if;
   end Image;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Kernel.Registry.Tree.Info (File).Unit_Part = Unit_Spec;
   end Is_Spec_File;

   -----------------
   -- Spaces_Only --
   -----------------

   function Spaces_Only (Text : String) return Boolean is
   begin
      if Text'Length = 0 then
         return False;

      else
         for J in Text'Range loop
            if Text (J) /= ' ' then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Spaces_Only;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Integer) return String is
      Str : constant String := Integer'Image (N);
   begin
      if Str (Str'First) = ' ' then
         return Str (Str'First + 1 .. Str'Last);
      else
         return Str;
      end if;
   end To_String;

end Docgen3.Utils;
