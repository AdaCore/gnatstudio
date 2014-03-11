------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Projects;                         use Projects;

with Basic_Types;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Project;               use GPS.Kernel.Project;
with Xref;                             use Xref;

package body Docgen2.Utils is

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Loc    : General_Location;
      Msg    : String)
   is
      Line : constant String := Natural'Image (Loc.Line);
      Col  : constant String := Natural'Image (Integer (Loc.Column));
      Err  : constant String :=
               Loc.File.Display_Base_Name & ":" &
               Line (Line'First + 1 .. Line'Last) & ":" &
               Col (Col'First + 1 .. Col'Last) & ": " & Msg;
   begin
      Kernel.Insert (Err, Mode => GPS.Kernel.Error);
      Parse_File_Locations (Kernel, Err, "Documentation");
   end Warning;

   ------------------
   -- Is_Spec_File --
   ------------------

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      --  Likely the same unit_part in all projects, so use the first one
      return Get_Registry
        (Kernel).Tree.Info_Set (File).First_Element.Unit_Part = Unit_Spec;
   end Is_Spec_File;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Construct : String;
      Loc       : General_Location;
      Lang      : Language_Access) return General_Entity
   is
      Entity        : General_Entity;
      Current_Loc   : General_Location := Loc;

   begin
      Entity := Kernel.Databases.Get_Entity
        (Name => Construct,
         Loc  => Loc);

      if Entity = No_General_Entity and then Get_Name (Lang) = "Ada" then
         for J in reverse Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Integer (Loc.Column) + J + 1 - Construct'First);

               Entity := Kernel.Databases.Get_Entity
                 (Name => Construct (J + 1 .. Construct'Last),
                  Loc  => Current_Loc);
               exit;
            end if;
         end loop;
      end if;

      return Entity;
   end Get_Entity;

   ----------------------------
   -- Get_Declaration_Entity --
   ----------------------------

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : General_Location;
      Db        : access Xref.General_Xref_Database_Record'Class;
      Lang      : Language_Access) return General_Entity
   is
      Entity        : General_Entity;
      Current_Loc   : General_Location := Loc;

   begin
      Entity := Db.Get_Entity (Construct, Loc);

      if Entity = No_General_Entity and then Get_Name (Lang) = "Ada" then
         for J in Construct'Range loop
            --  ??? Ada Specific ... should use language service
            --  Need to define it !
            if Construct (J) = '.' then
               Current_Loc.Column :=
                 Basic_Types.Visible_Column_Type
                   (Integer (Loc.Column) + J + 1 - Construct'First);

               Entity := Db.Get_Entity
                 (Construct (J + 1 .. Construct'Last), Current_Loc);
               exit when Entity /= No_General_Entity;
            end if;
         end loop;
      end if;

      return Entity;
   end Get_Declaration_Entity;

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

end Docgen2.Utils;
