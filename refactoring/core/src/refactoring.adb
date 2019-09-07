------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with Basic_Types;            use Basic_Types;

package body Refactoring is

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (File   : Language.Tree.Database.Structured_File_Access;
      Line   : Integer;
      Column : Basic_Types.Visible_Column_Type) return Universal_Location
   is
   begin
      return Universal_Location'
        (File                      => File,
         Line                      => Line,
         Column                    => Column,
         Index_In_Line             => 0,
         Index_In_File             => 0,
         Is_Column_Computed        => True,
         Is_Index_In_File_Computed => False,
         Is_Index_In_Line_Computed => False);
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (File          : Language.Tree.Database.Structured_File_Access;
      Index_In_File : Basic_Types.String_Index_Type)
      return Universal_Location
   is
   begin
      return Universal_Location'
        (File                      => File,
         Line                      => 0,
         Column                    => 0,
         Index_In_Line             => 0,
         Index_In_File             => Index_In_File,
         Is_Column_Computed        => False,
         Is_Index_In_File_Computed => True,
         Is_Index_In_Line_Computed => False);
   end To_Location;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Location : access Universal_Location) return Structured_File_Access
   is
   begin
      return Location.File;
   end Get_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Location : access Universal_Location) return Integer is
   begin
      if not Location.Is_Column_Computed
        and then not Location.Is_Index_In_Line_Computed
      then
         To_Line_Column
           (File                 => Location.File,
            Absolute_Byte_Offset => Location.Index_In_File,
            Line                 => Location.Line,
            Column               => Location.Column);

         Location.Is_Column_Computed := True;
      end if;

      return Location.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Location : access Universal_Location)
      return Basic_Types.Visible_Column_Type
   is
   begin
      if not Location.Is_Column_Computed then
         if not Location.Is_Index_In_File_Computed then
            Location.Index_In_File :=
              Get_Offset_Of_Line (Location.File, Location.Line)
              + Location.Index_In_Line - 1;
            Location.Is_Index_In_File_Computed := True;
         end if;

         To_Line_Column
           (File                 => Location.File,
            Absolute_Byte_Offset => Location.Index_In_File,
            Line                 => Location.Line,
            Column               => Location.Column);

         Location.Is_Column_Computed := True;
      end if;

      return Location.Column;
   end Get_Column;

   -----------------------
   -- Get_Index_In_File --
   -----------------------

   function Get_Index_In_File
     (Location : access Universal_Location)
      return Basic_Types.String_Index_Type
   is
   begin
      if not Location.Is_Index_In_File_Computed then
         Location.Index_In_File := To_String_Index
           (File   => Location.File,
            Line   => Location.Line,
            Column => Location.Column);

         Location.Is_Index_In_File_Computed := True;
      end if;

      return Location.Index_In_File;
   end Get_Index_In_File;

   -----------------------
   -- Get_Index_In_Line --
   -----------------------

   function Get_Index_In_Line
     (Location : access Universal_Location)
      return Basic_Types.String_Index_Type
   is
   begin
      if not Location.Is_Index_In_Line_Computed then
         Location.Index_In_Line := To_Line_String_Index
           (Location.File, Get_Line (Location), Get_Column (Location));

         Location.Is_Index_In_Line_Computed := True;
      end if;

      return Location.Index_In_Line;
   end Get_Index_In_Line;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
     (Location : access Universal_Location;
      Column   : Visible_Column_Type)
   is
      Dummy : Visible_Column_Type;
      pragma Unreferenced (Dummy);
   begin
      if not Location.Is_Column_Computed then
         --  In case the column, and line, is not known, force the computation
         --  so that we're sure to have the line information when invalidating
         --  other data.

         Dummy := Get_Column (Location);
      end if;

      Location.Is_Index_In_File_Computed := False;
      Location.Is_Index_In_Line_Computed := False;

      Location.Column := Column;

      Location.Is_Column_Computed := True;
   end Set_Column;

   -----------------------
   -- Set_Index_In_Line --
   -----------------------

   procedure Set_Index_In_Line
     (Location : access Universal_Location;
      Index    : Basic_Types.String_Index_Type)
   is
      Dummy : String_Index_Type;
      pragma Unreferenced (Dummy);
   begin
      if not Location.Is_Index_In_Line_Computed
        and then not Location.Is_Column_Computed
      then
         --  In case the column, and line, is not known, force the computation
         --  so that we're sure to have the line information when invalidating
         --  other data.

         Dummy := Get_Index_In_Line (Location);
      end if;

      Location.Is_Index_In_File_Computed := False;
      Location.Is_Column_Computed := False;

      Location.Index_In_Line := Index;
      Location.Is_Index_In_Line_Computed := True;
   end Set_Index_In_Line;

   ---------------------
   -- Set_Line_Column --
   ---------------------

   procedure Set_Line_Column
     (Location : access Universal_Location;
      Line     : Integer;
      Column   : Basic_Types.Visible_Column_Type) is
   begin
      Location.Is_Index_In_File_Computed := False;
      Location.Is_Index_In_Line_Computed := False;

      Location.Line := Line;
      Location.Column := Column;

      Location.Is_Column_Computed := True;
   end Set_Line_Column;

   -----------------------
   -- Set_Index_In_File --
   -----------------------

   procedure Set_Index_In_File
     (Location : access Universal_Location;
      Index    : Basic_Types.String_Index_Type)
   is
   begin
      Location.Index_In_File := Index;
      Location.Is_Index_In_File_Computed := True;

      Location.Is_Column_Computed := False;
      Location.Is_Index_In_Line_Computed := False;
   end Set_Index_In_File;

end Refactoring;
