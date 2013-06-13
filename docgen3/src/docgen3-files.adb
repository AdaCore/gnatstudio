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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with GPS.Messages_Windows;        use GPS.Messages_Windows;

package body Docgen3.Files is

   --------------
   -- Filename --
   --------------

   function Filename (File : Virtual_File) return Filesystem_String is
      Ext  : constant Filesystem_String := File.File_Extension;
      Base : constant Filesystem_String := File.Base_Name;
      Name : constant Filesystem_String :=
               Base (Base'First .. Base'Last - Ext'Length);
   begin
      return Name;
   end Filename;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Virtual_File) return Boolean is
   begin
      return To_Lower (+Base_Name (Left)) < To_Lower (+Base_Name (Right));
   end Less_Than;

   --------------------
   -- Remove_Element --
   --------------------

   procedure Remove_Element
     (List   : in out Files_List.Vector;
      Cursor : in out Files_List.Cursor)
   is
      Prev : constant Files_List.Extended_Index :=
        Files_List.To_Index (Cursor);
   begin
      List.Delete (Cursor);
      Cursor := List.To_Cursor (Prev);
   end Remove_Element;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : access Unbounded_String)
   is
      Name   : Virtual_File;
      Output : Writable_File;

   begin
      if not Is_Directory (Directory) then
         Directory.Make_Dir;
      end if;

      Name   := Create_From_Dir (Directory, Filename);
      Output := Name.Write_File;

      if Output = Invalid_File then
         Context.Kernel.Messages_Window.Insert
           ("Could not create " & Name.Display_Full_Name,
            Mode => Error);
         return;
      end if;

      Write (Output, To_String (Text.all));
      Close (Output);
   end Write_To_File;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : String)
   is
      Name   : Virtual_File;
      Output : Writable_File;

   begin
      if not Is_Directory (Directory) then
         Directory.Make_Dir;
      end if;

      Name   := Create_From_Dir (Directory, Filename);
      Output := Name.Write_File;

      if Output = Invalid_File then
         Context.Kernel.Messages_Window.Insert
           ("Could not create " & Name.Display_Full_Name,
            Mode => Error);
         return;
      end if;

      Write (Output, Text);
      Close (Output);
   end Write_To_File;

end Docgen3.Files;
