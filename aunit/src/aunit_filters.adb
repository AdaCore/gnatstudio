-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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

with String_Utils;              use String_Utils;
with Ada.Text_IO;               use Ada.Text_IO;

package body Aunit_Filters is

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Ada;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access)
   is
   begin
      if File'Length >= 4
        and then (File (File'Last - 3 .. File'Last) = ".ads"
                  or else File (File'Last - 3 .. File'Last) = ".adb")
      then
         State := Normal;
         declare
            Found     : Boolean := False;
            File_T    : File_Type;
            Index_End : Integer;
            Line      : String (1 .. 256);
            Line_Last : Integer;
         begin
            Ada.Text_IO.Open (File_T,
                              In_File,
                              Dir & File);
            while not Found loop
               Get_Line (File_T, Line, Line_Last);
               Index_End := 1;
               Skip_To_String (Line, Index_End, "--");
               if Index_End > Line_Last - 2 then
                  Index_End := 1;
                  Skip_To_String (Line, Index_End, " is");
                  if Index_End < Line_Last - 1 then
                     Text :=
                       new String' (Line (1 .. Index_End - 1));
                     Found := True;
                  end if;
               end if;
            end loop;
            Close (File_T);
         exception
            when Name_Error =>
               null;
            when Use_Error =>
               null;
            when End_Error =>
               Close (File_T);
         end;
      elsif File'Length >= 4
        and then File (File'Last - 3 .. File'Last) = ".gpr"
      then
         State := Highlighted;
         Text := new String'("PROJECT");
      else
         State := Insensitive;
         Text := new String'("");
      end if;

      Pixmap := Gdk.Pixmap.Null_Pixmap;
      Mask   := Gdk.Bitmap.Null_Bitmap;

      if File'Length >= 4 then
         if File (File'Last - 3 .. File'Last) = ".adb" then
            Pixmap := Filter.Body_Pixmap;
            Mask := Filter.Body_Bitmap;
         elsif File (File'Last - 3 .. File'Last) = ".ads" then
            Pixmap := Filter.Spec_Pixmap;
            Mask := Filter.Spec_Bitmap;
         end if;
      end if;
   end Use_File_Filter;

end Aunit_Filters;
