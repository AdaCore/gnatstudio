-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

with Ada.Text_IO;
with Unchecked_Deallocation;

package body Basic_Mapper is

   use Double_String_Table.String_Hash_Table;

   ----------------
   -- False_Free --
   ----------------

   procedure False_Free (X : in out String_Access) is
      pragma Unreferenced (X);
   begin
      null;
   end False_Free;

   --------------------
   -- Get_Other_Text --
   --------------------

   function Get_Other_Text
     (Mapper : File_Mapper_Access;
      Text   : String) return String
   is
      Element : String_Access;
   begin
      if Mapper = null then
         return "";
      end if;

      Element := Get (Mapper.Table_1, Text);

      if Element = No_Element then
         Element := Get (Mapper.Table_2, Text);
      end if;

      if Element = No_Element then
         return "";
      else
         return Element.all;
      end if;
   end Get_Other_Text;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry
     (Mapper : in out File_Mapper_Access;
      Text   : String)
   is
      Other : constant String := Get_Other_Text (Mapper, Text);
   begin
      Remove (Mapper.Table_1, Text);
      Remove (Mapper.Table_2, Text);

      if Other /= "" then
         Remove (Mapper.Table_1, Other);
         Remove (Mapper.Table_2, Other);
      end if;
   end Remove_Entry;

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Mapper : in out File_Mapper_Access;
      Text_1 : String;
      Text_2 : String) is
   begin
      if Mapper = null then
         Mapper := new File_Mapper;
      end if;

      Set (Mapper.Table_1, Text_1, new String'(Text_2));
      Set (Mapper.Table_2, Text_2, new String'(Text_1));
   end Add_Entry;

   -----------------
   -- Save_Mapper --
   -----------------

   procedure Save_Mapper
     (Mapper    : File_Mapper_Access;
      File_Name : String)
   is
      File    : Ada.Text_IO.File_Type;
      Element : Iterator;
   begin
      if Mapper = null then
         return;
      end if;

      if not Is_Regular_File (File_Name) then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      else
         Ada.Text_IO.Open (File, Ada.Text_IO.Out_File, File_Name);
      end if;

      Get_First (Mapper.Table_2, Element);

      while Get_Element (Element) /= No_Element loop
         declare
            Text  : constant String := Get_Element (Element).all;
            Other : constant String := Get_Other_Text (Mapper, Text);
         begin
            if Text /= "" and then Other /= "" then
               Ada.Text_IO.Put_Line (File, Text);
               Ada.Text_IO.Put_Line (File, Other);
            end if;
         end;

         Get_Next (Mapper.Table_2, Element);
      end loop;

      Ada.Text_IO.Close (File);
   end Save_Mapper;

   -----------------
   -- Load_Mapper --
   -----------------

   procedure Load_Mapper
     (Mapper    : out File_Mapper_Access;
      File_Name : String)
   is
      File     : Ada.Text_IO.File_Type;
      Buffer_1 : String (1 .. 8192);
      Buffer_2 : String (1 .. 8192);
      Last_1   : Integer := 1;
      Last_2   : Integer := 1;

   begin
      if Mapper = null then
         Mapper := new File_Mapper;
      end if;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      while Last_2 >= 0
        and then Last_1 >= 0
        and then not Ada.Text_IO.End_Of_File (File)
      loop
         Ada.Text_IO.Get_Line (File, Buffer_1, Last_1);
         Ada.Text_IO.Get_Line (File, Buffer_2, Last_2);
         Add_Entry (Mapper,
                    Buffer_1 (1 .. Last_1),
                    Buffer_2 (1 .. Last_2));
      end loop;

      Ada.Text_IO.Close (File);
   end Load_Mapper;

   ----------
   -- Free --
   ----------

   procedure Free (Mapper : in out File_Mapper_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (File_Mapper, File_Mapper_Access);

   begin
      if Mapper /= null then
         Double_String_Table.String_Hash_Table.Reset (Mapper.Table_1);
         Double_String_Table.String_Hash_Table.Reset (Mapper.Table_2);
         Unchecked_Free (Mapper);
      end if;
   end Free;

end Basic_Mapper;
