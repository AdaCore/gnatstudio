-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2007, AdaCore             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GPR_Creation;              use GPR_Creation;
with Projects.Registry;         use Projects, Projects.Registry;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Traces;                    use Traces;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Mmap;                 use GNAT.Mmap;

package body Adp_Converter is

   Me : constant Debug_Handle := Create ("Adp_wizard");

   function Get_Attribute_Value
     (Buffer : String_Access; Attr : String) return String;
   --  Return the first value found in Buffer for Attr

   type Source_Line is record
      First, Last : Integer := 1;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   function First_Line_Of (Buffer : String) return Source_Line;
   --  Return a pointer to the beginning of Buffer.

   function At_End (Line : Source_Line) return Boolean;
   --  Return True if Line is past the end of the buffer

   procedure Move_To_Next_Line
     (Buffer        : String;
      Line          : in out Source_Line;
      Starting_With : String := "");
   --  Move Iter to the next line starting with Starting_With.
   --  On exit, Line.Start points to the first significant character of
   --  the value, not at the beginning of the line.
   --  Last is left on the last character of the line, before ASCII.LF

   function Parse_Main_Units
     (Buffer : String_Access) return String_List_Access;
   --  Get the list of main units for the project

   function Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String) return String_List_Access;
   --  Get all the source directories in a .adp file

   function Parse_Object_Dirs
     (Buffer    : String_Access;
      Build_Dir : String) return String_List_Access;
   --  Get all the object directories in a .adp file

   ----------------------
   -- Convert_Adp_File --
   ----------------------

   procedure Convert_Adp_File
     (Adp_Filename   : String;
      Registry       : Projects.Registry.Project_Registry'Class;
      Project        : in out Project_Type;
      Spec_Extension : String;
      Body_Extension : String)
   is
      Buffer : String_Access := Read_Whole_File (Adp_Filename);
      Source_Dirs, Object_Dirs, Main_Units : String_List_Access;
   begin
      if Buffer = null then
         Trace (Me, "No such file " & Adp_Filename);
         return;
      end if;

      declare
         Build_Dir : constant String :=
           Normalize_Pathname
             (Get_Attribute_Value (Buffer, "build_dir"),
              Dir_Name (Adp_Filename));
      begin
         if Build_Dir = "" then
            Trace (Me, "No build dir specified in .adp file");
            return;
         end if;

         Source_Dirs := Parse_Source_Dirs (Buffer, Build_Dir);
         Object_Dirs := Parse_Object_Dirs (Buffer, Build_Dir);
         Main_Units  := Parse_Main_Units  (Buffer);

         Create_Gpr_Files
           (Registry          => Registry,
            Root_Project      => Project,
            Source_Dirs       => Source_Dirs.all,
            Object_Dirs       => Object_Dirs.all,
            Spec_Extension    => Spec_Extension,
            Body_Extension    => Body_Extension,
            Main_Units        => Main_Units,
            Builder_Switches  => Get_Attribute_Value (Buffer, "gnatmake_opt"),
            Compiler_Switches => Get_Attribute_Value (Buffer, "comp_opt"),
            Binder_Switches   => Get_Attribute_Value (Buffer, "bind_opt"),
            Linker_Switches   => Get_Attribute_Value (Buffer, "link_opt"),
            Cross_Prefix      => Get_Attribute_Value (Buffer, "cross_prefix"));

         Free (Main_Units);
         Free (Source_Dirs);
         Free (Object_Dirs);
         Free (Buffer);
      end;
   end Convert_Adp_File;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   function Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String) return String_List_Access
   is
      List, List2 : String_List_Access;
      Adp_Prefix  : constant String := "src_dir";
      Line        : Source_Line := First_Line_Of (Buffer.all);
   begin
      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         if List = null then
            List := new String_List (1 .. 1);
         else
            List2 := List;
            List := new String_List (List2'First .. List2'Last + 1);
            List (List2'Range) := List2.all;
            Unchecked_Free (List2);
         end if;

         List (List'Last) := new String'
           (Normalize_Pathname
              (Buffer (Line.First .. Line.Last), Build_Dir));
      end loop;
      return List;
   end Parse_Source_Dirs;

   ----------------------
   -- Parse_Main_Units --
   ----------------------

   function Parse_Main_Units
     (Buffer : String_Access) return String_List_Access
   is
      List, List2 : String_List_Access;
      Line : Source_Line := First_Line_Of (Buffer.all);
   begin
      loop
         Move_To_Next_Line (Buffer.all, Line, "main_unit");
         exit when At_End (Line);

         if List = null then
            List := new String_List (1 .. 1);
         else
            List2 := List;
            List := new String_List (List2'First .. List2'Last + 1);
            List (List2'Range) := List2.all;
            Unchecked_Free (List2);
         end if;

         List (List'Last) := new String'(Buffer (Line.First .. Line.Last));
      end loop;
      return List;
   end Parse_Main_Units;

   -----------------------
   -- Parse_Object_Dirs --
   -----------------------

   function Parse_Object_Dirs
     (Buffer    : String_Access;
      Build_Dir : String) return String_List_Access
   is
      Adp_Prefix  : constant String := "obj_dir";
      List, List2 : String_List_Access;
      Line        : Source_Line := First_Line_Of (Buffer.all);
      Found       : Boolean;
   begin
      List := new String_List (1 .. 1);
      List (1) := new String'(Build_Dir);

      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         declare
            Name : constant String := Normalize_Pathname
              (Buffer (Line.First .. Line.Last), Build_Dir);
         begin
            Found := False;
            for L in List'Range loop
               if List (L).all = Name then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               List2 := List;
               List := new String_List (List2'First .. List2'Last + 1);
               List (List2'Range) := List2.all;
               Unchecked_Free (List2);

               List (List'Last) := new String'(Name);
            end if;
         end;
      end loop;

      return List;
   end Parse_Object_Dirs;

   -------------------
   -- First_Line_Of --
   -------------------

   function First_Line_Of (Buffer : String) return Source_Line is
   begin
      return (First => Buffer'First, Last => Buffer'First - 1);
   end First_Line_Of;

   -----------------------
   -- Move_To_Next_Line --
   -----------------------

   procedure Move_To_Next_Line
     (Buffer        : String;
      Line          : in out Source_Line;
      Starting_With : String := "")
   is
      Prefix : constant String := Starting_With & '=';
   begin
      --  Move to next character, since Last was left on the first character
      --  before the ASCII.LF
      if Line.Last >= Buffer'First then
         Line.Last := Line.Last + 1;
      end if;

      loop
         Line.First := Line.Last + 1;
         exit when Line.First > Buffer'Last;

         Line.Last := Line.First;
         while Line.Last <= Buffer'Last
           and then Buffer (Line.Last) /= ASCII.LF
         loop
            Line.Last := Line.Last + 1;
         end loop;

         exit when Line.First + Starting_With'Length - 1 < Line.Last
           and then Prefix =
             Buffer (Line.First .. Line.First + Starting_With'Length);
      end loop;

      --  Point to first significant character of the value
      Line.First := Line.First + Starting_With'Length + 1;

      while Line.First <= Line.Last
        and then Buffer (Line.First) = ' '
      loop
         Line.First := Line.First + 1;
      end loop;

      if Line.Last <= Buffer'Last
        and then Buffer (Line.Last) = ASCII.LF
      then
         Line.Last := Line.Last - 1;
      end if;
   end Move_To_Next_Line;

   ------------
   -- At_End --
   ------------

   function At_End (Line : Source_Line) return Boolean is
   begin
      return Line.First > Line.Last;
   end At_End;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Buffer : String_Access; Attr : String) return String
   is
      Line : Source_Line := First_Line_Of (Buffer.all);
   begin
      Move_To_Next_Line (Buffer.all, Line, Attr);
      if At_End (Line) then
         return "";
      else
         return Buffer (Line.First .. Line.Last);
      end if;
   end Get_Attribute_Value;

end Adp_Converter;
