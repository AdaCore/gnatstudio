-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Basic_Mapper;              use Basic_Mapper;
with OS_Utils;                  use OS_Utils;
with String_Utils;              use String_Utils;

package body Log_Utils is

   --  The format for the mappings file is as follows :
   --
   --      File_1
   --      Log_1
   --      File_2
   --      Log_2
   --      File_3
   --      Log_3
   --
   --  and so on.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class) is
      Logs_Dir : constant String :=
        Normalize_Pathname (Get_Home_Dir (Kernel) & "/log_files");
      Mapping  : constant String :=
        Normalize_Pathname (Logs_Dir & "/mapping");
      Mapper   : File_Mapper_Access;
   begin
      if not Is_Directory (Logs_Dir) then
         Make_Dir (Logs_Dir);
      end if;

      if not Is_Regular_File (Mapping) then
         declare
            File : File_Descriptor;
         begin
            File :=
              Create_New_File (Mapping, Text);
            Close (File);
         end;
      end if;

      Load_Mapper (Mapper, Mapping);
      Set_Logs_Mapper (Kernel, Mapper);
   end Initialize;

   -----------------------
   -- Get_Log_From_File --
   -----------------------

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String
   is
      Mapper      : File_Mapper_Access := Get_Logs_Mapper (Kernel);
      Real_Name   : constant String := Normalize_Pathname (File_Name);
      Return_Name : constant String := Get_Other_Text (Mapper, Real_Name);
   begin
      --  ??? Right now, we save the mapping every time that we add
      --  an entry. This is a bit inefficient, we should save the mapping
      --  on disk only on exit.

      if Return_Name = "" then
         declare
            Logs_Dir : constant String :=
              Normalize_Pathname (Get_Home_Dir (Kernel) & "/log_files");
            File     : File_Descriptor;
            S : constant String := Logs_Dir
              & Directory_Separator
              & Base_Name (Real_Name)
              & "$log";
         begin
            if not Is_Regular_File
              (Logs_Dir & Directory_Separator & Base_Name (Real_Name) & "$log")
            then
               File := Create_New_File (S, Text);
               Close (File);
               Add_Entry (Mapper, Real_Name, S);
               Save_Mapper
                 (Mapper, Normalize_Pathname (Logs_Dir & "/mapping"));
               return S;

            else
               for J in Natural loop
                  declare
                     S : constant String := Logs_Dir
                       & Directory_Separator
                       & Base_Name (Real_Name)
                       & "$" & Image (J) & "$log";
                  begin
                     if not Is_Regular_File (S) then
                        File := Create_New_File (S, Text);
                        Close (File);
                        Add_Entry (Mapper, Real_Name, S);
                        Save_Mapper
                          (Mapper, Normalize_Pathname (Logs_Dir & "/mapping"));
                        return S;
                     end if;
                  end;
               end loop;

               return "";
            end if;
         end;
      else
         return Return_Name;
      end if;
   end Get_Log_From_File;

   -----------------------
   -- Get_File_From_Log --
   -----------------------

   function Get_File_From_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Log_Name : String) return String
   is
      Mapper : constant File_Mapper_Access := Get_Logs_Mapper (Kernel);
   begin
      return Get_Other_Text (Mapper, Normalize_Pathname (Log_Name));
   end Get_File_From_Log;

   -------------
   -- Get_Log --
   -------------

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String
   is
      R : String_Access;
   begin
      R := Read_File
        (Get_Log_From_File (Kernel, Normalize_Pathname (File_Name)));

      if R = null then
         return "";

      else
         declare
            S : constant String := R.all;
         begin
            Free (R);
            return S;
         end;
      end if;
   end Get_Log;

end Log_Utils;
