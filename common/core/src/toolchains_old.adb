------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with System;                   use System;
with Ada.Unchecked_Conversion;
with GNATCOLL.VFS_Utils;       use GNATCOLL.VFS_Utils;

package body Toolchains_Old is

   type Toolchains_Property_Record is record
      Active        : Boolean      := False;
      Tools_Path    : Virtual_File := No_File;
      Compiler_Path : Virtual_File := No_File;
   end record;

   Property : Toolchains_Property_Record;

   function Internal_Locate_Exec
     (Exec_Name  : Filesystem_String;
      Extra_Path : Virtual_File) return Virtual_File;
   --  Try to retrieve Exec_Name from Extra_Path. If not found, try to retrieve
   --  it from the regular path

   -------------------------------
   -- Set_Toolchains_Properties --
   -------------------------------

   procedure Set_Toolchains_Properties
     (Active               : Boolean;
      Tool_Search_Path     : Virtual_File;
      Compiler_Search_Path : Virtual_File) is
   begin
      Property.Active := Active;

      Property.Tools_Path    := Tool_Search_Path;
      Property.Compiler_Path := Compiler_Search_Path;
   end Set_Toolchains_Properties;

   --------------------------
   -- Is_Toolchains_Active --
   --------------------------

   function Is_Toolchains_Active return Boolean is
   begin
      return Property.Active;
   end Is_Toolchains_Active;

   --------------------------
   -- Get_Tool_Search_Path --
   --------------------------

   function Get_Tool_Search_Path return Virtual_File is
   begin
      if not Property.Active then
         return No_File;
      end if;

      return Property.Tools_Path;
   end Get_Tool_Search_Path;

   ------------------------------
   -- Get_Compiler_Search_Path --
   ------------------------------

   function Get_Compiler_Search_Path return Virtual_File is
   begin
      if not Property.Active then
         return No_File;
      end if;

      return Property.Compiler_Path;
   end Get_Compiler_Search_Path;

   -----------------
   -- Locate_Exec --
   -----------------

   function Locate_Exec
     (Exec_Name : Filesystem_String;
      Path      : File_Array) return Virtual_File
   is
      function Internal (C_Exec, C_Path : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "__gnat_locate_exec");

      procedure Free (Ptr : System.Address);
      pragma Import (C, Free, "free");

      function Strlen (S : System.Address) return Integer;
      pragma Import (C, Strlen, "strlen");

      C_Exec_Name : String := +Exec_Name & ASCII.NUL;
      C_Path      : String := +To_Path (Path) & ASCII.NUL;
      C_Ret       : System.Address;
      C_Ret_Len   : Integer;
      Result      : Filesystem_String_Access;

   begin
      C_Ret := Internal (C_Exec_Name'Address, C_Path'Address);

      if C_Ret = Null_Address then
         C_Ret_Len := 0;
      else
         C_Ret_Len := Strlen (C_Ret);
      end if;

      if C_Ret_Len = 0 then
         return No_File;
      else
         declare
            subtype Path_String is String (1 .. C_Ret_Len);
            type    Path_String_Access is access Path_String;

            function Address_To_Access is new Ada.Unchecked_Conversion
              (Source => Address, Target => Path_String_Access);

            Path_Access : constant Path_String_Access :=
                            Address_To_Access (C_Ret);
         begin
            Result := new Filesystem_String (1 .. C_Ret_Len);

            for J in 1 .. C_Ret_Len loop
               Result (J) := Path_Access (J);
            end loop;
         end;

         Free (C_Ret);

         --  Always return an absolute path name

         declare
            Absolute_Path : constant Filesystem_String :=
                              Normalize_Pathname (Result.all);
         begin
            Free (Result);
            return Create (Absolute_Path);
         end;
      end if;
   end Locate_Exec;

   --------------------------
   -- Internal_Locate_Exec --
   --------------------------

   function Internal_Locate_Exec
     (Exec_Name  : Filesystem_String;
      Extra_Path : Virtual_File) return Virtual_File
   is
      Ret : Virtual_File;
   begin
      Ret := Locate_Exec (Exec_Name, (1 => Extra_Path));

      if Ret = No_File then
         return Locate_On_Path (Exec_Name);
      else
         return Ret;
      end if;
   end Internal_Locate_Exec;

   ----------------------------
   -- Locate_Tool_Executable --
   ----------------------------

   function Locate_Tool_Executable
     (Exec_Name : Filesystem_String) return Virtual_File
   is
   begin
      if not Property.Active or else Property.Tools_Path = No_File then
         return Locate_On_Path (Exec_Name);
      else
         return Internal_Locate_Exec (Exec_Name, Property.Tools_Path);
      end if;
   end Locate_Tool_Executable;

   --------------------------------
   -- Locate_Compiler_Executable --
   --------------------------------

   function Locate_Compiler_Executable
     (Exec_Name : Filesystem_String) return Virtual_File
   is
   begin
      if not Property.Active or else Property.Compiler_Path = No_File then
         return Locate_On_Path (Exec_Name);
      else
         return Internal_Locate_Exec (Exec_Name, Property.Compiler_Path);
      end if;
   end Locate_Compiler_Executable;

end Toolchains_Old;
