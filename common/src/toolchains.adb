-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with System;                   use System;
with Ada.Unchecked_Conversion;

package body Toolchains is

   type Toolchains_Property_Record is record
      Active        : Boolean       := False;
      Tools_Path    : String_Access := null;
      Compiler_Path : String_Access := null;
   end record;

   Property : Toolchains_Property_Record;

   function Internal_Locate_Exec
     (Exec_Name : String; Extra_Path : String) return String_Access;
   --  Try to retrieve Exec_Name from Extra_Path. If not found, try to retrieve
   --  it from the regular path

   -------------------------------
   -- Set_Toolchains_Properties --
   -------------------------------

   procedure Set_Toolchains_Properties
     (Active               : Boolean;
      Tool_Search_Path     : String;
      Compiler_Search_Path : String) is
   begin
      Free (Property.Tools_Path);
      Free (Property.Compiler_Path);

      Property.Active := Active;

      if Tool_Search_Path /= "" then
         Property.Tools_Path := new String'(Tool_Search_Path);
      else
         Property.Tools_Path := null;
      end if;

      if Compiler_Search_Path /= "" then
         Property.Compiler_Path := new String'(Compiler_Search_Path);
      else
         Property.Compiler_Path := null;
      end if;
   end Set_Toolchains_Properties;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Toolchains_Active return Boolean is
   begin
      return Property.Active;
   end Is_Toolchains_Active;

   --------------------------
   -- Get_Tool_Search_Path --
   --------------------------

   function Get_Tool_Search_Path return String is
   begin
      if not Property.Active or else Property.Tools_Path = null then
         return "";
      end if;

      return Property.Tools_Path.all;
   end Get_Tool_Search_Path;

   ------------------------------
   -- Get_Compiler_Search_Path --
   ------------------------------

   function Get_Compiler_Search_Path return String is
   begin
      if not Property.Active or else Property.Compiler_Path = null then
         return "";
      end if;

      return Property.Compiler_Path.all;
   end Get_Compiler_Search_Path;

   -----------------
   -- Locate_Exec --
   -----------------

   function Locate_Exec
     (Exec_Name : String; Path : String) return String_Access
   is
      function Internal (C_Exec, C_Path : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "__gnat_locate_exec");

      procedure Free (Ptr : System.Address);
      pragma Import (C, Free, "free");

      function Strlen (S : System.Address) return Integer;
      pragma Import (C, Strlen, "strlen");

      C_Exec_Name : String := Exec_Name & ASCII.NUL;
      C_Path      : String := Path & ASCII.NUL;
      C_Ret       : System.Address;
      C_Ret_Len   : Integer;
      Result      : String_Access;

   begin
      C_Ret := Internal (C_Exec_Name'Address, C_Path'Address);

      if C_Ret = Null_Address then
         C_Ret_Len := 0;
      else
         C_Ret_Len := Strlen (C_Ret);
      end if;

      if C_Ret_Len = 0 then
         return null;
      else
         declare
            subtype Path_String is String (1 .. C_Ret_Len);
            type    Path_String_Access is access Path_String;

            function Address_To_Access is new Ada.Unchecked_Conversion
              (Source => Address, Target => Path_String_Access);

            Path_Access : constant Path_String_Access :=
                            Address_To_Access (C_Ret);
         begin
            Result := new String (1 .. C_Ret_Len);

            for J in 1 .. C_Ret_Len loop
               Result (J) := Path_Access (J);
            end loop;
         end;

         Free (C_Ret);

         --  Always return an absolute path name

         if not Is_Absolute_Path (Result.all) then
            declare
               Absolute_Path : constant String :=
                                 Normalize_Pathname (Result.all);
            begin
               Free (Result);
               Result := new String'(Absolute_Path);
            end;
         end if;

         return Result;
      end if;
   end Locate_Exec;

   --------------------------
   -- Internal_Locate_Exec --
   --------------------------

   function Internal_Locate_Exec
     (Exec_Name : String; Extra_Path : String) return String_Access
   is
      Ret : String_Access;
   begin
      Ret := Locate_Exec (Exec_Name, Extra_Path);

      if Ret = null then
         return System.OS_Lib.Locate_Exec_On_Path (Exec_Name);
      else
         return Ret;
      end if;
   end Internal_Locate_Exec;

   ----------------------------
   -- Locate_Tool_Executable --
   ----------------------------

   function Locate_Tool_Executable (Exec_Name : String) return String_Access is
   begin
      if not Property.Active or else Property.Tools_Path = null then
         return System.OS_Lib.Locate_Exec_On_Path (Exec_Name);
      end if;

      return Internal_Locate_Exec (Exec_Name, Property.Tools_Path.all);
   end Locate_Tool_Executable;

   --------------------------------
   -- Locate_Compiler_Executable --
   --------------------------------

   function Locate_Compiler_Executable
     (Exec_Name : String) return String_Access is
   begin
      if not Property.Active or else Property.Compiler_Path = null then
         return System.OS_Lib.Locate_Exec_On_Path (Exec_Name);
      end if;

      return Internal_Locate_Exec (Exec_Name, Property.Compiler_Path.all);
   end Locate_Compiler_Executable;

end Toolchains;
