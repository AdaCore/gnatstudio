-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2004                    --
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

with Interfaces.C;
with Ada.Exceptions; use Ada.Exceptions;
with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body DB_API is

   --  Exception messages.
   E_Init_Failed : constant String := "DB is not initialized";

   package C renames Interfaces.C;
   package CStrings renames Interfaces.C.Strings;

   use type C.int;
   use type CStrings.chars_ptr;
   use type System.Address;

   procedure Internal_Free (DB : DB_File);
   pragma Import (C, Internal_Free, "free");

   function Last_ErrNo (DB : DB_File) return C.int;
   pragma Import (C, Last_ErrNo, "ada_get_last_errno");

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (DB : DB_File) return String is
      function Internal_Err_Msg (DB : DB_File) return CStrings.chars_ptr;
      pragma Import (C, Internal_Err_Msg, "ada_get_errstr");
   begin
      if DB = null then
         return E_Init_Failed;
      else
         return CStrings.Value (Internal_Err_Msg (DB));
      end if;
   end Error_Message;

   ----------
   -- Open --
   ----------

   procedure Open
     (DB         : out DB_File;
      File_Names : String_List_Access;
      Success    : out Boolean)
   is
      function Internal_Open
        (Num_Of_Files : Integer;
         File_Names   : System.Address) return DB_File;
      pragma Import (C, Internal_Open, "ada_db_open");

      C_File_Names : aliased array (File_Names'Range) of chars_ptr;

   begin
      for J in C_File_Names'Range loop
         C_File_Names (J) := New_String (File_Names (J).all);
      end loop;

      DB := Internal_Open (C_File_Names'Length, C_File_Names'Address);

      for J in C_File_Names'Range loop
         Free (C_File_Names (J));
      end loop;

      if DB = null then
         Success := False;
         return;

      elsif Last_ErrNo (DB) /= 0 then
         Internal_Free (DB);
         DB      := null;
         Success := False;

      else
         Success := True;
      end if;
   end Open;

   ---------
   -- Dup --
   ---------

   function Dup (DB : DB_File) return DB_File is
      function Internal_Dup (DB : DB_File) return DB_File;
      pragma Import (C, Internal_Dup, "ada_db_dup");
   begin
      if DB = null then
         Raise_Exception (DB_Error'Identity,
           E_Init_Failed);
      else
         return Internal_Dup (DB);
      end if;
   end Dup;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (DB : DB_File) return Boolean is
   begin
      return DB /= null;
   end Is_Open;

   -----------
   -- Close --
   -----------

   procedure Close (DB : in out DB_File; Success : out Boolean) is
      procedure Internal_Close (DB : DB_File);
      pragma Import (C, Internal_Close, "ada_db_close");
   begin
      Success := True;

      if DB /= null then -- ignore uninitialized DB
         Internal_Close (DB);

         if Last_ErrNo (DB) /= 0 then
            Success := False;
            DB := null;
            return;
         end if;

         Internal_Free (DB);
         DB := null;
      end if;
   end Close;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (DB          : DB_File;
      Position    : Cursor_Position;
      Key         : String  := "";
      Exact_Match : Boolean := True)
   is
      procedure I_Set_Cursor
        (DB          : DB_File;
         Pos         : Cursor_Position;
         Key         : System.Address;
         Size        : Integer;
         Exact_Match : Integer);
      pragma Import (C, I_Set_Cursor, "ada_db_set_cursor");

      I_Key : constant String := Key & ASCII.NUL;

   begin
      if DB = null then
         Raise_Exception (DB_Error'Identity,
           E_Init_Failed);
      else
         if Position = By_Key then
            I_Set_Cursor
              (DB, Position, I_Key'Address, I_Key'Length,
               Boolean'Pos (Exact_Match));
         else
            I_Set_Cursor
              (DB, Position, System.Null_Address, 0,
               Boolean'Pos (Exact_Match));
         end if;

         if Last_ErrNo (DB) /= 0 then
            Raise_Exception (DB_Error'Identity,
              Error_Message (DB));
         end if;
      end if;
   end Set_Cursor;

   --------------------
   -- Release_Cursor --
   --------------------

   procedure Release_Cursor (DB : DB_File) is
      procedure I_Release_Cursor (DB : DB_File);
      pragma Import (C, I_Release_Cursor, "ada_db_free_cursor");
   begin
      if DB = null then
         return;
      end if;

      I_Release_Cursor (DB);
   end Release_Cursor;

   --------------
   -- Get_Pair --
   --------------

   procedure Get_Pair
     (DB       : DB_File;
      Movement : Cursor_Movement := Next;
      Result   : out Pair)
   is
      procedure I_Get_Pair
        (DB : DB_File; Move : Cursor_Movement; Result : out Pair);
      pragma Import (C, I_Get_Pair, "ada_db_get_pair");

   begin
      if DB = null then
         Raise_Exception (DB_Error'Identity, E_Init_Failed);
      else
         I_Get_Pair (DB, Movement, Result);

         if Result.DBI = -1 then
            if Last_ErrNo (DB) /= 0 then -- error occurred
               Raise_Exception (DB_Error'Identity,
                 Error_Message (DB));
            end if;

            Result := No_Pair;
         end if;
      end if;
   end Get_Pair;

   ---------------------
   -- Get_Field_Count --
   ---------------------

   function Get_Field_Count (The_CSF : CSF) return Natural is
      function I_Get_Field_Count (The_CSF : System.Address) return C.int;
      pragma Import (C, I_Get_Field_Count, "csf_get_field_count");
   begin
      return Natural (I_Get_Field_Count (The_CSF'Address));
   end Get_Field_Count;

   ---------
   -- "=" --
   ---------

   function "=" (P1, P2 : Pair) return Boolean is
   begin
      return P1.Key = P2.Key
        and then P1.Data = P2.Data
        and then P1.DBI = P2.DBI;
   end "=";

end DB_API;
