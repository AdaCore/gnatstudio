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

with Interfaces.C;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;
--  with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

package body DB_API is

   --  Exception messages.
   E_Mem_Failed  : constant String := "Memory allocation failed";
   E_Init_Failed : constant String := "DB is not initialized";

   package C renames Interfaces.C;
   package CStrings renames Interfaces.C.Strings;

   use type C.int;
   use type CStrings.chars_ptr;
   use type System.Address;

   --  Constants for Set_Cursor operation (see db_capi.c for details).
   POS_FIRST  : constant C.int := 1;
   POS_LAST   : constant C.int := 2;
   POS_BY_KEY : constant C.int := 3;

   --  Constants for Get_Pair operation
   MOVE_PREV   : constant C.int := 4;
   MOVE_NEXT   : constant C.int := 5;
   MOVE_BY_KEY : constant C.int := 6;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (DB : DB_File) return Boolean is
      function Internal_Is_Null (DB : DB_File) return C.int;
      pragma Import (C, Internal_Is_Null, "ada_db_is_null");
   begin
      --  return Internal_Is_Null (DB) /= 0;
      return DB = null;
   end Is_Null;
   pragma Inline (Is_Null);

   ----------------
   -- Last_ErrNo --
   ----------------
   function Last_ErrNo (DB : DB_File) return C.int;
   pragma Import (C, Last_ErrNo, "ada_get_last_errno");

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (DB : DB_File) return String is
      function Internal_Err_Msg (DB : DB_File) return CStrings.chars_ptr;
      pragma Import (C, Internal_Err_Msg, "ada_get_errstr");
   begin
      if Is_Null (DB) then
         return E_Init_Failed;
      else
         return CStrings.Value (Internal_Err_Msg (DB));
      end if;
   end Error_Message;

   ----------
   -- Open --
   ----------

   procedure Open (DB : out DB_File; File_Names : String_List_Access) is
      function Internal_Open
        (Num_Of_Files : Integer;
         File_Names   : System.Address) return DB_File;
      pragma Import (C, Internal_Open, "ada_db_open");

      procedure Free (List : in out String_List);

      procedure Free (List : in out String_List) is
      begin
         for I in List'Range loop
            Free (List (I));
         end loop;
      end Free;

      C_File_Names : String_List (File_Names'Range);
   begin
      for I in File_Names'Range loop
         C_File_Names (I) := new String' (File_Names (I).all & ASCII.NUL);
      end loop;

      DB := Internal_Open (C_File_Names'Length, C_File_Names'Address);

      Free (C_File_Names);

      if Is_Null (DB) then
         Raise_Exception (DB_Open_Error'Identity,
           E_Mem_Failed);
      end if;

      if Last_ErrNo (DB) /= 0 then
         Raise_Exception (DB_Open_Error'Identity,
           Error_Message (DB));
      end if;

      Set_Cursor (DB, First);
   end Open;

   ---------
   -- Dup --
   ---------

   function Dup (DB : DB_File) return DB_File is
      function Internal_Dup (DB : DB_File) return DB_File;
      pragma Import (C, Internal_Dup, "ada_db_dup");
   begin
      if Is_Null (DB) then
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
      return not Is_Null (DB);
   end Is_Open;

   -----------
   -- Close --
   -----------

   procedure Close (DB : in out DB_File) is
      procedure Internal_Close (DB : DB_File);
      pragma Import (C, Internal_Close, "ada_db_close");
      procedure Internal_Free (DB : DB_File);
      pragma Import (C, Internal_Free, "free");
   begin
      if not Is_Null (DB) then -- ignore uninitialized DB
         Internal_Close (DB);
         if Last_ErrNo (DB) /= 0 then
            Raise_Exception (DB_Close_Error'Identity,
              Error_Message (DB));
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
         Pos         : C.int;
         Key         : System.Address;
         Exact_Match : C.int);
      pragma Import (C, I_Set_Cursor, "ada_db_set_cursor");

      Pos : C.int;
      EM  : C.int := 1;
      I_Key : constant String := Key & ASCII.NUL;

   begin
      if Is_Null (DB) then
         Raise_Exception (DB_Error'Identity,
           E_Init_Failed);
      else
         if not Exact_Match then
            EM := 0;
         end if;
         case Position is
            when First  => Pos := POS_FIRST;
            when Last   => Pos := POS_LAST;
            when By_Key => Pos := POS_BY_KEY;
         end case;
         if Position = By_Key then
            I_Set_Cursor (DB, Pos, I_Key'Address, EM);
         else
            I_Set_Cursor (DB, Pos, System.Null_Address, EM);
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
      if Is_Null (DB) then
         return;
      end if;
      I_Release_Cursor (DB);
   end Release_Cursor;

   --------------
   -- Get_Pair --
   --------------

   function Get_Pair
     (DB       : DB_File;
      Movement : Cursor_Movement := Next) return Pair_Ptr
   is

      P        : Pair_Ptr;
      I_Pair   : System.Address;
      Move     : C.int;

      function I_Get_Pair
        (DB    : DB_File;
         Move  : C.int) return System.Address;
      pragma Import (C, I_Get_Pair, "ada_db_get_pair");

      procedure I_Free (Addr : System.Address);
      pragma Import (C, I_Free, "free");

      function I_Get_Key (I_Pair : System.Address) return CSF;
      pragma Import (C, I_Get_Key, "ada_get_key");

      function I_Get_Data (I_Pair : System.Address) return CSF;
      pragma Import (C, I_Get_Data, "ada_get_data");
   begin
      if Is_Null (DB) then
         Raise_Exception (DB_Error'Identity,
           E_Init_Failed);
      else
         case Movement is
            when Prev        => Move := MOVE_PREV;
            when Next        => Move := MOVE_NEXT;
            when Next_By_Key => Move := MOVE_BY_KEY;
         end case;

         I_Pair := I_Get_Pair (DB, Move);

         if I_Pair = System.Null_Address then
            if Last_ErrNo (DB) /= 0 then -- error occurred
               Raise_Exception (DB_Error'Identity,
                 Error_Message (DB));
            end if;
            return null;
         else
            P := new Pair;
            P.Key  := I_Get_Key (I_Pair);
            P.Data := I_Get_Data (I_Pair);
            I_Free (I_Pair);
            return P;
         end if;

      end if;
   end Get_Pair;

   ----------
   -- Free --
   ----------

   procedure Free (The_Pair : in out Pair_Ptr) is
      procedure I_CSF_Free (The_CSF : CSF);
      pragma Import (C, I_CSF_Free, "csf_free");
      procedure Pair_Free is
        new Ada.Unchecked_Deallocation (Pair, Pair_Ptr);
   begin
      if (The_Pair = null) then
         return;
      end if;
      I_CSF_Free (The_Pair.Key);
      I_CSF_Free (The_Pair.Data);
      Pair_Free  (The_Pair);
   end Free;

   --------------------
   -- Get_All_Fields --
   --------------------

   function Get_All_Fields
     (The_CSF : CSF;
      Separator : Character := ' ') return String
   is
      N : Natural := Get_Field_Count (The_CSF);
      S : Unbounded_String;
   begin
      for F in 1 .. N - 1 loop
         Append (S, Get_Field (The_CSF, F));
         Append (S, Separator);
      end loop;
      if (N > 0) then
         Append (S, Get_Field (The_CSF, N));
      end if;
      return To_String (S);
   end Get_All_Fields;

   ---------------------
   -- Get_Field_Count --
   ---------------------

   function Get_Field_Count (The_CSF : CSF) return Natural is
      function I_Get_Field_Count (The_CSF : CSF) return C.int;
      pragma Import (C, I_Get_Field_Count, "csf_get_field_count");
   begin
      return Natural (I_Get_Field_Count (The_CSF));
   end Get_Field_Count;

   ------------------------
   --  Get_Total_Length  --
   ------------------------

   function Get_Total_Length (The_CSF : CSF) return Natural is
      function I_Get_Total_Length (The_CSF : CSF) return C.int;
      pragma Import (C, I_Get_Total_Length,
                        "csf_get_total_length");
   begin
      return Natural (I_Get_Total_Length (The_CSF));
   end Get_Total_Length;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field (The_CSF : CSF; Index : Positive) return String is
      function I_Get_Field (The_CSF : CSF; Index : C.int)
        return CStrings.chars_ptr;
      pragma Import (C, I_Get_Field, "csf_get_field");
      R : CStrings.chars_ptr :=
        I_Get_Field (The_CSF, C.int (Index));
   begin
      if (R = CStrings.Null_Ptr) then
         Raise_Exception (Index_Out_Of_Range'Identity,
           "Index out of range: " & Positive'Image (Index) & " > "
             & Positive'Image (Get_Field_Count (The_CSF)));
      else
         return CStrings.Value (R);
      end if;
   end Get_Field;

   ------------------------
   --  Get_Field_Length  --
   ------------------------

   function Get_Field_Length (The_CSF : CSF; Index : Positive)
         return Integer is
      function I_Get_Field_Length (The_CSF : CSF; Index : C.int)
        return C.int;
      pragma Import (C, I_Get_Field_Length, "csf_get_field_length");
   begin
      return Integer (I_Get_Field_Length (The_CSF, C.int (Index)));
   end Get_Field_Length;

end DB_API;
