-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2000-2008, AdaCore                   --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with String_Utils;

package body GVD.Types is

   use GNAT.Strings;

   ----------
   -- Free --
   ----------

   procedure Free (Br : in out Breakpoint_Data) is
   begin
      Free (Br.Expression);
      Free (Br.Except);
      Free (Br.Subprogram);
      Free (Br.Condition);
      Free (Br.Commands);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Br_Array : in out Breakpoint_Array) is
   begin
      for B in Br_Array'Range loop
         Free (Br_Array (B));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Br_Access : in out Breakpoint_Array_Ptr) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Breakpoint_Array, Breakpoint_Array_Ptr);
   begin
      if Br_Access /= null then
         Free (Br_Access.all);
         Internal_Free (Br_Access);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Exception_Access : in out Exception_Array) is
   begin
      for E in Exception_Access'Range loop
         Free (Exception_Access (E).Name);
      end loop;
   end Free;

   -----------------------
   -- String_To_Address --
   -----------------------

   function String_To_Address (Address_String : String) return Address_Type is
      Index : Natural;
   begin
      if Address_String'Length < 3
        or else Address_String
          (Address_String'First .. Address_String'First + 1) /= "0x"
      then
         return Invalid_Address;
      end if;

      Index := Address_String'First + 2;

      while Index <= Address_String'Last
        and then Address_String (Index) = '0'
      loop
         Index := Index + 1;
      end loop;

      return
        (Address_String => Address_String,
         Last           => Address_String'Last - Address_String'First + 1,
         Length         => Address_String'Last - Index + 1,
         Offset         => 0);
   end String_To_Address;

   -----------------------
   -- Address_To_String --
   -----------------------

   function Address_To_String (Address : Address_Type) return String is
      use Ada.Strings.Unbounded;
   begin
      if Address.Offset = 0 then
         return Address.Address_String
           (Address.Address_String'First .. Address.Last);
      else
         declare
            Offset : Unbounded_String;
         begin
            if Address.Offset < 0 then
               Append (Offset, "-");
            else
               Append (Offset, "+");
            end if;

            Append
              (Offset,
               String_Utils.Image (abs Address.Offset));

            return Address.Address_String
              (Address.Address_String'First .. Address.Last) &
            To_String (Offset);
         end;
      end if;
   end Address_To_String;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean is
   begin
      return Address_1.Address_String
        (Address_1.Last - Address_1.Length + 1 .. Address_1.Last) =
        Address_2.Address_String
          (Address_2.Last - Address_2.Length + 1 .. Address_2.Last);
   end "=";

   ----------------
   -- Set_Offset --
   ----------------

   function Set_Offset
     (Address : Address_Type;
      Offset  : Natural) return Address_Type
   is
      Address_With_Offset : Address_Type := Address;
   begin
      Address_With_Offset.Offset := Offset;

      return Address_With_Offset;
   end Set_Offset;

   ---------
   -- ">" --
   ---------

   function ">"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean is
   begin
      if Address_1.Length > Address_2.Length then
         return True;
      elsif Address_1.Length < Address_2.Length then
         return False;
      else
         return Address_1.Address_String
           (Address_1.Last - Address_1.Length + 1 .. Address_1.Last) >
           Address_2.Address_String
             (Address_2.Last - Address_2.Length + 1 .. Address_2.Last);
      end if;
   end ">";

   ---------
   -- ">= --
   ---------

   function ">="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean is
   begin
      if Address_1.Length > Address_2.Length then
         return True;
      elsif Address_1.Length < Address_2.Length then
         return False;
      else
         return Address_1.Address_String
           (Address_1.Last - Address_1.Length + 1 .. Address_1.Last) >=
           Address_2.Address_String
             (Address_2.Last - Address_2.Length + 1 .. Address_2.Last);
      end if;
   end ">=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean is
   begin
      return Address_2 > Address_1;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean is
   begin
      return Address_2 >= Address_1;
   end "<=";

end GVD.Types;
