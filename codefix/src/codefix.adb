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

package body Codefix is

   ----------------------------------------------------------------------------
   --  type Dynamic_String
   ----------------------------------------------------------------------------

   ------------
   -- Affect --
   ------------

   procedure Assign (This : in out Dynamic_String; Value : String) is
   begin
      Free (This);
      This := new String'(Value);
   end Assign;

   ------------
   -- Affect --
   ------------

   procedure Assign (This : in out Dynamic_String; Value : Dynamic_String) is
   begin
      Free (This);
      This := new String'(Value.all);
   end Assign;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (This : in out Dynamic_String) is
   begin
      Get_Line (Standard_Input, This);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : File_Type; This : in out Dynamic_String) is
      Len    : Natural;
      Buffer : String (1 .. 4096);
   begin
      Get_Line (File, Buffer, Len);
      Free (This);
      This := new String'(Buffer (1 .. Len));
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : Dynamic_String) is
   begin
      Put_Line (Standard_Output, This);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; This : Dynamic_String) is
   begin
      Put_Line (File, This.all);
   end Put_Line;

   --------------------
   -- Dynamic_Arrays --
   --------------------

   package body Dynamic_Arrays is

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element
        (This     : in out Dynamic_Array;
         Element  : Data_Type;
         Position : Positive) is
      begin
         if This = null or else Position > This.all'Length then
            Resize (This, Position + Free_Buffer);
         end if;

         Free (This.all (Position));
         This.all (Position) := Element;
      end Set_Element;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element
        (This     : Dynamic_Array;
         Position : Positive) return Data_Type is
      begin
         if This = null or else Position > This.all'Length then
            return Null_Element;
         else
            return This.all (Position);
         end if;
      end Get_Element;

      --------------------
      -- Delete_Element --
      --------------------

      procedure Delete_Element
        (This     : in out Dynamic_Array;
         Position : Positive) is
      begin
         if This /= null and then Position <= This.all'Length then
            Free (This.all (Position));
            This.all (Position) := Null_Element;
         end if;
      end Delete_Element;

      ------------
      -- Resize --
      ------------

      procedure Resize (This : in out Dynamic_Array) is
      begin
         Resize (This, Get_Using_Size (This) + Free_Buffer);
      end Resize;

      ------------
      -- Resize --
      ------------

      procedure Resize (This : in out Dynamic_Array; New_Size : Positive) is
         Old_Array : Dynamic_Array;
      begin
         if This /= null and then New_Size = This.all'Length then
            return;
         end if;

         Old_Array := This;
         This := new Static_Array (1 .. New_Size);
         This.all := (others => Null_Element);
         if Old_Array = null then return; end if;

         if New_Size > Old_Array.all'Length then
            for J in 1 .. Old_Array.all'Length loop
               This.all (J) := Old_Array.all (J);
            end loop;
         else
            for J in 1 .. New_Size loop
               This.all (J) := Old_Array.all (J);
            end loop;
         end if;

         Delete (Old_Array);
      end Resize;

      --------------------
      -- Get_Using_Size --
      --------------------

      function Get_Using_Size (This : Dynamic_Array) return Natural is
      begin
         for J in reverse 1 .. This.all'Length loop
            if This.all (J) /= Null_Element then return J; end if;
         end loop;

         return 0;
      end Get_Using_Size;

      ----------
      -- Free --
      ----------

      procedure Free (This : in out Dynamic_Array) is
      begin
         if This /= null then
            for J in This.all'Range loop
               Free (This.all (J));
            end loop;
         end if;
         Delete (This);
      end Free;

      -----------
      -- Clone --
      -----------

      function Clone (This : Dynamic_Array) return Dynamic_Array is
      begin
         return new Static_Array'(This.all);
      end Clone;

   end Dynamic_Arrays;

end Codefix;
