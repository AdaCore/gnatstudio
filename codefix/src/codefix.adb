-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2007, AdaCore               --
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

   -------------------
   -- To_Char_Index --
   -------------------

   function To_Char_Index
     (Index : Column_Index; Str : String) return Char_Index
   is
      Current_Index : Char_Index := Char_Index (Str'First);
      Current_Col   : Column_Index := 1;
   begin
      loop
         exit when Current_Col >= Index;

         if Natural (Current_Index) < Str'Last
           and then Str (Natural (Current_Index)) = ASCII.HT
         then
            Current_Col := Current_Col + Column_Index (Tab_Width) -
              ((Current_Col - 1) mod Column_Index (Tab_Width));
         else
            Current_Col := Current_Col + 1;
         end if;

         Current_Index := Current_Index + 1;
      end loop;

      return Current_Index;
   end To_Char_Index;

   ---------------------
   -- To_Column_Index --
   ---------------------

   function To_Column_Index
     (Index : Char_Index; Str : String) return Column_Index
   is
      Current_Index : Char_Index := Char_Index (Str'First);
      Current_Col   : Column_Index := 1;
   begin
      loop
         exit when Current_Index >= Index;

         if Natural (Current_Index) <= Str'Last
           and then Str (Natural (Current_Index)) = ASCII.HT
         then
            Current_Col := Current_Col + Column_Index (Tab_Width) -
              ((Current_Col - 1) mod Column_Index (Tab_Width));
         else
            Current_Col := Current_Col + 1;
         end if;

         Current_Index := Current_Index + 1;
      end loop;

      return Current_Col;
   end To_Column_Index;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str.all)'
   begin
      This := new String'(Value);
      Free (Garbage);
   end Assign;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out String_Access; Value : String_Access) is
      Garbage : String_Access := This;
      --  Used to prevent usage like 'Assign (Str, Str)'
   begin
      This := Clone (Value);
      Free (Garbage);
   end Assign;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : File_Type; This : in out String_Access) is
      Len    : Natural;
      Buffer : String (1 .. 2048);
   begin
      --  We can't read lines longer than 2048 characters. Doesn't seem worth
      --  it anyway, since we are dealing with source files.
      Get_Line (File, Buffer, Len);
      Assign (This,  Buffer (1 .. Len));
   end Get_Line;

   -----------
   -- Clone --
   -----------

   function Clone (This : String_Access) return String_Access is
   begin
      if This = null then
         return null;
      else
         return new String'(This.all);
      end if;
   end Clone;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Mask : Useless_Entity_Operations;
      Flag : Useless_Entity_Operations) return Boolean
   is
   begin
      return (Mask and Flag) = Flag;
   end Is_Set;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out State_Node) is
   begin
      Free (This.Error);
   end Free;

   ---------------------
   -- Set_Error_State --
   ---------------------

   procedure Set_Error_State
     (List : in out State_List; Error : String; State : Error_State)
   is
      Node : State_Lists.List_Node := First (List);
   begin
      while Node /= State_Lists.Null_Node loop
         if Data (Node).Error.all = Error then
            Set_Data (Node, (new String'(Error), State));
            return;
         end if;
         Node := Next (Node);
      end loop;

      Append (List, (new String'(Error), State));
   end Set_Error_State;

   ---------------------
   -- Get_Error_State --
   ---------------------

   function Get_Error_State
     (List : State_List; Error : String) return Error_State
   is
      Node : State_Lists.List_Node := First (List);
   begin
      while Node /= State_Lists.Null_Node loop
         if Data (Node).Error.all = Error then
            return Data (Node).State;
         end if;
         Node := Next (Node);
      end loop;

      return Unknown;
   end Get_Error_State;

end Codefix;
