-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Unchecked_Deallocation;
with System; use System;
with Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body Odd.Histories is

   -------------
   -- Convert --
   -------------

   function Convert (Value : Data_Access) return System.Address is
   begin
      if Value = null then
         return Null_Address;
      else
         return Value.all'Address;
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Value : System.Address) return Data_Access is
      function To_Pointer is new Unchecked_Conversion (Address, Data_Access);
   begin
      return To_Pointer (Value);
   end Convert;

   ------------
   -- Append --
   ------------

   procedure Append (History : in out History_List;
                     Data    : Data_Type)
   is
      use Hlist;
   begin
      if History.List = Null_List then
          Hlist.Append (History.List,
                       new Data_Record' (Data => new Data_Type' (Data),
                                         Num_Repeats => 1));
      elsif Hlist.Get_Data (Hlist.Last (History.List)).Data.all = Data
      then
         Hlist.Get_Data (Hlist.Last (History.List)).Num_Repeats :=
           Hlist.Get_Data (Hlist.Last (History.List)).Num_Repeats + 1;
      else
         Hlist.Append (History.List,
                       new Data_Record' (Data => new Data_Type' (Data),
                                         Num_Repeats => 1));
      end if;
      History.Current := Hlist.Last (History.List);
      History.Position := After_End;
   end Append;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current (History : History_List) return Data_Type is
      use Hlist;
   begin
      if History.Current /= Null_List then
         return Hlist.Get_Data (History.Current).Data.all;
      else
         raise No_Such_Item;
      end if;
   end Get_Current;

   ----------------------------
   -- Get_Current_Repeat_Num --
   ----------------------------

   function Get_Current_Repeat_Num (History : History_List) return Natural is
      use Hlist;
   begin
      if History.Current /= Null_List then
         return Hlist.Get_Data (History.Current).Num_Repeats;
      else
         raise No_Such_Item;
      end if;
   end Get_Current_Repeat_Num;

   ----------------------
   -- Move_To_Previous --
   ----------------------

   procedure Move_To_Previous (History : in out History_List) is
      use Hlist;
   begin
      if History.Position = After_End then
         History.Position := Inside_History;
      elsif History.Current /= Null_List then
         if Prev (History.Current) = Null_List then
            if History.Position = Inside_History then
               History.Position := Before_Beginnning;
            end if;
            raise No_Such_Item;
         else
            History.Current := Hlist.Prev (History.Current);
         end if;
      else
         raise No_Such_Item;
      end if;
   end Move_To_Previous;

   ------------------
   -- Move_To_Next --
   ------------------

   procedure Move_To_Next (History : in out History_List) is
      use Hlist;
   begin
      if History.Position = Before_Beginnning then
         History.Position := Inside_History;
      elsif History.Current /= Hlist.Null_List then
         if Next (History.Current) = Hlist.Null_List then
            if History.Position = Inside_History then
               History.Position := After_End;
            end if;
            raise No_Such_Item;
         else
            History.Current := Next (History.Current);
         end if;
      else
         raise No_Such_Item;
      end if;
   end Move_To_Next;

   ----------
   -- Wind --
   ----------

   procedure Wind (History : in out History_List; D : Direction) is
      use Hlist;
   begin
      if History.List /= Hlist.Null_List then
         if D = Backward then
            History.Current := Hlist.First (History.List);
         else
            History.Current := Hlist.Last (History.List);
         end if;
      end if;
   end Wind;

   ------------
   -- Length --
   ------------

   function Length (History : in History_List) return Integer
   is
   begin
      return Integer (Hlist.Length (History.List));
   end Length;

   ----------
   -- Free --
   ----------

   procedure Free (History : in out History_List) is
      --  procedure Free_Data_Pointer is new Unchecked_Deallocation
      --  (Data_Type, Data_Pointer);
   begin
         Hlist.Free (History.List);
         --  ??? is this enough ?
   end Free;

end Odd.Histories;
