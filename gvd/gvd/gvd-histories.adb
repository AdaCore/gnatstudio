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

package body Odd.Histories is

   ------------
   -- Append --
   ------------

   procedure Append (History : in out History_List;
                     Data    : Data_Type)
   is
      Element : Hlist_Link;
   begin
      if History.First = null then
         History.First := new Hlist' (Data        => new Data_Type' (Data),
                                      Previous    => null,
                                      Next        => null,
                                      Num_Repeats => 1);
         History.Last := History.First;
         History.Current := History.First;
         History.Position := After_End;
         History.Length := 1;

      elsif History.Last.Data.all = Data then
         History.Last.Num_Repeats := History.Last.Num_Repeats + 1;
      else
         Element := new Hlist' (Data        => new Data_Type' (Data),
                                Previous    => History.Last,
                                Next        => null,
                                Num_Repeats => 1);
         History.Last.Next := Element;
         History.Last := Element;
         History.Length := History.Length + 1;
      end if;
      History.Current := History.Last;
      History.Position := After_End;
   end Append;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current (History : History_List) return Data_Type is
   begin
      if History.Current /= null
        and then History.Position = Inside_History
      then
         return History.Current.Data.all;
      else
         raise No_Such_Item;
      end if;
   end Get_Current;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current (History : History_List; Data : in Data_Type)
   is
      procedure Free_Data_Pointer is new Unchecked_Deallocation
        (Data_Type, Data_Pointer);
   begin
      if History.Current /= null
        and then History.Position = Inside_History
      then
         Free_Data_Pointer (History.Current.Data);
         History.Current.Data := new Data_Type' (Data);
      else
         raise No_Such_Item;
      end if;
   end Set_Current;

   ----------------------------
   -- Get_Current_Repeat_Num --
   ----------------------------

   function Get_Current_Repeat_Num (History : History_List) return Natural is
   begin
      if History.Current /= null then
         return History.Current.Num_Repeats;
      else
         raise No_Such_Item;
      end if;
   end Get_Current_Repeat_Num;

   ----------------------
   -- Move_To_Previous --
   ----------------------

   procedure Move_To_Previous (History : in out History_List) is
   begin
      if History.Position = After_End then
         History.Position := Inside_History;
      elsif History.Current /= null then
         if History.Current.Previous = null then
            if History.Position = Inside_History then
               History.Position := Before_Beginning;
            else
               raise No_Such_Item;
            end if;
         else
            History.Current := History.Current.Previous;
         end if;
      else
         raise No_Such_Item;
      end if;
   end Move_To_Previous;

   ------------------
   -- Move_To_Next --
   ------------------

   procedure Move_To_Next (History : in out History_List) is
   begin
      if History.Position = Before_Beginning then
         History.Position := Inside_History;
      elsif History.Current /= null then
         if History.Current.Next = null then
            if History.Position = Inside_History then
               History.Position := After_End;
            else
               raise No_Such_Item;
            end if;
         else
            History.Current := History.Current.Next;
         end if;
      else
         raise No_Such_Item;
      end if;
   end Move_To_Next;

   ----------
   -- Wind --
   ----------

   procedure Wind (History : in out History_List; D : Direction) is
   begin
      if History.First /= null then
         if D = Backward then
            History.Current := History.First;
            History.Position := Inside_History;
         else
            History.Current := History.Last;
            History.Position := After_End;
         end if;
      end if;
   end Wind;

   ------------
   -- Length --
   ------------

   function Length (History : in History_List) return Integer is
   begin
      return History.Length;
   end Length;

   ----------
   -- Free --
   ----------

   procedure Free (History : in out History_List) is
      procedure Free_Data_Pointer is new Unchecked_Deallocation
        (Data_Type, Data_Pointer);
      procedure Free_Hlist is new Unchecked_Deallocation
        (Hlist, Hlist_Link);
   begin
      if History.First /= null then
         History.Current := History.First;
         History.Position := Inside_History;
         loop
            Free_Data_Pointer (History.Current.Data);
            exit when History.Current.Next = null;
            History.Current := History.Current.Next;
            Free_Hlist (History.Current.Previous);
         end loop;
         Free_Hlist (History.Current);
         History.Current := null;
         History.Last := null;
         History.First := null;
      end if;
   end Free;

end Odd.Histories;
