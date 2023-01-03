------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body DAP.Histories is

   ------------
   -- Append --
   ------------

   procedure Append
     (History : History_List_Access;
      Data    : Data_Type)
   is
      Element : Hlist_Access;
   begin
      if not Is_Empty (History.Data)
        and then Last_Element (History.Data).Data.all = Data
      then
         Element := Last_Element (History.Data);
         Element.Num_Repeats := Element.Num_Repeats + 1;

      else
         Element :=
           new Hlist'
             (Data        => new Data_Type'(Data),
              Num_Repeats => 1);
         Append (History.Data, Element);
      end if;

      History.Current  := Last_Element (History.Data);
      History.Position := Length (History.Data) + 1;
   end Append;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current (History : History_List_Access) return Data_Type is
   begin
      if History.Current /= null
        and then History.Position > 0
        and then History.Position <= Length (History.Data)
      then
         return History.Current.Data.all;
      else
         raise No_Such_Item;
      end if;
   end Get_Current;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current (History : History_List_Access; Data : Data_Type) is
      procedure Free_Data_Pointer is new
        Ada.Unchecked_Deallocation (Data_Type, Data_Pointer);
   begin
      if History.Current /= null
        and then History.Position > 0
        and then History.Position <= Length (History.Data)
      then
         Free_Data_Pointer (History.Current.Data);
         History.Current.Data := new Data_Type'(Data);
      else
         raise No_Such_Item;
      end if;
   end Set_Current;

   ----------------------------
   -- Get_Current_Repeat_Num --
   ----------------------------

   function Get_Current_Repeat_Num
     (History : History_List_Access) return Natural is
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

   procedure Move_To_Previous (History : History_List_Access) is
   begin
      if History.Position > Length (History.Data) then
         History.Position := Length (History.Data);

      elsif History.Current /= null then
         if History.Position = 0 then
            raise No_Such_Item;

         elsif History.Position = 1 then
            History.Position := 0;

         else
            History.Position := History.Position - 1;
            History.Current := Element
              (History.Data, Integer (History.Position));
         end if;

      else
         raise No_Such_Item;
      end if;
   end Move_To_Previous;

   ------------------
   -- Move_To_Next --
   ------------------

   procedure Move_To_Next (History : History_List_Access) is
   begin
      if History.Position = 0 then
         History.Position := 1;
      end if;

      if History.Current /= null then
         if History.Position > Length (History.Data) then
            raise No_Such_Item;
         end if;

         History.Position := History.Position + 1;
         if History.Position <= Length (History.Data) then
            History.Current  := Element
              (History.Data, Integer (History.Position));
         end if;

      else
         raise No_Such_Item;
      end if;
   end Move_To_Next;

   ----------
   -- Wind --
   ----------

   procedure Wind (History : History_List_Access; D : Direction) is
   begin
      if not Is_Empty (History.Data) then
         if D = Backward then
            History.Current  := First_Element (History.Data);
            History.Position := 1;
         else
            History.Current  := Last_Element (History.Data);
            History.Position := Length (History.Data) + 1;
         end if;
      end if;
   end Wind;

   ------------
   -- Length --
   ------------

   function Length (History : History_List_Access) return Integer is
   begin
      return Integer (Length (History.Data));
   end Length;

   ----------
   -- Free --
   ----------

   procedure Free (History : History_List_Access) is
      procedure Free_Data_Pointer is new
        Ada.Unchecked_Deallocation (Data_Type, Data_Pointer);
      procedure Free_Hlist is new
        Ada.Unchecked_Deallocation (Hlist, Hlist_Access);

      Link : Hlist_Access;
   begin
      if not Is_Empty (History.Data) then
         for Index in 1 .. History.Length loop
            Link := Element (History.Data, Index);
            Free_Data_Pointer (Link.Data);
            Free_Hlist (Link);
         end loop;

         History.Data.Clear;
         History.Current  := null;
         History.Position := 1;
      end if;
   end Free;

end DAP.Histories;
