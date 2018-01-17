------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

package body Virtual_File_Indexes is

   ------------
   -- Create --
   ------------

   function Create return Comparison_Optimizer is
   begin
      return new Comparison_Optimizer_Record;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Opt : in out Comparison_Optimizer) is
      procedure Free_Opt is new Ada.Unchecked_Deallocation
        (Comparison_Optimizer_Record, Comparison_Optimizer);
      procedure Free_Key is new Ada.Unchecked_Deallocation
        (VF_Key_Record, VF_Key);

      It  : VF_Map.Cursor := Opt.Map.First;
      Tmp : VF_Key;
   begin
      while It /= VF_Map.No_Element loop
         Tmp := Element (It);
         Free_Key (Tmp);

         It := Next (It);
      end loop;

      Free_Opt (Opt);
   end Destroy;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Opt : Comparison_Optimizer;
      File : Virtual_File)
      return VF_Key
   is
      Key : VF_Key;
   begin
      if not Opt.Map.Contains (File) then
         Key := new VF_Key_Record;
         Key.File := File;
         Opt.Map.Insert (File, Key);

         declare
            It  : VF_Map.Cursor := Opt.Map.First;
            Ind : Integer := 0;
         begin
            while It /= VF_Map.No_Element loop
               Element (It).Order := Ind;

               It := Next (It);
               Ind := Ind + 1;
            end loop;
         end;

         return Key;
      else
         return Opt.Map.Element (File);
      end if;
   end Get_Key;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : VF_Key) return Boolean is
   begin
      return Left.Order < Right.Order;
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : VF_Key) return Boolean is
   begin
      return Left.Order = Right.Order;
   end "=";

end Virtual_File_Indexes;
