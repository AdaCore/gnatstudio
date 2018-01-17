------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

with HTables;
with String_Utils;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_HTables is

   Verbose : constant Boolean := False;

   --  Small regression test for the HTables package. This is not the prettiest
   --  code, but it does the job.

   --  Note: The implementation of Simple_HTable is based on the implementation
   --  of Static_HTable, so testing Simple_HTable only is enough to cover
   --  the entire HTables code.

   type Index is range 1 .. 1000;
   No_Element : constant Natural := Natural'First;
   type String_Access is access String;

   function Hash (F : String_Access) return Index;
   procedure Free (X : in out Natural);

   package SHT is new HTables.Simple_HTable
     (Header_Num   => Index,
      Element      => Natural,
      No_Element   => No_Element,
      Free_Element => Free,
      Key          => String_Access,
      Hash         => Hash,
      Equal        => "=");
   use SHT;

   type An_Index is range 0 .. 10_000;
   Elements : array (An_Index) of Natural;
   Element_Name   : array (An_Index) of String_Access;
   Elements_Found : array (An_Index) of Boolean;

   HT         : Instance;
   Elmt       : Natural;
   Elmt_Index : An_Index;
   Success    : Boolean;

   function Hash is new String_Utils.Hash (Index);

   procedure Free (X : in out Natural) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   function Hash (F : String_Access) return Index is
   begin
      return Hash (F.all);
   end Hash;

   function Element_Value (J : An_Index) return Natural;
   function Element_Value (J : An_Index) return Natural is
   begin
      return 3 * Natural (J) / 2 + 1;
      --  The rest of the test assumes this function is bijective.
   end Element_Value;

   procedure Get_Element_Index
     (V : Natural; VI : out An_Index; Success : out Boolean);
   procedure Get_Element_Index
     (V : Natural; VI : out An_Index; Success : out Boolean) is
   begin
      for K in An_Index'Range loop
         if Elements (K) = V then
            VI := K;
            Success := True;
            return;
         end if;
      end loop;
      VI := An_Index'First;
      Success := False;
   end Get_Element_Index;

   function Element_Name_Of (J : An_Index) return String;
   function Element_Name_Of (J : An_Index) return String is
   begin
      if J <= 255 then
         return String'(1 => Character'Val (J mod 255));
      else
         return Character'Val (J mod 255) & Element_Name_Of (J / 255);
      end if;
   end Element_Name_Of;

   function Element_Name_Access (J : An_Index) return String_Access;
   function Element_Name_Access (J : An_Index) return String_Access is
      Name : constant String := Element_Name_Of (J);
   begin
      return new String'(Name);
   end Element_Name_Access;

   procedure Check_Remove (J : An_Index);
   procedure Check_Remove (J : An_Index) is
      E : Natural;
   begin
      Remove (HT, Element_Name (J));
      E := Get (HT, Element_Name (J));
      if E /= No_Element then
         Put_Line
           ("*** Value returned by Get is" & E'Img &
              " but it should be" & No_Element'Img);
      end if;
      Elements_Found (J) := True;
      --  Mark this element as found. This will be used later when we
      --  check that all elements are found, and found once when iterating
      --  over the hash-table...
   end Check_Remove;

   procedure Check_All_Elements_Found_Once_And_Only_Once_By_Iterator;
   procedure Check_All_Elements_Found_Once_And_Only_Once_By_Iterator is
      Iter : Cursor;
   begin
      Get_First (HT, Iter);
      loop
         Elmt := Get_Element (Iter);
         exit when Elmt = No_Element;

         Get_Element_Index (Elmt, Elmt_Index, Success);
         if Success then
            if Elements_Found (Elmt_Index) then
               Put_Line
                 ("*** Element at index" & Elmt_Index'Img &
                    " found more than once.");
            end if;
            Elements_Found (Elmt_Index) := True;
         else
            Put_Line
              ("*** Strange value returned while iterating :" & Elmt'Img);
         end if;

         Get_Next (HT, Iter);
      end loop;

      --  Verify that all elements were found by the iterator...
      for J in Elements_Found'Range loop
         if not Elements_Found (J) then
            Put_Line
              ("*** Element at index" & J'Img & " not returned by iterator");
         end if;
      end loop;
   end Check_All_Elements_Found_Once_And_Only_Once_By_Iterator;

begin

   --  Initialize the Elements_Found array
   for J in An_Index'Range loop
      Elements (J)       := Element_Value (J);
      Element_Name (J)   := Element_Name_Access (J);
      Elements_Found (J) := False;
   end loop;

   if Verbose then
      Put_Line ("--- Filling-in the hash-table...");
   end if;

   --  Fill-in the htable:
   Reset (HT);
   for J in reverse An_Index loop
      --  Put_Line ("Index:" & J'Img);
      Set (HT, Element_Name (J), Element_Value (J));
   end loop;

   if Verbose then
      Put_Line ("--- Testing the values stored in the hash-table...");
   end if;

   --  check the values retrieved from the hash-table...
   for J in An_Index loop
      if Get (HT, Element_Name (J)) /= Element_Value (J) then
         Put_Line ("*** Simple.HTable.Get failed!");
         Put_Line
           ("       Expected: ('" & Element_Name (J).all & "'," &
              Natural'Image (Element_Value (J)) & ")");
         Put_Line
           ("          Found: ('" & Element_Name (J).all & "'," &
              Natural'Image (Element_Value (J)) & ")");
      end if;
   end loop;

   if Verbose then
      Put_Line ("--- Verifying the hash-table iterator...");
   end if;

   Check_All_Elements_Found_Once_And_Only_Once_By_Iterator;

   if Verbose then
      Put_Line ("--- Verify that Remove functions properly...");
   end if;

   --  Reinitialize the Elements_Found array first
   for J in Elements_Found'Range loop
      Elements_Found (J) := False;
   end loop;

   Check_Remove (4);
   Check_Remove (8);
   Check_Remove (7);
   Check_All_Elements_Found_Once_And_Only_Once_By_Iterator;

   if Verbose then
      Put_Line ("--- Check that Reset works properly...");
   end if;

   Reset (HT);

   declare
      Iter : Cursor;
   begin
      Get_First (HT, Iter);
      if Get_Element (Iter) /= No_Element then
         Put_Line ("*** Reset did not empty the hash-table.");
      end if;
   end;

end Test_HTables;
