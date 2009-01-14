------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a GPS specific implementation of this package.
--  Setting the GPS_MEMORY_CHECK env variable will disable all memory
--  deallocations.
--  In the future, we may want to connect a garbage collector instead.

with Ada.Exceptions;
with Ada.Exceptions.Traceback;
with GNAT.IO;                  use GNAT.IO;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.HTable;
with System.Address_Image;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Traceback_Entries; use System.Traceback_Entries;
with System.CRTL;

package body System.Memory is

   use Ada.Exceptions;

   function getenv (S : String) return System.Address;
   pragma Import (C, getenv);

   Memory_Check : Boolean :=
     getenv ("GPS_MEMORY_CHECK" & ASCII.NUL) /= Null_Address;
   pragma Export (Ada, Memory_Check, "__gps_memory_check");

   Memory_Monitor : Boolean :=
     getenv ("GPS_MEMORY_MONITOR" & ASCII.NUL) /= Null_Address;
   pragma Export (Ada, Memory_Monitor, "__memory_monitor");

   Stack_Trace_Depth : constant := 10;

   procedure Dump (Size : Positive);
   pragma Export (Ada, Dump, "__system__memory__dump");
   --  Dump information about memory usage.
   --  Size is the number of the biggest memory users we want to show.

   type Byte_Count is mod System.Max_Binary_Modulus;

   Total_Allocs   : Byte_Count := 0;
   Alloc_Count    : Long_Integer := 0;
   Total_Free     : Byte_Count := 0;
   Free_Count     : Long_Integer := 0;
   Realloc_Count  : Long_Integer := 0;
   High_Watermark : Byte_Count := 0;

   type Header is range 1 .. 1023;
   --  Number of elements in the hash-table

   type Tracebacks_Array_Access
      is access GNAT.Traceback.Tracebacks_Array;

   type Traceback_Htable_Elem;
   type Traceback_Htable_Elem_Ptr is access Traceback_Htable_Elem;

   type Traceback_Htable_Elem is record
      Traceback : Tracebacks_Array_Access;
      Count     : Natural;
      Total     : Byte_Count;
      Next      : Traceback_Htable_Elem_Ptr;
   end record;

   --  Subprograms used for the Backtrace_Htable instantiation

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr);
   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr;
   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access;
   function Hash (T : Tracebacks_Array_Access) return Header;
   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean;

   pragma Inline (Set_Next, Next, Get_Key, Hash, Equal);

   package Backtrace_Htable is new GNAT.HTable.Static_HTable
     (Header_Num => Header,
      Element    => Traceback_Htable_Elem,
      Elmt_Ptr   => Traceback_Htable_Elem_Ptr,
      Null_Ptr   => null,
      Set_Next   => Set_Next,
      Next       => Next,
      Key        => Tracebacks_Array_Access,
      Get_Key    => Get_Key,
      Hash       => Hash,
      Equal      => Equal);

   type Chunk_Data is record
      Total : Byte_Count;
   end record;
   No_Chunk_Data : constant Chunk_Data := (Total => 0);

   function Hash (F : System.Address) return Header;
   pragma Inline (Hash);

   package Chunks_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header,
      Element    => Chunk_Data,
      No_Element => No_Chunk_Data,
      Key        => System.Address,
      Hash       => Hash,
      Equal      => System."=");

   type Chunk_Kind is (Allocation, Deallocation);
   function Find_Or_Create_Traceback
     (Kind : Chunk_Kind;
      Ptr  : System.Address;
      Size : Storage_Count) return Byte_Count;
   --  Records an allocation or deallocation for the specified Ptr.
   --
   --  Returns the size of the memory that had been allocated for that pointer
   --  in the case of a deallocation, or the size that was just allocated in
   --  the case of an allocation.

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr)
   is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr is
   begin
      return E.Next;
   end Next;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean is
      use Ada.Exceptions.Traceback;
   begin
      return K1.all = K2.all;
   end Equal;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access
   is
   begin
      return E.Traceback;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (T : Tracebacks_Array_Access) return Header is
      Result : Integer_Address := 0;

   begin
      for X in T'Range loop
         Result := Result + To_Integer (PC_For (T (X)));
      end loop;

      return Header (1 + Result mod Integer_Address (Header'Last));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (F : System.Address) return Header is
   begin
      return Header (1 + To_Integer (F) mod Integer_Address (Header'Last));
   end Hash;

   Disable : Boolean := False;

   ------------------------------
   -- Find_Or_Create_Traceback --
   ------------------------------

   function Find_Or_Create_Traceback
     (Kind : Chunk_Kind;
      Ptr  : System.Address;
      Size : Storage_Count) return Byte_Count
   is
      Trace : aliased Tracebacks_Array (1 .. Integer (Stack_Trace_Depth));
      Len   : Natural;
      Elem  : Traceback_Htable_Elem_Ptr;
      Size_Was : Byte_Count := 0;
   begin
      if Disable then
         return 0;
      end if;

      Disable := True;
      Call_Chain (Trace, Len);

      case Kind is
         when Allocation =>
            Size_Was := Byte_Count (Size);
            Chunks_Htable.Set (Ptr, (Total => Size_Was));

         when Deallocation =>
            Size_Was := Chunks_Htable.Get (Ptr).Total;
            Chunks_Htable.Remove (Ptr);
      end case;

      --  Check if the traceback is already in the table

      Elem :=
        Backtrace_Htable.Get
          (Trace (Trace'First + 1 .. Len)'Unrestricted_Access);

      --  If not, insert it

      if Elem = null then
         if Kind = Allocation then
            Elem := new Traceback_Htable_Elem'
              (Traceback =>
               new Tracebacks_Array'(Trace (Trace'First + 1 .. Len)),
               Count     => 1,
               Total     => Byte_Count (Size),
               Next      => null);
            Backtrace_Htable.Set (Elem);
         end if;

      else
         case Kind is
            when Allocation =>
               Elem.Count := Elem.Count + 1;
               Elem.Total := Elem.Total + Byte_Count (Size);
            when Deallocation =>
               Elem.Count := Elem.Count - 1;
               Elem.Total := Elem.Total - Size_Was;
         end case;
      end if;

      Disable := False;
      return Size_Was;
   end Find_Or_Create_Traceback;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

      Size_Was    : Byte_Count;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      Result := System.CRTL.malloc (System.CRTL.size_t (Actual_Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      if Memory_Monitor then
         Size_Was := Find_Or_Create_Traceback
           (Allocation, Result, Storage_Count (Actual_Size));

         Total_Allocs := Total_Allocs + Size_Was;
         High_Watermark := Byte_Count'Max
           (High_Watermark, Total_Allocs - Total_Free);
         Alloc_Count := Alloc_Count + 1;
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
      Size_Was    : Byte_Count;
   begin
      if not Memory_Check then
         System.CRTL.free (Ptr);

         if Memory_Monitor then
            Size_Was := Find_Or_Create_Traceback (Allocation, Ptr, 0);
            Total_Free := Total_Free + Size_Was;
            Free_Count := Free_Count + 1;
         end if;
      end if;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;
      Size_Was    : Byte_Count;
   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      Result := System.CRTL.realloc (Ptr, System.CRTL.size_t (Actual_Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      if Memory_Monitor then
         Size_Was := Find_Or_Create_Traceback (Deallocation, Ptr, 0);
         Total_Free   := Total_Free + Size_Was;

         Size_Was := Find_Or_Create_Traceback
           (Allocation, Result, Storage_Count (Actual_Size));

         Total_Allocs := Total_Allocs + Size_Was;
         High_Watermark := Byte_Count'Max
           (High_Watermark, Total_Allocs - Total_Free);
         Realloc_Count := Realloc_Count + 1;
      end if;

      return Result;
   end Realloc;

   ----------
   -- Dump --
   ----------

   procedure Dump (Size : Positive) is
      Elem : Traceback_Htable_Elem_Ptr;

      Max  : array (1 .. Size) of Traceback_Htable_Elem_Ptr :=
        (others => null);
      --  Sorted array for the biggest memory users

   begin
      if not Memory_Monitor then
         Put_Line ("Memory monitor not activated");
         return;
      end if;

      Put_Line ("Allocs:" & Total_Allocs'Img
                & " bytes in" & Alloc_Count'Img & " chunks");
      Put_Line ("Free:" & Total_Free'Img & " bytes in" & Free_Count'Img
                & " chunks");
      Put_Line ("Realloc: " & Realloc_Count'Img & " calls");
      Put_Line ("Current watermark: "
                & Byte_Count'Image (Total_Allocs - Total_Free));
      Put_Line ("High watermark: " & High_Watermark'Img);

      Put_Line (Size'Img & " biggest memory users at this time:");

      Elem := Backtrace_Htable.Get_First;
      while Elem /= null loop
         --  Ignore if number of allocs is 1, it will never be the biggest
         --  user for a real application.
         if Elem.Count /= 1 then
            for M in Max'Range loop
               if Max (M) = null or else Max (M).Total < Elem.Total then
                  Max (M + 1 .. Max'Last) := Max (M .. Max'Last - 1);
                  Max (M) := Elem;
                  exit;
               end if;
            end loop;
         end if;
         Elem := Backtrace_Htable.Get_Next;
      end loop;

      for M in Max'Range loop
         exit when Max (M) = null;
         Put (Max (M).Total'Img & " bytes in"
              & Max (M).Count'Img & " chunks at");

         for J in Max (M).Traceback'Range loop
            Put (" 0x" & Address_Image (PC_For (Max (M).Traceback (J))));
         end loop;

         New_Line;
      end loop;
   end Dump;

end System.Memory;
