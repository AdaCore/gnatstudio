------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2005 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a GPS specific implementation of this package.
--  Enabling the Memory_Check trace will disable all memory deallocations.
--  In the future, we may want to connect a garbage collector instead.

with Ada.Exceptions;
with System.CRTL;
with Traces;

package body System.Memory is

   pragma Warnings (Off);

   use Ada.Exceptions;
   use Traces;

   Memory_Check : constant Debug_Handle := Create ("Memory_Check", Off, False);
   GC_Handle    : Debug_Handle := Create ("GC", Off, False);
   pragma Volatile (GC_Handle);
   pragma Warnings (Off, GC_Handle);

   Initialized : Boolean := False;

   function GC_malloc (Size : size_t) return System.Address;
   pragma Import (C, GC_malloc, "GC_malloc");

   function GC_malloc_atomic (Size : size_t) return System.Address;
   pragma Import (C, GC_malloc_atomic, "GC_malloc_atomic");

   function GC_realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, GC_realloc, "GC_realloc");

   procedure GC_free (Ptr : System.Address);
   pragma Import (C, GC_free, "GC_free");

   procedure GC_init;
   pragma Import (C, GC_init, "GC_init");

   procedure GC_disable;
   pragma Import (C, GC_disable, "GC_disable");

   GC_use_entire_heap : Integer;
   pragma Import (C, GC_use_entire_heap, "GC_use_entire_heap");

   GC_all_interior_pointers : Integer;
   pragma Import (C, GC_all_interior_pointers, "GC_all_interior_pointers");

   GC_dont_precollect : Integer;
   pragma Import (C, GC_dont_precollect, "GC_dont_precollect");

   GC_non_gc_bytes : Long_Integer;
   pragma Import (C, GC_non_gc_bytes, "GC_non_gc_bytes");

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;
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

      --  if GC_Handle /= null and then Active (GC_Handle) then
      if False then
         if not Initialized then
            Initialized := True;
            --  GC_use_entire_heap := 1;
            --  GC_all_interior_pointers := 0;
            --  GC_dont_precollect := 1;
            GC_init;
            --  GC_disable;
         end if;

         return GC_malloc (Actual_Size);
      end if;

      Result := System.CRTL.malloc (System.CRTL.size_t (Actual_Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      if False then
      --  if Active (GC_Handle) then
         return;
      end if;

      if not Active (Memory_Check) then
         System.CRTL.free (Ptr);
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
      Result : System.Address;
   begin
      if False then
      --  if Active (GC_Handle) then
         return GC_realloc (Ptr, Size);
      end if;

      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      Result := System.CRTL.realloc (Ptr, System.CRTL.size_t (Size));

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Realloc;

end System.Memory;
