------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . T T Y                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2002 Ada Core Technologies, Inc.              --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body GNAT.TTY is

   use System;

   procedure Check_TTY (Handle : TTY_Handle);
   --  Check the validity of Handle.
   --  Raise Program_Error if ttys are not supported.
   --  Raise Constraint_Error if Handle is an invalid handle.

   ------------------
   -- Allocate_TTY --
   ------------------

   procedure Allocate_TTY (Handle : out TTY_Handle) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gvd_new_tty");

   begin
      if not TTY_Supported then
         raise Program_Error;
      end if;

      Handle.Handle := Internal;
   end Allocate_TTY;

   ---------------
   -- Check_TTY --
   ---------------

   procedure Check_TTY (Handle : TTY_Handle) is
   begin
      if not TTY_Supported then
         raise Program_Error;
      elsif Handle.Handle = System.Null_Address then
         raise Constraint_Error;
      end if;
   end Check_TTY;

   ---------------
   -- Close_TTY --
   ---------------

   procedure Close_TTY (Handle : in out TTY_Handle) is
      procedure Internal (Handle : System.Address);
      pragma Import (C, Internal, "gvd_close_tty");

   begin
      Check_TTY (Handle);
      Internal (Handle.Handle);
      Handle.Handle := System.Null_Address;
   end Close_TTY;

   --------------------
   -- TTY_Descriptor --
   --------------------

   function TTY_Descriptor
     (Handle : TTY_Handle) return GNAT.OS_Lib.File_Descriptor
   is
      function Internal
        (Handle : System.Address) return GNAT.OS_Lib.File_Descriptor;
      pragma Import (C, Internal, "gvd_tty_fd");

   begin
      Check_TTY (Handle);
      return Internal (Handle.Handle);
   end TTY_Descriptor;

   --------------
   -- TTY_Name --
   --------------

   function TTY_Name (Handle : TTY_Handle) return String is
      function Internal (Handle : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gvd_tty_name");

   begin
      Check_TTY (Handle);
      return Value (Internal (Handle.Handle));
   end TTY_Name;

   -------------------
   -- TTY_Supported --
   -------------------

   function TTY_Supported return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "gvd_tty_supported");

   begin
      return Internal /= 0;
   end TTY_Supported;

   ---------------
   -- Reset_TTY --
   ---------------

   procedure Reset_TTY (Handle : TTY_Handle) is
      procedure Internal (Handle : System.Address);
      pragma Import (C, Internal, "gvd_reset_tty");

   begin
      Check_TTY (Handle);
      Internal (Handle.Handle);
   end Reset_TTY;

end GNAT.TTY;
