------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                             G N A T . T T Y                              --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides control over pseudo terminals, aka ttys.
--  This package is only supported on unix systems. See function TTY_Supported
--  to test dynamically whether other functions of this package can be called.

with System;
with GNAT.OS_Lib;

package GNAT.TTY is

   type TTY_Handle is private;
   --  Handle for a tty descriptor.

   function TTY_Supported return Boolean;
   --  If True, the other functions of this package can be called.
   --  Otherwise, all functions in this package will raise Program_Error if
   --  called.

   procedure Allocate_TTY (Handle : out TTY_Handle);
   --  Allocate a new tty.

   procedure Reset_TTY (Handle : TTY_Handle);
   --  Reset settings of a given tty.

   procedure Close_TTY (Handle : in out TTY_Handle);
   --  Close a given tty.

   function TTY_Name (Handle : TTY_Handle) return String;
   --  Return the external name of a tty.
   --  The name depends on the tty handling on the given target. It will
   --  typically look like: "/dev/ptya1"

   function TTY_Descriptor
     (Handle : TTY_Handle) return GNAT.OS_Lib.File_Descriptor;
   --  Return the low level descriptor associated with Handle.

private

   type TTY_Handle is record
      Handle : System.Address := System.Null_Address;
   end record;

end GNAT.TTY;
