-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2008, AdaCore               --
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

with SN.DB_Structures, DB_API;
use  SN.DB_Structures, DB_API;

package SN.Find_Fns is

   Invalid_Symbol_Type : exception;
   --  Raised when a bad symbol passed to To_String function

   function To_String (Sym_Type : Symbol_Type) return String;
   --  converts symbol type into string

   procedure Set_Cursor_At
     (DB             : DB_File;
      Name           : String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String);
   --  Change the cursor in DB to point to the first entry matching the
   --  parameters. If any of the parameters is left to its default value, only
   --  the preceding parameters are taken into account.
   --  The cursor must be released by the caller.

   procedure Set_Cursor_At
     (DB             : DB_File;
      Filename       : String);
   --  Same as above, for a table that only uses a file name as an index (F).
   --  If Filename is Invalid_String, all entries will be returned.

   procedure Set_Cursor_At
     (DB             : DB_File;
      Class          : String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String);
   --  Same as above with one extra parameter

   procedure Get_Pair
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Result         : out Pair);
   --  Get the value in the database DB matching the key given by the
   --  parameters. The returned value must be freed by the user.
   --  No_Pair is returned if no such entry was found.
   --  Matching is done on as many parameters as possible, until one of them
   --  has the default invalid value, in which case the first entry in DB
   --  matching the parameters so far is returned, or Not_Found is raised if
   --  there are none.

   procedure Get_Pair
     (DB                : DB_File;
      Class_Or_Function : String := Invalid_String;
      Name              : String := Invalid_String;
      Start_Position    : Point  := Invalid_Point;
      Filename          : String := Invalid_String;
      Result            : out Pair);
   --  Same as above, with a different index

   ----------
   -- Find --
   ----------
   --  The following subprograms are higher level interfaces to Get_Pair, which
   --  automatically free the Pair.

   procedure Find_Key
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Key;
      Success        : out Boolean);
   --  Only applicable to CL, CON, E, EC, FD, FT, FU, GV, MA, T

   procedure Find_Key
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Class_Key;
      Success        : out Boolean);
   --  Only applicable to IV, MD, MI

   procedure Find_Key
     (DB             : DB_File;
      Function_Name  : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Key            : out Entity_Function_Key;
      Success        : out Boolean);
   --  Only applicable to LV

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CL_Table;
      Success        : out Boolean);
   --  Find a class in the ".cl" class.
   --  If one parameter is not specified, all following parameters are ignored,
   --  and the first value matching the parameters so far is returned.

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CON_Table;
      Success        : out Boolean);
   --  Find functions for Constants table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out E_Table;
      Success        : out Boolean);
   --  Find functions for Enumerations table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out EC_Table;
      Success        : out Boolean);
   --  Find functions for Enum-constants table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FD_Table;
      Success        : out Boolean);
   --  Find an entry in the ".fu" table.
   --  Null_FD is returned if this function couldn't be find.

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FU_Table;
      Success        : out Boolean);
   --  Find functions for Functions table

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out GV_Table;
      Success  : out Boolean);
   --  Find functions for Variables table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Variable_Name  : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out IV_Table;
      Success        : out Boolean);
   --  Find functions for Instance variables table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MA_Table;
      Success        : out Boolean);
   --  Find functions for Macros table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MD_Table;
      Success        : out Boolean);
   --  Find functions for Method definitions table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out TA_Table;
      Success        : out Boolean);
   --  Find functions for the Template arguments table

   procedure Find
     (DB             : DB_File;
      Class          : String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MI_Table;
      Success        : out Boolean);
   --  Find functions for Method implementations table

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out T_Table;
      Success        : out Boolean);
   --  Find functions for Typedefs table

   pragma Inline (Find);
end SN.Find_Fns;
