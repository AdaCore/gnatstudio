-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

   Not_Found           : exception;
   --  Raised by the find function when key does
   --  not correspond to a variable

   Invalid_Symbol_Type : exception;
   --  Raised when a bad symbol passed to To_String function

   function To_String (Sym_Type : Symbol_Type) return String;
   --  converts symbol type into string

   procedure To_String
     (Sym_Type : Symbol_Type;
      Str      : in out String;
      Where    : in out Integer);
   --  Store, in Str, at position Where, are textual representation of
   --  Sym_Type. Where is left to the first character following this
   --  representation.

   procedure To_String
     (P     : Point;
      Str   : in out String;
      Where : in out Integer);
   --  Store, in Str, at position Where, a 000000.000 representation of
   --  Point. Where is left to the first character following this
   --  representation.

--     procedure Find
--       (DB              : DB_File;
--        Ref_Class       : String      := Invalid_String;
--        Ref_Symbol_Name : String      := Invalid_String;
--        Ref_Type        : String      := Invalid_String;
--        Class           : String      := Invalid_String;
--        Symbol_Name     : String      := Invalid_String;
--        Sym_Type        : Symbol_Type := Undef;
--        Access_Type     : String      := Invalid_String;
--        Position        : Point       := Invalid_Point;
--        Filename        : String      := Invalid_String;
--        Tab             : out BY_Table);
   --  Find functions for Referred by table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CL_Table);
   --  Find functions for Classes table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out CON_Table);
   --  Find functions for Constants table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out E_Table);
   --  Find functions for Enumerations table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out EC_Table);
   --  Find functions for Enum-constants table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FD_Table);
   --  Find functions for Function table

   procedure Find
     (DB             : DB_File;
      Filename       : String      := Invalid_String;
      Start_Position : Point       := Invalid_Point;
      Class          : String      := Invalid_String;
      Identifier     : String      := Invalid_String;
      Sym_Type       : Symbol_Type := Undef;
      Tab            : out FIL_Table);
   --  Find functions for Symbols of files table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FR_Table);
   --  Find functions for Friends table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FU_Table);
   --  Find functions for Functions table

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out GV_Table);
   --  Find functions for Variables table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Base_Class     : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out IN_Table);
   --  Find functions for Inheritances table

   procedure Find
     (DB                : DB_File;
      Included_File     : String := Invalid_String;
      Start_Position    : Point  := Invalid_Point;
      Include_From_File : String := Invalid_String;
      Tab               : out IU_Table);
   --  Find functions for Include table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Variable_Name  : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out IV_Table);
   --  Find functions for Instance variables table

   procedure Find
     (DB             : DB_File;
      Function_Name  : String := Invalid_String;
      Variable_Name  : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out LV_Table);
   --  Find functions for Local variables table

   procedure Find
     (DB             : DB_File;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MA_Table);
   --  Find functions for Macros table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out MD_Table);
   --  Find functions for Method definitions table

   procedure Find
     (DB             : DB_File;
      Class          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out FU_Table);
   --  Find functions for Method implementations table

   procedure Find
     (DB       : DB_File;
      Name     : String := Invalid_String;
      Position : Point  := Invalid_Point;
      Filename : String := Invalid_String;
      Tab      : out T_Table);
   --  Find functions for Typedefs table

   procedure Find
     (DB             : DB_File;
      Scope          : String := Invalid_String;
      Name           : String := Invalid_String;
      Start_Position : Point  := Invalid_Point;
      Filename       : String := Invalid_String;
      Tab            : out TA_Table);
   --  Find functions for Templates arguments table

   procedure Find
     (DB          : DB_File;
      Class       : String      := Invalid_String;
      Symbol_Name : String      := Invalid_String;
      Sym_Type    : Symbol_Type := Undef;
      Ref_Class   : String      := Invalid_String;
      Ref_Symbol  : String      := Invalid_String;
      Ref_Type    : String      := Invalid_String;
      Access_Type : String      := Invalid_String;
      Position    : Point       := Invalid_Point;
      Filename    : String      := Invalid_String;
      Tab         : out TO_Table);
   --  Find functions for Refers to table

end SN.Find_Fns;
