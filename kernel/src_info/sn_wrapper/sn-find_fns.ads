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
      --  raised by the find function when key does
      --  not correspond to a variable

      Invalid_Symbol_Type : exception;
      --  raised when a bad symbol passed to To_String function

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

      --  Find functions for Referred by table
      function Find (DB : DB_File;
            Ref_Class : String := Invalid_String;
            Ref_Symbol_Name : String := Invalid_String;
            Ref_Type : String := Invalid_String;
            Class : String := Invalid_String;
            Symbol_Name : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef;
            Access_Type : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return BY_Table;

      --  Find functions for Classes table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CL_Table;

      --  Find functions for Common blocks table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COM_Table;

      --  Find functions for Constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CON_Table;

      --  Find functions for Common value table
      function Find (DB : DB_File;
            Common_Block : String := Invalid_String;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COV_Table;

      --  Find functions for Enumerations table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return E_Table;

      --  Find functions for Enum-constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return EC_Table;

      --  Find functions for Project files table
      function Find (DB : DB_File;
            Name : String := Invalid_String)
      return F_Table;

      --  Find functions for Function table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FD_Table;

      --  Find functions for Symbols of files table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Identifier : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef)
      return FIL_Table;

      --  Find functions for Friends table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FR_Table;

      --  Find functions for Functions table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FU_Table;

      --  Find functions for Variables table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return GV_Table;

      --  Find functions for Inheritances table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Base_Class : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IN_Table;

      --  Find functions for Include table
      function Find (DB : DB_File;
            Included_File : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Include_From_File : String := Invalid_String)
      return IU_Table;

      --  Find functions for Instance variables table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IV_Table;

      --  Find functions for Local variables table
      function Find (DB : DB_File;
            Function_Name : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return LV_Table;

      --  Find functions for Macros table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MA_Table;

      --  Find functions for Method definitions table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MD_Table;

      --  Find functions for Method implementations table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FU_Table;

      --  Find functions for Remarks table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Method_Or_Function : String := Invalid_String)
      return REM_Table;

      --  Find functions for Subroutines table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return SU_Table;

      --  Find functions for Typedefs table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return T_Table;

      --  Find functions for Templates arguments table
      function Find
        (DB             : DB_File;
         Scope          : String := Invalid_String;
         Name           : String := Invalid_String;
         Start_Position : Point  := Invalid_Point;
         Filename       : String := Invalid_String)
      return TA_Table;

      --  Find functions for Refers to table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Symbol_Name : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef;
            Ref_Class : String := Invalid_String;
            Ref_Symbol : String := Invalid_String;
            Ref_Type : String := Invalid_String;
            Access_Type : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return TO_Table;

      --  Find functions for Unions table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return UN_Table;
end SN.Find_Fns;

