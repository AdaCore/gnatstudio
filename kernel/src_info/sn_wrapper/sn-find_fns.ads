with SN.DB_Structures,
     DB_API;
use  SN.DB_Structures,
     DB_API;

package SN.Find_Fns is
      Not_Found           : exception;
      --  raised by the find function when key does
      --  not correspond to a variable

      Invalid_Symbol_Type : exception;
      --  raised when a bad symbol passed to To_String function

      procedure To_String (Sym_Type : in Symbol_Type; Str : in String_Access;
          Where : in out Integer);
      --  converts symbol type into string

      procedure To_String (P : in Point; Str : in String_Access;
                           Where : in out Integer);
      --  converts Point to 000000.000 string

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
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CL_Table;

      --  Find functions for Common blocks table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COM_Table;

      --  Find functions for Constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CON_Table;

      --  Find functions for Common value table
      function Find (DB : DB_File;
            Common_Block : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COV_Table;

      --  Find functions for Enumerations table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return E_Table;

      --  Find functions for Enum-constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return EC_Table;

      --  Find functions for Project files table
      function Find (DB : DB_File;
            Name : String := Invalid_String)
      return F_Table;

      --  Find functions for Function table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FD_Table;

      --  Find functions for Symbols of files table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Identifier : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef)
      return FIL_Table;

      --  Find functions for Friends table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FR_Table;

      --  Find functions for Functions table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
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
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IN_Table;

      --  Find functions for Include table
      function Find (DB : DB_File;
            Included_file : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Include_from_file : String := Invalid_String)
      return IU_Table;

      --  Find functions for Instance variables table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IV_Table;

      --  Find functions for Local variables table
      function Find (DB : DB_File;
            Function_Name : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return LV_Table;

      --  Find functions for Macros table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MA_Table;

      --  Find functions for Method definitions table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MD_Table;

      --  Find functions for Method implementations table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MI_Table;

      --  Find functions for Remarks table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Method_or_function : String := Invalid_String)
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

