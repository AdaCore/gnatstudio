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

with SN; use SN;
with DB_API; use DB_API;
with Simple_Vector;
with GNAT.OS_Lib;

package SN.DB_Structures is

   --  STRING_MAX_SIZE : constant Integer := 1024;
   --  MAX_NUMBER_OF_ARGS : constant Integer := 256;
   --  MAX_NUMBER_OF_FIELDS_IN_SN_DB_TABLE : constant Integer := 12;

   package Segment_Vector is new Simple_Vector (Segment);

   --------------------------------------------------------
   --     SourceNavigator database table structures      --
   --------------------------------------------------------

   type BY_Table is record
      Referred_Class : Segment;
      Referred_Symbol_Name : Segment;
      Referred_Symbol : Symbol_Type;
      Class : Segment;
      Symbol_Name : Segment;
      Symbol : Symbol_Type;
      Access_Type : Segment;
      File_Name : Segment;
      Position : Point;
      Referred_Argument_Types : Segment_Vector.Node_Access;
      Caller_Argument_Types : Segment_Vector.Node_Access;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Referred-By

   type CL_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Template_Parameters : Segment;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Classes

   type COM_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Common blocks

   type CON_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Declared_Type : Segment;
      Type_Start_Position : Point;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Constants

   type COV_Table is record
      Common_Block : Segment;
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Common value

   type E_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Enumerations

   type EC_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Enumeration_Name : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Enumeration constants

   type F_Table is record
      Name : Segment;
      Group : Segment;
      Parsing_Time : Segment;
      Highlight_File : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Project File

   type FD_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Return_Type : Segment;
      Arg_Types : Segment_Vector.Node_Access;
      Arg_Names : Segment_Vector.Node_Access;
      Comments : Segment;
      Template_Parameters : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Function declarations

   type FIL_Table is record
      File_Name : Segment;
      Start_Position : Point;
      Class : Segment;
      Identifier : Segment;
      Symbol : Symbol_Type;
      End_Position : Point;
      Highlight_Start_Position : Point;
      Highlight_End_Position : Point;
      Types_Of_Arguments : Segment_Vector.Node_Access;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Symbols of files

   type FR_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Return_Type : Segment;
      Arg_Types : Segment_Vector.Node_Access;
      Arg_Names : Segment_Vector.Node_Access;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Friends

   type FU_Table is record
      Class : Segment;
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Return_Type : Segment;
      Arg_Types : Segment_Vector.Node_Access;
      Arg_Names : Segment_Vector.Node_Access;
      Comments : Segment;
      Template_Parameters : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Function implementations
   --  The same structure used for method implementations (MI)
   --  Field 'Class' for functions contains '#'

   type GV_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Value_Type : Segment;
      Comments : Segment;
      Type_Start_Position : Point;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Global variables

   type IN_Table is record
      Class : Segment;
      Base_Class : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Inheritances

   type IU_Table is record
      Included_File : Segment;
      Included_From_File : Segment;
      Included_At_Position : Point;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Includes

   type IV_Table is record
      Class : Segment;
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Value_Type : Segment;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Instance variables

   type LV_Table is record
      Function_Name : Segment;
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Class : Segment;
      Value_Type : Segment;
      Arg_Types : Segment_Vector.Node_Access;
      Comments : Segment;
      Type_Start_Position : Point;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Local variables

   type MA_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Macros

   type MD_Table is record
      Class : Segment;
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Return_Type : Segment;
      Arg_Types : Segment_Vector.Node_Access;
      Arg_Names : Segment_Vector.Node_Access;
      Template_Parameters : Segment;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Method definitions

   type REM_Table is record
      File_Name : Segment;
      Position : Point;
      Class : Segment;
      Method_Or_Function : Segment;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Remarks

   type SU_Table is record
      Name : Segment;
      File_Name : Segment;
      Position : Point;
      Attributes : SN_Attributes;
      Comments : Segment;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Subroutines

   type T_Table is record
      Name : Segment;
      File_Name : Segment;
      Start_Position : Point;
      End_Position : Point;
      Attributes : SN_Attributes;
      Original : Segment;
      Comments : Segment;
      Class_Name : Segment; -- name of enclosed class
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  Typedefs

   type TA_Table is record
      Scope          : Segment; -- name of the class or function/method
      Class_Name     : Segment; -- name of method's class
      Name           : Segment;
      Start_Position : Point;
      File_Name      : Segment;
      Type_Position  : Point;
      Attributes     : SN_Attributes;
      Value_Type     : Segment;
      Template_Parameters  : Segment;
      Comments       : Segment;
      Buffer         : GNAT.OS_Lib.String_Access;
   end record;
      --  Template arguments

   type TO_Table is record
      Class : Segment;
      Symbol_Name : Segment;
      Symbol : Symbol_Type;
      Referred_Class : Segment;
      Referred_Symbol_Name : Segment;
      Referred_Symbol : Symbol_Type;
      Access_Type : Segment;
      File_Name : Segment;
      Position : Point;
      Caller_Argument_Types : Segment_Vector.Node_Access;
      Referred_Argument_Types : Segment_Vector.Node_Access;
      Buffer : GNAT.OS_Lib.String_Access;
   end record;
   --  References-To

   subtype UN_Table is CL_Table;
   --  Unions

   ---------------------------------------------------------
   --                 Parse functions                     --
   ---------------------------------------------------------

   function Parse_Pair (Key_Data_Pair : Pair) return BY_Table;
   --  Function for parsing BY_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return CL_Table;
   --  Function for parsing CL_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return COM_Table;
   --  Function for parsing COM_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return CON_Table;
   --  Function for parsing CON_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return COV_Table;
   --  Function for parsing COV_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return E_Table;
   --  Function for parsing E_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return EC_Table;
   --  Function for parsing EC_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return F_Table;
   --  Function for parsing F_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return FD_Table;
   --  Function for parsing FD_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return FIL_Table;
   --  Function for parsing FIL_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return FR_Table;
   --  Function for parsing FR_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return FU_Table;
   --  Function for parsing FU_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return GV_Table;
   --  Function for parsing GV_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return IN_Table;
   --  Function for parsing IN_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return IU_Table;
   --  Function for parsing IU_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return IV_Table;
   --  Function for parsing IV_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return LV_Table;
   --  Function for parsing LV_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return MA_Table;
   --  Function for parsing MA_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return MD_Table;
   --  Function for parsing MD_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return REM_Table;
   --  Function for parsing REM_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return SU_Table;
   --  Function for parsing SU_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return T_Table;
   --  Function for parsing T_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return TA_Table;
   --  Function for parsing TA_Table key-data pair

   function Parse_Pair (Key_Data_Pair : Pair) return TO_Table;
   --  Function for parsing TO_Table key-data pair

   ---------------------------------------------------------

   procedure Free (target : in out BY_Table);
   --  frees resources allocated by BY_Table

   procedure Free (target : in out CL_Table);
   --  frees resources allocated by CL_Table

   procedure Free (target : in out COM_Table);
   --  frees resources allocated by COM_Table

   procedure Free (target : in out CON_Table);
   --  frees resources allocated by CON_Table

   procedure Free (target : in out COV_Table);
   --  frees resources allocated by COV_Table

   procedure Free (target : in out E_Table);
   --  frees resources allocated by E_Table

   procedure Free (target : in out EC_Table);
   --  frees resources allocated by EC_Table

   procedure Free (target : in out F_Table);
   --  frees resources allocated by F_Table

   procedure Free (target : in out FD_Table);
   --  frees resources allocated by FD_Table

   procedure Free (target : in out FIL_Table);
   --  frees resources allocated by FIL_Table

   procedure Free (target : in out FR_Table);
   --  frees resources allocated by FR_Table

   procedure Free (target : in out FU_Table);
   --  frees resources allocated by FU_Table

   procedure Free (target : in out GV_Table);
   --  frees resources allocated by GV_Table

   procedure Free (target : in out IN_Table);
   --  frees resources allocated by IN_Table

   procedure Free (target : in out IU_Table);
   --  frees resources allocated by IU_Table

   procedure Free (target : in out IV_Table);
   --  frees resources allocated by IV_Table

   procedure Free (target : in out LV_Table);
   --  frees resources allocated by LV_Table

   procedure Free (target : in out MA_Table);
   --  frees resources allocated by MA_Table

   procedure Free (target : in out MD_Table);
   --  frees resources allocated by MD_Table

   procedure Free (target : in out REM_Table);
   --  frees resources allocated by REM_Table

   procedure Free (target : in out SU_Table);
   --  frees resources allocated by SU_Table

   procedure Free (target : in out T_Table);
   --  frees resources allocated by T_Table

   procedure Free (target : in out TA_Table);
   --  frees resources allocated by TA_Table

   procedure Free (target : in out TO_Table);
   --  frees resources allocated by TO_Table

   ---------------------------------------------------------
   Number_Of_Allocated_Buffers : Integer := 0;
   ---------------------------------------------------------

   Incorrect_Pair : exception;

end SN.DB_Structures;
