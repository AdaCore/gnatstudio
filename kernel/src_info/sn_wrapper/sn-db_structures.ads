-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2004                      --
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

--  This file provides an interface to the Source Navigator data
--  structures and database.
--  See http://sourcenav.sourceforge.net/online-docs/progref/dbaseAPI.html
--  for more information.
--  Source Navigator builds a number of tables, but we do not provide a
--  mapping for the tables we do not use:
--     ".by" : Same information as the ".to" table
--     ".com": Used for Fortran common blocks only
--     ".cov": Used for Fortran common variables only
--     ".rem": Used for comments/remarks
--     ".su" : Used for Fortran subroutines

with SN; use SN;
with DB_API; use DB_API;

package SN.DB_Structures is

   Buffer_String_Size : constant := 2_048;
   --  Maximum size for the buffer string

   subtype Buffer_String is String (1 .. Buffer_String_Size);
   --  Used to record a copy of the dbm key and data objects.
   --  Internal C offsets (starting at 0) are changed to match 'First for this
   --  array.

   Null_Buffer : constant Buffer_String := (others => ' ');

   type Entity_Key is record
      Name           : Segment;
      File_Name      : Segment;
      Start_Position : Point;
      Key            : Buffer_String;
   end record;

   type Entity_Class_Key is record
      Class          : Segment;
      Name           : Segment;
      File_Name      : Segment;
      Start_Position : Point;
      Key            : Buffer_String;
   end record;

   type Entity_Function_Key is record
      Function_Name  : Segment;
      Name           : Segment;
      File_Name      : Segment;
      Start_Position : Point;
      Key            : Buffer_String;
   end record;

   type CL_Table is record
      Name                : Segment;
      File_Name           : Segment;
      Start_Position      : Point;
      Key                 : Buffer_String;

      End_Position        : Point;
      Attributes          : SN_Attributes;
--  ??? Why are these fields commented out
--      Template_Parameters : Segment;
--      Comments        : Segment;
--      Data                : Buffer_String;

      DBI                 : Integer;
   end record;
   --  Interface to the ".cl" tables.
   --  Used for C++ classes and struct, and Fortran structures.
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{}?{class template}?{}?{comment}

   type CON_Table is record
      Name                : Segment;
      File_Name           : Segment;
      Start_Position      : Point;
      Key                 : Buffer_String;

--      End_Position        : Point;
--      Attributes          : SN_Attributes;
      Declared_Type       : Segment;
      Type_Start_Position : Point;
--      Comments            : Segment;
      Data                : Buffer_String;

      DBI                 : Integer;
   end record;
   --  Interface to the ".con" tables.
   --  Used for C++ #define, const and static final constructs.
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{dec_type}?{}?{}?{comment}

   type E_Table is record
      Name                : Segment;
      File_Name           : Segment;
      Start_Position      : Point;
      Key                 : Buffer_String;

      End_Position        : Point;
--      Attributes          : SN_Attributes;
--      Comments        : Segment;
--      Data                : Buffer_String;

      DBI                 : Integer;
   end record;
   --  Interface to the ".e" tables.
   --  Used for C++ enumerations.
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{}?{}?{}?{comment}

   type EC_Table is record
      Name                : Segment;
      File_Name           : Segment;
      Start_Position      : Point;
      Key                 : Buffer_String;

--      End_Position        : Point;
--      Attributes          : SN_Attributes;
      Enumeration_Name    : Segment;
      Data                : Buffer_String;

      DBI                 : Integer;
   end record;
   --  Interface to the ".ec" tables.
   --  Used for C++ enumeration values
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{enum_name}?{}?{}?{comment}

   type F_Table is record
      File_Name           : Segment;
      Key                 : Buffer_String;

      Language            : Segment;
      Timestamp           : Segment;
      Data                : Buffer_String;
   end record;
   --  Interface to the ".f" tables.
   --  Used for list of files in a project

   type FD_Table is record
      Name                : Segment;
      File_Name           : Segment;
      Start_Position      : Point;
      Key                 : Buffer_String;

      End_Position        : Point;
      Attributes          : SN_Attributes;
      Return_Type         : Segment;
      Arg_Types           : Segment;
--      Arg_Names : Segment_Vector.Node_Access;
--      Comments            : Segment;
--      Template_Parameters : Segment;
      Data                : Buffer_String;

      DBI                 : Integer;
   end record;
   --  Interface to the ".fd" tables.
   --  Used for C++ function declarations.
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{ret_type}?{arg_types}?{arg_names}?
   --            {comment}

   type FIL_Table is record
      File_Name                : Segment;
      Start_Position           : Point;
      Class                    : Segment;
      Identifier               : Segment;
      Symbol                   : Symbol_Type;
      Key                      : Buffer_String;

      End_Position             : Point;
      Highlight_Start_Position : Point;
      Highlight_End_Position   : Point;
      Types_Of_Arguments       : Segment;
      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".fil" tables.
   --  Used for symbols of files.
   --  Format is the following:
   --     key  => filename?start_position?class?identifier?type
   --     data => end_position?high_start_pos?high_end_pos?arg_types

   type FR_Table is record
      Name                     : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

      End_Position             : Point;
      Attributes               : SN_Attributes;
      Return_Type              : Segment;
      Arg_Types                : Segment;
      --  Arg_Names            : Segment_Vector.Node_Access;
      --  Comments             : Segment;
      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".fr" tables.
   --  Used for friend classes
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{ret_type}?{arg_types}?{arg_names}?\
   --            {comment}

   type FU_Table is record
      Class                    : Segment;
      Name                     : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

      End_Position             : Point;
      Attributes               : SN_Attributes;
      Return_Type              : Segment;
      Arg_Types                : Segment;
      Arg_Names                : Segment;
--      Comments                 : Segment;
--      Template_Parameters      : Segment;
      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   No_FU_Table : constant FU_Table;
   --  Interface to ".fu" tables and ".mi" tables
   --  Used to describe function and method implementations.
   --  Class is left to '#' for functions
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{ret_type}?{arg_types}?{arg_names}?
   --             {comment}

   type GV_Table is record
      Name                     : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

--      End_Position             : Point;
--      Attributes               : SN_Attributes;
      Value_Type               : Segment;
      Class                    : Segment;
--      Template_Parameters  : Segment;
--      Comments                 : Segment;
      Type_Start_Position      : Point;
      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".gv" tables.
   --  Used to describe global variables.
   --  Format is the following:
   --     key  => name?position?filename
   --     data => end_position?attributes?{type}?{template?parameter}?{comment}

   type IN_Table is record
      Class                    : Segment;
      Base_Class               : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

      End_Position             : Point;
      Attributes               : SN_Attributes;
      --  Class_Template       : Segment
      --  Comments             : Segment;
--      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".in" tables.
   --  Used to describe inheritance
   --  Format is the following:
   --     key  => class?base-class?start_position?filename
   --     data => end_position?attributes?{}?{class template}?
   --            {inheritance?template}?{comment}

   type IU_Table is record
      Included_File            : Segment;
      Included_From_File       : Segment;
      Included_At_Position     : Point;
      Key                      : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".iu" tables.
   --  Used to describe includes.
   --  Format is the following:
   --     key  => included_file?start_position?include_from_file
   --     data => 0.0?0x0?{}?{}?{}?{}

   type IV_Table is record
      Class                   : Segment;
      Name                    : Segment;
      File_Name               : Segment;
      Start_Position          : Point;
      Key                     : Buffer_String;

--      End_Position            : Point;
--      Attributes              : SN_Attributes;
      Value_Type              : Segment;
      --  Comments            : Segment;
      Data                    : Buffer_String;

      DBI                     : Integer;
   end record;
   --  Interface to the ".iv" tables.
   --  Used to describe instance variables
   --  Format is the following:
   --     key  => class?variable-name?start_position?filename
   --     data => end_position?attributes?{type}?{}?{}?{comment}

   type LV_Table is record
      Function_Name           : Segment;
      Name                    : Segment;
      File_Name               : Segment;
      Start_Position          : Point;
      Key                     : Buffer_String;

--      End_Position            : Point;
--      Attributes              : SN_Attributes;
      Class                   : Segment;
      Value_Type              : Segment;
      Arg_Types               : Segment;
--      Comments                : Segment;
--      Type_Start_Position     : Point;
      Data                    : Buffer_String;

      DBI                     : Integer;
   end record;
   --  Interface to the ".lv" tables.
   --  Used to describe local variables
   --  Format is the following:
   --     key  => function?variable-name?start_position?filename
   --     data => end_position?attributes?{class}?{type}?{args}?{comment}

   type MA_Table is record
      Name                    : Segment;
      File_Name               : Segment;
      Start_Position          : Point;
      Key                     : Buffer_String;

      End_Position            : Point;
--      Attributes              : SN_Attributes;
      --  Comments            : Segment;
--      Data                    : Buffer_String;

      DBI                     : Integer;
   end record;
   --  Interface to the ".ma" tables
   --  Used to describe macros
   --  Format is the following:
   --     key  => name?start_position?filename
   --     data => end_position?attributes?{}?{}?{}?{comment}

   subtype MI_Table is FU_Table;
   --  Interface to ".mi" tables
   --  This is used for methods implementations
   --  Format is the following:
   --     key  => class?name?start_position?filename
   --     data => end_position?attributes?{ret_type}?{arg_types}?{arg_names}?
   --             {comment}

   type MD_Table is record
      Class                    : Segment;
      Name                     : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

      End_Position             : Point;
      Attributes               : SN_Attributes;
      Return_Type              : Segment;
      Arg_Types                : Segment;
      Arg_Names                : Segment;
--      Template_Parameters      : Segment;
--      Comments                 : Segment;
      Data                     : Buffer_String;

      DBI                      : Integer;
   end record;
   --  Interface to the ".md" tables
   --  This is used to describe method definitions
   --  Format is the following:
   --     key  => class?name?start_position?filename
   --     data => end_position?attributes?{ret_type}?{arg_types}?{arg_names}?
   --            {comment}

   type T_Table is record
      Name                     : Segment;
      File_Name                : Segment;
      Start_Position           : Point;
      Key                      : Buffer_String;

--      End_Position             : Point;
--      Attributes               : SN_Attributes;
      Original                 : Segment; -- either "type" or "A::type"
--      Comments                 : Segment;
--      Class_Name               : Segment; -- name of enclosing class for Name
      Data                     : Buffer_String;
      Data_Size                : Integer;  --  Last significant char in Data

      DBI                      : Integer;
   end record;
   --  Interface to the ".t" tables
   --  This is used to describe typedefs
   --  Format is the following:
   --     key  => name?position?filename
   --     data => end_position?attributes?{original}?{}?{comment}

   type TA_Table is record
      Class                 : Segment; -- name of the class or function/method
      Name                   : Segment;
      File_Name              : Segment;
      Start_Position         : Point;
      Key                    : Buffer_String;

      Type_Position          : Point;
--      Attributes             : SN_Attributes;
      Value_Type             : Segment;
--      Template_Parameters    : Segment;
--      Class_Name             : Segment; -- name of method's class
--      Comments               : Segment;
      Data                   : Buffer_String;

      DBI                    : Integer;
   end record;
   --  Interface to the ".ta" tables.
   --  This is used to describe template arguments
   --  Format is the following:
   --     key  => scope?name?start_position?filename
   --     data => type_position?attributes?{type}?{template_params}?
   --             {comment}

   type TO_Table is record
      Class                   : Segment;
      Symbol_Name             : Segment;
      Symbol                  : Symbol_Type;
      Referred_Class          : Segment;
      Referred_Symbol_Name    : Segment;
      Referred_Symbol         : Symbol_Type;
      Access_Type             : Segment;
      File_Name               : Segment;
      Position                : Point;
      Key                     : Buffer_String;

      Caller_Argument_Types   : Segment;
      Referred_Argument_Types : Segment;
      Data                    : Buffer_String;

      DBI                     : Integer;
   end record;
   --  Interface to ".to" tables.
   --  This is used to describe cross-references
   --  Format is the following:
   --    key  => class?symbol-name?type?ref-class?ref-symbol?ref-type?access?
   --            position?filename
   --    data => {caller_argument_types}?{ref_argument_types}

   subtype UN_Table is CL_Table;
   --  Interface to the ".un" tables.
   --  This is used to describe unions

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Key);
   --  Only applicable to CL, CON, E, EC, FD, FT, GV, MA, T, UN

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Class_Key);
   --  Only applicable to IV, FU, MD, MI

   procedure Parse_Key (Key_Data_Pair : Pair; Key : out Entity_Function_Key);
   --  Only applicable to LV

   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CL_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out CON_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out E_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out EC_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out F_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FD_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FIL_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FR_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out FU_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out GV_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IN_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IU_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out IV_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out LV_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MA_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out MD_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out T_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TA_Table);
   procedure Parse_Pair (Key_Data_Pair : Pair; Tab : out TO_Table);
   --  These functions provide the necessary functions to parse the contents
   --  of the Source Navigator tables

   function Get_Class_Name (Key : Buffer_String; Seg : Segment) return String;
   --  Return the class name (or '#' if Seg is Empty_Segment

private
   No_FU_Table : constant FU_Table :=
                   (Class          => Invalid_Segment,
                    Name           => Invalid_Segment,
                    File_Name      => Invalid_Segment,
                    Start_Position => Invalid_Point,
                    Key            => Null_Buffer,
                    End_Position   => Invalid_Point,
                    Attributes     => 0,
                    Return_Type    => Invalid_Segment,
                    Arg_Types      => Invalid_Segment,
                    Arg_Names      => Invalid_Segment,
                    Data           => Null_Buffer,
                    DBI            => 0);
end SN.DB_Structures;
