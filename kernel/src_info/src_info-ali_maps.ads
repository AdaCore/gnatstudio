-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
--  ??? Document the purpose of this package

private package Src_Info.ALI_Maps is

   --  This package has been made private because it is using some
   --  declarations which are in the private section of Src_Info.
   --  The definitions below are defined in this package rather than
   --  in Src_Info because they are specific to the both the Ada language
   --  and the ALI file format. And finally, these definitions can not
   --  be put in the Src_Info.ALI package because this information is
   --  also used by routines that dump routines defined in Src_Info.Debug.

   type E_Kind_To_Char_Map is array (E_Kind) of Character;

   E_Kind_To_Char : constant E_Kind_To_Char_Map :=
     (Access_Object                    => 'p',
      Access_Type                      => 'P',
      Array_Object                     => 'a',
      Array_Type                       => 'A',
      Boolean_Object                   => 'b',
      Boolean_Type                     => 'B',
      Class_Wide_Object                => 'c',
      Class_Wide_Type                  => 'C',
      Decimal_Fixed_Point_Object       => 'd',
      Decimal_Fixed_Point_Type         => 'D',
      Entry_Or_Entry_Family            => 'Y',
      Enumeration_Literal              => 'n',
      Enumeration_Object               => 'e',
      Enumeration_Type                 => 'E',
      Exception_Entity                 => 'X',
      Floating_Point_Object            => 'f',
      Floating_Point_Type              => 'F',
      Generic_Function_Or_Operator     => 'v',
      Generic_Package                  => 'k',
      Generic_Procedure                => 'u',
      Label_On_Block                   => 'q',
      Label_On_Loop                    => 'l',
      Label_On_Statement               => 'L',
      Modular_Integer_Object           => 'm',
      Modular_Integer_Type             => 'M',
      Named_Number                     => 'N',
      Non_Generic_Function_Or_Operator => 'V',
      Non_Generic_Package              => 'K',
      Non_Generic_Procedure            => 'U',
      Ordinary_Fixed_Point_Object      => 'o',
      Ordinary_Fixed_Point_Type        => 'O',
      Private_Type                     => '+',
      Protected_Object                 => 'w',
      Protected_Type                   => 'W',
      Record_Object                    => 'r',
      Record_Type                      => 'R',
      Signed_Integer_Object            => 'i',
      Signed_Integer_Type              => 'I',
      String_Object                    => 's',
      String_Type                      => 'S',
      Task_Object                      => 't',
      Task_Type                        => 'T');

   type Reference_Kind_To_Char_Map is array (Reference_Kind) of Character;

   Reference_Kind_To_Char : constant Reference_Kind_To_Char_Map :=
     (Reference                                => 'r',
      Modification                             => 'm',
      Body_Entity                              => 'b',
      Completion_Of_Private_Or_Incomplete_Type => 'c',
      Type_Extension                           => 'x',
      Implicit                                 => 'i',
      End_Of_Spec                              => 'e',
      End_Of_Spec_With_Label                   => 'E',
      End_Of_Body                              => 't',
      End_Of_Body_With_Label                   => 'T');

   type Reference_Kind_To_Boolean_Map is array (Reference_Kind) of Boolean;

   Is_End_Reference : constant Reference_Kind_To_Boolean_Map :=
     (Reference                                => False,
      Modification                             => False,
      Body_Entity                              => False,
      Completion_Of_Private_Or_Incomplete_Type => False,
      Type_Extension                           => False,
      Implicit                                 => False,
      End_Of_Spec                              => True,
      End_Of_Spec_With_Label                   => True,
      End_Of_Body                              => True,
      End_Of_Body_With_Label                   => True);

   type E_Scope_To_Char_Map is array (E_Scope) of Character;

   E_Scope_To_Char : constant E_Scope_To_Char_Map :=
     (Global_Scope => '*',
      Local_Scope  => ' ');

end Src_Info.ALI_Maps;
