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

with Generic_List;

with Codefix.Text_Manager; use Codefix.Text_Manager;

package Codefix.Ada_Tools is

   package Words_Lists is new Generic_List (Word_Cursor);
   use Words_Lists;

   function Get_Use_Clauses
     (Clause_Name  : String;
      File_Name    : String;
      Current_Text : Text_Navigator_Abstr'Class;
      Exclusive    : Boolean := False)
   return Words_Lists.List;
   --  Returns all the use causes that are related to a with or an
   --  instantiation name. If Exclusive is  true, then only uses clauses
   --  that are not linked to any other with will be returned.

private

   type Use_Type is record
      Position : File_Cursor;
      Name     : Dynamic_String;
      Nb_Ref   : Natural := 0;
   end record;

   type Ptr_Use is access all Use_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Use_Type, Ptr_Use);
   --  ??? Has to be changed

   type Arr_Use is array (Natural range <>) of Ptr_Use;

   type Arr_Str is array (Natural range <>) of Dynamic_String;

   type With_Type (Nb_Elems : Natural) is record
      Name_Str : Dynamic_String;
      Name     : Arr_Str (1 .. Nb_Elems);
      Clauses  : Arr_Use (1 .. Nb_Elems);
   end record;

   type Ptr_With is access all With_Type;
   procedure Free is new Ada.Unchecked_Deallocation (With_Type, Ptr_With);
   --  ??? Has to be changed

   package With_Lists is new Generic_List (Ptr_With);
   use With_Lists;

   package Use_Lists is new Generic_List (Ptr_Use);
   use Use_Lists;

   function Get_Parts_Number (Str : String) return Positive;
   function Get_Arr_Str (Str : String) return Arr_Str;
   procedure Try_Link_Clauses
     (With_Clause : Ptr_With; Use_Clause  : Ptr_Use);

   function List_All_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String)
     return With_Lists.List;

   function List_All_Use
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : String)
     return Use_Lists.List;

   procedure Link_All_Clauses
     (List_Of_With : in out With_Lists.List;
      List_Of_Use  : in out Use_Lists.List);

end Codefix.Ada_Tools;
