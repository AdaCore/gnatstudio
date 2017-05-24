------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides some tools that can be used in ada formal
--  errors and commands.

with GPS_Vectors;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with Codefix.Text_Manager;  use Codefix.Text_Manager;

package Codefix.Ada_Tools is

   package Words_Lists is new GPS_Vectors (Word_Cursor);
   use Words_Lists;

   procedure Get_Use_Clauses
     (Clause_Name  : String;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Current_Text : Text_Navigator_Abstr'Class;
      Exclusive    : Boolean := False;
      Result       : out Words_Lists.Vector);
   --  Return all the use clauses as Result that are related to a with or an
   --  instantiation name. If Exclusive is true, then only use clauses
   --  that are not linked to any other will be returned.

   procedure Get_Use_Duplicates_On_Line
     (Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Result       : out Words_Lists.Vector);
   --  Return use clauses which are placed in the same line and aren't first
   --  use clauses for this item in the unit

   function Get_Next_With_Position
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File) return File_Cursor'Class;

   function Search_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Pkg_Name     : String) return File_Cursor'Class;

private

   type Use_Type is record
      Position : File_Cursor;
      Name     : Unbounded_String;
      Nb_Ref   : Natural := 0;
   end record;

   type Ptr_Use is access all Use_Type;

   procedure Free (This : in out Ptr_Use);

   type Arr_Use is array (Natural range <>) of Ptr_Use;

   type Arr_Str is array (Natural range <>) of Unbounded_String;
   --  ??? Should use subprogram in basic_types.ads

   type With_Type (Nb_Elems : Natural) is record
      Name_Str : Unbounded_String;
      Name     : Arr_Str (1 .. Nb_Elems);
      Clauses  : Arr_Use (1 .. Nb_Elems);
   end record;

   type Ptr_With is access all With_Type;

   procedure Free is new Ada.Unchecked_Deallocation (With_Type, Ptr_With);

   package With_Lists is new GPS_Vectors (Ptr_With);
   use With_Lists;

   package Use_Lists is new GPS_Vectors (Ptr_Use);
   use Use_Lists;

   function Get_Parts_Number (Str : String) return Positive;
   --  Return the number of parts separed with dots in String.

   function Get_Arr_Str (Str : String) return Arr_Str;
   --  Return an array in witch each line is initialized with a part of Str.

   procedure Try_Link_Clauses
     (With_Clause : Ptr_With; Use_Clause  : Ptr_Use);
   --  Make the link between With_Clause and Use_Clause if the use is a use of
   --  a package evocated in With_Clause.

   procedure List_All_With
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Result       : out With_Lists.Vector);
   --  List all the with clauses existing in File_Name.

   procedure List_All_Use
     (Current_Text : Text_Navigator_Abstr'Class;
      File_Name    : GNATCOLL.VFS.Virtual_File;
      Result       : out Use_Lists.Vector);
   --  List all the use clauses existing in File_Name.

   procedure Link_All_Clauses
     (List_Of_With : in out With_Lists.Vector;
      List_Of_Use  : in out Use_Lists.Vector);
   --  Link all with clauses to use clauses when possible.

end Codefix.Ada_Tools;
