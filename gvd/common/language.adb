-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Unchecked_Deallocation;
with Gtkada.Types; use Gtkada.Types;
with Odd.Types;    use Odd.Types;
with GNAT.Regpat;  use GNAT.Regpat;

package body Language is

   type Language_Detection_Record is record
      Pattern  : String_Access;
      Language : Language_Access;
   end record;
   type Language_Detection_Array is array (Positive range <>)
     of Language_Detection_Record;
   type Language_Detection_Access is access Language_Detection_Array;

   Language_Detection : Language_Detection_Access;
   --  Global table that indicates which languages to associate with file
   --  names, so that one can find the best language associates with a
   --  given file name.

   ---------------------
   -- Break_Exception --
   ---------------------

   function Break_Exception
     (Debugger  : access Language_Root;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String is
   begin
      return "";
   end Break_Exception;

   -----------
   -- Start --
   -----------

   function Start
     (Debugger  : access Language_Root)
     return String
   is
   begin
      return "";
   end Start;

   ----------
   -- Free --
   ----------

   procedure Free (Lang : in out Language_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Language_Root'Class, Language_Access);
   begin
      Internal (Lang);
   end Free;

   ----------------
   -- Looking_At --
   ----------------

   procedure Looking_At
     (Lang      : access Language_Root;
      Buffer    : String;
      Entity    : out Language_Entity;
      Next_Char : out Positive) is
   begin
      Next_Char := Buffer'First + 1;
      Entity := Normal_Text;
   end Looking_At;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps (Lang : access Language_Root)
                             return Explorer_Categories
   is
      E : Explorer_Categories (1 .. 0);
   begin
      return E;
   end Explorer_Regexps;

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Thread_Information_Array) is
   begin
      for J in Info'Range loop
         Free (Info (J).Information);
      end loop;
   end Free;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang : access Language_Root;
      File_Name : String)
     return Boolean
   is
   begin
      return False;
   end Is_System_File;

   -----------------
   -- Thread_List --
   -----------------

   function Thread_List (Lang : access Language_Root) return String is
   begin
      raise Program_Error;
      return "";
   end Thread_List;

   -------------------
   -- Thread_Switch --
   -------------------

   function Thread_Switch
     (Lang   : access Language_Root;
      Thread : Natural) return String is
   begin
      raise Program_Error;
      return "";
   end Thread_Switch;

   -----------------------
   -- Parse_Thread_List --
   -----------------------

   function Parse_Thread_List
     (Lang   : access Language_Root;
      Output : String) return Thread_Information_Array
   is
      Null_Array : Thread_Information_Array (1 .. 0);
   begin
      raise Program_Error;
      return Null_Array;
   end Parse_Thread_List;

   ------------------------
   -- Add_File_Extension --
   ------------------------

   procedure Add_File_Extension
     (Language : Language_Access;
      Pattern  : String)
   is
      procedure Free is new Unchecked_Deallocation
        (Language_Detection_Array, Language_Detection_Access);
      Table_Index : Positive := Positive'Last;
      Tmp         : Language_Detection_Access;
      T           : String_Access;
   begin
      --  Do we already have this language in the table
      if Language_Detection /= null then
         for Index in Language_Detection'Range loop
            if Language_Detection (Index).Language = Language then
               Table_Index := Index;
               exit;
            end if;
         end loop;
      end if;

      --  If not, extend the table
      if Table_Index = Positive'Last then
         if Language_Detection = null then
            Tmp := new Language_Detection_Array (1 .. 1);
            Table_Index := 1;
         else
            Tmp := new Language_Detection_Array
              (Language_Detection'First .. Language_Detection'Last + 1);
            Tmp (Language_Detection'Range) := Language_Detection.all;
            Table_Index := Tmp'Last;
         end if;
         Free (Language_Detection);
         Language_Detection := Tmp;
      end if;

      --  Add the new item in the table
      Language_Detection (Table_Index).Language := Language;

      if Language_Detection (Table_Index).Pattern = null then
         Language_Detection (Table_Index).Pattern :=
           new String'("(" & Pattern & ")");
      else
         T := Language_Detection (Table_Index).Pattern;
         Language_Detection (Table_Index).Pattern :=
           new String'(T.all & "|(" & Pattern & ")");
         Free (T);
      end if;
   end Add_File_Extension;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File (File_Name : String)
     return Language_Access
   is
   begin
      if Language_Detection /= null then
         for Index in Language_Detection'Range loop
            if Match (Language_Detection (Index).Pattern.all, File_Name) then
               return Language_Detection (Index).Language;
            end if;
         end loop;
      end if;
      return null;
   end Get_Language_From_File;

end Language;
