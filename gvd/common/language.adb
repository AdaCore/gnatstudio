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

package body Language is

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

   -----------------
   -- Dereference --
   -----------------

   function Dereference
     (Lang     : access Language_Root;
      Variable : String) return String
   is
      pragma Warnings (Off, Lang);
   begin
      return Variable;
   end Dereference;

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

end Language;
