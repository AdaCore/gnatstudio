-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2006                      --
--                              AdaCore                              --
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

--  Case support for case insensitive languages. This package has
--  services to change the casing of a word (identifier or keyword) and
--  to handle a set of casing exceptions.

with Glib;        use Glib;

with String_Hash;

package Case_Handling is

   type Casing_Policy is (Disabled, End_Of_Line, On_The_Fly);
   for Casing_Policy'Size use Integer'Size;
   pragma Convention (C, Casing_Policy);
   --  The list of supported casing policies.
   --  - Disable means that no auto-casing will be applied to the buffer
   --  - End_Of_Line casing done when pressing return
   --  - On_The_Fly casing is done when inserting a word separator

   type Casing_Type is (Unchanged, Upper, Lower, Mixed, Smart_Mixed);
   for Casing_Type'Size use Integer'Size;
   pragma Convention (C, Casing_Type);
   --  Casing used for identifiers and reserved words.
   --  Only relevant for case insensitive languages.
   --  - Mixed: Set first character of each word and characters after an
   --    underscore to upper-case, all other characters are set to lower-case.
   --  - Smart_Mixed: As Mixed but never force an upper-case to lower-case.

   procedure Mixed_Case (S : in out UTF8_String; Smart : Boolean := False);
   --  Return S with a casing matching Ada style: upper case after an
   --  underscore or a dot.
   --  If smart is set, do not change upper-case letters in S

   ---------------------
   -- Case Exceptions --
   ---------------------

   type Casing_Exceptions is private;
   --  This is the case exceptions handler, a set of exceptions to the
   --  standard casing rule can be recorded into this object.

   No_Casing_Exception : constant Casing_Exceptions;

   procedure Set_Case
     (C      : Casing_Exceptions;
      Word   : in out UTF8_String;
      Casing : Casing_Type);
   --  Change the case of Str as specified by Casing. This routine also
   --  checks for case exceptions.

   procedure Add_Exception
     (C         : in out Casing_Exceptions;
      Word      : String;
      Read_Only : Boolean);
   --  Add a case exception into the container. Read_Only must be set for
   --  case exception that can't be removed interactively.

   procedure Add_Substring_Exception
     (C         : in out Casing_Exceptions;
      Substring : String;
      Read_Only : Boolean);
   --  Add a substring case exception into the container. Read_Only must be set
   --  for case exception that can't be removed interactively.

   procedure Remove_Exception (C : in out Casing_Exceptions; Word : String);
   --  Remove a case exception from the container

   procedure Remove_Substring_Exception
     (C         : in out Casing_Exceptions;
      Substring : String);
   --  Remove a substring case exception from the container

   procedure Destroy (C : in out Casing_Exceptions);
   --  Destroy the case exceptions handler, release all memory associated
   --  with this object.

private

   type Word_Access is access String;

   type W_Node is record
      Read_Only : Boolean;
      --  Set to True if this case exception is read only (can't be removed).
      --  Such case exception comes from a global .xml files.
      Word      : Word_Access;
   end record;

   Null_Node : constant W_Node := (False, null);

   procedure Free (N : in out W_Node);

   package Casing_Exception_Table is new String_Hash (W_Node, Free, Null_Node);
   use Casing_Exception_Table;

   type Exceptions_Table is access String_Hash_Table.HTable;
   --  Exception Word handler, each exception is inserted into this hash
   --  table. The key is the word in lower-case, the associated
   --  value is the word with the right casing.

   type Casing_Exceptions is record
      E : Exceptions_Table := new String_Hash_Table.HTable;
      S : Exceptions_Table := new String_Hash_Table.HTable;
   end record;

   No_Casing_Exception : constant Casing_Exceptions := (E => null, S => null);

end Case_Handling;
