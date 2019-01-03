------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

--  This package implements a global streamable symbol table. It is global
--  because:
--
--  1. It makes sense for a symbol table most of the time.
--
--  2. It is easy to make it streamable.
--
--  The modus operandi is as follows:
--
--  - When writing symbols, we just write their string value in the file
--  - When reading symbol, we create a new symbol from the read string value.
--
--  This is based on GNATCOLL's symbol table, and this is why this module is
--  a child module of GNATCOLL.Symbols. Since we create a new private Symbol
--  type, we need to stub every operation for the symbol table, which is easy
--  since (fortunately) GNATCOLL's symbol table has few.
--
--  The module is generic because this way, every instantiation will have its
--  own symbol type, incompatible with the others. This allows us to use an
--  address based hash safely.

with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with Ada.Streams; use Ada.Streams;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with Ada.Containers; use Ada.Containers;

generic
package GNATCOLL.Symbols.Streamable_Symbol_Table is

   type Streamable_Symbol is private;
   type Symbol_Table_Type (<>) is new Symbol_Table_Record with private;
   type Symbol_Table_Access is access all Symbol_Table_Type;

   function "+" (S : Streamable_Symbol) return Symbol;
   function "+" (S : Symbol) return Streamable_Symbol;

   function Find
     (Table : access Symbol_Table_Type;
      Str   : String) return Streamable_Symbol;
   --  Find the symbol represented by Str in the symbol table, creating it if
   --  it doesn't exist, and return it

   function Get
     (Sym : Streamable_Symbol; Empty_If_Null : Boolean := True)
      return Cst_String_Access;
   --  Get the string represented by Sym

   function Get
     (Sym : Streamable_Symbol; Empty_If_Null : Boolean := True)
      return String;
   --  Get the string represented by Sym

   function Hash
     (S : Streamable_Symbol) return Ada.Containers.Hash_Type;
   --  Return a hash for this symbol. Since those tables are global, and
   --  Streamable_Symbol can *only* be used in this table, this is safe.

   Symbol_Table : constant Symbol_Table_Access;
   --  Instance of the symbol table.

private

   type Streamable_Symbol is new Symbol;
   type Symbol_Table_Type is new Symbol_Table_Record with null record;

   Symbol_Table : constant Symbol_Table_Access := new Symbol_Table_Type;

   procedure Output_Symbol
     (Stream   : not null access Root_Stream_Type'Class;
      Instance : Streamable_Symbol);

   function Input_Symbol
     (Stream   : not null access Root_Stream_Type'Class)
      return Streamable_Symbol;

   procedure Input_Symbol
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Streamable_Symbol);

   for Streamable_Symbol'Input  use Input_Symbol;
   for Streamable_Symbol'Output use Output_Symbol;

   for Streamable_Symbol'Read  use Input_Symbol;
   for Streamable_Symbol'Write use Output_Symbol;

end GNATCOLL.Symbols.Streamable_Symbol_Table;
