------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2018, AdaCore                   --
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

with System.Storage_Elements; use System.Storage_Elements;

package body GNATCOLL.Symbols.Streamable_Symbol_Table is

   ---------
   -- "+" --
   ---------

   function "+" (S : Streamable_Symbol) return Symbol is (Symbol (S));
   function "+" (S : Symbol) return Streamable_Symbol
   is (Streamable_Symbol (S));

   ----------
   -- Find --
   ----------

   function Find
     (Table : access Symbol_Table_Type;
      Str   : String) return Streamable_Symbol
   is
     (+Symbol'(Table.Find (Str)));

   ---------
   -- Get --
   ---------

   overriding function Get
     (Sym : Streamable_Symbol; Empty_If_Null : Boolean := True)
      return Cst_String_Access
   is
     (Get (+Sym, Empty_If_Null));

   function Get
     (Sym : Streamable_Symbol; Empty_If_Null : Boolean := True)
      return String
   is
     (Get (Sym, Empty_If_Null).all);

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (S : Streamable_Symbol) return Ada.Containers.Hash_Type
   is
     (Hash_Type
        (To_Integer (Get (S).all'Address) mod
             Integer_Address (Hash_Type'Last)));
   --  For this version of the symbol table, we'll use a hash based on the
   --  allocated string's address. Since the table is generic and global,
   --  the instantiated version of streamable symbol cannot be confused
   --  with symbols from other tables (barring unchecked conversions of course)

   -------------------
   -- Output_Symbol --
   -------------------

   procedure Output_Symbol
     (Stream   : not null access Root_Stream_Type'Class;
      Instance : Streamable_Symbol) is
   begin
      String'Output (Stream, Get (Instance));
   end Output_Symbol;

   ------------------
   -- Input_Symbol --
   ------------------

   function Input_Symbol
     (Stream   : not null access Root_Stream_Type'Class)
      return Streamable_Symbol is
   begin
      return Symbol_Table.Find (String'Input (Stream));
   end Input_Symbol;

   ------------------
   -- Input_Symbol --
   ------------------

   procedure Input_Symbol
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Streamable_Symbol) is
   begin
      Item := Input_Symbol (Stream);
   end Input_Symbol;

end GNATCOLL.Symbols.Streamable_Symbol_Table;
