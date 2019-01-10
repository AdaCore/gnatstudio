------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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
with Ada.Characters.Latin_1;
with System.Machine_Code;

package body GPS.Valgrind.Binding is

   -----------------------
   -- Do_Client_Request --
   -----------------------

   procedure Do_Client_Request (Kind : Client_Request_Kinds) is
      use Ada.Characters.Latin_1;

      Default : constant Word := 0;
      Args : array (0 .. 5) of Word := (Map (Kind), others => 0);
      Result : Word;
   begin
      System.Machine_Code.Asm
        (Template => "roll $3,{%%}edi; roll $13,{%%}edi;" & LF &
           HT & "roll $29,{%%}edi; roll $19,{%%}edi;" & LF &
           --  %RDX = client_request ( %RAX )
           HT & "xchgl {%%}ebx, {%%}ebx",
         Outputs => Word'Asm_Output ("=d", Result),
         Inputs  =>
           (System.Address'Asm_Input ("a", Args'Address),
            Word'Asm_Input ("0", Default)),
         Clobber => "memory");
   end Do_Client_Request;

end GPS.Valgrind.Binding;
