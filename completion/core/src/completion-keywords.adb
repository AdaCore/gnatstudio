------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Glib.Unicode; use Glib.Unicode;

package body Completion.Keywords is

   Resolver_ID : constant String := "Keywords";

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access Completion_Keywords;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      Proposal : Simple_Completion_Proposal;
      List     : Completion_List_Extensive_Pckg.Extensive_List_Pckg.Vector;
      Keywords : constant String_List := Language.Keywords (Context.Lang);
      Word     : UTF8_String
        (Natural (Offset) + 1 .. Natural (Context.Offset)) :=
           Context.Buffer (Natural (Offset) + 1 .. Natural (Context.Offset));
   begin
      if not Get_Language_Context (Context.Lang).Case_Sensitive then
         Word := UTF8_Strdown (Word);
      end if;

      for J in Keywords'Range loop
         declare
            K : String renames Keywords (J).all;
         begin
            if K'Length >= Word'Length
              and then K (K'First .. K'First + Word'Length - 1) = Word
            then
               Proposal := (Resolver => Resolver,
                            Name     => Keywords (J),
                            Category => Cat_Unknown);

               Completion_List_Extensive_Pckg.Extensive_List_Pckg.Append
                 (List, Proposal);
            end if;
         end;
      end loop;

      Completion_List_Pckg.Append
        (Result.List, Completion_List_Extensive_Pckg.To_Extensive_List (List));
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : Completion_Keywords)
      return String
   is
      pragma Unreferenced (Resolver);
   begin
      return Resolver_ID;
   end Get_Id;

end Completion.Keywords;
