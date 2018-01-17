------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Glib.Unicode;          use Glib.Unicode;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Completion.Aliases is

   function "=" (A, B : String) return Boolean
                 renames Ada.Strings.Fixed.Equal_Case_Insensitive;

   overriding function Deep_Copy
     (Proposal : Alias_Completion_Proposal)
      return Completion_Proposal'Class is
   begin
      return Alias_Completion_Proposal'
        (Simple_Completion_Proposal
           (Completion.Deep_Copy (Simple_Completion_Proposal (Proposal)))
         with Alias => Proposal.Alias);
   end Deep_Copy;

   overriding function Get_Action_Name
     (Proposal : Alias_Completion_Proposal) return String
   is ("Expand alias under cursor");

   overriding function Get_Documentation
     (Proposal : Alias_Completion_Proposal) return String
   is ("<b>Alias</b> " & Proposal.Alias.Get_Expansion);

   overriding function Get_Custom_Icon_Name
     (Proposal : Alias_Completion_Proposal) return String
   is ("gps-emblem-alias-symbolic");

   overriding function Get_Label
     (Proposal : Alias_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class) return String
   is
      pragma Unreferenced (Db);
   begin
      return Proposal.Name.all & " (alias)";
   end Get_Label;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access Completion_Aliases;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      Proposal : Alias_Completion_Proposal;
      List     : Completion_List_Extensive_Pckg.Extensive_List_Pckg.Vector;
      Word     : UTF8_String
        (Natural (Offset) + 1 .. Natural (Context.Offset)) :=
           Context.Buffer (Natural (Offset) + 1 .. Natural (Context.Offset));
   begin
      if not Get_Language_Context (Context.Lang).Case_Sensitive then
         Word := UTF8_Strdown (Word);
      end if;

      for Alias of Get_Aliases_List loop
         declare
            Name : constant String := Alias.Get_Name;
         begin
            if Name'Length >= Word'Length
              and then
                Name (Name'First .. Name'First + Word'Length - 1) = Word
            then
               Proposal := (Resolver => Resolver,
                            Name     => new String'(Alias.Get_Name),
                            Category => Cat_Custom,
                            Alias    => Alias);
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
     (Resolver : Completion_Aliases)
      return String
   is ("Aliases");

end Completion.Aliases;
