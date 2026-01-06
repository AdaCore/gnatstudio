------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
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

with Glib.Convert;          use Glib.Convert;
with Glib.Unicode;          use Glib.Unicode;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

with GPS.Kernel.Actions;    use GPS.Kernel.Actions;

package body Completion.Aliases is

   function "=" (A, B : String) return Boolean
                 renames Ada.Strings.Fixed.Equal_Case_Insensitive;

   ---------------
   -- Deep_Copy --
   ---------------

   overriding function Deep_Copy
     (Proposal : Alias_Completion_Proposal)
      return Completion_Proposal'Class is
   begin
      return Alias_Completion_Proposal'
        (Simple_Completion_Proposal
           (Completion.Deep_Copy (Simple_Completion_Proposal (Proposal)))
         with Alias => Proposal.Alias);
   end Deep_Copy;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : Alias_Completion_Proposal) return String
   is
     ("<b>Alias</b> " & Escape_Text (Proposal.Alias.Get_Expansion));

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : Alias_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class) return String
   is
      pragma Unreferenced (Db);
   begin
      return Proposal.Name.all & " (alias)";
   end Get_Label;

   ---------------------
   -- Get_Filter_Text --
   ---------------------

   overriding function Get_Filter_Text
     (Proposal : Alias_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
      pragma Unreferenced (Db);
   begin
      return Proposal.Name.all;
   end Get_Filter_Text;

   -------------------
   -- Get_Sort_Text --
   -------------------

   overriding function Get_Sort_Text
     (Proposal : Alias_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
     ("~" & Get_Label (Proposal, Db));

   -----------------
   -- On_Selected --
   -----------------

   overriding procedure On_Selected
     (Proposal : Alias_Completion_Proposal;
      Kernel   : not null Kernel_Handle)
   is
      Action  : constant String := "Expand alias under cursor";
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Success := Execute_Action
        (Kernel               => Kernel,
         Action               => Action,
         Error_Msg_In_Console => True,
         Synchronous          => True);
   end On_Selected;
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
        (Natural (Offset) + 1 .. Natural (Context.End_Offset)) :=
          Context.Buffer
            (Natural (Offset) + 1 .. Natural (Context.End_Offset));
   begin
      if not Get_Language_Context (Context.Lang).Case_Sensitive then
         Word := UTF8_Strdown (Word);
      end if;

      --  Don't propose aliases completion on empty words, dotted names,
      --  comments or strings.
      if Word = ""
        or else Context.In_Comment
        or else Context.In_String
        or else
        (Offset > 0
         and then Context.Buffer (Natural (Offset)) in '.' | ':')
      then
         return;
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
