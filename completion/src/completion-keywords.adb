-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
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

with Glib.Unicode; use Glib.Unicode;
with GPS.Intl;     use GPS.Intl;

package body Completion.Keywords is

   Resolver_ID : constant String := "Keywords";

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   procedure Get_Completion_Root
     (Resolver   : access Completion_Keywords;
      Offset     : Integer;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      Proposal : Simple_Completion_Proposal;
      List     : Completion_List_Extensive_Pckg.Extensive_List_Pckg.List;
      Keywords : constant String_List := Language.Keywords (Context.Lang);
      Word     : UTF8_String (Offset + 1 .. Context.Offset) :=
                   Context.Buffer (Offset + 1 .. Context.Offset);
      Doc      : constant String :=
                   -(Get_Name (Context.Lang) & " keyword.");
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
                            Category => Cat_Custom,
                            Documentation => new String'(Doc));

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

   function Get_Id
     (Resolver : Completion_Keywords)
      return String
   is
      pragma Unreferenced (Resolver);
   begin
      return Resolver_ID;
   end Get_Id;

   ----------
   -- Free --
   ----------

   procedure Free
     (Resolver : in out Completion_Keywords)
   is
   begin
      null;
   end Free;

end Completion.Keywords;
