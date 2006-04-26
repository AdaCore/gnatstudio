-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Completion is

   ----------
   -- Next --
   ----------

   function Next (Resolver : access Completion_Resolver'Class)
      return Completion_Resolver_Access
   is
      It : Completion_Resolver_List_Pckg.List_Node := First
        (Resolver.Manager.Resolvers);
   begin
      while It /= Completion_Resolver_List_Pckg.Null_Node loop
         if Data (It) = Completion_Resolver_Access (Resolver) then
            if Next (It) /= Completion_Resolver_List_Pckg.Null_Node then
               return Data (Next (It));
            else
               return null;
            end if;
         end if;

         It := Next (It);
      end loop;

      return null;
   end Next;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
     (Manager : in out Completion_Manager; Buffer : String_Access)
   is
   begin
      Manager.Buffer := Buffer;
   end Set_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer (Manager : Completion_Manager) return String_Access is
   begin
      return Manager.Buffer;
   end Get_Buffer;

   -----------------------
   -- Register_Resolver --
   -----------------------

   procedure Register_Resolver
     (Manager  : access Completion_Manager;
      Resolver : access Completion_Resolver'Class) is
   begin
      Append (Manager.Resolvers, Completion_Resolver_Access (Resolver));
      Resolver.Manager := Completion_Manager_Access (Manager);
   end Register_Resolver;

   ----------
   -- Free --
   ----------

   procedure Free (Resolver : in out Completion_Resolver_Access) is
      procedure Internal_Free is new
        Ada.Unchecked_Deallocation
          (Completion_Resolver'Class, Completion_Resolver_Access);
   begin
      Free (Resolver.all);
      Internal_Free (Resolver);
   end Free;

   -------------------
   -- Free_Proposal --
   -------------------

   procedure Free_Proposal (Proposal : in out Completion_Proposal'Class) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free_Proposal;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Proposal : in out Completion_Proposal; Mode : Proposal_Mode)
   is
   begin
      Proposal.Mode := Mode;
   end Set_Mode;

   ---------------------
   -- Get_Composition --
   ---------------------

   function Get_Composition
     (Proposal   : Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean) return Completion_List
   is
      Composition : constant Completion_List := Get_Composition
        (Completion_Proposal'Class (Proposal), Offset);
      It          : Completion_Iterator := First (Composition);
      Result_List : Completion_List;
   begin
      while It /= Null_Completion_Iterator loop
         declare
            Name : constant String := Get_Name (Get_Proposal (It));
         begin
            if Is_Partial then
               if Identifier'Length < Name'Length
                 and then Name
                   (Name'First .. Name'First + Identifier'Length - 1)
                 = Identifier
               then
                  Append (Result_List, Get_Proposal (It));
               end if;
            else
               if Identifier = Name then
                  Append (Result_List, Get_Proposal (It));
               end if;
            end if;

            It := Next (It);
         end;
      end loop;

      return Result_List;
   end Get_Composition;

   -----------------
   -- Get_Manager --
   ------------------

   function Get_Resolver (Proposal : Completion_Proposal)
     return Completion_Resolver_Access is
   begin
      return Proposal.Resolver;
   end Get_Resolver;

   ----------------------------
   -- Refine_Completion_List --
   ----------------------------

   function Refine_Completion_List
     (Previous_Completion : Completion_List;
      Character_Added     : Character) return Completion_List
   is
      pragma Unreferenced (Previous_Completion, Character_Added);

      Result : Completion_List;
   begin
      return Result;
   end Refine_Completion_List;

   -----------
   -- First --
   -----------

   function First (This : Completion_List) return Completion_Iterator is
   begin
      return Completion_Iterator
        (Completion_List_Pckg.First (Completion_List_Pckg.List (This)));
   end First;

   ----------
   -- Next --
   ----------

   function Next (This : Completion_Iterator) return Completion_Iterator is
   begin
      return Completion_Iterator
        (Completion_List_Pckg.Next (Completion_List_Pckg.List_Node (This)));
   end Next;

   ------------------
   -- Get_Proposal --
   ------------------

   function Get_Proposal (This : Completion_Iterator)
     return Completion_Proposal'Class is
   begin
      return Data (This);
   end Get_Proposal;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Proposal : Simple_Completion_Proposal) return String is
   begin
      return Proposal.Name.all;
   end Get_Name;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Proposal : Simple_Completion_Proposal)
      return Language_Category
   is
      pragma Unreferenced (Proposal);
   begin
      return Cat_Unknown;
   end Get_Category;

   ---------------------
   -- Get_Composition --
   ---------------------

   function Get_Composition
     (Proposal : Simple_Completion_Proposal; Offset : Positive)
      return Completion_List
   is
      pragma Unreferenced (Proposal, Offset);
   begin
      return Null_Completion_List;
   end Get_Composition;

   ---------------------
   -- Get_Composition --
   ---------------------

   function Get_Composition
     (Proposal   : Simple_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean) return Completion_List
   is
      pragma Unreferenced (Proposal, Identifier, Offset, Is_Partial);
   begin
      return Null_Completion_List;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters (Proposal : Simple_Completion_Proposal)
      return Natural
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Number_Of_Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Simple_Completion_Proposal) is
   begin
      Free (Proposal.Name);
   end Free;

   -----------
   -- Match --
   -----------

   function Match (Seeked_Name, Tested_Name : String; Is_Partial : Boolean)
      return Boolean
   is
      Lower_Seeked_Name : constant String := To_Lower (Seeked_Name);
      Lower_Tested_Name : constant String := To_Lower (Tested_Name);
   begin
      if Seeked_Name'Length > Tested_Name'Length then
         return False;
      end if;

      return (Is_Partial
              and then Lower_Tested_Name
                (Lower_Tested_Name'First
                 .. Lower_Tested_Name'First + Lower_Seeked_Name'Length - 1)
              = Lower_Seeked_Name)
        or else (not Is_Partial
                   and then Lower_Tested_Name = Lower_Seeked_Name);
   end Match;

end Completion;
