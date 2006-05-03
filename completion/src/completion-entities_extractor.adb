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

with Entities.Queries;        use Entities.Queries;
with VFS;                     use VFS;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Completion.Entities_Extractor is

   ------------------------------------
   -- New_Entity_Completion_Resolver --
   ------------------------------------

   function New_Entity_Completion_Resolver
     (Tree    : Construct_Tree_Access;
      Project : Project_Type;
      Handler : access Language_Handler_Record'Class)
      return Entity_Completion_Resolver is
   begin
      return
        (Manager => null,
         Tree    => Tree,
         Project => Project,
         Handler => Language_Handler (Handler));
   end New_Entity_Completion_Resolver;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion (Proposal : Entity_Completion_Proposal)
      return UTF8_String is
   begin
      return Get_Name (Proposal.Entity).all;
   end Get_Completion;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Proposal : Entity_Completion_Proposal)
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
     (Proposal : Entity_Completion_Proposal; Offset : Positive)
      return Completion_List
   is
      Result : Completion_List := Null_Completion_List;
      pragma Unreferenced (Offset);
   begin
      if Get_Kind (Proposal.Entity).Is_Type then
         declare
            It : Calls_Iterator := Get_All_Called_Entities (Proposal.Entity);
         begin
            while not At_End (It) loop
               Append
                 (Result,
                  Entity_Completion_Proposal'
                    (Proposal.Mode, Proposal.Resolver, 0, Get (It)));

               Next (It);
            end loop;

            Destroy (It);
         end;
      end if;

      return Result;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters (Proposal : Entity_Completion_Proposal)
     return Natural
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Number_Of_Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Entity_Completion_Proposal) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion (Proposal : Unit_Completion_Proposal)
      return UTF8_String
   is
   begin
      return Get_Name (Proposal.Info).all;
   end Get_Completion;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category (Proposal : Unit_Completion_Proposal)
     return Language_Category
   is
      pragma Unreferenced (Proposal);
   begin
      return Cat_With;
   end Get_Category;

   --------------------
   -- Get_Compositon --
   --------------------

   function Get_Composition
     (Proposal : Unit_Completion_Proposal; Offset : Positive)
      return Completion_List
   is
      pragma Unreferenced (Offset);

      Files        : VFS.File_Array_Access :=
        Get_Source_Files
          (Entity_Completion_Resolver (Proposal.Resolver.all).Project, True);
      Current_File : Source_File;
      Result       : Completion_List;
      Resolver     : Entity_Completion_Resolver := Entity_Completion_Resolver
        (Proposal.Resolver.all);

      procedure Add_Nested_Packages (Unit_Info : Entity_Information);

      -------------------------
      -- Add_Nested_Packages --
      -------------------------

      procedure Add_Nested_Packages (Unit_Info : Entity_Information)
      is
         It   : Calls_Iterator;
         Info : Entity_Information;
      begin
         It := Get_All_Called_Entities (Unit_Info);

         while not At_End (It) loop
            Info := Get (It);

            if Get_File (Get_Declaration_Of (Info))
              = Get_File (Get_Declaration_Of (Unit_Info))
              and then Get_Kind (Info).Kind = Package_Kind then
               Append
                 (Result,
                  Unit_Completion_Proposal'
                    (Show_Identifiers, Proposal.Resolver, 0, Info, True));
            end if;

            Next (It);
         end loop;

         Destroy (It);
      end Add_Nested_Packages;

   begin
      if not Proposal.Nested then
         for J in Files.all'Range loop
            Current_File := Get_Source_Info
              (Get_LI_Handler_From_File (Resolver.Handler, Files.all (J)),
               Files.all (J));

            if Current_File /= null then
               if Match
                 (Get_Full_Name (Proposal.Info) & ".",
                  Get_Unit_Name (Current_File), True)
               then
                  --  If it's a child unit, then just add it

                  Append
                    (Result,
                     Unit_Completion_Proposal'
                       (Show_Identifiers,
                        Completion_Resolver_Access (Proposal.Resolver),
                        0,
                        Get_Unit_Info (Current_File),
                        False));

               elsif Match
                 (Get_Full_Name (Proposal.Info),
                  Get_Unit_Name (Current_File), True)
               then
                  --  If it's this unit, then see if there are nested packages

                  Add_Nested_Packages (Get_Unit_Info (Current_File));

               end if;
            end if;

         end loop;

         Unchecked_Free (Files);

      else
         Add_Nested_Packages (Proposal.Info);
      end if;

      return Result;
   end Get_Composition;

   -----------------------------
   -- Get_Number_Of_Parameter --
   -----------------------------

   function Get_Number_Of_Parameters (Proposal : Unit_Completion_Proposal)
     return Natural
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Number_Of_Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Unit_Completion_Proposal) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entities
     (Name         : String;
      Unit_Info    : Entity_Information;
      View_Private : Boolean;
      Resolver     : Completion_Resolver_Access;
      Is_Partial   : Boolean := False) return Completion_List
   is
      It     : Calls_Iterator;
      Result : Completion_List;
   begin
      It := Get_All_Called_Entities (Unit_Info);

      while not At_End (It) loop
         declare
            Info        : constant Entity_Information := Get (It);
            Entity_Name : constant String := Get_Name (Info).all;
         begin
            if Match (Name, Entity_Name, Is_Partial)
              and then
                (View_Private or else Get_Attributes (Info) (Global))
            then
               Append
                 (Result,
                  Entity_Completion_Proposal'
                    (Show_Identifiers, Resolver, 0, Get (It)));
            end if;
         end;

         Next (It);
      end loop;

      Destroy (It);

      return Result;
   end Get_Entities;

   -------------------------
   -- Get_Source_For_Unit --
   -------------------------

   function Get_Source_For_Unit
     (Handler   : access Language_Handler_Record'Class;
      Project   : Project_Type;
      Unit_Name : String) return Source_File
   is
      Files        : VFS.File_Array_Access :=
        Get_Source_Files (Project, True);
      Current_File : Source_File;
   begin
      for J in Files.all'Range loop
         Current_File := Get_Source_Info
           (Get_LI_Handler_From_File (Handler, Files.all (J)),
            Files.all (J));

         if Current_File /= null
           and then To_Lower
             (Get_Unit_Name (Current_File)) = To_Lower (Unit_Name)
         then
            return Current_File;
         end if;
      end loop;

      Unchecked_Free (Files);

      --  ??? We should return some error value in this case !
      return Current_File;
   end Get_Source_For_Unit;

   -----------------------
   -- Get_Possibilities --
   -----------------------

   function Get_Possibilities
     (Resolver   : access Entity_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter) return Completion_List
   is
      It     : Construct_Tree_Iterator := First (Resolver.Tree.all);
      Result : Completion_List;
   begin
      if (Filter and All_Visible_Entities) /= 0 then
         while It /= Null_Construct_Tree_Iterator
           and then Get_Construct (It).Sloc_Start.Index <= Offset
         loop
            if Get_Construct (It).Category = Cat_With then
               null;
            elsif Get_Construct (It).Category = Cat_Use then
               declare
                  File : constant Source_File := Get_Source_For_Unit
                    (Resolver.Handler,
                     Resolver.Project,
                     Get_Construct (It).Name.all);
               begin
                  Concat
                    (Result,
                     Get_Entities
                       (Identifier,
                        Get_Unit_Info (File),
                        False,
                        Completion_Resolver_Access (Resolver),
                        Is_Partial));
               end;
            end if;

            It := Next (Resolver.Tree.all, It, Jump_Over);
         end loop;
      end if;

      if (Filter and All_Accessible_Units) /= 0 then
         declare
            Files        : VFS.File_Array_Access :=
              Get_Source_Files (Resolver.Project, True);
            Current_File : Source_File;
         begin
            for J in Files.all'Range loop
               Current_File := Get_Source_Info
                 (Get_LI_Handler_From_File (Resolver.Handler, Files.all (J)),
                  Files.all (J));

               if Current_File /= null
                 and then Match
                   (Identifier, Get_Unit_Name (Current_File), Is_Partial)
               then
                  Append
                    (Result,
                     Unit_Completion_Proposal'
                       (Show_Identifiers,
                        Completion_Resolver_Access (Resolver),
                        0,
                        Get_Unit_Info (Current_File),
                        False));
               end if;

            end loop;

            Unchecked_Free (Files);
         end;
      end if;

      return Result;
   end Get_Possibilities;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Completion_Resolver) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info (Source : Source_File) return Entity_Information is
      It        : Entity_Iterator;
      --  Result    : Completion_List;
      --  Unit_Name : String := Get_Unit_Name (Source);
   begin
      Find_All_Entities_In_File (It, Source);

      while not At_End (It) loop
         declare
            Info        : constant Entity_Information := Get (It);
            --  Entity_Name : constant String := Get_Name (Info).all;
            Scope       : constant Entity_Information :=
              Get_Caller (Declaration_As_Reference (Info));
         begin
            if Scope = null and then Get_Kind (Info).Kind = Package_Kind then
               Destroy (It);
               return Info;
            end if;
         end;

         Next (It);
      end loop;

      Destroy (It);

      return null;
   end Get_Unit_Info;

end Completion.Entities_Extractor;
