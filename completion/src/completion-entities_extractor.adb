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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Language.Ada;            use Language.Ada;
with Doc_Utils;               use Doc_Utils;
with GPS.Intl;                use GPS.Intl;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNAT.Strings;            use GNAT.Strings;

package body Completion.Entities_Extractor is

   use Completion_List_Pckg;

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
      if Proposal.Is_All then
         return "all";
      else
         return Get_Name (Proposal.Entity).all;
      end if;
   end Get_Completion;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation (Proposal : Entity_Completion_Proposal)
      return UTF8_String
   is
      Buffer        : Unbounded_String;
      It            : Subprogram_Iterator :=
        Get_Subprogram_Parameters (Proposal.Entity);
      Parameter     : Entity_Information;
      Returned      : Entity_Information;
      Type_Of       : Entity_Information;
      Max_Char_Name : Integer;

      Comments      : constant String := Get_Documentation
        (Lang_Handler              => Entity_Completion_Resolver
           (Proposal.Resolver.all).Handler,
         Entity                    => Proposal.Entity,
         Declaration_File_Contents => "");
   begin
      Buffer := To_Unbounded_String
        (Attributes_To_String (Get_Attributes (Proposal.Entity))
         & ' '
         & (-Kind_To_String (Get_Kind (Proposal.Entity))) & ' '
         & Get_Full_Name (Proposal.Entity, ".") & ""
         & ASCII.LF);

      if Comments /= "" then
         Append (Buffer, ASCII.LF & Comments & ASCII.LF);
      end if;

      if Get_Kind (Proposal.Entity).Kind = Function_Or_Operator or else
        Get_Kind (Proposal.Entity).Kind = Procedure_Kind
      then
         Get (It, Parameter);

         if Parameter /= null then
            Append
              (Buffer, ASCII.LF & "<b>Parameters: </b>" & ASCII.LF);
            Max_Char_Name := 0;

            while Parameter /= null loop
               if Get_Name (Parameter)'Length > Max_Char_Name then
                  Max_Char_Name := Get_Name (Parameter)'Length;
               end if;

               Next (It);
               Get (It, Parameter);
            end loop;

            It := Get_Subprogram_Parameters (Proposal.Entity);
            Get (It, Parameter);

            while Parameter /= null loop
               Append (Buffer, Get_Name (Parameter).all);

               for J in Get_Name (Parameter)'Length + 1 .. Max_Char_Name loop
                  Append (Buffer, " ");
               end loop;

               Append (Buffer, " : ");

               case Get_Type (It) is
               when In_Parameter =>
                  Append (Buffer, "in     ");
               when Out_Parameter =>
                  Append (Buffer, "out    ");
               when In_Out_Parameter =>
                  Append (Buffer, "in out ");
               when Access_Parameter =>
                  Append (Buffer, "access ");
               end case;

               if Get_Type (It) = Access_Parameter then
                  Type_Of := Pointed_Type (Parameter);
               else
                  Type_Of := Get_Type_Of (Parameter);
               end if;

               if Type_Of /= null then

                  if Get_Name (Type_Of) /= null then
                     Append
                       (Buffer,
                        Get_Name (Type_Of).all & ASCII.LF);
                  end if;
               else
                  Append
                    (Buffer, ASCII.LF);
               end if;

               Next (It);
               Get (It, Parameter);
            end loop;
         end if;
      end if;

      if Get_Kind (Proposal.Entity).Kind = Function_Or_Operator then
         Returned := Get_Returned_Type (Proposal.Entity);

         if Returned /= null then
            Append
              (Buffer, ASCII.LF & "<b>Return: </b>" & ASCII.LF);

            if Get_Name (Returned) /= null then
               Append
                 (Buffer,
                  Get_Name (Returned).all & ASCII.LF);
            end if;
         end if;
      end if;

      return To_String (Buffer);
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Proposal : Entity_Completion_Proposal)
      return Completion.File_Location
   is
      Location : constant Entities.File_Location :=
        Get_Location (Declaration_As_Reference (Proposal.Entity));
      Result   : Completion.File_Location;
   begin
      Result.File_Path := Get_Filename (Location.File);
      Result.Line      := Location.Line;
      Result.Column    := Location.Column;

      return Result;
   end Get_Location;

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

   procedure Get_Composition
     (Proposal   : Entity_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List)
   is
      pragma Unreferenced (Offset);
      Type_Of      : Entity_Information := null;
      Calls_Filter : Possibilities_Filter := Everything;
   begin
      if Get_Kind (Proposal.Entity).Kind = Package_Kind then
         if (Proposal.Filter and All_Accessible_Units) /= 0 then
            Append
              (Result.List,
               Entity_Tree_Wrapper'
                 (Completion_List_Pckg.Virtual_List_Component with
                  Handler       => Get_LI_Handler_By_Name
                    (Entity_Completion_Resolver
                       (Proposal.Resolver.all).Handler, Get_Name (Ada_Lang)),
                  Resolver      => Proposal.Resolver,
                  Name          => new String'(Identifier),
                  Is_Partial    => Is_Partial,
                  Filter        => All_Accessible_Units,
                  Next_Filter   => Proposal.Filter,
                  Parent_Entity => Proposal.Entity));

            Calls_Filter := Calls_Filter and not All_Visible_Packages;
         end if;

         if (Proposal.Filter and All_Visible_Entities) /= 0 then
            Append
              (Result.List,
               Calls_Wrapper'
                 (Resolver   => Get_Resolver (Proposal),
                  Scope      => Proposal.Entity,
                  Name       => new String'(Identifier),
                  Is_Partial => Is_Partial,
                  Filter     => Calls_Filter));
         end if;
      else
         if Get_Kind (Proposal.Entity).Kind = Function_Or_Operator then
            Type_Of := Get_Returned_Type (Proposal.Entity);
         elsif Get_Kind (Proposal.Entity).Is_Type then
            Type_Of := Proposal.Entity;
         else
            Type_Of := Get_Type_Of (Proposal.Entity);
         end if;

         if Type_Of = null then
            return;
         end if;

         if Get_Kind (Type_Of).Kind = Access_Kind then

            Type_Of := Pointed_Type (Type_Of);

            if Match (Identifier, "all", Is_Partial) then
               Append
                 (Result.List,
                  Unique_Entity_Wrapper'
                    (Resolver => Proposal.Resolver,
                     Entity   => Type_Of,
                     Is_All   => True));
            end if;

         end if;

         declare
            Parents : constant Entity_Information_Array :=
              Get_Parent_Types (Type_Of, True);
         begin
            for J in reverse Parents'Range loop
               Append
                 (Result.List,
                  Calls_Wrapper'
                    (Resolver   => Get_Resolver (Proposal),
                     Scope      => Parents (J),
                     Name       => new String'(Identifier),
                     Is_Partial => Is_Partial,
                     Filter     => Calls_Filter));
            end loop;
         end;

         Append
           (Result.List,
            Calls_Wrapper'
              (Resolver   => Get_Resolver (Proposal),
               Scope      => Type_Of,
               Name       => new String'(Identifier),
               Is_Partial => Is_Partial,
               Filter     => Calls_Filter));
      end if;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters (Proposal : Entity_Completion_Proposal)
     return Natural
   is
      Number      : Integer := 0;
      It          : Subprogram_Iterator;
      Dummy_Param : Entity_Information;
   begin
      It := Get_Subprogram_Parameters (Proposal.Entity);
      Get (It, Dummy_Param);

      while  Dummy_Param /= null loop
         Number := Number + 1;
         Next (It);
         Get (It, Dummy_Param);
      end loop;

      return Number;
   end Get_Number_Of_Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Entity_Completion_Proposal) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free;

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

   procedure Get_Possibilities
     (Resolver   : access Entity_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List)
   is
      pragma Unreferenced (Offset);
   begin
      if ((Filter and All_Visible_Entities) /= 0)
          or else ((Filter and All_Accessible_Units) /= 0)
      then
         Append
           (Result.List,
            Entity_Tree_Wrapper'
              (Completion_List_Pckg.Virtual_List_Component with
               Handler       => Get_LI_Handler_By_Name
                 (Resolver.Handler, Get_Name (Ada_Lang)),
               Resolver      => Completion_Resolver_Access (Resolver),
               Name          => new String'(Identifier),
               Is_Partial    => Is_Partial,
               Filter        => Filter,
               Next_Filter   => Filter,
               Parent_Entity => null));

         Result.Searched_Identifier := new String'(Identifier);
      end if;
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
      It : Entities.Queries.Entity_Iterator;
   begin
      Find_All_Entities_In_File (It, Source);

      while not At_End (It) loop
         declare
            Info      : constant Entity_Information := Get (It);
            Scope     : constant Entity_Information :=
              Get_Caller (Declaration_As_Reference (Info));
            Reference : constant Entity_Reference :=
              Declaration_As_Reference (Info);
         begin
            if Scope = null
              and then Is_Container (Get_Kind (Info).Kind)
              and then Get_Location
                (Reference).File = Source
            then
               Destroy (It);
               return Info;
            end if;
         end;

         Next (It);
      end loop;

      Destroy (It);

      return null;
   end Get_Unit_Info;

   -----------
   -- First --
   -----------

   function First (Tree : Entity_Tree_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      It : Entity_Iterator_Wrapper := Entity_Iterator_Wrapper'
        (It            => Start (Tree.Handler, To_Lower (Tree.Name.all)),
         Resolver      => Tree.Resolver,
         Is_Partial    => Tree.Is_Partial,
         Name          => Tree.Name,
         Filter        => Tree.Filter,
         Next_Filter   => Tree.Next_Filter,
         Parent_Entity => Tree.Parent_Entity);
   begin
      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Entity_Iterator_Wrapper) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Entity_Iterator_Wrapper) return Boolean is
      Entity : Entity_Information;
      Caller : Entity_Information;
   begin
      if At_End (It) then
         return True;
      end if;

      Entity := Get (It.It);

      --  In any case, we always return global symbols. Non global symbols are
      --  hidden by this engine.

      if not Get_Attributes (Entity) (Global)
        or else not Match (It.Name.all, Get_Name (Entity).all, It.Is_Partial)
      --  ??? It would be good if the underlying iterator was able to make
      --  the difference between full names and partial names. This way, the
      --  above test would not be needed, and we would simply not iterate over
      --  unmatching names.
      then
         return False;
      end if;

      Caller := Get_Caller (Declaration_As_Reference (Entity));

      if Caller /= null and then Get_Kind (Caller).Kind = Record_Kind then
         --  Do no add fields
         return False;
      end if;

      if (It.Filter and All_Visible_Entities) /= 0 then
         return True;
      end if;

      if (It.Filter and All_Accessible_Units) /= 0 then
         return
           (Get_Kind (Entity).Kind = Package_Kind
            or else
              (Caller = null
               and then
                 (Get_Kind (Entity).Kind = Procedure_Kind
                 or else Get_Kind (Entity).Kind = Function_Or_Operator)))
           and then
             (It.Parent_Entity = null
              or else It.Parent_Entity = Get_Parent_Package (Entity, False)
              or else It.Parent_Entity = Caller);

      end if;

      return True;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Entity_Iterator_Wrapper) is
   begin
      Next (It.It);

      while not Is_Valid (It) loop
         Next (It.It);
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (This : Entity_Iterator_Wrapper)
      return Completion_Proposal'Class
   is
   begin
      return  Entity_Completion_Proposal'
        (Mode             => Show_Identifiers,
         Resolver         => This.Resolver,
         Entity           => Get (This.It),
         Is_All           => False,
         Filter           => This.Next_Filter);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Iterator_Wrapper) is
   begin
      Free (This.It);
   end Free;

   -----------
   -- First --
   -----------

   function First (Scope : Calls_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      It : Calls_Iterator_Wrapper :=
        (It         => Get_All_Called_Entities (Scope.Scope),
         Resolver   => Scope.Resolver,
         Scope      => Scope.Scope,
         Name       => Scope.Name,
         Is_Partial => Scope.Is_Partial,
         Filter     => Scope.Filter);
   begin
      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Calls_Iterator_Wrapper) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Calls_Iterator_Wrapper) is
   begin
      Next (It.It);

      while not Is_Valid (It) loop
         Next (It.It);
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (This : Calls_Iterator_Wrapper)
      return Completion_Proposal'Class
   is
   begin
      return  Entity_Completion_Proposal'
        (Mode     => Show_Identifiers,
         Resolver => This.Resolver,
         Entity   => Get (This.It),
         Is_All   => False,
         Filter   => This.Filter);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Calls_Wrapper) is
   begin
      Free (This.Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Calls_Iterator_Wrapper) is
   begin
      Destroy (This.It);
   end Free;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Calls_Iterator_Wrapper) return Boolean is
      It_Reference : Entity_Reference;
   begin
      if At_End (It) then
         return True;
      end if;

      It_Reference := Declaration_As_Reference (Get (It.It));

      return Get_Caller (It_Reference) = It.Scope
        and then ((It.Filter and All_Visible_Packages) /= 0
                  or else Get_Kind (Get (It.It)).Kind /= Package_Kind)
        and then Match (It.Name.all,
                        Get_Name (Get (It.It)).all,
                        It.Is_Partial);
   end Is_Valid;

   -----------
   -- First --
   -----------

   function First (Parent : Child_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      It : Child_Iterator_Wrapper;
   begin
      Get_Child_Types (It.It, Parent.Parent);
      It.Resolver := Parent.Resolver;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Child_Iterator_Wrapper) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Child_Iterator_Wrapper) is
   begin
      Next (It.It);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (This : Child_Iterator_Wrapper)
      return Completion_Proposal'Class is
   begin
      return Entity_Completion_Proposal'
        (Mode     => Show_Identifiers,
         Resolver => This.Resolver,
         Entity   => Get (This.It),
         Is_All   => False,
         Filter   => Everything);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Child_Iterator_Wrapper) is
   begin
      Destroy (This.It);
   end Free;

   -----------
   -- First --
   -----------

   function First (List : Source_File_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      It : Source_File_Iterator;
   begin
      It.Files := List.Files;
      It.It := List.Files.all'First;
      It.Resolver := List.Resolver;
      It.Parent := List.Parent;

      Set_Unit (It);

      if It.Unit = null
        or else It.Parent = null
        or else Get_Parent_Package (It.Unit, False) /= It.Parent
      then
         Next (It);
      end if;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Source_File_Iterator) return Boolean is
   begin
      return It.It > It.Files.all'Last;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Source_File_Iterator) is
   begin
      It.It := It.It + 1;
      Set_Unit (It);

      while not At_End (It)
        and then
          (It.Unit = null
           or else It.Parent = null
           or else Get_Parent_Package (It.Unit, False) /= It.Parent)
      loop
         It.It := It.It + 1;
         Set_Unit (It);
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (This : Source_File_Iterator)
      return Completion_Proposal'Class
   is
   begin
      return Entity_Completion_Proposal'
        (Mode     => Show_Identifiers,
         Resolver => This.Resolver,
         Entity   => This.Unit,
         Is_All   => False,
         Filter   => Everything);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Source_File_Component) is
   begin
      Unchecked_Free (This.Files);
   end Free;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit (It : in out Source_File_Iterator) is
   begin
      if At_End (It) then
         It.Unit := null;
      else
         declare
            Source : constant Source_File :=
              Get_Source_Info
                (Get_LI_Handler_From_File
                     (Entity_Completion_Resolver (It.Resolver.all).Handler,
                      It.Files.all (It.It)),
                 It.Files.all (It.It));
         begin
            if Source = null then
               It.Unit := null;
            else
               It.Unit := Get_Unit_Info (Source);
            end if;
         end;
      end if;
   end Set_Unit;

   -----------
   -- First --
   -----------

   function First (Wrapper : Unique_Entity_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class is
   begin
      return Unique_Entity_Iterator_Wrapper'
        (Resolver => Wrapper.Resolver,
         Entity   => Wrapper.Entity,
         Is_All   => Wrapper.Is_All);
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Unique_Entity_Iterator_Wrapper) return Boolean is
   begin
      return It.Entity = null;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Unique_Entity_Iterator_Wrapper) is
   begin
      It.Entity := null;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (This : Unique_Entity_Iterator_Wrapper)
      return Completion_Proposal'Class
   is
   begin
      return Entity_Completion_Proposal'
        (Mode     => Show_Identifiers,
         Resolver => This.Resolver,
         Entity   => This.Entity,
         Is_All   => This.Is_All,
         Filter   => Everything);
   end Get;

end Completion.Entities_Extractor;
