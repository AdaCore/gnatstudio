-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with Glib.Unicode; use Glib.Unicode;

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Completion is

   use Completion_List_Pckg;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Completion_List) is
   begin
      Free (List.List);
      Free (List.Searched_Identifier);
   end Free;

   --------------------------
   -- Get_Completed_String --
   --------------------------

   function Get_Completed_String (This : Completion_List) return String
   is
   begin
      if This.Searched_Identifier /= null then
         return This.Searched_Identifier.all;
      else
         return "";
      end if;
   end Get_Completed_String;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Completion_Context_Record) is
      pragma Unreferenced (Context);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Completion_Context) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Completion_Context_Record'Class, Completion_Context);
   begin
      Internal (Context);
   end Free;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Context : Completion_Context) return String_Access is
   begin
      return Context.Buffer;
   end Get_Buffer;

   ---------------------------
   -- Get_Completion_Offset --
   ---------------------------

   function Get_Completion_Offset
     (Context : Completion_Context) return Natural is
   begin
      return Context.Offset;
   end Get_Completion_Offset;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Completion_Id) return Boolean is
   begin
      return Left.Id_Length < Right.Id_Length
        or else
          (Left.Id_Length = Right.Id_Length
           and then
             (Left.Line < Right.Line
              or else
               (Left.Line = Right.Line
                and then
                  (Left.Column < Right.Column
                   or else
                     (Left.Column = Right.Column
                      and then
                      (Left.Id < Right.Id
                       or else
                         (Left.Id = Right.Id
                          and then
                            (Left.File < Right.File
                             or else
                             (Left.File = Right.File
                              and then
                              Left.Resolver_Id < Right.Resolver_Id)))))))));
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Completion_Id) return Boolean is
   begin
      return Left.Resolver_Id = Right.Resolver_Id
        and then Left.Id = Right.Id
        and then Left.File = Right.File
        and then Left.Line = Right.Line
        and then Left.Column = Right.Column;
   end "=";

   ----------
   -- Next --
   ----------

   function Next
     (Resolver : access Completion_Resolver'Class)
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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Completion_Manager_Access) is
      procedure Internal_Free is new
        Ada.Unchecked_Deallocation
          (Completion_Manager'Class, Completion_Manager_Access);
   begin
      Free (This.Resolvers, False);
      Free (This.Contexts, True);
      Internal_Free (This);
   end Free;

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

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Manager : access Completion_Manager;
      Buffer  : String_Access;
      Offset  : Natural) return Completion_Context
   is
      New_Context : constant Completion_Context :=
        new Completion_Context_Record;
   begin
      New_Context.Buffer := Buffer;
      New_Context.Offset := Offset;

      Append (Manager.Contexts, New_Context);

      return New_Context;
   end Create_Context;

   ------------------------
   -- From_Completion_Id --
   ------------------------

   function From_Completion_Id
     (Manager : access Completion_Manager; Id : Completion_Id)
      return Completion_Proposal_Access
   is
      It : Completion_Resolver_List_Pckg.List_Node := First
        (Manager.Resolvers);
      Result : Completion_Proposal_Access := null;
   begin
      while It /= Completion_Resolver_List_Pckg.Null_Node loop
         Result := From_Completion_Id (Data (It), Id);

         if Result /= null then
            return Result;
         end if;

         It := Next (It);
      end loop;

      return null;
   end From_Completion_Id;

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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Completion_Proposal_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Completion_Proposal'Class, Completion_Proposal_Access);
   begin
      Internal (This);
   end Free;

   -------------------
   -- Free_Proposal --
   -------------------

   procedure Free_Proposal (Proposal : in out Completion_Proposal'Class) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free_Proposal;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Proposal : Completion_Proposal)  return UTF8_String is
   begin
      return Get_Completion (Completion_Proposal'Class (Proposal));
   end Get_Label;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Proposal : Completion_Proposal)
      return UTF8_String is
   begin
      return Get_Completion (Completion_Proposal'Class (Proposal));
   end Get_Id;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   function Get_Caret_Offset
     (Proposal : Completion_Proposal)
      return Basic_Types.Character_Offset_Type is
   begin
      return
        Basic_Types.Character_Offset_Type
          (UTF8_Strlen
               (Get_Completion (Completion_Proposal'Class (Proposal))));
   end Get_Caret_Offset;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Proposal : Completion_Proposal) return UTF8_String
   is
      pragma Unreferenced (Proposal);
   begin
      return "";
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Proposal : Completion_Proposal) return File_Location
   is
      pragma Unreferenced (Proposal);
   begin
      return Null_File_Location;
   end Get_Location;

   -----------------------
   -- Append_Expression --
   -----------------------

   procedure Append_Expression
     (Proposal             : in out Completion_Proposal;
      Number_Of_Parameters : Natural)
   is
      pragma Unreferenced (Proposal, Number_Of_Parameters);
   begin
      null;
   end Append_Expression;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Proposal : Completion_Proposal) return Boolean is
      pragma Unreferenced (Proposal);
   begin
      return True;
   end Is_Valid;

   -----------------
   -- Get_Manager --
   ------------------

   function Get_Resolver (Proposal : Completion_Proposal)
     return Completion_Resolver_Access is
   begin
      return Completion_Resolver_Access (Proposal.Resolver);
   end Get_Resolver;

   -----------
   -- First --
   -----------

   function First (This : Completion_List) return Completion_Iterator is
      It : Completion_Iterator := (It => First (This.List), others => <>);

      Next_Done : Boolean := False;
   begin
      while not Is_Valid (It) loop
         Next (It);

         Next_Done := True;
      end loop;

      if not Next_Done and then not At_End (It) then
         Completion_Id_Set.Insert
           (It.Already_Extracted, To_Completion_Id (Get_Proposal (It)));
      end if;

      return It;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (This : in out Completion_Iterator) is
   begin
      loop
         Next (This.It);

         exit when At_End (This);

         if Is_Valid (This) then
            declare
               Id : constant Completion_Id :=
                 To_Completion_Id (Get_Proposal (This));
            begin
               if Completion_Id_Set.Find
                 (This.Already_Extracted, Id) = Completion_Id_Set.No_Element
               then
                  Completion_Id_Set.Insert (This.Already_Extracted, Id);

                  exit;
               end if;
            end;
         end if;
      end loop;
   end Next;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Completion_Iterator) return Boolean is
   begin
      return At_End (It) or else Is_Valid (Get_Proposal (It));
   end Is_Valid;

   ------------------
   -- Get_Proposal --
   ------------------

   function Get_Proposal
     (This : Completion_Iterator) return Completion_Proposal'Class is
   begin
      return Get (This.It);
   end Get_Proposal;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
     (Proposal : Simple_Completion_Proposal) return UTF8_String is
   begin
      return Proposal.Name.all;
   end Get_Completion;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Proposal : Simple_Completion_Proposal) return Language_Category
   is
      pragma Unreferenced (Proposal);
   begin
      return Cat_Unknown;
   end Get_Category;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (Proposal : Simple_Completion_Proposal) return Construct_Visibility
   is
      pragma Unreferenced (Proposal);
   begin
      return Visibility_Public;
   end Get_Visibility;

   ---------------------
   -- Get_Composition --
   ---------------------

   procedure Get_Composition
     (Proposal   : Simple_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List)
   is
      pragma Unreferenced (Proposal, Identifier, Offset, Is_Partial, Result);
   begin
      null;
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters
     (Proposal : Simple_Completion_Proposal) return Natural
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Number_Of_Parameters;

   -----------
   -- Match --
   -----------

   function Match
     (Proposal   : Simple_Completion_Proposal;
      Identifier : String;
      Is_Partial : Boolean;
      Context    : Completion_Context;
      Offset     : Integer;
      Filter     : Possibilities_Filter) return Boolean
   is
      pragma Unreferenced
        (Proposal, Identifier, Is_Partial, Context, Offset, Filter);
   begin
      --  ??? Implement correcly this function
      return True;
   end Match;
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

   function Match
     (Seeked_Name, Tested_Name : String; Is_Partial : Boolean) return Boolean
   is
   begin
      if (not Is_Partial and then Seeked_Name'Length /= Tested_Name'Length)
        or else Seeked_Name'Length > Tested_Name'Length
      then
         return False;
      end if;

      for J in 1 .. Seeked_Name'Length loop
         if To_Lower (Tested_Name (J + Tested_Name'First - 1)) /=
           To_Lower (Seeked_Name (J + Seeked_Name'First - 1))
         then
            return False;
         end if;
      end loop;

      return True;
   end Match;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   function To_Completion_Id
     (Proposal : Simple_Completion_Proposal) return Completion_Id is
   begin
      return (Proposal.Name'Length,
              "SIMPLE  ",
              Proposal.Name.all,
              VFS.No_File, 0, 0);
   end To_Completion_Id;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Completion_Iterator) is
   begin
      Free (This.It);
   end Free;

   ------------
   -- At_End --
   ------------

   function At_End (This : Completion_Iterator) return Boolean is
   begin
      return At_End (This.It);
   end At_End;

   ----------------------------
   -- Set_Next_Param_Written --
   ----------------------------

   procedure Set_Next_Param_Written
     (Profile : Profile_Manager_Access; Success : out Boolean) is
   begin
      if Profile = null then
         Success := False;
         return;
      end if;

      for J in Profile.Parameters'Range loop
         if not Profile.Parameters (J).Is_Written then
            Profile.Parameters (J).Is_Written := True;
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
   end Set_Next_Param_Written;

   -----------------------
   -- Set_Param_Written --
   -----------------------

   procedure Set_Param_Written
     (Profile : Profile_Manager_Access; Name : String; Success : out Boolean)
   is
   begin
      if Profile = null then
         Success := False;
         return;
      end if;

      for J in Profile.Parameters'Range loop
         --  ??? Handle case sensitivity here !
         if Profile.Parameters (J).Name.all = Name
           and then not Profile.Parameters (J).Is_Written
         then
            Profile.Parameters (J).Is_Written := True;
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
   end Set_Param_Written;

end Completion;
