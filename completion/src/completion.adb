-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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
with Ada.Unchecked_Deallocation;

with Glib.Unicode; use Glib.Unicode;

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

   --------------
   -- Get_File --
   --------------

   function Get_File (Context : Completion_Context) return Virtual_File is
   begin
      return Context.File;
   end Get_File;

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
      It : Completion_Resolver_Map_Pckg.Cursor := First
        (Resolver.Manager.Resolvers);
   begin
      while It /= Completion_Resolver_Map_Pckg.No_Element loop
         if Element (It) = Completion_Resolver_Access (Resolver) then
            if Next (It) /= Completion_Resolver_Map_Pckg.No_Element then
               return Element (Next (It));
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
      Insert
        (Manager.Resolvers,
         Get_Id (Resolver.all),
         Completion_Resolver_Access (Resolver));
      Append
        (Manager.Ordered_Resolvers,
         Completion_Resolver_Access (Resolver));
      Resolver.Manager := Completion_Manager_Access (Manager);
   end Register_Resolver;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Manager : access Completion_Manager;
      File    : GNATCOLL.VFS.Virtual_File;
      Buffer  : String_Access;
      Lang    : Language_Access;
      Offset  : Natural) return Completion_Context
   is
      New_Context : constant Completion_Context :=
        new Completion_Context_Record;
   begin
      New_Context.Buffer := Buffer;
      New_Context.Offset := Offset;
      New_Context.File := File;
      New_Context.Lang := Lang;

      Append (Manager.Contexts, New_Context);

      return New_Context;
   end Create_Context;

   ------------------
   -- Get_Resolver --
   ------------------

   function Get_Resolver
     (Manager : access Completion_Manager;
      Name    : String) return Completion_Resolver_Access
   is
   begin
      return Element (Manager.Resolvers, Name);
   end Get_Resolver;

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

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Proposal : Simple_Completion_Proposal) return UTF8_String is
   begin
      if Proposal.Documentation = null then
         return "";
      end if;

      return Proposal.Documentation.all;
   end Get_Documentation;

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

   -----------
   -- Match --
   -----------

   function Match
     (Proposal : Simple_Completion_Proposal;
      Context  : Completion_Context;
      Offset   : Integer) return Boolean
   is
      pragma Unreferenced (Proposal, Context, Offset);
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
      Free (Proposal.Documentation);
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
              GNATCOLL.VFS.No_File, 0, 0);
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

end Completion;
