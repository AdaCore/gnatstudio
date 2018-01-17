------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with System;
with System.Address_Image;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

--  with Glib;         use Glib;
--  with Glib.Unicode; use Glib.Unicode;

with GPS.Editors; use GPS.Editors;

with Language;     use Language;

with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

with clang_c_Index_h; use clang_c_Index_h;
with Language.Libclang; use Language.Libclang;
with GPS.Core_Kernels; use GPS.Core_Kernels;
with Glib.Unicode; use Glib.Unicode;
with Glib; use Glib;
with String_Utils; use String_Utils;

package body Completion.C.Libclang is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LIBCLANG.COMPLETION");

   function Current_Location
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Editor_Location'Class;
   --  Return the current location
   --  ??? Duplicated with Completion.Python - could be shared

   ---------------
   -- Proposals --
   ---------------

   type Libclang_Completion is new Completion_Proposal
   with record
      Label         : Unbounded_String;
      Documentation : Unbounded_String;
      Completion    : Unbounded_String;
      Category      : Language_Category;
   end record;

   overriding procedure Free (Proposal : in out Libclang_Completion) is null;

   overriding function Deep_Copy
     (Proposal : Libclang_Completion)
      return Completion_Proposal'Class is (Libclang_Completion'(Proposal));

   overriding function Match
     (Proposal   : Libclang_Completion;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean is (True);
   --  This function is not actually used, need to be clarified some day ???

   overriding function Get_Action_Name
     (Proposal : Libclang_Completion)
      return String is ("");
   --  No action linked to Libclang_Completion resolvers

   overriding function Get_Visibility
     (Proposal : Libclang_Completion) return Construct_Visibility
   is (Visibility_Public);
   --  Libclang only returns visible (eg. public) entities already, so
   --  everything will be public

   overriding function Get_Completion
     (Proposal : Libclang_Completion;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Basic_Types.UTF8_String is (+Proposal.Completion);
   --  Return stored completion string

   overriding function Get_Category
     (Proposal : Libclang_Completion) return Language_Category
   is (Proposal.Category);
   --  Return the category of the object proposed for the completion

   overriding function Get_Documentation
     (Proposal : Libclang_Completion)
      return String is (To_String (Proposal.Documentation));
   --  Return stored documentation directly

   overriding function Get_Custom_Icon_Name
     (Proposal : Libclang_Completion) return String is ("");
   --  No custom icon

   overriding function Get_Label
     (Proposal : Libclang_Completion;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return String is (To_String (Proposal.Label));
   --  Return stored label directly

   overriding function To_Completion_Id
     (Proposal : Libclang_Completion;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id;

   package Completion_Proposal_Lists
   is new Ada.Containers.Doubly_Linked_Lists
     (Libclang_Completion);

   ----------------------
   -- Lazy computation --
   ----------------------

   --  The following implements a virtual list binding to a Libclang object
   --  which is capable of computing lazily a list of completion proposals.

   type Libclang_Component is
     new Completion_List_Pckg.Virtual_List_Component
   with record
      Resolver : access Libclang_Resolver;
   end record;

   type Libclang_Iterator is
     new Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      Resolver        : access Libclang_Resolver;
      Next_Num        : Natural := 0;
      Pending_Results : Completion_Proposal_Lists.List;
      --  The number of the next item to get.
   end record;

   overriding function First (List : Libclang_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   overriding function At_End (It : Libclang_Iterator) return Boolean;
   overriding procedure Next (It : in out Libclang_Iterator);
   overriding function Get
     (It : in out Libclang_Iterator) return Completion_Proposal'Class;

   -----------
   -- First --
   -----------

   overriding
   function First
     (List : Libclang_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Iterator : Libclang_Iterator;
   begin
      Iterator.Resolver := List.Resolver;
      Iterator.Next_Num := 0;
      Next (Iterator);
      return Iterator;
   end First;

   ------------------
   -- Compute_Next --
   ------------------

   procedure Compute_Next (It : in out Libclang_Iterator);
   procedure Compute_Next (It : in out Libclang_Iterator)
   is
      The_Res  : constant Clang_Completion_Result :=
        It.Resolver.Completions_Array (It.Next_Num);
      Strs     : constant Completion_Strings := Spellings (The_Res);
   begin

      --  Since we sorted the results by priority, the first result is gonna
      --  be the current parameter completion (highest priority) if there is
      --  a current parameter completion.

      if It.Next_Num = 1
        and then Is_Parameter_Completion (It.Resolver.Completions_Array (1))
      then

         --  In this case we create a custom completion that informs the user
         --  about the current parameter. If he triggers it it will insert a
         --  /* */ comment containing the name of the parameter

         declare
            C : constant Clang_Completion_Result :=
              It.Resolver.Completions_Array (1);
            Param_Index : constant Natural := Get_Current_Param_Index (C);
            Param_Name : constant String := Extract_Param_Name_From_Chunk
              (Get_Chunks (C) (Param_Index));
         begin
            It.Pending_Results.Append
              ((Resolver => It.Resolver,
               Label => +(Param_Name & " (param)"),
               Completion => +("/* " & Param_Name & " = */"),
               Documentation => Strs.Doc,
               Category => Cat_Parameter));
         end;

      else

         --  In the other (regular) case we just fill the completion proposal
         --  with the needed information

         It.Pending_Results.Append
           ((Resolver => It.Resolver,
             Label => Strs.Completion,
             Completion => Strs.Completion,
             Documentation => Strs.Doc,
             Category =>
               (case Kind (The_Res) is
                   when CXCursor_EnumDecl
                        | CXCursor_EnumConstantDecl => Cat_Literal,
                   when CXCursor_FieldDecl => Cat_Field,
                   when CXCursor_FunctionDecl
                        | CXCursor_FunctionTemplate => Cat_Function,
                   when CXCursor_ParmDecl => Cat_Parameter,
                   when CXCursor_TypedefDecl
                        | CXCursor_TypeAliasDecl => Cat_Type,
                   when CXCursor_VarDecl => Cat_Variable,
                   when others => Cat_Unknown)));
      end if;
   end Compute_Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Libclang_Iterator) is
   begin
      loop
         It.Pending_Results.Delete_First;

         if It.Pending_Results.Is_Empty then
            It.Next_Num := It.Next_Num + 1;
            exit when It.Next_Num > It.Resolver.Completions_Array'Length;
            Compute_Next (It);
         end if;

         exit
           when Starts_With
             (To_String
                (Libclang_Completion (It.Get).Label),
              It.Resolver.Prefix.all);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : Libclang_Iterator) return Boolean is
   begin
      return It.Next_Num > Num_Results (It.Resolver.Completions)
        and then It.Pending_Results.Is_Empty;
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (It : in out Libclang_Iterator) return Completion_Proposal'Class
   is
   begin
      return
        (if not It.Pending_Results.Is_Empty
         then Completion_Proposal'Class (It.Pending_Results.First_Element)
         else Null_Completion_Proposal);
   end Get;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : Libclang_Completion;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id
   is
      pragma Unreferenced (Db);
   begin
      return (Length (Proposal.Completion),
              "LIBCLANG",
              +Proposal.Completion,
              GNATCOLL.VFS.No_File, 0, 0);
   end To_Completion_Id;

   --------------------------------------
   -- New_Libclang_Completion_Resolver --
   --------------------------------------

   function New_Libclang_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File)
      return Completion_Resolver_Access
   is
      pragma Unreferenced (Current_File);
      R : Libclang_Resolver;
   begin
      R.Manager := null;
      R.Kernel  := Kernel;

      return new Libclang_Resolver'(R);
   end New_Libclang_Completion_Resolver;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver : access Libclang_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      pragma Unreferenced (Offset);
      Loc : Editor_Location'Class :=
        Current_Location (Resolver.Kernel, Context.File);

      Filename  : constant String := +Context.File.Full_Name.all;
      Component : Libclang_Component;

      function Unichar_To_UTF8 (Char : Gunichar) return String;
      function Unichar_To_UTF8 (Char : Gunichar) return String is
         The_Char   : String (1 .. 6);
         Last : Natural;
      begin
         Unichar_To_UTF8 (Char, The_Char, Last);
         return The_Char (1 .. Last);
      end Unichar_To_UTF8;

   begin
      --  Find the prefix of the word

      declare
         Unichar    : Gunichar;
         Prefix     : Unbounded_String;
      begin
         loop
            Loc := Loc.Forward_Char (-1);
            Unichar := Gunichar (Loc.Get_Char);

            --  Exit when we are out of an identifier, eg. the current char is
            --  neither an alphanumeric character, neither an underscore

            exit when not
              (Is_Alnum (Unichar) or else Unichar = Character'Pos ('_'));

            Insert (Prefix, 1, Unichar_To_UTF8 (Unichar));

            --  Exit here if we are on the beginning of the buffer

            exit when Loc.Offset = 0;
         end loop;

         Result.Searched_Identifier := new String'(+Prefix);
      end;

      Trace (Me, "Completion prefix: " & Result.Searched_Identifier.all);
      Trace (Me, "Completion at " & Loc.Line'Img & " : "
             & Natural'Image (Loc.Line_Offset + 2));
      Component.Resolver := Resolver;
      Resolver.Prefix := new String'(Result.Searched_Identifier.all);

      declare
         Unsaved_Files : constant Unsaved_File_Array :=
           (1 => Create_Unsaved_File
              (Filename,
               Ada.Strings.Unbounded.String_Access (Context.Buffer)));
      begin
         Resolver.TU := Translation_Unit
           (Core_Kernel (Resolver.Kernel), Context.File);

         Resolver.Unsaved_File_Inst := Unsaved_Files (1);
         Resolver.Completions := Complete_At
           (Resolver.TU,
            Filename      => +Context.File.Full_Name,
            Line          => Loc.Line,
            Column        => Loc.Line_Offset + 2,
            Unsaved_Files => Unsaved_Files,
            Options       =>
              (Include_Macros
               or Include_Code_Patterns or Include_Brief_Comments));

         declare
            use Completion_Results_Arrays;

            function "<"
              (Left, Right : Clang_Completion_Result) return Boolean;
            --  We want to store completion results:
            --    * First by libclang given priorities
            --    * Second by lexical order

            function "<"
              (Left, Right : Clang_Completion_Result) return Boolean
            is
               P_Left : constant Natural := Priority (Left);
               P_Right : constant Natural := Priority (Right);
            begin
               if P_Left = P_Right then
                  return Typed_Text (Left) < Typed_Text (Right);
               else
                  return P_Left < P_Right;
               end if;
            end "<";

            procedure Sort is new In_Place_Sort_Gen ("<");

            Completions_Array : Completion_Results_Array
              (1 .. Num_Results (Resolver.Completions));
         begin
            for I in 1 .. Num_Results (Resolver.Completions) loop
               Completions_Array (I) :=
                 Nth_Result (Resolver.Completions, I);
            end loop;

            Sort (Completions_Array);

            Resolver.Completions_Array :=
              new Completion_Results_Array'(Completions_Array);
         end;
      end;

      Completion_List_Pckg.Append (Result.List, Component);
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : Libclang_Resolver)
      return String is
   begin
      return "libclang" & System.Address_Image (Resolver'Address);
   end Get_Id;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Libclang_Resolver) is
   begin
      Dispose (This.Completions);
      Free (This.Prefix);
      Destroy_Unsaved_File (This.Unsaved_File_Inst);
      Completion_Results_Arrays.Free (This.Completions_Array);
   end Free;

   ----------------------
   -- Current_Location --
   ----------------------

   function Current_Location
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Editor_Location'Class
   is
      Buf : constant Editor_Buffer'Class :=
        GPS.Editors.Get (This        => Kernel.Get_Buffer_Factory.all,
                         File        => File,
                         Force       => False,
                         Open_Buffer => False,
                         Open_View   => False);
   begin
      return Buf.Current_View.Cursor;
   end Current_Location;

end Completion.C.Libclang;
