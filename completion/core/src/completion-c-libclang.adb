------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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
with Language.Cpp; use Language.Cpp;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

with Completion.C.Libclang.Utils; use Completion.C.Libclang.Utils;

with UTF8_Utils; use UTF8_Utils;

with clang_c_Index_h; use clang_c_Index_h;

package body Completion.C.Libclang is

   Me : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("COMPLETION_LIBCLANG", On);

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("COMPLETION_LIBCLANG.DIAGNOSTICS", Off);

   Indexer : Clang_Index;
   --  ??? This should be shared GPS-wide

   function Current_Location
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Editor_Location'Class;
   --  Return the current location
   --  ??? Duplicated with Completion.Python - could be shared

   ---------------
   -- Proposals --
   ---------------

   type Simple_Libclang_Completion_Proposal is new Simple_Completion_Proposal
   with record
      Label         : Unbounded_String;
      Documentation : Unbounded_String;
   end record;

   overriding function Get_Action_Name
     (Proposal : Simple_Libclang_Completion_Proposal)
      return String is ("");

   overriding function Get_Documentation
     (Proposal : Simple_Libclang_Completion_Proposal)
      return String is (To_String (Proposal.Documentation));

   overriding function Get_Custom_Icon_Name
     (Proposal : Simple_Libclang_Completion_Proposal) return String is ("");

   overriding function Get_Label
     (Proposal : Simple_Libclang_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return String is (To_String (Proposal.Label));

   overriding function To_Completion_Id
     (Proposal : Simple_Libclang_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id;

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
      Resolver    : access Libclang_Resolver;
      Next_Num    : Positive := 1;
      --  The number of the next item to get.
   end record;

   overriding function First (List : Libclang_Component)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   overriding function At_End (It : Libclang_Iterator) return Boolean;
   overriding procedure Next (It : in out Libclang_Iterator);
   overriding function Get
     (It : Libclang_Iterator) return Completion_Proposal'Class;

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
      Iterator.Next_Num := 1;

      while not At_End (Iterator)
        and then not Starts_With
          (To_String
             (Simple_Libclang_Completion_Proposal (Get (Iterator)).Label),
           Iterator.Resolver.Prefix.all)
      loop
         Next (Iterator);
      end loop;

      return Iterator;
   end First;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Libclang_Iterator) is
   begin
      It.Next_Num := It.Next_Num + 1;

      while not At_End (It)
      loop
         declare
            C : constant Simple_Libclang_Completion_Proposal :=
              Simple_Libclang_Completion_Proposal (It.Get);
         begin
            if Starts_With (To_String (C.Label), It.Resolver.Prefix.all) then
               return;
            end if;
         end;

         Next (It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : Libclang_Iterator) return Boolean is
   begin
      return It.Next_Num > Num_Results (It.Resolver.Completions);
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (It : Libclang_Iterator) return Completion_Proposal'Class
   is
      Proposal : Simple_Libclang_Completion_Proposal;
      The_Res  : constant Clang_Completion_Result :=
        Nth_Result (It.Resolver.Completions, It.Next_Num);
      Strs     : constant Completion_Strings := Spelling (The_Res);
      --  ??? Break this into steps and add traces
   begin
      Proposal.Resolver := It.Resolver;
      Proposal.Name := new String'(To_String (Strs.Completion));

      Proposal.Label := Strs.Completion;
      Proposal.Documentation := Strs.Doc;

      case Kind (The_Res) is
         when CXCursor_EnumDecl | CXCursor_EnumConstantDecl =>
            Proposal.Category := Cat_Literal;

         when CXCursor_FieldDecl =>
            Proposal.Category := Cat_Field;

         when CXCursor_FunctionDecl | CXCursor_FunctionTemplate =>
            Proposal.Category := Cat_Function;

         when CXCursor_ParmDecl =>
            Proposal.Category := Cat_Parameter;

         when CXCursor_TypedefDecl | CXCursor_TypeAliasDecl =>
            Proposal.Category := Cat_Type;

         when CXCursor_VarDecl =>
            Proposal.Category := Cat_Variable;

         when others =>
            Proposal.Category := Cat_Unknown;
      end case;

      return Proposal;
   end Get;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : Simple_Libclang_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id
   is
      pragma Unreferenced (Db);
   begin
      return (Proposal.Name'Length,
              "LIBCLANG",
              Proposal.Name.all,
              GNATCOLL.VFS.No_File, 0, 0);
   end To_Completion_Id;

   -----------------------------------------
   -- New_C_Construct_Completion_Resolver --
   -----------------------------------------

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

      if Indexer = No_Index then
         Indexer := Create_Index (True, Active (Diagnostics));
      end if;

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

      C_Context  : C_Completion_Context;
      Expression : Parsed_Expression;
      Token      : Token_Record;
   begin
      --  Find the prefix of the word
      --  ??? we should use libclang for that.

      --  Below is a simple implementation, to be used if we switch away
      --  from a C resolver manager but before we have the tree exploration
      --  in libclang

--        declare
--           Unichar    : Gunichar;
--           The_Char   : String (1 .. 6);
--           Last       : Natural;
--           Prefix : String (1 .. Integer (Loc.Column) + 1);
--           Prefix_Last : Natural := Prefix'Last;
--        begin
--           Loc := Loc.Forward_Char (-1);
--           loop
--              Unichar := Gunichar (Loc.Get_Char);
--              Unichar_To_UTF8 (Unichar, The_Char, Last);
--
--              Loc := Loc.Forward_Char (-1);
--              exit when not (Is_Alnum (Unichar)
--                             or else Unichar = Character'Pos ('_'));
--              Prefix (Prefix_Last - Last + 1 .. Prefix_Last) :=
--                The_Char (1 .. Last);
--              Prefix_Last := Prefix_Last - Last;
--              exit when Integer (Loc.Column) <= 1;
--           end loop;
--
--           Result.Searched_Identifier := new String'
--             (Prefix (Prefix_Last + 1 .. Prefix'Last));
--        end;

      if Context.all not in C_Completion_Context'Class then
         return;
      end if;

      C_Context  := C_Completion_Context (Context.all);
      Expression := C_Context.Expression;

      case Token_List.Length (Expression.Tokens) is
         when 0 =>
            return;

         --  No context available: our candidates are all the C/C++ entities
         --  whose prefix matches this one! We perform the computation of
         --  proposals in an incremental way using iterators.

         when others =>
            Token := Token_List.Data (Token_List.Last (Expression.Tokens));

            case Token.Tok_Type is
               when  Tok_Identifier =>
                  declare
                     Prefix : constant String :=
                       Context.Buffer
                         (Natural (Token.Token_First)
                          .. Natural (Token.Token_Last));

                  begin
                     Result.Searched_Identifier := new String'(Prefix);
                     Resolver.Prefix := new String'(Prefix);
                  end;
               when Tok_Dereference | Tok_Dot | Tok_Scope | Tok_Colon =>
                  Result.Searched_Identifier := new String'("");
                  Resolver.Prefix := new String'("");
               when others =>
                  return;
            end case;
      end case;

      Trace (Me, "Completion prefix: " & Result.Searched_Identifier.all);
      Component.Resolver := Resolver;

      declare
         Unsaved_Files : constant Unsaved_File_Array :=
           (1 => Create_Unsaved_File
              (Filename,
               Ada.Strings.Unbounded.String_Access (Context.Buffer)));
         --  ??? We should fill other unsaved_files!

         Lang             : constant String :=
           Resolver.Kernel.Lang_Handler.Get_Language_From_File (Context.File);
         C_Switches       : String_List_Access;
         Ignored          : Boolean;

         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Resolver.Kernel.Registry.Tree.Info_Set
                (Context.File).First_Element);

      begin
         --  Retrieve the switches for this file
         Switches (F_Info.Project,
                   "compiler", Context.File, Lang, C_Switches, Ignored);

         declare
            The_Switches     : Unbounded_String_Array (C_Switches'Range);

         begin
            for J in C_Switches'Range loop
               The_Switches (J) := To_Unbounded_String (C_Switches (J).all);
            end loop;

            Resolver.TU := Parse_Translation_Unit
              (Index             => Indexer,
               Source_Filename   => Filename,
               Command_Line_Args =>

               --  We pass to libclang a list of switches made of:
               --  ... the C/C++ switches specified in this project
               The_Switches

               --  ... a -I<dir> for each directory in the subprojects
               --  of this project
               & Get_Project_Source_Dirs
                 (Resolver.Kernel, F_Info.Project, Lang)

               --  ... a -I<dir> for each dir in the compiler search path
               & Get_Compiler_Search_Paths
                 (Resolver.Kernel, F_Info.Project, Lang),

               Unsaved_Files     => Unsaved_Files,
               Options           => No_Translation_Unit_Flags);

            Free (C_Switches);
         end;

         Loc := Loc.Forward_Char (0 - UTF8_Length (Resolver.Prefix.all));

         Resolver.Completions := Complete_At
           (Resolver.TU,
            Filename      => +Context.File.Full_Name,
            Line          => Loc.Line,
            Column        => Integer (Loc.Column),
            Unsaved_Files => Unsaved_Files);
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
      Dispose (This.TU);
      Dispose (This.Completions);
      Free (This.Prefix);
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
