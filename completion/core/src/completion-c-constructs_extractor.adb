------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with Ada.Containers.Vectors;
with Language.Cpp;     use Language.Cpp;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;  use GNATCOLL.Traces;
with Xref;             use Xref;
with Ada.Containers.Indefinite_Vectors;

package body Completion.C.Constructs_Extractor is
   Me : constant Trace_Handle := Create ("GPS.COMPLETION.C");

   Resolver_ID : constant String := "CNST_C  ";

   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   procedure Get_Completion_Root_With_Context
     (Resolver : access C_Completion_Resolver;
      Context  : Completion_Context;
      Result   : in out Completion_List);
   --  Subsidiary subprogram of Get_Completion_Root which takes care of
   --  generating the proposals when we have a context. In such case the
   --  number of proposals is small and thus we generate all the proposals
   --  at once (ie. we do not generate the proposals incrementally).

   function To_Language_Category
     (Db : access General_Xref_Database_Record'Class;
      E  : Root_Entity'Class) return Language_Category;
   --  Make a simple association between entity categories and construct
   --  categories. This association is known to be inaccurate.

   ---------------------------------
   -- Completion_Proposal_Handler --
   ---------------------------------

   package Completion_Proposal_Handler is
      type C_Completion_Proposal is
        new Simple_Completion_Proposal with private;

      Null_C_Completion_Proposal : constant C_Completion_Proposal;

      function New_C_Completion_Proposal
        (Resolver    : access C_Completion_Resolver;
         Entity      : Root_Entity'Class;
         With_Params : Boolean := False;
         Is_Param    : Boolean := False) return C_Completion_Proposal;
      --  Constructor

      overriding function Get_Category
         (Proposal : C_Completion_Proposal) return Language_Category;

      overriding function Get_Completion
        (Proposal : C_Completion_Proposal;
         Db       : access Xref.General_Xref_Database_Record'Class)
         return UTF8_String;
      --  Handle the completion of a single parameter of a subprogram call, the
      --  completion of all the parameters of a subprogram call, and also the
      --  completion of a single entity name.

      overriding function Get_Label
        (Proposal : C_Completion_Proposal;
         Db       : access Xref.General_Xref_Database_Record'Class)
         return UTF8_String;
      --  Generate the label "<entity> without params" when the proposal
      --  requests the completion of the parameters of a subprogram call and
      --  the entity of the proposal has no parameters; generate the label
      --  "params of <entity>" when the proposal requests the completion with
      --  parameters and the entity of the proposal has parameters; otherwise
      --  generate the label "<entity>".

      overriding function Get_Location
        (Proposal : C_Completion_Proposal;
         Db       : access Xref.General_Xref_Database_Record'Class)
         return File_Location;

      overriding function Get_Visibility
        (Proposal : C_Completion_Proposal) return Construct_Visibility;

      overriding function Is_Valid
        (Proposal : C_Completion_Proposal) return Boolean;

      overriding function To_Completion_Id
        (Proposal : C_Completion_Proposal;
         Db       : access Xref.General_Xref_Database_Record'Class)
         return Completion_Id;

   private
      type C_Completion_Proposal is new Simple_Completion_Proposal with record
         Entity_Info : Root_Entity_Ref;

         With_Params : Boolean := False;
         --  Set to true if Entity_Info is a subprogram and we need to provide
         --  completions with all the parameters needed to call it

         Is_Param    : Boolean := False;
         --  Set to true if Entity_Info is a formal parameter of a subprogram
         --  call and we need to provide completion for this single parameter
      end record;

      Null_C_Completion_Proposal : constant C_Completion_Proposal :=
        (Simple_Completion_Proposal (Null_Completion_Proposal)
            with Entity_Info => Root_Entity_Refs.Empty_Holder,
                 With_Params => False,
                 Is_Param    => False);

   end Completion_Proposal_Handler;
   use Completion_Proposal_Handler;

   --------------------------
   -- Completion_Iterators --
   --------------------------

   --  Iterators used to perform an incremental computation of the completions
   --  when no context is available. In such case the computation of the
   --  proposals may be long for large C/C++ projects. This architecture has
   --  to do with the fact that GPS is mono-thread, and, in order for the UI
   --  to be responsive, we need to process UI events (such as responding to
   --  user input or just refreshing the display); we cannot stay long without
   --  processing UI events.
   --
   --  So what we do is the following: we don't compute all possible
   --  completions in one go, this takes too much time and blocks the UI for
   --  too long. Instead, we first compute a small chunk, then we process some
   --  UI events, then we compute the next small chunk of possible completions,
   --  then process some UI events, etc.
   --
   --  In the code, here is where things happen:
   --
   --  1 - In Completion_Module.Smart_Complete, we register a number of
   --      Resolvers, then get the initial completion (call to
   --      Get_Initial_Completion_List)
   --
   --  2 - Then we launch the completion window, which displays the first
   --      results, and then calls every 100 ms the function Idle_Expression
   --      (in package Completion_Window) which asks the Resolvers to give
   --      the next chunk of completions.

   package Completion_Iterators is

      type C_Construct_Wrapper is
        new Completion_List_Pckg.Virtual_List_Component with private;

      function New_C_Construct_Wrapper
        (Resolver : access C_Completion_Resolver;
         Context  : Completion_Context;
         Prefix   : String) return C_Construct_Wrapper;
      --  Constructor

      overriding function First
        (Wrapper : C_Construct_Wrapper)
         return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;

      overriding procedure Free (This : in out C_Construct_Wrapper);

   private
      type C_Construct_Wrapper is
        new Completion_List_Pckg.Virtual_List_Component
      with record
         Resolver : access C_Completion_Resolver;
         Context  : Completion_Context;
         Prefix   : GNAT.Strings.String_Access;
      end record;

      package Entities_List is new Ada.Containers.Indefinite_Vectors
        (Index_Type => Natural, Element_Type => Root_Entity'Class);

      package Files_List is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => GNATCOLL.VFS.Virtual_File);

      type Stages is (Stage_1, Stage_2, Stage_End);
      --  In stage 1 we search for proposals iterating through all the entities
      --  visible available through the transitive closure of the request for
      --  completions (that is, we search for proposals traversing all the
      --  include files). In stage 2 we search for global proposals which
      --  were not added to the list of proposal during stage 1.

      type Construct_Iterator_Wrapper is new
        Completion_List_Pckg.Virtual_List_Component_Iterator
      with record
         Stage    : Stages;
         Resolver : access C_Completion_Resolver;
         Prefix   : GNAT.Strings.String_Access;

         --  Components associated with Stage 1

         Entities        : Entities_List.Vector;
         Processed_Files : Files_List.Vector;
         Queued_Files    : Files_List.Vector;
         --  List of processed files and queue of pending files. The whole list
         --  of files stored in these structures are used to avoid processing
         --  twice an include file. The overall behavior is that files found as
         --  part of processing all the entities of a given file are appended
         --  to Queued_Files; when all the entities of the next queued file are
         --  processed the file is moved to the list of processed files.

         Cursor            : Entities_In_File_Cursor;
         In_First_Entity   : Boolean;
         Is_Valid_Proposal : Boolean;

         --  Components associated with Stage 2

         Project_Cursor    : Entities_In_Project_Cursor;
      end record;

      overriding function At_End
        (It : Construct_Iterator_Wrapper) return Boolean;

      overriding procedure Next (It : in out Construct_Iterator_Wrapper);

      overriding function Get
        (This : in out Construct_Iterator_Wrapper)
         return Completion_Proposal'Class;

      overriding procedure Free (This : in out Construct_Iterator_Wrapper);

   end Completion_Iterators;
   use Completion_Iterators;

   --------------------------
   -- Completion_Iterators --
   --------------------------

   package body Completion_Iterators is

      ------------
      -- At_End --
      ------------

      overriding function At_End
        (It : Construct_Iterator_Wrapper) return Boolean is
      begin
         return It.Stage = Stage_End;
      end At_End;

      -----------
      -- First --
      -----------

      overriding function First
        (Wrapper : C_Construct_Wrapper)
         return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
      is
         Db     : constant General_Xref_Database :=
                    Wrapper.Resolver.Kernel.Databases;
         Result : Construct_Iterator_Wrapper;
         Project : Project_Type;
         File : constant Virtual_File := Get_File (Wrapper.Context);
      begin
         Result.Stage    := Stage_1;
         Result.Resolver := Wrapper.Resolver;
         Result.Prefix   := Wrapper.Prefix;

         Result.Processed_Files.Append (File);

         --  ??? Should get the project from elsewhere
         declare
            F_Info : constant File_Info'Class :=
              File_Info'Class
                (Wrapper.Resolver.Kernel.Registry.Tree.Info_Set
                   (File).First_Element);
         begin
            Project := F_Info.Project;
         end;
         Result.Cursor := Db.Entities_In_File (File, Project);
         Result.In_First_Entity := True;
         Result.Is_Valid_Proposal := True;

         --  Move the cursor to the first entity that has the prefix
         Result.Next;

         return Result;
      end First;

      ----------
      -- Free --
      ----------

      overriding procedure Free (This : in out C_Construct_Wrapper) is
      begin
         Free (This.Prefix);
      end Free;

      overriding procedure Free (This : in out Construct_Iterator_Wrapper) is
      begin
         --  We do not free Prefix here since it will be done by the free
         --  associated with the C_Construct_Wrapper

         This.Queued_Files.Clear;
      end Free;

      ---------
      -- Get --
      ---------

      overriding function Get
        (This : in out Construct_Iterator_Wrapper)
         return Completion_Proposal'Class is
      begin
         case This.Stage is
            when Stage_1 =>
               return New_C_Completion_Proposal
                 (Resolver => This.Resolver,
                  Entity   => Get (This.Cursor));

            when Stage_2 =>
               return New_C_Completion_Proposal
                 (Resolver => This.Resolver,
                  Entity   => Get (This.Project_Cursor));

            when Stage_End =>
               return Null_C_Completion_Proposal;
         end case;
      end Get;

      -----------------------------
      -- New_C_Construct_Wrapper --
      -----------------------------

      function New_C_Construct_Wrapper
        (Resolver : access C_Completion_Resolver;
         Context  : Completion_Context;
         Prefix   : String) return C_Construct_Wrapper is
      begin
         return C_Construct_Wrapper'
                   (Resolver => Resolver,
                    Context  => Context,
                    Prefix   => new String'(Prefix));
      end New_C_Construct_Wrapper;

      ----------
      -- Next --
      ----------

      overriding procedure Next (It : in out Construct_Iterator_Wrapper) is

         function Has_Prefix (Name : String) return Boolean;
         --  Returns True if Name has prefix It.Prefix

         function Contains (E : Root_Entity'Class) return Boolean;
         --  Return True if It.Entities contains E

         --------------
         -- Contains --
         --------------

         function Contains (E : Root_Entity'Class) return Boolean is
         begin
            return It.Entities.Contains (E);
         end Contains;

         ----------------
         -- Has_Prefix --
         ----------------

         function Has_Prefix (Name : String) return Boolean is
         begin
            return Name'Length >= It.Prefix'Length
              and then
                Name (Name'First .. Name'First + It.Prefix'Length - 1)
              = It.Prefix.all;
         end Has_Prefix;

         Db      : constant General_Xref_Database :=
           It.Resolver.Kernel.Databases;
         Decl    : General_Entity_Declaration;
         Project : Project_Type;

      begin
         case It.Stage is
            when Stage_1 =>
               --  Processing the first entity we must not move the cursor

               if It.In_First_Entity then
                  It.In_First_Entity := False;

               elsif not At_End (It.Cursor) then
                  Next (It.Cursor);
               end if;

               loop
                  --  Iterate through entities of the current file appending
                  --  include files to the queue of files to be processed and
                  --  searching for a global entity whose prefix matches

                  while not At_End (It.Cursor) loop
                     declare
                        E    : constant Root_Entity'Class := Get (It.Cursor);
                     begin
                        if Get_Display_Kind (E) = "include file" then
                           Decl := Get_Declaration (E);

                           if not It.Processed_Files.Contains (Decl.Loc.File)
                             and then not
                               It.Queued_Files.Contains (Decl.Loc.File)
                           then
                              It.Queued_Files.Append (Decl.Loc.File);
                           end if;
                        end if;

                        if Has_Prefix (Get_Name (E)) then
                           It.Entities.Append (E);
                           return;  --  Proposal found!
                        end if;
                     end;

                     Next (It.Cursor);
                  end loop;

                  --  Next file

                  if Natural (It.Queued_Files.Length) = 0 then
                     It.Stage := Stage_2;
                     It.Project_Cursor :=
                       Db.All_Entities_From_Prefix
                         (It.Prefix.all, Is_Partial => True);

                     while not At_End (It.Project_Cursor) loop
                        if not Contains (Get (It.Project_Cursor)) then
                           return;
                        end if;

                        Next (It.Project_Cursor);
                     end loop;

                     It.Stage := Stage_End;
                     return;
                  end if;

                  --  ??? Should get the project from elsewhere
                  declare
                     F_Info : constant File_Info'Class :=
                       File_Info'Class
                         (It.Resolver.Kernel.Registry.Tree.Info_Set
                            (It.Queued_Files.First_Element).First_Element);
                  begin
                     Project := F_Info.Project;
                  end;
                  It.Cursor := Db.Entities_In_File
                    (It.Queued_Files.First_Element, Project);

                  --  Move the file to the list of processed files

                  It.Processed_Files.Append (It.Queued_Files.First_Element);
                  It.Queued_Files.Delete_First;
               end loop;

            when Stage_2 =>
               Next (It.Project_Cursor);

               while not At_End (It.Project_Cursor) loop
                  if not Contains (Get (It.Project_Cursor)) then
                     return;
                  end if;

                  Next (It.Project_Cursor);
               end loop;

               It.Stage := Stage_End;

            when Stage_End =>
               null;
         end case;

      exception
         when E : others =>
            Trace (Me, E);
            raise;
      end Next;

   end Completion_Iterators;

   ---------------------------------
   -- Completion_Proposal_Handler --
   ---------------------------------

   package body Completion_Proposal_Handler is

      ------------------
      -- Get_Category --
      ------------------

      overriding function Get_Category
        (Proposal : C_Completion_Proposal) return Language_Category is
      begin
         return Proposal.Category;
      end Get_Category;

      --------------------
      -- Get_Completion --
      --------------------

      overriding function Get_Completion
        (Proposal : C_Completion_Proposal;
         Db       : access General_Xref_Database_Record'Class)
         return UTF8_String
      is
         pragma Unreferenced (Db);
         function Single_Param_Text (Param : Root_Entity'Class) return String;
         --  Generate the named notation associated with Param as a C comment.
         --  For example: "/* Param */"

         function All_Params_Text return String;
         --  Recursively traverse the whole list of parameters of the
         --  subprogram proposal and generate the named notation
         --  associated with all the parameters as C comments.

         -----------------------
         -- Single_Param_Text --
         -----------------------

         function Single_Param_Text (Param : Root_Entity'Class)
                                     return String is
         begin
            return "/* " & Get_Name (Param) & " */";
         end Single_Param_Text;

         ---------------------
         -- All_Params_Text --
         ---------------------

         function All_Params_Text return String is
            Separator : constant String := "," & ASCII.LF;
            Spaces    : constant String := "  ";
            Params    : Xref.Parameter_Array :=
              Parameters (Proposal.Entity_Info.Element);

            function Next_Params
              (P : Integer; Prev_Params : String) return String;
            --  Prev_Params is used to accumulate the output.

            function Next_Params
              (P : Integer; Prev_Params : String) return String is
            begin
               if P > Params'Last then
                  return Prev_Params;
               else
                  return
                    Next_Params
                      (P + 1,
                       Prev_Params
                       & Separator
                       & Single_Param_Text (Params (P).Parameter.all)
                       & Spaces);
               end if;
            end Next_Params;

            --  Start of processing for All_Params_Text

         begin
            if Params'Length /= 0 then
               return A : constant String := Next_Params
                 (Params'First + 1,
                  Single_Param_Text
                    (Params (Params'First).Parameter.all) & Spaces)
               do
                  Free (Params);
               end return;
            else
               return A : constant String := Next_Params
                 (Params'First + 1,
                  Single_Param_Text (No_Root_Entity) & Spaces)
               do
                  Free (Params);
               end return;
            end if;
         end All_Params_Text;

         --  Start of processing for Get_Completion

      begin
         if Proposal.With_Params then
            return All_Params_Text & ")";

         elsif Proposal.Is_Param then
            return Single_Param_Text (Proposal.Entity_Info.Element);

         else
            return Proposal.Name.all;
         end if;
      end Get_Completion;

      ---------------
      -- Get_Label --
      ---------------

      overriding function Get_Label
        (Proposal : C_Completion_Proposal;
         Db       : access General_Xref_Database_Record'Class)
         return UTF8_String is
         pragma Unreferenced (Db);
      begin
         if not Proposal.With_Params then
            return Proposal.Name.all;
         else
            declare
               Params : constant Parameter_Array :=
                 Parameters (Proposal.Entity_Info.Element);
            begin
               if Params'Length = 0 then
                  return Proposal.Name.all & " without params";
               else
                  return "params of " & Proposal.Name.all;
               end if;
            end;
         end if;
      end Get_Label;

      ------------------
      -- Get_Location --
      ------------------

      overriding function Get_Location
        (Proposal : C_Completion_Proposal;
         Db       : access General_Xref_Database_Record'Class)
         return File_Location
      is
         pragma Unreferenced (Db);
         Loc : constant General_Location :=
                 Get_Declaration (Proposal.Entity_Info.Element).Loc;
      begin
         return (Loc.File, Loc.Line, Loc.Column);
      end Get_Location;

      --------------------
      -- Get_Visibility --
      --------------------

      overriding function Get_Visibility
        (Proposal : C_Completion_Proposal) return Construct_Visibility is
         pragma Unreferenced (Proposal);
      begin
         return Visibility_Public;
      end Get_Visibility;

      --------------
      -- Is_Valid --
      --------------

      overriding function Is_Valid
        (Proposal : C_Completion_Proposal) return Boolean is
      begin
         return Proposal /= Null_C_Completion_Proposal;
      end Is_Valid;

      -------------------------------
      -- New_C_Completion_Proposal --
      -------------------------------

      function New_C_Completion_Proposal
         (Resolver    : access C_Completion_Resolver;
          Entity      : Root_Entity'Class;
          With_Params : Boolean := False;
          Is_Param    : Boolean := False) return C_Completion_Proposal
      is
         Db : constant General_Xref_Database := Resolver.Kernel.Databases;
         H  : Root_Entity_Ref;
      begin
         H.Replace_Element (Entity);
         return C_Completion_Proposal'
           (Resolver      => Resolver,
            Name          => new String'(Get_Name (Entity)),
            Category      => To_Language_Category (Db, Entity),
            Entity_Info   => H,
            With_Params   => With_Params,
            Is_Param      => Is_Param);
      end New_C_Completion_Proposal;

      ----------------------
      -- To_Completion_Id --
      ----------------------

      overriding function To_Completion_Id
        (Proposal : C_Completion_Proposal;
         Db       : access Xref.General_Xref_Database_Record'Class)
         return Completion_Id
      is
         pragma Unreferenced (Db);
         Id  : constant String := Proposal.Name.all;
         Loc : constant General_Location :=
           Get_Declaration (Proposal.Entity_Info.Element).Loc;

      begin
         return (Id_Length   => Id'Length,
                 Resolver_Id => Resolver_ID,
                 Id          => Id,
                 File        => Loc.File,
                 Line        => Loc.Line,
                 Column      => Integer (Loc.Column));
      end To_Completion_Id;
   end Completion_Proposal_Handler;

   -----------------------------------------
   -- New_C_Construct_Completion_Resolver --
   -----------------------------------------

   function New_C_Construct_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File) return Completion_Resolver_Access
   is
      pragma Unreferenced (Current_File);
   begin
      return new C_Completion_Resolver'
                   (Manager     => null,
                    Kernel      => Kernel);
   end New_C_Construct_Completion_Resolver;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out C_Completion_Resolver) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver : access C_Completion_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      pragma Unreferenced (Offset);
      C_Context  : C_Completion_Context;
      Expression : Parsed_Expression;
      Token      : Token_Record;
   begin
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

         when 1 =>
            Token := Expression.Tokens.First_Element;

            --  No action needed if the prefix is not an identifier. Thus we
            --  also avoid proposing internal names generated by the G++
            --  compiler when the prefix is just ".".

            if Token.Tok_Type /= Tok_Identifier then
               return;
            end if;

            declare
               Prefix : constant String :=
                          Context.Buffer
                            (Natural (Token.Token_First)
                               .. Natural (Token.Token_Last));

            begin
               Append (Result.List,
                 New_C_Construct_Wrapper
                   (Resolver => Resolver,
                    Context  => Context,
                    Prefix   => Prefix));

               Result.Searched_Identifier := new String'(Prefix);
               return;
            end;

         --  Analyze the context to suggest better proposals

         when others =>
            Get_Completion_Root_With_Context (Resolver, Context, Result);
      end case;
   end Get_Completion_Root;

   --------------------------------------
   -- Get_Completion_Root_With_Context --
   --------------------------------------

   procedure Get_Completion_Root_With_Context
     (Resolver : access C_Completion_Resolver;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      Db : constant General_Xref_Database := Resolver.Kernel.Databases;

      C_Context  : constant C_Completion_Context :=
                     C_Completion_Context (Context.all);
      Expression : constant Parsed_Expression :=
                     C_Context.Expression;

      procedure Append_Proposal
        (To_List : in out Extensive_List_Pckg.Vector;
         E       : Root_Entity'Class);
      --  Append to the list To_List the proposal E

      procedure Append_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.Vector;
         E       : Root_Entity'Class);
      --  Append to the list To_List one proposal containing all the
      --  parameters needed to call E. In addition, in order to provide the
      --  same behavior of Ada completions, one proposal per parameter is
      --  added to To_List.

      procedure Append_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.Vector;
         Scope        : Root_Entity'Class;
         Prefix_Token : Token_Record);
      --  Append to To_List all the proposals defined in Scope which match
      --  the prefix available in Prefix_Token. If Prefix_Token is not an
      --  identifier then all the entities defined in Scope are appended
      --  to To_List.

      function Is_Self_Referenced_Type
        (E : Root_Entity'Class) return Boolean;
      --  Return true if the type of E is itself

      procedure Prev_Token;
      --  Displace Tok_Index and Tok_Prev to reference the previous token

      ---------------------
      -- Append_Proposal --
      ---------------------

      procedure Append_Proposal
        (To_List : in out Extensive_List_Pckg.Vector;
         E       : Root_Entity'Class)
      is
      begin
         Append (To_List, New_C_Completion_Proposal (Resolver, E));
      end Append_Proposal;

      ---------------------------------
      -- Append_Proposal_With_Params --
      ---------------------------------

      procedure Append_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.Vector;
         E       : Root_Entity'Class)
      is
         Params : Xref.Parameter_Array := Parameters (E);

      begin
         Append (To_List,
           New_C_Completion_Proposal
             (Resolver    => Resolver,
              Entity      => E,
              With_Params => True));

         for P in Params'Range loop
            Append (To_List,
              New_C_Completion_Proposal
                (Resolver => Resolver,
                 Entity   => Params (P).Parameter.all,
                 Is_Param => True));
         end loop;

         Free (Params);
      end Append_Proposal_With_Params;

      ----------------------------
      -- Append_Scope_Proposals --
      ----------------------------

      procedure Append_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.Vector;
         Scope        : Root_Entity'Class;
         Prefix_Token : Token_Record)
      is
         Prefix_Text   : constant String :=
                           Context.Buffer
                             (Natural (Prefix_Token.Token_First)
                                .. Natural (Prefix_Token.Token_Last));
         It : Abstract_Entities_Cursor'Class :=
           Get_All_Called_Entities (Scope);

      begin
         while not At_End (It) loop
            declare
               E  : constant Root_Entity'Class := Get (It);
            begin
               --  Do not suggest types found in the scope of other types. Done
               --  to avoid suggesting in C/C++ types used in declaration of
               --  struct components. For example:
               --
               --        typedef struct {
               --           my_type_1 c1;
               --           my_type_2 c2;
               --        } my_type_3;
               --
               --  This ensures that we only suggest c1 and c2 for completion.

               if Is_Type (Scope)
                 and then Is_Type (E)
               then
                  null;

                  --  Do not suggest subprogram parameters

               elsif Is_Parameter_Of (E) /= No_Root_Entity then
                  null;

                  --  The last token is a delimiter (dot, scope or dereference)

               elsif Prefix_Token.Tok_Type /= Tok_Identifier then
                  Append_Proposal (To_List, E);

                  --  The last token is an identifier. Check if the name of the
                  --  entity is a valid prefix

               else
                  declare
                     Nam : constant String := Get_Name (E);

                  begin
                     if Prefix_Text'Length <= Nam'Length
                       and then
                         Nam (Nam'First .. Nam'First + Prefix_Text'Length - 1)
                       = Prefix_Text
                     then
                        Append_Proposal (To_List, E);
                     end if;
                  end;
               end if;
            end;
            Next (It);
         end loop;

         Destroy (It);
      end Append_Scope_Proposals;

      -----------------------------
      -- Is_Self_Referenced_Type --
      -----------------------------

      function Is_Self_Referenced_Type
        (E : Root_Entity'Class) return Boolean is
      begin
         return
           E /= No_Root_Entity
           and then Is_Type (E)
           and then Is_Container (E)
           and then Is_Global (E)
           and then Get_Name (Caller_At_Declaration (E))
             = Get_Name (E);
      end Is_Self_Referenced_Type;

      ----------------
      -- Prev_Token --
      ----------------

      Token      : Token_Record;
      Token_Prev : Token_Record;
      Tok_Index  : Token_List.Cursor;
      Tok_Prev   : Token_List.Cursor;

      procedure Prev_Token is
         use Token_List;

      begin
         Tok_Index := Tok_Prev;

         if Has_Element (Tok_Prev) then
            Token    := Element (Tok_Index);
            Tok_Prev := Previous (Tok_Index);

            if Has_Element (Tok_Prev) then
               Token_Prev := Element (Tok_Prev);
            else
               Token_Prev := Null_Token;
            end if;

         else
            Token      := Null_Token;
            Token_Prev := Null_Token;
         end if;
      end Prev_Token;

      --  Local variables

      Last_Token      : constant Token_Record :=
        Expression.Tokens.Last_Element;
      Last_Token_Text : constant String :=
                          Context.Buffer
                            (Natural (Last_Token.Token_First)
                               .. Natural (Last_Token.Token_Last));
      E_List          : Extensive_List_Pckg.Vector;

      use Token_List;
      use type Ada.Containers.Count_Type;

   --  Start of processing for Get_Completion_Root_With_Context

   begin
      pragma Assert (Context.all in C_Completion_Context'Class);
      pragma Assert (Expression.Tokens.Length > 1);

      Tok_Index  := Last (Expression.Tokens);
      Tok_Prev   := Previous (Tok_Index);
      Token      := Element (Tok_Index);
      Token_Prev := Element (Tok_Prev);

      if Token.Tok_Type = Tok_Identifier
        and then (Token_Prev.Tok_Type = Tok_Dot
                    or else Token_Prev.Tok_Type = Tok_Dereference
                    or else Token_Prev.Tok_Type = Tok_Scope)
      then
         Prev_Token;
      end if;

      if Token.Tok_Type = Tok_Left_Paren then
         Prev_Token;

         if Token.Tok_Type /= Tok_Identifier then
            return;
         end if;

         --  Adding subprogram call proposals

         declare
            Prefix : constant String :=
                       Context.Buffer
                         (Natural (Token.Token_First)
                            .. Natural (Token.Token_Last));
            Iter   : Entities_In_Project_Cursor :=
                       Db.All_Entities_From_Prefix
                         (Prefix => Prefix, Is_Partial => False);

         begin
            while not At_End (Iter) loop
               declare
                  E      : constant Root_Entity'Class := Get (Iter);
               begin
                  if Is_Subprogram (E) then
                     Append_Proposal_With_Params
                       (To_List => E_List,
                        E       => E);
                  end if;
               end;
               Next (Iter);
            end loop;

            Destroy (Iter);

            Completion_List_Pckg.Append
              (Result.List,
               To_Extensive_List (E_List));
         end;

      elsif Token.Tok_Type = Tok_Dot
        or else Token.Tok_Type = Tok_Dereference
        or else Token.Tok_Type = Tok_Scope
      then
         Prev_Token;

         --  Skip array subscripts

         if Token.Tok_Type = Tok_Right_Sq_Bracket then
            declare
               Expr_Depth : Natural := 0;

            begin
               loop
                  case Token.Tok_Type is
                     when Tok_Right_Sq_Bracket =>
                        Expr_Depth := Expr_Depth + 1;

                     when Tok_Left_Sq_Bracket =>
                        Expr_Depth := Expr_Depth - 1;

                     when others =>
                        null;
                  end case;

                  Prev_Token;

                  exit when
                    (Expr_Depth = 0
                       and then
                         Token.Tok_Type /= Tok_Right_Sq_Bracket)
                    or else Tok_Prev = Token_List.No_Element;
               end loop;
            end;
         end if;

         --  Protect us against wrong sources

         if Token = Null_Token then
            return;
         end if;

         --  Adding proposals

         declare
            Prefix   : constant String :=
                         Context.Buffer
                           (Natural (Token.Token_First)
                              .. Natural (Token.Token_Last));
            Iter     : Entities_In_Project_Cursor :=
                         Db.All_Entities_From_Prefix
                           (Prefix => Prefix, Is_Partial => False);

         begin
            while not At_End (Iter) loop
               declare
                  E        : Root_Entity'Class := Get (Iter);
               begin
                  if not Is_Type (E) then
                     if Is_Access (E) then
                        E := Pointed_Type (E);

                     elsif Is_Array (E) then
                        E := Component_Type (E);

                        --  Class or record
                     elsif Has_Methods (E) then
                        E := Get_Type_Of (E);

                     else
                        E := Get_Type_Of (E);
                     end if;
                  end if;

                  if E /= No_Root_Entity then

                     --  Class or record

                     if Has_Methods (E) then

                        --  Handle named typedef structs since the compiler
                        --  generates two entites in the LI file with the
                        --  same name. For example:
                        --
                        --     typedef struct {    // First_Entity
                        --       ...
                        --     } my_type;          // Second_Entity
                        --
                        --  When we declare an object of this type:
                        --
                        --     my_type obj;
                        --
                        --  The scope of obj references Second_Entity, whose
                        --  (parent) type is First_Entity (which is the entity
                        --  needed for completion purposes)

                        if Is_Self_Referenced_Type (E) then
                           E := Caller_At_Declaration (E);
                        end if;

                        Append_Scope_Proposals
                          (To_List      => E_List,
                           Scope        => E,
                           Prefix_Token => Last_Token);
                     end if;
                  end if;
               end;
               Next (Iter);
            end loop;

            Destroy (Iter);

            Completion_List_Pckg.Append
              (Result.List,
               To_Extensive_List (E_List));

            if Last_Token.Tok_Type = Tok_Identifier then
               Result.Searched_Identifier :=
                 new String'(Last_Token_Text);
            end if;
         end;
      end if;
   end Get_Completion_Root_With_Context;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : C_Completion_Resolver) return String
   is
      pragma Unreferenced (Resolver);
   begin
      return Resolver_ID;
   end Get_Id;

   --------------------------
   -- To_Language_Category --
   --------------------------

   function To_Language_Category
     (Db : access General_Xref_Database_Record'Class;
      E   : Root_Entity'Class) return Language_Category
   is
      pragma Unreferenced (Db);
   begin
      if Has_Methods (E) then
         return Cat_Class;

      elsif Is_Subprogram (E) then
         return Cat_Function;

      elsif not Is_Type (E) then
         return Cat_Variable;
      end if;

      return Cat_Unknown;
   end To_Language_Category;

end Completion.C.Constructs_Extractor;
