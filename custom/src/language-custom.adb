------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.Expect;             use GNAT.Expect;
with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Glib;
with Glib.Module;             use Glib.Module;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;

--  ??? Would be nice if languages registered themselves somewhere instead
--  of having a static knowledge of all the language defined.
with Language.Ada;
with Language.C;
with Language.Cpp;
with Language.Java;
with Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Custom_Naming_Editors;     use Custom_Naming_Editors;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Intl;                  use GPS.Intl;
with Project_Viewers;           use Project_Viewers;
with Projects;                  use Projects;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with String_Utils;              use String_Utils;
with XML_Utils;                 use XML_Utils;

package body Language.Custom is

   Me : constant Trace_Handle := Create ("Language.Custom");

   procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
     (Project_Field_Array, Project_Field_Array_Access);

   Null_Context : aliased Language_Context :=
     (Syntax => (Comment_Start                 => null,
                 Comment_End                   => null,
                 New_Line_Comment_Start        => null,
                 New_Line_Comment_Start_Regexp => null),
      String_Delimiter              => ASCII.NUL,
      Quote_Character               => ASCII.NUL,
      Constant_Character            => ASCII.NUL,
      Can_Indent                    => False,
      Syntax_Highlighting           => False,
      Case_Sensitive                => True,
      Accurate_Xref                 => False,
      Use_Semicolon                 => False);

   Custom_Root : Custom_Language_Access;
   --  Holds a linked list of custom languages, so that we can implement
   --  dynamic inheritance between custom languages.

   function Language_Category_Value (S : String) return Language_Category;
   --  Same as Language_Category'Value ("Cat_" & S), not relying on
   --  Constraint_Error, returning Cat_Custom if S does not match.

   -----------------------------
   -- Language_Category_Value --
   -----------------------------

   function Language_Category_Value (S : String) return Language_Category is
      Upper : constant String := "CAT_" & To_Upper (S);
   begin
      for L in Language_Category loop
         if L'Img = Upper then
            return L;
         end if;
      end loop;
      return Cat_Custom;
   end Language_Category_Value;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Index : String) return String is
   begin
      if Lang.Parent = null then
         return "";
      else
         return Array_Item_Name (Lang.Parent, Name, Index);
      end if;
   end Array_Item_Name;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Custom_Language;
      Name : String) return String is
   begin
      if Lang.Parent = null then
         return "";
      else
         return Dereference_Name (Lang.Parent, Name);
      end if;
   end Dereference_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access Custom_Language) return Explorer_Categories is
   begin
      if Lang.Categories'Length > 0
        or else Lang.Parent = null
      then
         return Lang.Categories.all;
      else
         return Explorer_Regexps (Lang.Parent);
      end if;
   end Explorer_Regexps;

   --------------------------------
   -- Get_Indentation_Parameters --
   --------------------------------

   overriding procedure Get_Indentation_Parameters
     (Lang         : access Custom_Language;
      Params       : out Indent_Parameters;
      Indent_Style : out Indentation_Kind) is
   begin
      if Lang.Parent /= null then
         Params       := Lang.Parent.Indent_Params;
         Indent_Style := Lang.Parent.Indent_Style;

      else
         Params       := Lang.Indent_Params;
         Indent_Style := Lang.Indent_Style;
      end if;
   end Get_Indentation_Parameters;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access Custom_Language) return Language_Context_Access is
   begin
      if Lang.Parent = null
        or else Lang.Context /= Null_Context'Access
      then
         return Lang.Context;
      else
         return Get_Language_Context (Lang.Parent);
      end if;
   end Get_Language_Context;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Lang : in out Custom_Language) is
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Explorer_Categories, Explorer_Categories_Access);
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Project_Field_Array, Project_Field_Array_Access);
   begin
      if Lang.Categories /= null then
         Language.Free (Lang.Categories.all);
         Unchecked_Free (Lang.Categories);
      end if;

      Basic_Types.Unchecked_Free (Lang.Keywords);
      GNAT.Strings.Free (Lang.Keywords_Regexp);
      GNAT.Strings.Free (Lang.Keywords_List);
      Free (Lang.Name);

      if Lang.Context /= Null_Context'Access then
         Free (Lang.Context);
      end if;

      if Lang.Project_Fields /= null then
         Free (Lang.Project_Fields.all);
         Unchecked_Free (Lang.Project_Fields);
      end if;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Handler : access Language_Handler_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Top     : XML_Utils.Node_Ptr)
   is
      use type Strings.String_Access;

      Lang : constant Custom_Language_Access :=
        new Language.Custom.Custom_Language;

      N, Node                 : Node_Ptr;
      Parent                  : Node_Ptr;
      Comment_Start           : String_Ptr;
      Comment_End             : String_Ptr;
      Tmp_Str                 : String_Ptr;
      Flags                   : Regexp_Flags := Multiple_Lines;
      New_Line_Comment_Start  : String_Ptr;
      Num_Categories          : Natural := 0;
      Tmp                     : Project_Field_Array_Access;
      Str                     : Unbounded_String;
      Keyword_Append          : Boolean := False; -- default is override

      function Contains_Special_Characters (S : String) return Boolean;
      --  Return whether S contains special regexp characters.

      function Get_String (S : String_Ptr) return String;
      --  Return a deep copy of S, or empty string if S is null

      procedure Parse_Character
        (Node          : Node_Ptr;
         Name          : String;
         Result        : out Character;
         Default_Value : Character := ASCII.NUL);
      --  Parse a character from field Name in a given Node.
      --  If the field cannot be found, set Result to Default_Value.

      procedure Parse_Boolean
        (Node          : Node_Ptr;
         Name          : String;
         Result        : out Boolean;
         Default_Value : Boolean := False);
      --  Parse a boolean from field Name in a given Node.
      --  If the field cannot be found, set Result to Default_Value.

      procedure Parse_Shared_Lib (Lib_Name : String);
      --  Parse xml part related to shared library symbols

      ---------------------------------
      -- Contains_Special_Characters --
      ---------------------------------

      Special_Char_Set : constant Character_Set := To_Set ("^$| *+?{[.(\-");
      --  Set of special characters that can be found in a regpat.

      function Contains_Special_Characters (S : String) return Boolean is
      begin
         for J in S'Range loop
            if Is_In (S (J), Special_Char_Set) then
               return True;
            end if;
         end loop;

         return False;
      end Contains_Special_Characters;

      ----------------
      -- Get_String --
      ----------------

      function Get_String (S : String_Ptr) return String is
      begin
         if S = null then
            return "";
         else
            return S.all;
         end if;
      end Get_String;

      ---------------------
      -- Parse_Character --
      ---------------------

      procedure Parse_Character
        (Node          : Node_Ptr;
         Name          : String;
         Result        : out Character;
         Default_Value : Character := ASCII.NUL)
      is
         Field : String_Ptr;
      begin
         Field := Get_Field (Node, Name);

         if Field = null then
            Result := Default_Value;
         else
            Result := Field (Field'First);
         end if;
      end Parse_Character;

      -------------------
      -- Parse_Boolean --
      -------------------

      procedure Parse_Boolean
        (Node          : Node_Ptr;
         Name          : String;
         Result        : out Boolean;
         Default_Value : Boolean := False)
      is
         Field : String_Ptr;
      begin
         Field := Get_Field (Node, Name);

         if Field = null then
            Result := Default_Value;
         else
            Result := Boolean'Value (Field.all);
         end if;

      exception
         when Constraint_Error =>
            Result := Default_Value;
      end Parse_Boolean;

      ----------------------
      -- Parse_Shared_Lib --
      ----------------------

      procedure Parse_Shared_Lib (Lib_Name : String) is
         Dyn_Module : G_Module;

      begin
         Dyn_Module := Module_Open (Module_Build_Path ("", Lib_Name));

         if Dyn_Module = null then
            Dyn_Module := Module_Open (Lib_Name);
         end if;

         if Dyn_Module = null then
            Trace (Me, "Couldn't open shared lib: " & Lib_Name);
            return;
         end if;

      end Parse_Shared_Lib;

   begin  -- Initialize
      Lang.Next := Custom_Root;
      Custom_Root := Lang;

      Lang.Name := new String'(Get_String (Get_Field (Top, "Name")));

      Register_Language (Handler, Language_Access (Lang), null);
      Get_Registry (Kernel).Environment.Register_Default_Language_Extension
        (Language_Name       => Get_Name (Lang),
         Default_Spec_Suffix =>
           Get_String (Get_Field (Top, "Spec_Suffix")),
         Default_Body_Suffix =>
           Get_String (Get_Field (Top, "Body_Suffix")),
         Obj_Suffix => Get_String (Get_Field (Top, "Obj_Suffix")));

      Project_Viewers.Register_Naming_Scheme_Editor
        (Kernel, Get_Name (Lang), Custom_Naming_Editor_Factory'Access);

      Node := Top.Child;
      while Node /= null loop
         if Node.Tag.all = "Extension" then
            Get_Registry (Kernel).Environment.Add_Language_Extension
              (Language_Name => Get_Name (Lang),
               Extension     => Node.Value.all);
         end if;

         Node := Node.Next;
      end loop;

      Tmp_Str := Get_Field (Top, "Parent");

      if Tmp_Str /= null then
         declare
            Lang_Name : constant String := To_Lower (Tmp_Str.all);
            A_Lang    : Custom_Language_Access;
         begin
            if Lang_Name = "ada" then
               Lang.Parent := Language.Ada.Ada_Lang;

            elsif Lang_Name = "c" then
               Lang.Parent := Language.C.C_Lang;

            elsif Lang_Name = "c++" then
               Lang.Parent := Language.Cpp.Cpp_Lang;

            elsif Lang_Name = "java" then
               Lang.Parent := Language.Java.Java_Lang;

            else
               --  loop through custom languages to find parent
               --  Skip first language, since this is Lang itself

               A_Lang := Custom_Root.Next;

               while A_Lang /= null loop
                  if A_Lang.Name /= null
                    and then To_Lower (A_Lang.Name.all) = Lang_Name
                  then
                     Lang.Parent := Language_Access (A_Lang);
                  end if;

                  A_Lang := A_Lang.Next;
               end loop;
            end if;
         end;
      end if;

      Tmp_Str := Get_Field (Top, "Engine");

      if Tmp_Str /= null then
         Parse_Shared_Lib (Tmp_Str.all);
      end if;

      --  Concatenate all project fields

      Node := Find_Tag (Top.Child, "Project_Field");
      while Node /= null loop
         if Lang.Project_Fields = null then
            Lang.Project_Fields := new Project_Field_Array (1 .. 1);
         else
            Tmp := Lang.Project_Fields;
            Lang.Project_Fields := new Project_Field_Array
              (Tmp'First .. Tmp'Last + 1);
            Lang.Project_Fields (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;

         Lang.Project_Fields (Lang.Project_Fields'Last) :=
           (Attribute_Name  => new String'(Get_Attribute (Node, "Name")),
            Attribute_Index => new String'(Get_Attribute (Node, "Index")),
            Description     => new String'(Node.Value.all),
            Values          => null,   --  ??? Not supported in XML yet
            Editable        => True);  --  ??? Not supported in XML yet

         Node := Find_Tag (Node.Next, "Project_Field");
      end loop;

      Node := Find_Tag (Top.Child, "Context");

      if Node = null then
         Lang.Context := Null_Context'Access;
      else
         Comment_Start := Get_Field (Node, "Comment_Start");
         Comment_End := Get_Field (Node, "Comment_End");

         declare
            NL : constant Node_Ptr :=
                   Find_Tag (Node.Child, "New_Line_Comment_Start");
         begin
            if NL = null then
               New_Line_Comment_Start := null;

            elsif Get_Attribute (NL, "mode", "override") = "append"
              and then Lang.Parent /= null
              and then Get_Language_Context (Lang.Parent) /= null
              and then Get_Language_Context
                (Lang.Parent).Syntax.New_Line_Comment_Start /= null
            then
               New_Line_Comment_Start :=
                 new String'(Get_Language_Context
                             (Lang.Parent).Syntax.New_Line_Comment_Start.all
                             & '|' &
                             NL.Value.all);
            else
               New_Line_Comment_Start := NL.Value;
            end if;
         end;

         Lang.Context := new Language_Context;
         if Comment_Start /= null then
            Lang.Context.Syntax.Comment_Start :=
              new String'(Comment_Start.all);
         end if;

         if Comment_End /= null then
            Lang.Context.Syntax.Comment_End :=
              new String'(Comment_End.all);
         end if;

         if New_Line_Comment_Start = null then
            Lang.Context.Syntax.New_Line_Comment_Start_Regexp := null;
         else
            if Contains_Special_Characters (New_Line_Comment_Start.all) then
               Lang.Context.Syntax.New_Line_Comment_Start_Regexp :=
                 new Pattern_Matcher'
                   (Compile ("^(" & New_Line_Comment_Start.all & ")"));

            else
               Lang.Context.Syntax.New_Line_Comment_Start :=
                 new String'(New_Line_Comment_Start.all);
            end if;
         end if;

         Parse_Character
           (Node, "String_Delimiter", Lang.Context.String_Delimiter);
         Parse_Character
           (Node, "Quote_Character", Lang.Context.Quote_Character);
         Parse_Character
           (Node, "Constant_Character", Lang.Context.Constant_Character);
         Parse_Boolean
           (Node, "Can_Indent", Lang.Context.Can_Indent);
         Parse_Boolean
           (Node, "Syntax_Highlighting", Lang.Context.Syntax_Highlighting);
         Parse_Boolean
           (Node, "Case_Sensitive", Lang.Context.Case_Sensitive, True);
         Parse_Boolean
           (Node, "Accurate_Xref", Lang.Context.Accurate_Xref, False);
         Parse_Boolean
           (Node, "Use_Semicolon", Lang.Context.Use_Semicolon, False);

         if not Lang.Context.Case_Sensitive then
            Flags := Flags or Case_Insensitive;
         end if;
      end if;

      --  Concatenate all Keyword tags

      Node := Top.Child;

      loop
         Node := Find_Tag (Node, "Keywords");

         exit when Node = null;

         --  Check inherited mode

         if Get_Attribute (Node, "mode", "override") = "append" then
            Keyword_Append := True;
         end if;

         Append (Str, Node.Value.all);
         Node := Node.Next;
      end loop;

      if Keyword_Append
        and then Lang.Parent /= null
        and then Strings.String_Access'(Keywords (Lang.Parent)) /= null
        and then Keywords (Lang.Parent).all /= ""
      then
         Lang.Keywords_Regexp :=
           new String'(Keywords (Lang.Parent).all & '|' & To_String (Str));

      elsif Str /= Null_Unbounded_String then
         Lang.Keywords_Regexp := new String'(To_String (Str));

      else
         Lang.Keywords_Regexp := new String'("");
      end if;

      declare
         Keywords : constant String := Lang.Keywords_Regexp.all;
      begin
         if Keywords /= "" then
            Lang.Keywords := new Pattern_Matcher'
              (Compile ("^(" & Keywords & ")", Flags and not Multiple_Lines));
         end if;
      exception
         when Expression_Error =>
            Insert (Kernel, -"Invalid regexp in <keywords>: ^("
                    & Keywords & ")", Mode => Error);
      end;

      Node := Find_Tag (Top.Child, "Wordchars");
      if Node /= null then
         Lang.Word_Chars := To_Set (Decode (Node.Value.all));
      end if;

      Parent := Find_Tag (Top.Child, "Categories");

      if Parent = null then
         Lang.Categories := new Explorer_Categories (1 .. 0);
         return;
      end if;

      Node := Parent.Child;

      --  Count the number of categories

      loop
         exit when Node = null;
         Num_Categories := Num_Categories + 1;
         Node := Node.Next;
      end loop;

      Lang.Categories := new Explorer_Categories (1 .. Num_Categories);
      Node := Parent.Child;

      for J in 1 .. Num_Categories loop
         Assert (Me, Node.Tag.all = "Category",
                 "Expecting <category> node in XML file");

         --  Concatenate all Pattern tags

         Str := Null_Unbounded_String;
         N   := Node.Child;

         loop
            N := Find_Tag (N, "Pattern");

            exit when N = null;

            Append (Str, N.Value.all);
            N := N.Next;
         end loop;

         declare
            Pattern       : constant String := To_String (Str);
            Category      : Language_Category;
            Index         : Integer;
            End_Index     : Integer;
            Category_Name : Symbol := No_Symbol;

         begin
            declare
               Name : constant String := Get_Field (Node, "Name").all;
            begin
               Category := Language_Category_Value (Name);
               if Category = Cat_Custom then
                  Category_Name := Find (Kernel.Symbols, Name);
               end if;
            end;

            Index := Integer'Value (Get_Field (Node, "Index").all);
            if Get_Field (Node, "End_Index") /= null then
               End_Index := Integer'Value (Get_Field (Node, "End_Index").all);
            else
               End_Index := 0;
            end if;

            Lang.Categories (J) :=
              (Category       => Category,
               Category_Name  => Category_Name,
               Regexp         => new Pattern_Matcher'
                 (Compile (Pattern, Flags)),
               Position_Index => Index,
               End_Index      => End_Index,
               Make_Entry     => null);

         exception
            when Constraint_Error =>
               --  ??? Should display an error instead.
               Lang.Categories (J) :=
                 (Cat_Unknown, No_Symbol, null, 0, 0, null);
               Trace (Me, "Invalid Category found for language "
                      & Lang.Name.all);
         end;

         Node := Node.Next;
      end loop;
   end Initialize;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Custom_Language;
      Str  : String) return Boolean is
   begin
      if Lang.Parent = null then
         return False;
      else
         return Is_Simple_Type (Lang.Parent, Str);
      end if;
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Custom_Language) return Strings.String_Access is
   begin
      return Lang.Keywords_Regexp;
   end Keywords;

   overriding function Keywords
     (Lang : access Custom_Language) return Pattern_Matcher_Access is
   begin
      if Lang.Keywords = null then
         if Lang.Parent = null then
            return null;
         else
            return Keywords (Lang.Parent);
         end if;
      else
         return Lang.Keywords;
      end if;
   end Keywords;

   overriding function Keywords
     (Lang : access Custom_Language) return GNAT.Strings.String_List
   is
      use type Strings.String_List_Access;
   begin
      --  ??? This list is never populated. We should add a facility for
      --  specifying lists of keywords in XML, and maybe compute the
      --  regexp from this list.
      if Lang.Keywords_List = null then
         return (1 .. 0 => null);
      end if;

      return Lang.Keywords_List.all;
   end Keywords;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Field : String) return String is
   begin
      if Lang.Parent = null then
         return "";
      else
         return Record_Field_Name (Lang.Parent, Name, Field);
      end if;
   end Record_Field_Name;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Lang : access Custom_Language) return String is
   begin
      if Lang.Name = null then
         return "";
      else
         return Lang.Name.all;
      end if;
   end Get_Name;

   ------------------
   -- Comment_Line --
   ------------------

   overriding function Comment_Line
     (Lang    : access Custom_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      pragma Unreferenced (Clean);
   begin
      if Lang.Parent = null then
         return Comment_Line
           (Language_Root (Lang.all)'Access, Line, Comment);
      else
         return Comment_Line (Lang.Parent, Line, Comment);
      end if;
   end Comment_Line;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang   : access Custom_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : Glib.UTF8_String;
      Result : out Construct_List) is
   begin
      if Lang.Parent = null or else Lang.Categories'Length > 0 then
         Parse_Constructs (Language_Root (Lang.all)'Access, File,
                           Buffer, Result);
      else
         Parse_Constructs (Lang.Parent, File, Buffer, Result);
      end if;
   end Parse_Constructs;

   -------------------
   -- Format_Buffer --
   -------------------

   overriding procedure Format_Buffer
    (Lang                : access Custom_Language;
     Buffer              : String;
     Replace             : Replace_Text_Callback;
     From, To            : Natural := 0;
     Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
     Indent_Offset       : Natural := 0;
     Case_Exceptions     : Case_Handling.Casing_Exceptions :=
       Case_Handling.No_Casing_Exception;
     Is_Optional_Keyword : access function (S : String)
                                            return Boolean := null)
   is
      pragma Unreferenced (Is_Optional_Keyword);

      function Is_Keyword (S : String) return Boolean;
      --  Return True if S is a keyword of Lang

      ----------------
      -- Is_Keyword --
      ----------------

      function Is_Keyword (S : String) return Boolean is
         Matched : Match_Array (0 .. 1);
      begin
         Match (Lang.Keywords.all, S, Matched);
         return Matched (0) /= GNAT.Regpat.No_Match;
      end Is_Keyword;

   begin
      if Lang.Parent = null then
         Format_Buffer
           (Language_Root (Lang.all)'Access,
            Buffer, Replace, From, To,
            Indent_Params, Indent_Offset, Case_Exceptions);
      else
         Format_Buffer
           (Lang.Parent, Buffer, Replace, From, To,
            Indent_Params, Indent_Offset, Case_Exceptions,
            Is_Keyword'Access);
      end if;
   end Format_Buffer;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access Custom_Language;
      Buffer   : String;
      Callback : Entity_Callback) is
   begin
      if Lang.Parent = null
        or else Lang.Context /= Null_Context'Access
        or else Lang.Keywords /= null
      then
         Parse_Entities
           (Language_Root (Lang.all)'Access, Buffer, Callback);
      else
         Parse_Entities (Lang.Parent, Buffer, Callback);
      end if;
   end Parse_Entities;

   ------------------
   -- Is_Word_Char --
   ------------------

   overriding function Is_Word_Char
     (Lang : access Custom_Language;
      Char : Wide_Wide_Character) return Boolean is
   begin
      if Is_Entity_Letter (Char) then
         return True;
      end if;

      return Is_In (Char, Lang.Word_Chars);
   end Is_Word_Char;

end Language.Custom;
