-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;             use GNAT.Regpat;

with Glib;                    use Glib;
with Glib.Module;             use Glib.Module;
with Glib.Xml_Int;            use Glib.Xml_Int;
with Traces;                  use Traces;

--  ??? Would be nice if languages registered themselves somewhere instead
--  of having a static knowledge of all the language defined.
with Language.Ada;
with Language.C;
with Language.Cpp;
with Language.Java;
with Entities;
with Dummy_Parser;              use Dummy_Parser;
with Language_Handlers;         use Language_Handlers;
with Projects.Registry;         use Projects.Registry;
with Custom_Naming_Editors;     use Custom_Naming_Editors;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Intl;                  use GPS.Intl;
with Project_Viewers;           use Project_Viewers;
with Naming_Editors;            use Naming_Editors;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with String_Utils;              use String_Utils;

package body Language.Custom is

   Me : constant Debug_Handle := Create ("Language.Custom");

   No_Match : aliased Pattern_Matcher := Never_Match;

   procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
     (Project_Field_Array, Project_Field_Array_Access);

   Null_Context : aliased Language_Context :=
     (Comment_Start_Length          => 0,
      Comment_End_Length            => 0,
      Comment_Start                 => "",
      Comment_End                   => "",
      New_Line_Comment_Start        => null,
      New_Line_Comment_Start_Regexp => No_Match'Access,
      String_Delimiter              => ASCII.NUL,
      Quote_Character               => ASCII.NUL,
      Constant_Character            => ASCII.NUL,
      Can_Indent                    => False,
      Syntax_Highlighting           => False,
      Case_Sensitive                => True);

   Custom_Root : Custom_Language_Access;
   --  Holds a linked list of custom languages, so that we can implement
   --  dynamic inheritance between custom languages.

   function Custom_Naming_Scheme_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class; Lang : String)
      return Naming_Editors.Language_Naming_Editor;
   --  Create the naming scheme editor page

   Dummy_Handler : constant Entities.LI_Handler := Create_Dummy_LI_Handler;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
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

   function Dereference_Name
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

   function Explorer_Regexps
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

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
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

   ---------------------------------
   -- Custom_Naming_Scheme_Editor --
   ---------------------------------

   function Custom_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Naming_Editors.Language_Naming_Editor
   is
      Naming : Custom_Naming_Editor;
   begin
      Gtk_New (Naming, Kernel, Lang);
      return Naming_Editors.Language_Naming_Editor (Naming);
   end Custom_Naming_Scheme_Editor;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Lang    : access Custom_Language'Class;
      Handler : access Language_Handler_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Top     : Glib.Xml_Int.Node_Ptr)
   is
      use type Strings.String_Access;

      N, Node                 : Node_Ptr;
      Parent                  : Node_Ptr;
      Comment_Start           : String_Ptr;
      Comment_End             : String_Ptr;
      Tmp_Str                 : String_Ptr;
      Flags                   : Regexp_Flags;
      New_Line_Comment_Start  : String_Ptr;
      Num_Categories,
      Comment_Start_Length,
      Comment_End_Length      : Natural := 0;
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

         procedure Get_Symbol is new Generic_Module_Symbol (Comment_Line_Proc);
         procedure Get_Symbol is new Generic_Module_Symbol
           (Parse_Constructs_Proc);
         procedure Get_Symbol is new Generic_Module_Symbol
           (Format_Buffer_Proc);
         procedure Get_Symbol is new Generic_Module_Symbol
           (Parse_Entities_Proc);

      begin
         Dyn_Module := Module_Open (Module_Build_Path ("", Lib_Name));

         if Dyn_Module = null then
            Dyn_Module := Module_Open (Lib_Name);
         end if;

         if Dyn_Module = null then
            Trace (Me, "Couldn't open shared lib: " & Lib_Name);
            return;
         end if;

         declare
            Comment   : constant String_Ptr :=
                          Get_Field (Top, "Comment_Line");
            Parse_C   : constant String_Ptr :=
                          Get_Field (Top, "Parse_Constructs");
            Format    : constant String_Ptr :=
                          Get_Field (Top, "Format_Buffer");
            Parse_E   : constant String_Ptr :=
                          Get_Field (Top, "Parse_Entities");
            Success   : Boolean;

         begin
            if Comment /= null then
               Get_Symbol
                 (Dyn_Module, Comment.all, Lang.Comment_Line, Success);
            end if;

            if Parse_C /= null then
               Get_Symbol
                 (Dyn_Module, Parse_C.all,
                  Lang.Parse_Constructs, Success);
            end if;

            if Format /= null then
               Get_Symbol
                 (Dyn_Module, Format.all, Lang.Format_Buffer, Success);
            end if;

            if Parse_E /= null then
               Get_Symbol
                 (Dyn_Module, Parse_E.all, Lang.Parse_Entities, Success);
            end if;
         end;
      end Parse_Shared_Lib;

   begin  -- Initialize
      Lang.Next := Custom_Root;
      Custom_Root := Custom_Language_Access (Lang);

      Lang.Name := new String'(Get_String (Get_Field (Top, "Name")));

      Register_Language (Handler, Language_Access (Lang), null,
                         LI => Dummy_Handler);
      Register_Default_Language_Extension
        (Get_Registry (Kernel).all,
         Language_Name       => Get_Name (Lang),
         Default_Spec_Suffix =>
           Get_String (Get_Field (Top, "Spec_Suffix")),
         Default_Body_Suffix =>
           Get_String (Get_Field (Top, "Body_Suffix")));

      Project_Viewers.Register_Naming_Scheme_Editor
        (Kernel, Get_Name (Lang), Custom_Naming_Scheme_Editor'Access);

      Node := Top.Child;
      while Node /= null loop
         if Node.Tag.all = "Extension" then
            Add_Language_Extension
              (Get_Registry (Kernel).all,
               Language_Name => Get_Name (Lang),
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

            if Lang.Parent /= null then
               Lang.Indent_Params := Lang.Parent.Indent_Params;
               Lang.Indent_Style  := Lang.Parent.Indent_Style;
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

         if Comment_Start /= null then
            Comment_Start_Length := Comment_Start'Length;
         end if;

         Comment_End := Get_Field (Node, "Comment_End");

         if Comment_End /= null then
            Comment_End_Length := Comment_End'Length;
         end if;

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
                (Lang.Parent).New_Line_Comment_Start /= null
            then
               New_Line_Comment_Start :=
                 new String'(Get_Language_Context
                             (Lang.Parent).New_Line_Comment_Start.all & '|' &
                             NL.Value.all);
            else
               New_Line_Comment_Start := NL.Value;
            end if;
         end;

         Lang.Context := new Language_Context
           (Comment_Start_Length,
            Comment_End_Length);

         if Comment_Start /= null then
            Lang.Context.Comment_Start := Comment_Start.all;
         end if;

         if Comment_End /= null then
            Lang.Context.Comment_End := Comment_End.all;
         end if;

         if New_Line_Comment_Start = null then
            Lang.Context.New_Line_Comment_Start_Regexp := No_Match'Access;
         else
            if Contains_Special_Characters (New_Line_Comment_Start.all) then
               Lang.Context.New_Line_Comment_Start_Regexp :=
                 new Pattern_Matcher'
                   (Compile ("^(" & New_Line_Comment_Start.all & ")"));

            else
               Lang.Context.New_Line_Comment_Start :=
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

         if Lang.Context.Case_Sensitive then
            Flags := Multiple_Lines;
         else
            Flags := Multiple_Lines or Case_Insensitive;
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
         Lang.Word_Chars := new Gunichar_Array (Node.Value'Range);
         for N in Node.Value'Range loop
            Lang.Word_Chars (N) := Character'Pos (Node.Value (N));
         end loop;
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
            Pattern : constant String := To_String (Str);
         begin
            Lang.Categories (J) :=
              (Category       => Language_Category'Value
                 ("Cat_" & Get_Field (Node, "Name").all),
               Regexp         => new Pattern_Matcher'
                 (Compile (Pattern, Flags)),
               Position_Index =>
                 Integer'Value (Get_Field (Node, "Index").all),
               Make_Entry     => null);

         exception
            when Constraint_Error =>
               --  ??? Should display an error instead.
               Lang.Categories (J) := (Cat_Unknown, null, 0, null);
               Trace (Me, "Invalid Category found for language "
                      & Lang.Name.all);
         end;

         Node := Node.Next;
      end loop;
   end Initialize;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
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

   function Keywords
     (Lang : access Custom_Language) return Strings.String_Access is
   begin
      return Lang.Keywords_Regexp;
   end Keywords;

   function Keywords
     (Lang : access Custom_Language) return Pattern_Matcher_Access is
   begin
      if Lang.Keywords = null then
         if Lang.Parent = null then
            return No_Match'Access;
         else
            return Keywords (Lang.Parent);
         end if;
      else
         return Lang.Keywords;
      end if;
   end Keywords;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
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

   function Get_Name (Lang : access Custom_Language) return String is
   begin
      if Lang.Name = null then
         return "";
      else
         return Lang.Name.all;
      end if;
   end Get_Name;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   function Get_Project_Fields
     (Lang : access Custom_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return (1 .. 0 => No_Project_Field);

      --  ??? Support for project fields in xml files isn't complete yet,
      --  so the following code is currently disabled. When enabled, it
      --  will mean that languages will by default inherit the fields from
      --  the parent, which is probably not desirable. Consider instead
      --  having an explicit XML tag to ask for inheritance.

      --  if Lang.Project_Fields = null then
      --     if Lang.Parent = null then
      --        return (1 .. 0 => No_Project_Field);
      --     else
      --        return Get_Project_Fields (Lang.Parent);
      --     end if;
      --  else
      --     return Lang.Project_Fields.all;
      --  end if;
   end Get_Project_Fields;

   -------------------
   -- New_Construct --
   -------------------

   function New_Construct return Construct_Access is
      Result : constant Construct_Access := new Construct_Information;
   begin
      return Result;
   end New_Construct;

   -------------------
   -- Set_Construct --
   -------------------

   procedure Set_Construct
     (Construct      : Construct_Access;
      Category       : Language_Category;
      Name           : chars_ptr;
      Profile        : chars_ptr;
      Sloc_Start     : Source_Location;
      Sloc_Entity    : Source_Location;
      Sloc_End       : Source_Location;
      Is_Declaration : Boolean;
      Prev, Next     : Construct_Access)
   is
      N, P : GNAT.Strings.String_Access;
   begin
      if Name /= Null_Ptr then
         N := new String'(Value (Name));
      end if;

      if Profile /= Null_Ptr then
         P := new String'(Value (Profile));
      end if;

      Construct.all :=
        (Category,
         Is_Declaration => Is_Declaration,
         Visibility     => Visibility_Public,
         Name           => N,
         Profile        => P,
         Sloc_Start     => Sloc_Start,
         Sloc_Entity    => Sloc_Entity,
         Sloc_End       => Sloc_End,
         Prev           => Prev,
         Next           => Next,
         Attributes     => (others => False));
   end Set_Construct;

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Lang    : access Custom_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      pragma Unreferenced (Clean);
      procedure C_Free (S : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, C_Free, "free");
   begin
      if Lang.Comment_Line = null then
         if Lang.Parent = null then
            return Comment_Line
              (Language_Root (Lang.all)'Access, Line, Comment);
         else
            return Comment_Line (Lang.Parent, Line, Comment);
         end if;
      end if;

      if Comment then
         declare
            S   : constant chars_ptr :=
                    Lang.Comment_Line (Line, True, Line'Length);
            Val : constant String := Value (S);

         begin
            C_Free (S);
            return Val;
         end;
      else
         declare
            S   : constant chars_ptr :=
                    Lang.Comment_Line (Line, False, Line'Length);
            Val : constant String := Value (S);

         begin
            C_Free (S);
            return Val;
         end;
      end if;
   end Comment_Line;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang   : access Custom_Language;
      Buffer : Glib.UTF8_String;
      Result : out Construct_List) is
   begin
      if Lang.Parse_Constructs = null then
         if Lang.Parent = null
           or else Lang.Categories'Length > 0
         then
            Parse_Constructs (Language_Root (Lang.all)'Access, Buffer, Result);
         else
            Parse_Constructs (Lang.Parent, Buffer, Result);
         end if;
      else
         Lang.Parse_Constructs
           (Buffer, Result, Buffer'Length,
            New_Construct'Address, Set_Construct'Address);
      end if;
   end Parse_Constructs;

   -------------------
   -- Format_Buffer --
   -------------------

   procedure Format_Buffer
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

      procedure Replace_Cb
        (Line, First, Last : Integer;
         S                 : chars_ptr);
      pragma Convention (C, Replace_Cb);
      --  Convention C wrapper for Replace

      function Is_Keyword (S : String) return Boolean;
      --  Return True if S is a keyword of Lang

      ----------------
      -- Replace_Cb --
      ----------------

      procedure Replace_Cb
        (Line, First, Last : Integer;
         S                 : chars_ptr) is
      begin
         Replace (Line, First, Last, Value (S));
      end Replace_Cb;

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
      if Lang.Format_Buffer = null then
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

      else
         Lang.Format_Buffer
           (Buffer,
            Replace_Cb'Address, From, To,
            Indent_Params, Buffer'Length);
      end if;
   end Format_Buffer;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Custom_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      function Entity_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      pragma Convention (C, Entity_Cb);
      --  Convention C wrapper for Callback

      ---------------
      -- Entity_Cb --
      ---------------

      function Entity_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is
      begin
         return Callback (Entity, Sloc_Start, Sloc_End, Partial_Entity);
      end Entity_Cb;

   begin
      if Lang.Parse_Entities = null then
         if Lang.Parent = null
           or else Lang.Context /= Null_Context'Access
           or else Lang.Keywords /= null
         then
            Parse_Entities (Language_Root (Lang.all)'Access, Buffer, Callback);
         else
            Parse_Entities (Lang.Parent, Buffer, Callback);
         end if;

      else
         Lang.Parse_Entities
           (Buffer, Entity_Cb'Address, Buffer'Length);
      end if;
   end Parse_Entities;

   ------------------
   -- Is_Word_Char --
   ------------------

   function Is_Word_Char
     (Lang : access Custom_Language; Char : Glib.Gunichar) return Boolean
   is
   begin
      if Is_Entity_Letter (Char) then
         return True;
      end if;

      if Lang.Word_Chars /= null then
         for C in Lang.Word_Chars'Range loop
            if Char = Lang.Word_Chars (C) then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Is_Word_Char;

end Language.Custom;
