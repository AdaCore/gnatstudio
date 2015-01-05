------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada_Analyzer; use Ada_Analyzer;
use Ada.Containers;
with Glib;
with Language_Handlers; use Language_Handlers;
with GNATCOLL.Xref; use GNATCOLL.Xref;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package body GPS.Syntax_Tree is

   function Str (GL : General_Location) return String
   is
     ("< " & GL.Line'Img & GL.Column'Img & " >");
   --  Returns a string representation of a General_Location, for debugging
   --  purposes.
   pragma Unreferenced (Str);

   function Str (EL : Editor_Location'Class) return String
   is
     ("< " & EL.Line'Img & EL.Column'Img & " >");
   --  Returns a string representation of an Editor_Location, for debugging
   --  purposes.
   pragma Unreferenced (Str);

   function To_Str (US : Unbounded_String) return String renames To_String;
   function To_UStr
     (US : String) return Unbounded_String renames To_Unbounded_String;
   --  Shortcuts for unbounded strings conversions

   package body Tok is

      ------------
      -- Create --
      ------------

      function Create
        (Editor : Editor_Buffer'Class;
         Start_Location : Editor_Location'Class;
         End_Location : Editor_Location'Class;
         Kernel : Core_Kernel) return Tokenizer
      is
         Ada_Source : constant Glib.UTF8_String :=
           Editor.Get_Chars (Start_Location, End_Location);

         T : aliased constant Tokenizer :=
           (Tokens => new Token_List,
            Src => new String'(Ada_Source),
            Kernel => Kernel,
            Start_Location => new Editor_Location'Class'(Start_Location));
         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;

         function CB
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);
         begin
            T.Tokens.Append
              ((Entity, Sloc_Start, Sloc_End, T));
            return False;
         end CB;

      begin
         Analyze_Ada_Source
           (Ada_Source,
            Get_Language_By_Name (Kernel.Lang_Handler, "ada").Symbols,
            Default_Indent_Parameters,
            Format     => False,
            Callback => CB'Unrestricted_Access);
         return T;
      end Create;

      ----------------
      -- Get_String --
      ----------------

      function Get_String (Tok : Token) return String
      is
         T : constant Tokenizer := Tok.Tokenizr;
      begin
         return T.Src (Tok.Sloc_Start.Index .. Tok.Sloc_End.Index);
      end Get_String;

      ---------
      -- Get --
      ---------

      function Get (T : Tokenizer; I : Natural) return String is
      begin
         return Get_String (Get_Token (T, I));
      end Get;

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token (T : Tokenizer; I : Natural) return Token is
         J : Natural := 1;
      begin
         for Tok of T.Tokens.all loop
            case Tok.Entity is
               when Comment_Text | Annotated_Comment_Text => null;
               when others =>
                  if I = J then
                     return Tok;
                  end if;
                  J := J + 1;
            end case;
         end loop;
         raise Tokenizer_Exception;
      end Get_Token;

      ----------------
      -- Is_Comment --
      ----------------

      function Is_Comment (T : Tokenizer) return Boolean is
        (case T.Tokens.First_Element.Entity is
            when Comment_Text | Annotated_Comment_Text => True,
            when others => False);

      -----------------
      -- Get_Comment --
      -----------------

      function Get_Comment (T : Tokenizer) return Token is
      begin
         if Is_Comment (T) then
            return T.Tokens.First_Element;
         else
            raise Tokenizer_Exception;
         end if;
      end Get_Comment;

      ---------
      -- Has --
      ---------

      function Has (T : Tokenizer; I : Natural) return Boolean is
      begin
         return I <= Natural (T.Tokens.Length);
      end Has;

      ---------
      -- Pop --
      ---------

      procedure Pop (T : in out Tokenizer; I : Natural) is
         J : Natural := 0;
      begin
         loop
            case T.Tokens.First_Element.Entity is
               when Comment_Text | Annotated_Comment_Text => null;
               when others => J := J + 1;
            end case;
            T.Tokens.Delete_First;
            exit when J >= I;
         end loop;
      end Pop;

      ---------------
      -- Start_Loc --
      ---------------

      function Start_Loc (Tok : Token) return Editor_Location_Access is
      begin
         return new Editor_Location'Class'
           (Tok.Tokenizr.Start_Location.Forward_Char
              (Tok.Sloc_Start.Index - 1));
      end Start_Loc;

      -------------
      -- End_Loc --
      -------------

      function End_Loc (Tok : Token) return Editor_Location_Access is
      begin
         return new Editor_Location'Class'
           (Tok.Tokenizr.Start_Location.Forward_Char
              (Tok.Sloc_End.Index - 1));
      end End_Loc;

   end Tok;

   package body Gen_Ops is

      -----------------
      -- Parse_Lists --
      -----------------

      function Parse_Lists
        (T : in out Tokenizer;
         Sep : String;
         Els_Parser : Parse_List_Proc_Type) return List.List is
      begin
         declare
            L : List.List;
         begin
            loop
               Els_Parser (T, L);
               exit when T.Get (1) /= Sep;
               T.Pop (1);
            end loop;
            Put_Line ("L length : " & L.Length'Img);
            return L;
         end;
      end Parse_Lists;

      ----------------
      -- Parse_List --
      ----------------

      function Parse_List
        (T : in out Tokenizer;
         Sep : String;
         El_Parser : Parse_Proc_Type) return List.List
      is
         L : List.List;
         Current_El : AST_Subnode;
         Has_Node : Boolean;
      begin
         loop
            El_Parser (T, Current_El, Has_Node);
            exit when not Has_Node;
            L.Append (Current_El);
            exit when T.Get (1) /= Sep;
            T.Pop (1);
         end loop;
         return L;
      end Parse_List;

      -------------
      -- Unparse --
      -------------

      function Unparse
        (Node_List : List.List; Sep : String) return String
      is
         Buf : Unbounded_String;
         I : Natural := 1;
      begin
         for Node of Node_List loop
            Append (Buf, Node.Unparse);
            exit when I = Natural (Node_List.Length);
            Append (Buf, Sep);
            I := I + 1;
         end loop;
         return To_Str (Buf);
      end Unparse;

   end Gen_Ops;

   function Parse_Subprogram_Definition
     (T : in out Tokenizer) return Subprogram_Definition;
   --  Parsing function for Subprogram_Definition

   function Parse_Subprogram_Args
     (T : in out Tokenizer) return Args_Lists.List;
   --  Parsing function for the args profile list

   procedure Parse_Subprogram_Arg
     (T : in out Tokenizer;
      Arg : out Subprogram_Arguments;
      Has_Node : out Boolean);
   --  Parsing function for an Arg group

   procedure Parse_Id (T : in out Tokenizer; Node : out Identifier);
   procedure Parse_Id (T : in out Tokenizer;
                       Node : out Identifier;
                       Has_Node : out Boolean);
   --  Parsing function for an Identifier

   function Parse_Subtype (T : in out Tokenizer) return Node_Subtype;
   --  Parsing function for a Subtype

   function Parse_Function_Return_Type
     (T : in out Tokenizer) return Type_Expression;
   --  Parsing function for the return type of a function

   procedure Parse_Arg_Type (T : in out Tokenizer; Node : out Type_Expression);
   --  Parsing function for the type of a subprogram argument

   procedure Parse_Access_Type
     (T : in out Tokenizer; Node : out Type_Expression);
   --  Parsing function for an access type

   procedure Parse_Spec
     (T : in out Tokenizer;
      Spec : out Subprogram_Specification;
      Kind : Subprogram_Kind);
   --  Parsing function for a whole Subprogram_Specification

   function Parse_Comment_List
     (T : in out Tokenizer;
      Start_Ahead : Natural := 0) return Comments_Lists.List;
   --  Parse a group of comments. Will skip as much as Start_Ahead tokens that
   --  are not comments before beginning to group comments into List.
   --  Used to parse args comments.

   function Unparse_Args
     (Node : Subprogram_Arguments;
      Align_Names_Length : Natural := 0) return String;
   --  Unparse a group of args

   function Subprogram_Start
     (Loc : Editor_Location'Class) return Editor_Location'Class;
   --  Return the parent subprogram's start location enclosing Loc
   --  or Null_Editor_Location if there is no parent subprogram.

   --------------
   -- Contains --
   --------------

   function Contains
     (Start_Loc, End_Loc, Loc : Editor_Location'Class) return Boolean is
   begin
      return (Start_Loc.Line <= Loc.Line
              and then Start_Loc.Column <= Loc.Column
              and then End_Loc.Line >= Loc.Line
              and then End_Loc.Column + 1 >= Loc.Column);
   end Contains;

   function Contains
     (Node : AST_Node_Record; Loc : Editor_Location'Class) return Boolean is
   begin
      return Contains (Node.Start_Loc.all, Node.End_Loc.all, Loc);
   end Contains;

   -------------------------
   -- Get_Entity_Location --
   -------------------------

   function Get_Entity_Location
     (Node : Subprogram_Definition) return General_Location
   is
      Entity_Loc : constant Editor_Location'Class :=
        Node.Name.Start_Loc.all;
   begin
      return ((File => Node.Buffer.File,
               Line => Entity_Loc.Line,
               Column => Entity_Loc.Column));
   end Get_Entity_Location;

   ------------------------
   -- To_Editor_Location --
   ------------------------

   function To_Editor_Location
     (B : Editor_Buffer'Class;
      GL : General_Location;
      F : Editor_Buffer_Factory_Access) return Editor_Location'Class
   is
     (F.Get
        (GL.File,
         Open_Buffer => True,
         Open_View   => False).New_Location
        (GL.Line,
         GL.Column));

   ------------------------
   -- Parse_Comment_List --
   ------------------------

   function Parse_Comment_List
     (T : in out Tokenizer;
      Start_Ahead : Natural := 0) return Comments_Lists.List
   is
      Counter : Natural := Start_Ahead;
      List : Comments_Lists.List;
   begin
      for Tok of T.Tokens.all loop
         case Tok.Entity is
            when Comment_Text | Annotated_Comment_Text =>
               List.Append
                 (Node_Comment'
                    (Comment => To_UStr (Get_String (Tok)),
                     Start_Loc => Start_Loc (Tok),
                     End_Loc => End_Loc (Tok),
                     Kernel => T.Kernel));
            when others =>
               if Counter = 0 then
                  exit;
               else
                  Counter := Counter - 1;
               end if;
         end case;
      end loop;
      return List;
   end Parse_Comment_List;

   ----------------
   -- Parse_Spec --
   ----------------

   procedure Parse_Spec
     (T : in out Tokenizer;
      Spec : out Subprogram_Specification;
      Kind : Subprogram_Kind) is
   begin
      Spec.Kernel := T.Kernel;
      Spec.Start_Loc := Start_Loc (T.Get_Token (1));
      Spec.Args := Parse_Subprogram_Args (T);

      if T.Get (1) = ")" then
         Spec.Args_Profile_End_Loc := End_Loc (T.Get_Token (1));
         Spec.End_Loc := End_Loc (T.Get_Token (1));
         T.Pop (1);
      else
         raise Parser_Exception;
      end if;

      if Kind = Kind_Function then
         Spec.Ret_Type := Parse_Function_Return_Type (T);
         Spec.End_Loc := Spec.Ret_Type.Contents.End_Loc;
      end if;
   end Parse_Spec;

   -----------------------
   -- Parse_Access_Type --
   -----------------------

   procedure Parse_Access_Type
     (T : in out Tokenizer; Node : out Type_Expression)
   is
      Acc_Mod : Access_Modifier_Type;
      Loc_Start : Editor_Location_Access;
      Access_Subp : access Node_Access_Subprogram_Type;
      Access_T : access Node_Access_Type;
      Kernel : constant Core_Kernel := T.Kernel;
   begin
      --  TODO : Check for not null
      if T.Get (1) /= "access" then
         raise Parser_Exception;
      else
         Loc_Start := Start_Loc (T.Get_Token (1));
         T.Pop (1);

         --  We have a subprogram access
         if T.Get (1) = "procedure"
           or else T.Get (1) = "function"
           or else T.Get (1) = "protected"
         then
            Access_Subp := new Node_Access_Subprogram_Type;
            Access_Subp.Kernel := T.Kernel;
            Node.Contents := Type_Expression_Access (Access_Subp);

            if T.Get (1) = "protected" then
               Access_Subp.Is_Protected := True;
               T.Pop (1);
            end if;

            Access_Subp.Kind :=
              (if T.Get (1) = "function" then Kind_Function
               else Kind_Procedure);

            Parse_Spec (T, Access_Subp.Spec, Access_Subp.Kind);
            Node.Contents.Start_Loc := Loc_Start;
            Node.Contents.End_Loc := Access_Subp.Spec.End_Loc;

         else
            if T.Get (1) = "all" then
               Acc_Mod := Modifier_All;
               T.Pop (1);
            elsif T.Get (1) = "constant" then
               Acc_Mod := Modifier_Constant;
               T.Pop (1);
            else
               Acc_Mod := Modifier_None;
            end if;

            Access_T := new Node_Access_Type'
              (General_Access_Modifier => Acc_Mod,
               Access_To => Parse_Subtype (T),
               Start_Loc => Loc_Start,
               Kernel => Kernel,
               others => <>);

            Node.Contents := Type_Expression_Access (Access_T);
            Node.Contents.End_Loc := Access_T.Access_To.End_Loc;
         end if;
      end if;
   end Parse_Access_Type;

   --------------------
   -- Parse_Arg_Type --
   --------------------

   procedure Parse_Arg_Type
     (T : in out Tokenizer; Node : out Type_Expression)
   is
      TE : access Node_Subtype;
   begin
      if T.Get (1) = "access" then
         Parse_Access_Type (T, Node);
      else
         TE := new Node_Subtype'(Parse_Subtype (T));
         Node.Contents := Type_Expression_Access (TE);
      end if;
   end Parse_Arg_Type;

   --------------------------------
   -- Parse_Function_Return_Type --
   --------------------------------

   function Parse_Function_Return_Type
     (T : in out Tokenizer) return Type_Expression
   is
      N : Type_Expression;
   begin
      if T.Get (1) /= "return" then
         raise Parser_Exception;
      end if;
      T.Pop (1);
      Parse_Arg_Type (T, N);
      return N;
   end Parse_Function_Return_Type;

   --------------
   -- Parse_Id --
   --------------

   procedure Parse_Id (T : in out Tokenizer; Node : out Identifier)
   is
      Tok : constant Token := T.Get_Token (1);
   begin
      Node := Identifier'(Name => To_UStr (Get_String (Tok)),
                          Start_Loc => Start_Loc (Tok),
                          End_Loc   => End_Loc (Tok),
                          Kernel => T.Kernel);
      T.Pop (1);
   end Parse_Id;

   procedure Parse_Id (T : in out Tokenizer;
                       Node : out Identifier;
                       Has_Node : out Boolean)
   is
   begin
      Parse_Id (T, Node);
      Has_Node := True;
   exception
      when Parser_Exception =>
         Has_Node := False;
   end Parse_Id;

   -------------------
   -- Parse_Subtype --
   -------------------

   function Parse_Subtype (T : in out Tokenizer) return Node_Subtype is
      Tok : constant Token := T.Get_Token (1);
      Class_Wide : Boolean := False;
      Sloc_Start, Sloc_End : Editor_Location_Access;
   begin
      Sloc_Start := Start_Loc (Tok);
      Sloc_End := End_Loc (Tok);
      T.Pop (1);

      if T.Has (1)
        and then T.Get (1) = "'"
        and then T.Get (2) = "Class"
      then
         Sloc_End := End_Loc (T.Get_Token (2));
         T.Pop (2);
         Class_Wide := True;
      end if;

      return Node_Subtype'(Type_Name => To_UStr (Get_String (Tok)),
                           Start_Loc => Sloc_Start,
                           End_Loc   => Sloc_End,
                           Class_Wide => Class_Wide,
                           Kernel => T.Kernel);
   end Parse_Subtype;

   package Id_Ops is new Gen_Ops (Identifier, Id_Lists);

   --------------------------
   -- Parse_Subprogram_Arg --
   --------------------------

   procedure Parse_Subprogram_Arg
     (T : in out Tokenizer;
      Arg : out Subprogram_Arguments;
      Has_Node : out Boolean)
   is
      Id_List : constant Id_Lists.List :=
        Id_Ops.Parse_List (T, ",", Parse_Id'Access);
      Type_Node : Type_Expression;
      Args_Mode : Args_Mode_Kind := Kind_In;
      Null_Exclusion : Boolean := False;
      Has_In : Boolean := False;
      Kernel : constant Core_Kernel := T.Kernel;
      Colon_Location : Editor_Location_Access;
   begin
      Has_Node := True;

      if T.Get (1) /= ":" then
         raise Parser_Exception;
      end if;
      Colon_Location := Start_Loc (T.Get_Token (1));
      T.Pop (1);

      if T.Get (1) = "in" then
         T.Pop (1);
         Has_In := True;
      end if;

      if T.Get (1) = "out" then
         T.Pop (1);
         Args_Mode := (if Has_In then Kind_InOut else Kind_Out);
      end if;

      if T.Get (1) = "not" and then T.Get (2) = "null" then
         T.Pop (2);
         Null_Exclusion := True;
      end if;

      Parse_Arg_Type (T, Type_Node);

      Arg := Subprogram_Arguments'
        (Names => Id_List,
         Args_Type => Type_Node,
         Start_Loc => Id_List.First_Element.Start_Loc,
         End_Loc => Type_Node.Contents.End_Loc,
         Mode => Args_Mode,
         Null_Exclusion => Null_Exclusion,
         Comments => Parse_Comment_List (T, 1),
         Kernel => Kernel,
         Colon_Location => Colon_Location);
   exception
      when Parser_Exception =>
         Has_Node := False;
   end Parse_Subprogram_Arg;

   package Args_Ops is new Gen_Ops (Subprogram_Arguments, Args_Lists);

   ---------------------------
   -- Parse_Subprogram_Args --
   ---------------------------

   function Parse_Subprogram_Args
     (T : in out Tokenizer) return Args_Lists.List
   is
   begin
      if T.Get (1) /= "(" then
         return Args_Lists.Empty_List;
      else
         T.Pop (1);
         return L : Args_Lists.List do
            L := Args_Ops.Parse_List (T, ";", Parse_Subprogram_Arg'Access);
         end return;
      end if;
   end Parse_Subprogram_Args;

   ---------------------------------
   -- Parse_Subprogram_Definition --
   ---------------------------------

   function Parse_Subprogram_Definition
     (T : in out Tokenizer) return Subprogram_Definition is
   begin
      return Node_Subp : Subprogram_Definition do
         Node_Subp.Kernel := T.Kernel;
         Node_Subp.Start_Loc := Start_Loc (T.Get_Token (1));

         if T.Get (1) = "overriding" then
            T.Pop (1);
            Node_Subp.Is_Overriding := True;
         end if;

         if T.Get (1) = "function" then
            Node_Subp.Kind := Kind_Function;
         elsif T.Get (1) = "procedure" then
            Node_Subp.Kind := Kind_Procedure;
         else
            raise Parser_Exception;
         end if;

         T.Pop (1);

         Parse_Id (T, Node_Subp.Name);

         Parse_Spec (T, Node_Subp.Spec, Node_Subp.Kind);
         Node_Subp.End_Loc := Node_Subp.Spec.End_Loc;
      end return;
   end Parse_Subprogram_Definition;

   -------------
   -- Unparse --
   -------------

   overriding function Unparse
     (Node : Subprogram_Definition) return String
   is
     ((if Node.Is_Overriding then "overriding " else "") &
      (if Node.Kind = Kind_Function
       then "function " else "procedure ") &
        Node.Name.Unparse & " " &
        Node.Spec.Unparse);

   ------------------
   -- Unparse_Args --
   ------------------

   function Unparse_Args
     (Node : Subprogram_Arguments;
      Align_Names_Length : Natural := 0) return String
   is
      Args_Str : Unbounded_String
        := To_UStr (Id_Ops.Unparse (Node.Names, ","));
   begin
      while Length (Args_Str) < Align_Names_Length loop
         Append (Args_Str, " ");
      end loop;
      Append (Args_Str, " : " & Node.Unparse_Type);
      return To_Str (Args_Str);
   end Unparse_Args;

   ------------------
   -- Unparse_Spec --
   ------------------

   function Unparse_Spec
     (Node : Subprogram_Specification;
      With_Parentheses : Boolean := True;
      Vertical : Boolean := False;
      Align_Params : Boolean := False) return String
   is
      Result : Unbounded_String := To_UStr ("");
      Is_Not_Last : Boolean;
      Align_Names_Length : Natural := 0;
      use Args_Lists;
   begin

      if With_Parentheses then
         Append (Result, "(");
      end if;

      if Align_Params then
         for Arg of Node.Args loop
            Align_Names_Length
              := Natural'Max (Id_Ops.Unparse (Arg.Names, ",")'Length,
                              Align_Names_Length);
         end loop;
      end if;

      for C in Node.Args.Iterate loop
         Is_Not_Last := Has_Element (Next (C));
         Append (Result, Unparse_Args (Element (C), Align_Names_Length));

         if Is_Not_Last then
            Append (Result, ";");
         end if;

         if Element (C).Has_Comments then
            if Element (C).Comments.Length = 1 then
               Append (Result, " "
                       & Element (C).Comments.First_Element.Unparse);
            else
               Append (Result, ASCII.LF);
               for Comment of Element (C).Comments loop
                  Append (Result, Comment.Unparse);
               end loop;
            end if;
         elsif Vertical and Is_Not_Last then
            Append (Result, ASCII.LF);
         elsif Is_Not_Last then
            Append (Result, " ");
         end if;
      end loop;

      if With_Parentheses then
         Append (Result, ")");
      end if;

      return To_Str (Result);
   end Unparse_Spec;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Node : Subprogram_Definition) return General_Entity is
   begin
      return Get_Entity
        (Node.Kernel.Databases,
         To_Str (Node.Name.Name),
         Node.Get_Entity_Location);
   end Get_Entity;

   -------------
   -- Is_Body --
   -------------

   function Is_Body (Node : Subprogram_Definition) return Boolean
   is
     (Node.Kernel.Databases.Get_Body
        (Node.Get_Entity) = Node.Get_Entity_Location);

   --------------------
   -- Is_Declaration --
   --------------------

   function Is_Declaration (Node : Subprogram_Definition) return Boolean
   is
     (Node.Kernel.Databases.Get_Declaration
        (Node.Get_Entity).Loc = Node.Get_Entity_Location);

   ---------------------
   -- Get_Counterpart --
   ---------------------

   function Get_Counterpart
     (Node : Subprogram_Definition;
      Factory : Editor_Buffer_Factory_Access)
      return Subprogram_Definition'Class
   is
      Entity : General_Entity;
      Decl_Loc, Body_Loc : General_Location;

      function Get_Counterpart_Loc return Editor_Location'Class;
      function Get_Counterpart_Loc return Editor_Location'Class is
      begin
         if Decl_Loc = Node.Get_Entity_Location then
            return To_Editor_Location
              (Node.Buffer, Body_Loc, Factory);
         elsif Body_Loc = Node.Get_Entity_Location then
            return To_Editor_Location
              (Node.Buffer, Decl_Loc, Factory);
         else
            return Nil_Editor_Location;
         end if;
      end Get_Counterpart_Loc;

   begin
      Entity := Node.Get_Entity;
      Decl_Loc := Node.Kernel.Databases.Get_Declaration (Entity).Loc;
      Body_Loc := Node.Kernel.Databases.Get_Body (Entity);

      if Decl_Loc = Body_Loc then
         return Null_Subprogram_Definition;
      end if;

      declare
         Counterpart_Loc : constant Editor_Location'Class
           := Get_Counterpart_Loc;
      begin
         if Counterpart_Loc = Nil_Editor_Location then
            return Null_Subprogram_Definition;
         else
            return Get_Subprogram_At
              (Counterpart_Loc.Buffer, Node.Kernel, Counterpart_Loc);
         end if;
      end;
   exception
      when Parser_Exception | Tokenizer_Exception =>
         return Null_Subprogram_Definition;
   end Get_Counterpart;

   ----------------------------
   -- Is_Args_Layout_Aligned --
   ----------------------------

   function Is_Args_Layout_Aligned
     (Subp : Subprogram_Specification) return Boolean
   is
      Aligned : Boolean := True;
      use Args_Lists;
   begin
      if Subp.Args.Length = 1 then
         return False;
      end if;

      for C in Subp.Args.Iterate loop
         Aligned := Aligned and then
           (Previous (C) = No_Element or else
            Element (C).Colon_Location.Column =
                Element (Previous (C)).Colon_Location.Column);
      end loop;
      return Aligned;
   end Is_Args_Layout_Aligned;

   -----------------------------
   -- Is_Args_Layout_Vertical --
   -----------------------------

   function Is_Args_Layout_Vertical
     (Subp : Subprogram_Specification) return Boolean
   is
      Vertical : Boolean := True;
      use Args_Lists;
   begin
      if Subp.Args.Length = 1 then
         return False;
      end if;

      for C in Subp.Args.Iterate loop
         Vertical :=
           (Vertical
            and then
            (Previous (C) = No_Element or else
               Element (C).Start_Loc.Line >
                   Element (Previous (C)).Start_Loc.Line));
      end loop;

      return Vertical;
   end Is_Args_Layout_Vertical;

   -------------
   -- Unparse --
   -------------

   overriding function Unparse
     (Node : Subprogram_Specification) return String
   is
     ("(" & Args_Ops.Unparse (Node.Args, "; ") & ")"
      & (if Node.Ret_Type.Contents /= null
         then " return " & Unparse (Node.Ret_Type.Contents.all)
         else ""));

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Subprogram_Specification) return Boolean
   is
      use Args_Lists;
   begin
      return Left.Args = Right.Args
        and then
          ((Left.Ret_Type = Null_Type_Expression
            and then Right.Ret_Type = Null_Type_Expression)
           or else
             (Left.Ret_Type /= Null_Type_Expression
              and then Right.Ret_Type /= Null_Type_Expression
              and then
                Left.Ret_Type.Contents.all = Right.Ret_Type.Contents.all));
   end "=";

   ------------------
   -- Unparse_Type --
   ------------------

   function Unparse_Type
     (Node : Subprogram_Arguments) return String
   is
     ((case Node.Mode is
          when Kind_InOut => "in out ",
          when Kind_Out => "out ",
          when Kind_In => "") &
      (if Node.Null_Exclusion then "not null " else "")
      & Node.Args_Type.Contents.Unparse);

   -------------
   -- Unparse --
   -------------

   overriding function Unparse
     (Node : Subprogram_Arguments) return String
   is
     (Id_Ops.Unparse (Node.Names, ",") & " : " & Node.Unparse_Type);

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Subprogram_Arguments) return Boolean
   is
      use Id_Lists;
   begin
      --  Doesn't account for comments
      return
        Left.Names = Right.Names
        and then Left.Mode = Right.Mode
        and then Left.Args_Type.Contents.all = Right.Args_Type.Contents.all
        and then Left.Null_Exclusion = Right.Null_Exclusion;
   end "=";

   -------------
   -- Unparse --
   -------------

   overriding function Unparse
     (Node : Node_Subtype) return String
   is
     (To_Str (Node.Type_Name)
      & (if Node.Class_Wide then "'Class" else ""));

   overriding function Unparse
     (Node : Node_Access_Type) return String
   is
     ("access " &
      ((case Node.General_Access_Modifier is
           when Modifier_All => "all ",
           when Modifier_Constant => "constant ",
           when Modifier_None => "")  &
       (if Node.Null_Exclusion then "not null " else "") &
         Node.Access_To.Unparse));

   overriding function Unparse
     (Node : Node_Access_Subprogram_Type) return String
   is
      ("access" &
       ((if Node.Is_Protected then "protected " else "") &
        (if Node.Kind = Kind_Function
         then "function " else "procedure ") &
          Unparse (Node.Spec)));

   overriding function Unparse
     (Node : Identifier) return String
   is
     (To_Str (Node.Name));

   overriding function Unparse
     (Node : Node_Comment) return String
   is
     (To_Str (Node.Comment));

   ------------------
   -- Hard_Replace --
   ------------------

   procedure Hard_Replace (Node : AST_Node_Record'Class) is
   begin
      Node.Buffer.Delete (Node.Start_Loc.all, Node.End_Loc.all);
      Node.Buffer.Insert (Node.Start_Loc.all, Unparse (Node));
   end Hard_Replace;

   --------------------
   -- Copy_Structure --
   --------------------

   procedure Copy_Structure (Source : AST_Node_Record'Class;
                             Dest : in out AST_Node_Record'Class)
   is
      Start_Loc : constant Editor_Location_Access := Dest.Start_Loc;
      End_Loc : constant Editor_Location_Access := Dest.End_Loc;
   begin
      Dest := Source;
      Dest.Start_Loc := Start_Loc;
      Dest.End_Loc := End_Loc;
   end Copy_Structure;

   ----------------------
   -- Subprogram_Start --
   ----------------------

   function Subprogram_Start
     (Loc : Editor_Location'Class) return Editor_Location'Class
   is
      Ret : constant Editor_Location'Class := Loc.Block_Start;
   begin
      if Loc.Line = 1 and Loc.Column = 1 then
         return Nil_Editor_Location;
      end if;
      case Ret.Block_Type is
         when Cat_Function | Cat_Procedure =>
            return Ret;
         when Cat_Unknown =>
            return Nil_Editor_Location;
         when others =>
            return Subprogram_Start
              (Ret.Forward_Char (-1));
      end case;
   end Subprogram_Start;

   -----------------------
   -- Get_Subprogram_At --
   -----------------------

   function Get_Subprogram_At
     (Editor : Editor_Buffer'Class;
      Kernel : Core_Kernel;
      Location : Editor_Location'Class) return Subprogram_Definition
   is
      Subp_Start : constant Editor_Location'Class
        := Subprogram_Start (Location);
   begin
      if Subp_Start = Nil_Editor_Location then
         return Null_Subprogram_Definition;
      end if;

      declare
         T : Tokenizer :=
           Create (Editor, Subp_Start, Editor.End_Of_Buffer, Kernel);
      begin
         return Parse_Subprogram_Definition (T);
      end;
   exception
      when Parser_Exception | Tokenizer_Exception =>
         --  TODO : Put a trace
         return Null_Subprogram_Definition;
   end Get_Subprogram_At;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Object : in out Type_Expression) is
   begin
      if Object.Contents /= null then
         Object.Contents :=
           new Type_Expression_Record'Class'(Object.Contents.all);
      end if;
   end Adjust;

begin
   Null_Subprogram_Specification :=
     (null, null, null,
      Args_Lists.Empty_List, Null_Type_Expression, null);
   Null_Subprogram_Definition :=
     (null, null, null,
      Null_Identifier, Kind_Function, False, Null_Subprogram_Specification);
end GPS.Syntax_Tree;
