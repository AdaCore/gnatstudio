-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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

with Glib;                  use Glib;
with Glib.Xml_Int;          use Glib.Xml_Int;
with GNAT.Regpat;           use GNAT.Regpat;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Language.Custom is

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang, Name, Index);
   begin
      return "";
   end Array_Item_Name;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Custom_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang, Name);
   begin
      return "";
   end Dereference_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Custom_Language) return Explorer_Categories is
   begin
      return Lang.Categories.all;
   end Explorer_Regexps;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Custom_Language) return Language_Context is
   begin
      return Lang.Context.all;
   end Get_Language_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Lang : access Custom_Language'Class;
      Top  : Glib.Xml_Int.Node_Ptr)
   is
      Node          : Node_Ptr;
      Parent        : Node_Ptr;
      Comment_Start : String_Ptr;
      Comment_End   : String_Ptr;
      Flags         : Regexp_Flags;
      New_Line_Comment_Start : String_Ptr;
      Num_Categories,
      Comment_Start_Length,
      Comment_End_Length,
      New_Line_Comment_Start_Length : Natural := 0;
      Keywords      : Unbounded_String;

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

      function Get_String (S : String_Ptr) return String_Ptr;
      --  Return a deep copy of S, or (1 => ASCII.NUL) if S is null.

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

      ----------------
      -- Get_String --
      ----------------

      function Get_String (S : String_Ptr) return String_Ptr is
      begin
         if S = null then
            return new String'(1 => ASCII.NUL);
         else
            return new String'(S.all);
         end if;
      end Get_String;

   begin
      Lang.Name := Get_String (Get_Field (Top, "Name"));
      Lang.Spec_Suffix := Get_String (Get_Field (Top, "Spec_Suffix"));
      Lang.Body_Suffix := Get_String (Get_Field (Top, "Body_Suffix"));

      --  Concatenate all Keyword tags

      Node := Top.Child;

      loop
         Node := Find_Tag (Node, "Keywords");

         exit when Node = null;

         Append (Keywords, Node.Value.all);
         Node := Node.Next;
      end loop;

      declare
         KW : constant String := To_String (Keywords);
      begin
         if KW /= "" then
            Lang.Keywords := new Pattern_Matcher'(Compile (KW));
         end if;
      end;

      Node := Find_Tag (Top.Child, "Context");

      if Node = null then
         --  ??? Return an error code
         Lang.Categories := new Explorer_Categories (1 .. 0);
         Lang.Context := new Language_Context'
           (Comment_Start_Length          => 0,
            Comment_End_Length            => 0,
            New_Line_Comment_Start_Length => 0,
            Comment_Start                 => "",
            Comment_End                   => "",
            New_Line_Comment_Start        => "",
            String_Delimiter              => ASCII.NUL,
            Quote_Character               => ASCII.NUL,
            Constant_Character            => ASCII.NUL,
            Can_Indent                    => False,
            Syntax_Highlighting           => False,
            Case_Sensitive                => True);

         return;
      end if;

      Comment_Start := Get_Field (Node, "Comment_Start");

      if Comment_Start /= null then
         Comment_Start_Length := Comment_Start'Length;
      end if;

      Comment_End := Get_Field (Node, "Comment_End");

      if Comment_End /= null then
         Comment_End_Length := Comment_End'Length;
      end if;

      New_Line_Comment_Start :=
        Get_Field (Node, "New_Line_Comment_Start");

      if New_Line_Comment_Start /= null then
         New_Line_Comment_Start_Length := New_Line_Comment_Start'Length;
      end if;

      Lang.Context := new Language_Context
        (Comment_Start_Length,
         Comment_End_Length,
         New_Line_Comment_Start_Length);

      if Comment_Start /= null then
         Lang.Context.Comment_Start := Comment_Start.all;
      end if;

      if Comment_End /= null then
         Lang.Context.Comment_End := Comment_End.all;
      end if;

      if New_Line_Comment_Start /= null then
         Lang.Context.New_Line_Comment_Start := New_Line_Comment_Start.all;
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
         pragma Assert (Node.Tag.all = "Category");

         begin
            Lang.Categories (J) :=
              (Category       => Language_Category'Value
                 ("Cat_" & Get_Field (Node, "Name").all),
               Regexp         => new Pattern_Matcher'
                 (Compile (Get_Field (Node, "Pattern").all, Flags)),
               Position_Index =>
                 Integer'Value (Get_Field (Node, "Index").all),
               Icon           => null,  -- ??? subprogram_xpm'Access,
               Make_Entry     => null);

         exception
            when Constraint_Error =>
               --  ??? Should display an error instead.
               Lang.Categories (J) := (Cat_Unknown, null, 0, null, null);
         end;

         Node := Node.Next;
      end loop;
   end Initialize;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Custom_Language;
      Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return False;
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Custom_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      if Lang.Keywords = null then
         return Never_Match;
      else
         return Lang.Keywords.all;
      end if;
   end Keywords;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Custom_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang, Name, Field);
   begin
      return "";
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

   ---------------------
   -- Get_Spec_Suffix --
   ---------------------

   function Get_Spec_Suffix (Lang : access Custom_Language) return String is
   begin
      if Lang.Spec_Suffix = null then
         return "";
      else
         return Lang.Spec_Suffix.all;
      end if;
   end Get_Spec_Suffix;

   ---------------------
   -- Get_Body_Suffix --
   ---------------------

   function Get_Body_Suffix (Lang : access Custom_Language) return String is
   begin
      if Lang.Body_Suffix = null then
         return "";
      else
         return Lang.Body_Suffix.all;
      end if;
   end Get_Body_Suffix;

end Language.Custom;
