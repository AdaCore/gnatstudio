------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2013-2019, AdaCore                   --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Basic_Types;
with GNATCOLL.JSON;     use GNATCOLL.JSON;

with GNATdoc.Utils;     use GNATdoc.Utils;

package body GNATdoc.Backend.HTML.Source_Code is

   procedure Append_Text_Object
     (Self  : in out Source_Code_Printer'Class;
      Class : String;
      Text  : String;
      Href  : String := "");

   function Is_In_Private_Part
     (Self   : Source_Code_Printer'Class;
      Entity : Entity_Id) return Boolean;
   --  Returns True when Entity located in private part of currently processed
   --  scope.

   -------------------------
   -- Aspect_Comment_Text --
   -------------------------

   not overriding procedure Aspect_Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Comment_Text;

   ----------------------------
   -- Annotated_Comment_Text --
   ----------------------------

   not overriding procedure Annotated_Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Comment_Text;

   ----------------------------
   -- Annotated_Keyword_Text --
   ----------------------------

   not overriding procedure Annotated_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Keyword_Text;

   ------------------------
   -- Append_Text_Object --
   ------------------------

   procedure Append_Text_Object
     (Self  : in out Source_Code_Printer'Class;
      Class : String;
      Text  : String;
      Href  : String := "")
   is
      function Is_Current_Line_Visible return Boolean;
      --  Returns True when current line is visible.

      -----------------------------
      -- Is_Current_Line_Visible --
      -----------------------------

      function Is_Current_Line_Visible return Boolean is
      begin
         return Self.Show_Private
           or else No (Self.Current_Scope.Entity)
           or else Self.Current_Scope.Private_First = 0
           or else Self.Current_Line
                     not in Self.Current_Scope.Private_First
                              .. Self.Current_Scope.Private_Last - 1;
      end Is_Current_Line_Visible;

      Slice_First : Natural := Text'First;
      Slice_Last  : Natural;
      LF_Pattern  : constant String (1 .. 1) := (1 => ASCII.LF);
      Object      : JSON_Value;

   begin
      while Slice_First <= Text'Last loop
         Slice_Last := Index (Text (Slice_First .. Text'Last), LF_Pattern);

         if Slice_Last >= Slice_First then
            if Is_Current_Line_Visible then
               if Slice_First < Slice_Last then
                  Object := Create_Object;
                  Object.Set_Field ("kind", "span");
                  Object.Set_Field ("cssClass", Class);
                  Object.Set_Field
                    ("text", Text (Slice_First .. Slice_Last - 1));

                  if Href /= "" then
                     Object.Set_Field ("href", Href);
                  end if;

                  Append (Self.Line, Object);
               end if;

               Object := Create_Object;
               Object.Set_Field ("kind", "line");
               Object.Set_Field ("number", Self.Current_Line);
               Object.Set_Field ("children", Self.Line);
               Append (Self.Result, Object);
               Self.Line := Empty_Array;
            end if;

            Self.Current_Line := Self.Current_Line + 1;

         else
            if Is_Current_Line_Visible then
               Object := Create_Object;
               Object.Set_Field ("kind", "span");
               Object.Set_Field ("cssClass", Class);
               Object.Set_Field ("text", Text (Slice_First .. Text'Last));

               if Href /= "" then
                  Object.Set_Field ("href", Href);
               end if;

               Append (Self.Line, Object);
            end if;

            Slice_Last := Text'Last;
         end if;

         Slice_First := Slice_Last + 1;
      end loop;

      --  Unwind scope when went outside of current scope.

      if Present (Self.Current_Scope.Entity)
        and then Self.Current_Line > Self.Current_Scope.Private_Last
      then
         Self.Current_Scope := Self.Scope_Stack.Last_Element;
         Self.Scope_Stack.Delete_Last;
      end if;
   end Append_Text_Object;

   -------------------------
   -- Aspect_Keyword_Text --
   -------------------------

   not overriding procedure Aspect_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Keyword_Text;

   -----------------
   -- Aspect_Text --
   -----------------

   not overriding procedure Aspect_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Normal_Text;

   ----------------
   -- Block_Text --
   ----------------

   not overriding procedure Block_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   --------------------
   -- Character_Text --
   --------------------

   not overriding procedure Character_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("character", Self.Buffer (First.Index .. Last.Index));
   end Character_Text;

   ------------------
   -- Comment_Text --
   ------------------

   not overriding procedure Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("comment", Self.Buffer (First.Index .. Last.Index));
   end Comment_Text;

   --------------
   -- End_File --
   --------------

   not overriding procedure End_File
     (Self     : in out Source_Code_Printer;
      Result   : out GNATCOLL.JSON.JSON_Value;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

      Object : JSON_Value;

   begin
      --  Append last line

      if Length (Self.Line) /= 0 then
         Object := Create_Object;
         Object.Set_Field ("kind", "line");
         Object.Set_Field ("number", Self.Current_Line);
         Object.Set_Field ("children", Self.Line);
         Append (Self.Result, Object);
         Self.Line := Empty_Array;
         Self.Current_Line := Self.Current_Line + 1;
      end if;

      --  Construct "code" object

      Result := Create_Object;
      Result.Set_Field ("kind", "code");
      Result.Set_Field ("children", Self.Result);

      --  Cleanup

      Self.Buffer := null;
      Self.Result := Empty_Array;
   end End_File;

   ---------------------
   -- Identifier_Text --
   ---------------------

   not overriding procedure Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

      Identifier : constant String := Self.Buffer (First.Index .. Last.Index);

      Simple_First  : Natural;
      Simple_Column : Natural;
      --  Index and column of first character of last simple identifier of
      --  qualified identifier

      Entity        : Entity_Id;

   begin
      --  Lookup for simple name in qualified identifier.
      --
      --  ??? This code need to be improved for multibyte encodings support.
      --
      --  ??? This code doesn't support qualified identifiers splitted into
      --  several lines.

      Simple_First := Index (Identifier, ".", Ada.Strings.Backward);

      if Simple_First = 0 then
         Simple_First  := First.Index;
         Simple_Column := First.Column;

      else
         Simple_First := Simple_First + 1;
         Simple_Column := First.Column + (Simple_First - First.Index);
      end if;

      --  Resolve entity

      Entity :=
        Find_Unique_Entity
          (Location      =>
             (Self.File,
              No_File,  --  ??? unknown
              Self.Current_Line,
              Basic_Types.Visible_Column_Type (Simple_Column)),
           In_References => True);

      --  Detect whether current entity can start scope with private part:
      --   - it should be package, task or protected type
      --   - it should not be generic instantiation
      --   - it should not be renaming
      --  Also, all declarations in private part are ignored.

      if Present (Entity)
        and then Get_Kind (Entity)
                   in E_Package | E_Task_Type | E_Protected_Type
        and then No (LL.Get_Instance_Of (Entity))
        and then not Is_Alias (Entity)
        and then not Self.Is_In_Private_Part (Entity)
      then
         Self.Scope_Stack.Append (Self.Current_Scope);
         Self.Current_Scope :=
           (Entity        => Entity,
            Private_First => Get_First_Private_Entity_Loc (Entity).Line,
            Private_Last  => Get_End_Of_Syntax_Scope_Loc (Entity).Line);
      end if;

      if No (Entity)
        or else Is_Excluded (Entity)
        or else not Is_Decorated (Entity)
        or else (Get_Kind (Entity) = E_Package
                 and then Present (LL.Get_Instance_Of (Entity)))
        --  Documentation pages for generic package instantiations are not
        --  generated, thus links to them is not generated too.
      then
         Self.Append_Text_Object ("identifier", Identifier);

      else
         Self.Append_Text_Object
           ("identifier", Identifier, Get_Docs_Href (Entity));
      end if;
   end Identifier_Text;

   ------------------------
   -- Is_In_Private_Part --
   ------------------------

   function Is_In_Private_Part
     (Self   : Source_Code_Printer'Class;
      Entity : Entity_Id) return Boolean is
   begin
      return
        not Self.Show_Private
        and then Present (Entity)
        and then Present (Self.Current_Scope.Entity)
        and then Self.Current_Scope.Private_First /= 0
        and then LL.Get_Location (Entity).Line
        in Self.Current_Scope.Private_First .. Self.Current_Scope.Private_Last;
   end Is_In_Private_Part;

   ------------------
   -- Keyword_Text --
   ------------------

   not overriding procedure Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("keyword", Self.Buffer (First.Index .. Last.Index));
   end Keyword_Text;

   -----------------
   -- Normal_Text --
   -----------------

   not overriding procedure Normal_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("text", Self.Buffer (First.Index .. Last.Index));
   end Normal_Text;

   -----------------
   -- Number_Text --
   -----------------

   not overriding procedure Number_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("number", Self.Buffer (First.Index .. Last.Index));
   end Number_Text;

   -------------------
   -- Operator_Text --
   -------------------

   not overriding procedure Operator_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   -----------------------------
   -- Partial_Identifier_Text --
   -----------------------------

   not overriding procedure Partial_Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   ----------------
   -- Start_File --
   ----------------

   not overriding procedure Start_File
     (Self         : in out Source_Code_Printer;
      File         : GNATCOLL.VFS.Virtual_File;
      Buffer       : not null GNAT.Strings.String_Access;
      First_Line   : Positive;
      Show_Private : Boolean;
      Continue     : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.File          := File;
      Self.Buffer        := Buffer;
      Self.Current_Line  := First_Line;
      Self.Show_Private  := Show_Private;
      Self.Current_Scope := (GNATdoc.Atree.No_Entity, 0, 0);
      Self.Scope_Stack.Clear;
   end Start_File;

   -----------------
   -- String_Text --
   -----------------

   not overriding procedure String_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("string", Self.Buffer (First.Index .. Last.Index));
   end String_Text;

   ---------------
   -- Type_Text --
   ---------------

   not overriding procedure Type_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

end GNATdoc.Backend.HTML.Source_Code;
