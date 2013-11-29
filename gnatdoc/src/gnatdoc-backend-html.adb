------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;

with GNAT.Strings;                     use GNAT.Strings;
with GNATCOLL.JSON;                    use GNATCOLL.JSON;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with Language;                         use Language;
with Templates_Parser;                 use Templates_Parser;

with GNATdoc.Backend.HTML.Source_Code; use GNATdoc.Backend.HTML.Source_Code;
with GNATdoc.Comment;                  use GNATdoc.Comment;

package body GNATdoc.Backend.HTML is
   Me : constant Trace_Handle := Create ("GNATdoc.1-HTML_Backend");

   type Template_Kinds is
     (Tmpl_Documentation_HTML,      --  Documentation (HTML page)
      Tmpl_Documentation_JS,        --  Documentation (JS data)
      Tmpl_Documentation_Index_JS,  --  Index of documentation (JS data)
      Tmpl_Entities_Category_HTML,  --  Entities' category (HTML page)
      Tmpl_Entities_Category_JS,    --  Entities' category (JS data)
      Tmpl_Entities_Categories_Index_JS,
                                    --  Entities' category index (JS data)
      Tmpl_Inheritance_Index_JS,    --  Inheritance tree (JS data)
      Tmpl_Source_File_HTML,        --  Source file (HTML page)
      Tmpl_Source_File_JS,          --  Source file (JavaScript data)
      Tmpl_Source_File_Index_JS);   --  Index of source files (JavaScript data)

   procedure Extract_Summary_And_Description
     (Entity      : Entity_Id;
      Summary     : out JSON_Array;
      Description : out JSON_Array);
   --  Extracts summary and description of the specified entity

   function Get_Template
     (Self : HTML_Backend'Class;
      Kind : Template_Kinds) return GNATCOLL.VFS.Virtual_File;
   --  Returns file name of the specified template.

   function To_JSON_Representation
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return GNATCOLL.JSON.JSON_Array;
   --  Parses Text and converts it into JSON representation.

   procedure Generate_Entities_Category
     (Self       : in out HTML_Backend'Class;
      Entities   : EInfo_List.Vector;
      Label      : String;
      File_Name  : String;
      Categories : in out JSON_Array);
   --  Generates entities category HTML page and JS data.

   procedure Generate_Inheritance_Index (Self : in out HTML_Backend'Class);
   --  Generates inheritance index for Ada tagged and interface types

   function "<" (Left : Entity_Id; Right : Entity_Id) return Boolean;

   package Entity_Id_Ordered_Sets is
     new Ada.Containers.Ordered_Sets (Entity_Id);

   function Get_Srcs_Base_Href (Entity : Entity_Id) return String;
   --  Returns href to source file where this entity is declared. Returned URI
   --  doesn't have fragment identifier.

   function Get_Srcs_Href (Entity : Entity_Id) return String;
   --  Returns href to source file where this entity is declared. Returned URI
   --  includes fragment identifier to navigate to source code line.

   ---------
   -- "<" --
   ---------

   function "<" (Left : Entity_Id; Right : Entity_Id) return Boolean is
      LN : constant String := To_Lower (Get_Short_Name (Left));
      LL : constant General_Location := Atree.LL.Get_Location (Left);
      RN : constant String := To_Lower (Get_Short_Name (Right));
      RL : constant General_Location := Atree.LL.Get_Location (Right);

   begin
      if LN < RN then
         return True;

      elsif LN = RN then
         if LL.File < RL.File then
            return True;

         elsif LL.File = RL.File then
            if LL.Line < RL.Line then
               return True;

            elsif LL.Line = RL.Line then
               return LL.Column < RL.Column;
            end if;
         end if;
      end if;

      return False;
   end "<";

   -------------------------------------
   -- Extract_Summary_And_Description --
   -------------------------------------

   procedure Extract_Summary_And_Description
     (Entity      : Entity_Id;
      Summary     : out JSON_Array;
      Description : out JSON_Array) is
   begin
      Summary     := Empty_Array;
      Description := Empty_Array;

      if Present (Get_Comment (Entity)) then
         declare
            Cursor : Tag_Cursor := New_Cursor (Get_Comment (Entity));
            Tag    : Tag_Info_Ptr;

         begin
            while not At_End (Cursor) loop
               Tag := Get (Cursor);

               if Tag.Tag = "summary" then
                  Summary := To_JSON_Representation (Tag.Text);

               elsif Tag.Tag = "description"
                 or Tag.Tag = ""
               then
                  Description := To_JSON_Representation (Tag.Text);
               end if;

               Next (Cursor);
            end loop;
         end;
      end if;
   end Extract_Summary_And_Description;

   -----------------------
   -- Print_Source_Code --
   -----------------------

   procedure Print_Source_Code
     (Self       : HTML_Backend'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Buffer     : GNAT.Strings.String_Access;
      First_Line : Positive;
      Printer    : in out Source_Code.Source_Code_Printer'Class;
      Text       : out Ada.Strings.Unbounded.Unbounded_String);

   procedure Print_Source_Code
     (Self       : HTML_Backend'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Buffer     : GNAT.Strings.String_Access;
      First_Line : Positive;
      Printer    : in out Source_Code.Source_Code_Printer'Class;
      Text       : out Ada.Strings.Unbounded.Unbounded_String)
   is

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Callback function to dispatch parsed entities to source code printer

      Sloc_First : Source_Location;
      Sloc_Last  : Source_Location;

      --------------
      -- Callback --
      --------------

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Continue : Boolean := True;

      begin
         if Sloc_Last.Index + 1 < Sloc_Start.Index then
            Sloc_First := Sloc_Last;
            Sloc_First.Index := Sloc_First.Index + 1;
            Sloc_Last := Sloc_Start;
            Sloc_Last.Index := Sloc_Last.Index - 1;

            Printer.Normal_Text (Sloc_First, Sloc_Last, Continue);

            if not Continue then
               return True;
            end if;
         end if;

         Sloc_Last := Sloc_End;

         case Entity is
            when Normal_Text =>
               Printer.Normal_Text (Sloc_Start, Sloc_End, Continue);

            when Identifier_Text =>
               Printer.Identifier_Text (Sloc_Start, Sloc_End, Continue);

            when Partial_Identifier_Text =>
               Printer.Partial_Identifier_Text
                 (Sloc_Start, Sloc_End, Continue);

            when Block_Text =>
               Printer.Block_Text (Sloc_Start, Sloc_End, Continue);

            when Type_Text =>
               Printer.Type_Text (Sloc_Start, Sloc_End, Continue);

            when Number_Text =>
               Printer.Number_Text (Sloc_Start, Sloc_End, Continue);

            when Keyword_Text =>
               Printer.Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Comment_Text =>
               Printer.Comment_Text (Sloc_Start, Sloc_End, Continue);

            when Annotated_Keyword_Text =>
               Printer.Annotated_Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Annotated_Comment_Text =>
               Printer.Annotated_Comment_Text (Sloc_Start, Sloc_End, Continue);

            when Aspect_Keyword_Text =>
               Printer.Aspect_Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Aspect_Text =>
               Printer.Aspect_Text (Sloc_Start, Sloc_End, Continue);

            when Character_Text =>
               Printer.Character_Text (Sloc_Start, Sloc_End, Continue);

            when String_Text =>
               Printer.String_Text (Sloc_Start, Sloc_End, Continue);

            when Operator_Text =>
               Printer.Operator_Text (Sloc_Start, Sloc_End, Continue);
         end case;

         return not Continue;
      end Callback;

      Lang     : Language_Access;
      Continue : Boolean := True;

   begin
      Lang := Get_Language_From_File (Self.Context.Lang_Handler, File);

      Sloc_Last := (First_Line, 0, 0);
      Printer.Start_File (File, Buffer, First_Line, Continue);

      if Continue then
         Lang.Parse_Entities (Buffer.all, Callback'Unrestricted_Access);
         Printer.End_File (Text, Continue);
      end if;
   end Print_Source_Code;

   --------------------------------
   -- Generate_Inheritance_Index --
   --------------------------------

   procedure Generate_Inheritance_Index (Self : in out HTML_Backend'Class) is

      procedure Analyze_Inheritance_Tree
        (Entity     : Entity_Id;
         Root_Types : in out EInfo_List.Vector);
      --  Analyze inheritance tree, lookup root types and append them to the
      --  list.

      procedure Build
        (Entity : Entity_Id;
         List   : in out JSON_Array);

      ------------------------------
      -- Analyze_Inheritance_Tree --
      ------------------------------

      procedure Analyze_Inheritance_Tree
        (Entity     : Entity_Id;
         Root_Types : in out EInfo_List.Vector) is
      begin
         if Get_IDepth_Level (Entity) = 0 then
            if not Root_Types.Contains (Entity) then
               Root_Types.Append (Entity);
            end if;

         else
            Analyze_Inheritance_Tree (Get_Parent (Entity), Root_Types);

            for Progenitor of Get_Progenitors (Entity).all loop
               Analyze_Inheritance_Tree (Progenitor, Root_Types);
            end loop;
         end if;
      end Analyze_Inheritance_Tree;

      -----------
      -- Build --
      -----------

      procedure Build
        (Entity : Entity_Id;
         List   : in out JSON_Array)
      is
         Object  : constant JSON_Value := Create_Object;
         Derived : JSON_Array;
         Aux     : Entity_Id_Ordered_Sets.Set;

      begin
         for D of Get_Direct_Derivations (Entity).all loop
            Aux.Insert (D);
         end loop;

         for D of Aux loop
            Build (D, Derived);
         end loop;

         Set_Label_And_Href (Object, Entity);

         if Derived /= Empty_Array then
            Object.Set_Field ("inherited", Derived);
         end if;

         Append (List, Object);
      end Build;

      Root_Types : EInfo_List.Vector;
      Aux        : Entity_Id_Ordered_Sets.Set;
      Types      : JSON_Array;

   begin
      --  Collect root types

      for Entity of Self.Entities.Tagged_Types loop
         Analyze_Inheritance_Tree (Entity, Root_Types);
      end loop;

      for Entity of Self.Entities.Interface_Types loop
         Analyze_Inheritance_Tree (Entity, Root_Types);
      end loop;

      for T of Root_Types loop
         Aux.Insert (T);
      end loop;

      for T of Aux loop
         Build (T, Types);
      end loop;

      declare
         Translation : Translate_Set;

      begin
         Insert
           (Translation,
            Assoc
              ("INHERITANCE_INDEX_DATA",
               String'(Write (Create (Types), False))));
         Write_To_File
           (Self.Context,
            Get_Doc_Directory (Self.Context.Kernel),
            "inheritance_index.js",
            Parse
              (+Self.Get_Template (Tmpl_Inheritance_Index_JS).Full_Name,
               Translation));
      end;
   end Generate_Inheritance_Index;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Self                : in out HTML_Backend;
      Update_Global_Index : Boolean)
   is
      pragma Unreferenced (Update_Global_Index);

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Callback function to dispatch parsed entities to source code printer

      Buffer     : GNAT.Strings.String_Access;
      Lang       : Language_Access;
      Printer    : Source_Code_Printer;
      Sloc_First : Source_Location;
      Sloc_Last  : Source_Location;
      Text       : Unbounded_String;
      Continue   : Boolean := True;

      Sources    : JSON_Array;
      Object     : JSON_Value;

      --------------
      -- Callback --
      --------------

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Continue : Boolean := True;

      begin
         if Sloc_Last.Index + 1 < Sloc_Start.Index then
            Sloc_First := Sloc_Last;
            Sloc_First.Index := Sloc_First.Index + 1;
            Sloc_Last := Sloc_Start;
            Sloc_Last.Index := Sloc_Last.Index - 1;

            Printer.Normal_Text (Sloc_First, Sloc_Last, Continue);

            if not Continue then
               return True;
            end if;
         end if;

         Sloc_Last := Sloc_End;

         case Entity is
            when Normal_Text =>
               Printer.Normal_Text (Sloc_Start, Sloc_End, Continue);

            when Identifier_Text =>
               Printer.Identifier_Text (Sloc_Start, Sloc_End, Continue);

            when Partial_Identifier_Text =>
               Printer.Partial_Identifier_Text
                 (Sloc_Start, Sloc_End, Continue);

            when Block_Text =>
               Printer.Block_Text (Sloc_Start, Sloc_End, Continue);

            when Type_Text =>
               Printer.Type_Text (Sloc_Start, Sloc_End, Continue);

            when Number_Text =>
               Printer.Number_Text (Sloc_Start, Sloc_End, Continue);

            when Keyword_Text =>
               Printer.Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Comment_Text =>
               Printer.Comment_Text (Sloc_Start, Sloc_End, Continue);

            when Annotated_Keyword_Text =>
               Printer.Annotated_Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Annotated_Comment_Text =>
               Printer.Annotated_Comment_Text (Sloc_Start, Sloc_End, Continue);

            when Aspect_Keyword_Text =>
               Printer.Aspect_Keyword_Text (Sloc_Start, Sloc_End, Continue);

            when Aspect_Text =>
               Printer.Aspect_Text (Sloc_Start, Sloc_End, Continue);

            when Character_Text =>
               Printer.Character_Text (Sloc_Start, Sloc_End, Continue);

            when String_Text =>
               Printer.String_Text (Sloc_Start, Sloc_End, Continue);

            when Operator_Text =>
               Printer.Operator_Text (Sloc_Start, Sloc_End, Continue);
         end case;

         return not Continue;
      end Callback;

   begin
      --  Generate annotated sources and compute index of source files.

      for File of Self.Src_Files loop
         Trace
           (Me, "generate annotated source for " & String (File.Base_Name));

         Lang      := Get_Language_From_File (Self.Context.Lang_Handler, File);
         Buffer    := File.Read_File;
         Sloc_Last := (0, 0, 0);
         Printer.Start_File (File, Buffer, 1, Continue);

         if Continue then
            Lang.Parse_Entities (Buffer.all, Callback'Unrestricted_Access);
            Printer.End_File (Text, Continue);

            if Continue then
               --  Write HTML page

               declare
                  Translation : Translate_Set;

               begin
                  Insert
                    (Translation,
                     Assoc ("SOURCE_FILE_JS", +File.Base_Name & ".js"));
                  Write_To_File
                    (Self.Context,
                     Get_Doc_Directory
                       (Self.Context.Kernel).Create_From_Dir ("srcs"),
                     File.Base_Name & ".html",
                     Parse
                       (+Self.Get_Template (Tmpl_Source_File_HTML).Full_Name,
                        Translation,
                        Cached => True));
               end;

               --  Write JSON data file

               declare
                  Translation : Translate_Set;

               begin
                  Insert (Translation, Assoc ("SOURCE_FILE_DATA", Text));
                  Write_To_File
                    (Self.Context,
                     Get_Doc_Directory
                       (Self.Context.Kernel).Create_From_Dir ("srcs"),
                     File.Base_Name & ".js",
                     Parse
                       (+Self.Get_Template (Tmpl_Source_File_JS).Full_Name,
                        Translation,
                        Cached => True));
               end;

               --  Append source file to the index

               Object := Create_Object;
               Object.Set_Field ("file", "srcs/" & String (File.Base_Name));
               Append (Sources, Object);
            end if;
         end if;

         Free (Buffer);
      end loop;

      --  Write JSON data file for index of source files.

      declare
         Translation : Translate_Set;

      begin
         Insert
           (Translation,
            Assoc
              ("SOURCE_FILE_INDEX_DATA",
               String'(Write (Create (Sources), False))));
         Write_To_File
           (Self.Context,
            Get_Doc_Directory (Self.Context.Kernel),
            "source_file_index.js",
            Parse
              (+Self.Get_Template (Tmpl_Source_File_Index_JS).Full_Name,
               Translation));
      end;

      --  Write JSON data file for index of documentation files.

      declare
         Translation : Translate_Set;
         Result      : GNATCOLL.JSON.JSON_Array;

      begin
         for Object of Self.Doc_Files loop
            Append (Result, Object);
         end loop;

         Insert
           (Translation,
            Assoc
              ("DOCUMENTATION_INDEX_DATA",
               String'(Write (Create (Result), False))));
         Write_To_File
           (Self.Context,
            Get_Doc_Directory (Self.Context.Kernel),
            "documentation_index.js",
            Parse
              (+Self.Get_Template (Tmpl_Documentation_Index_JS).Full_Name,
               Translation));
      end;

      declare
         Categories_Index : JSON_Array;

      begin
         --  Generate entities by category pages and compute list of categories

         Self.Generate_Entities_Category
           (Self.Entities.Variables,
            "Constants & Variables",
            "objects",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Simple_Types,
            "Simple Types",
            "simple_types",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Access_Types,
            "Access Types",
            "access_types",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Record_Types,
            "Record Types",
            "record_types",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Interface_Types,
            "Interface Types",
            "interface_types",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Tagged_Types,
            "Tagged Types",
            "tagged_types",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Tagged_Types,
            "Tasks & Task Types",
            "tasks",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.CPP_Classes,
            "C++ Classes",
            "cpp_classes",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Subprgs,
            "Subprograms",
            "subprograms",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Pkgs,
            "Packages",
            "packages",
            Categories_Index);

         --  Generate entities' categories index JSON data file.

         declare
            Translation : Translate_Set;

         begin
            Insert
              (Translation,
               Assoc
                 ("ENTITIES_CATEGORIES_INDEX_DATA",
                  String'(Write (Create (Categories_Index), False))));
            Write_To_File
              (Self.Context,
               Get_Doc_Directory (Self.Context.Kernel),
               "entities_categories_index.js",
               Parse
                 (+Self.Get_Template
                      (Tmpl_Entities_Categories_Index_JS).Full_Name,
                  Translation));
         end;
      end;

      Self.Generate_Inheritance_Index;
   end Finalize;

   --------------------------------
   -- Generate_Entities_Category --
   --------------------------------

   procedure Generate_Entities_Category
     (Self       : in out HTML_Backend'Class;
      Entities   : EInfo_List.Vector;
      Label      : String;
      File_Name  : String;
      Categories : in out JSON_Array)
   is
      Entries : JSON_Array;
      Object  : JSON_Value;
      Set     : Entity_Id_Ordered_Sets.Set;
      Scope   : Entity_Id;

   begin
      if not Entities.Is_Empty then
         for Entity of Entities loop
            Set.Insert (Entity);
         end loop;

         for Entity of Set loop
            Object := Create_Object;
            Set_Label_And_Href (Object, Entity);

            if Is_Decorated (Entity) then
               Scope := Get_Scope (Entity);
               Object.Set_Field ("declared", Get_Full_Name (Scope));
               Object.Set_Field ("srcHref", Get_Srcs_Href (Entity));
            end if;

            Append (Entries, Object);
         end loop;

         Object := Create_Object;
         Object.Set_Field ("label", Label);
         Object.Set_Field ("entities", Entries);

         declare
            Translation : Translate_Set;

         begin
            Insert
              (Translation,
               Assoc
                 ("ENTITIES_CATEGORY_DATA",
                  String'(Write (Object, False))));
            Write_To_File
              (Self.Context,
               Get_Doc_Directory
                 (Self.Context.Kernel).Create_From_Dir ("entities"),
               +File_Name & ".js",
               Parse
                 (+Self.Get_Template (Tmpl_Entities_Category_JS).Full_Name,
                  Translation));
         end;
         declare
            Translation : Translate_Set;

         begin
            Insert
              (Translation,
               Assoc
                 ("ENTITIES_CATEGORY_JS", File_Name & ".js"));
            Write_To_File
              (Self.Context,
               Get_Doc_Directory
                 (Self.Context.Kernel).Create_From_Dir ("entities"),
               +File_Name & ".html",
               Parse
                 (+Self.Get_Template (Tmpl_Entities_Category_HTML).Full_Name,
                  Translation));

            Object := Create_Object;
            Object.Set_Field ("label", Label);
            Object.Set_Field ("href", "entities/" & File_Name & ".html");

            Append (Categories, Object);
         end;
      end if;
   end Generate_Entities_Category;

   ---------------------------------
   -- Generate_Lang_Documentation --
   ---------------------------------

   overriding procedure Generate_Lang_Documentation
     (Self        : in out HTML_Backend;
      Tree        : access Tree_Type;
      Entity      : Entity_Id;
      Entities    : Collected_Entities;
      Scope_Level : Natural)
   is
      pragma Unreferenced (Scope_Level);

      procedure Build_Entity_Entries
        (Entity_Entries : in out JSON_Array;
         Label          : String;
         Entities       : EInfo_List.Vector);

      --------------------------
      -- Build_Entity_Entries --
      --------------------------

      procedure Build_Entity_Entries
        (Entity_Entries : in out JSON_Array;
         Label          : String;
         Entities       : EInfo_List.Vector)
      is
         Entity_Kind_Entry : constant JSON_Value := Create_Object;
         Entity_Entry      : JSON_Value;
         Aux               : JSON_Array;
         Summary           : JSON_Array;
         Description       : JSON_Array;
         Printer           : Source_Code_Printer;

      begin
         for E of Entities loop
            Extract_Summary_And_Description (E, Summary, Description);

            if Get_Src (E) /= Null_Unbounded_String then
               declare
                  Buffer     : aliased String := To_String (Get_Src (E));
                  Aux_String : Unbounded_String;

               begin
                  Self.Print_Source_Code
                    (Tree.File,
                     Buffer'Unchecked_Access,
                     LL.Get_Location (E).Line,
                     Printer,
                     Aux_String);
                  Prepend (Description, Read (To_String (Aux_String)));
               end;
            end if;

            Entity_Entry := Create_Object;
            Entity_Entry.Set_Field ("label", Get_Short_Name (E));
            Entity_Entry.Set_Field ("line", LL.Get_Location (E).Line);
            Entity_Entry.Set_Field
              ("column", Integer (LL.Get_Location (E).Column));
            Entity_Entry.Set_Field ("src", Get_Srcs_Base_Href (E));
            Entity_Entry.Set_Field ("summary", Summary);
            Entity_Entry.Set_Field ("description", Description);

            if Is_Subprogram_Or_Entry (E)
              and then Present (Get_Comment (E))
            then
               --  Extract parameters

               declare
                  Cursor      : Tag_Cursor := New_Cursor (Get_Comment (E));
                  Tag         : Tag_Info_Ptr;
                  Parameter   : JSON_Value;
                  Parameters  : JSON_Array;
                  Declaration : Xref.General_Entity_Declaration;

               begin
                  while not At_End (Cursor) loop
                     Tag := Get (Cursor);

                     if Tag.Tag = "param" then
                        Declaration :=
                          Xref.Get_Declaration
                            (Self.Context.Database, Tag.Entity);
                        Parameter := Create_Object;
                        Parameter.Set_Field ("label", Declaration.Name);
                        Parameter.Set_Field ("line", Declaration.Loc.Line);
                        Parameter.Set_Field
                          ("column", Natural (Declaration.Loc.Column));
                        Parameter.Set_Field
                          ("description", To_JSON_Representation (Tag.Text));
                        Append (Parameters, Parameter);
                     end if;

                     Next (Cursor);
                  end loop;

                  if Length (Parameters) /= 0 then
                     Entity_Entry.Set_Field ("parameters", Parameters);
                  end if;
               end;

               --  Extract return value

               declare
                  Cursor  : Tag_Cursor := New_Cursor (Get_Comment (E));
                  Tag     : Tag_Info_Ptr;
                  Returns : JSON_Value;

               begin
                  while not At_End (Cursor) loop
                     Tag := Get (Cursor);

                     if Tag.Tag = "return" then
                        Returns := Create_Object;
                        Returns.Set_Field
                          ("description", To_JSON_Representation (Tag.Text));
                        Entity_Entry.Set_Field ("returns", Returns);
                     end if;

                     Next (Cursor);
                  end loop;
               end;

               --  Extract exceptions

               declare
                  Cursor  : Tag_Cursor := New_Cursor (Get_Comment (E));
                  Tag     : Tag_Info_Ptr;
                  Returns : JSON_Value;

               begin
                  while not At_End (Cursor) loop
                     Tag := Get (Cursor);

                     if Tag.Tag = "exception" then
                        Returns := Create_Object;
                        Returns.Set_Field
                          ("description", To_JSON_Representation (Tag.Text));
                        Entity_Entry.Set_Field ("exceptions", Returns);
                     end if;

                     Next (Cursor);
                  end loop;
               end;

            elsif Is_Tagged (E) then
               declare
                  Super     : Entity_Id;
                  Inherits  : JSON_Array;
                  Inherited : JSON_Array;
                  Object    : JSON_Value;

               begin
                  --  Compute set of 'supertypes'

                  Super := Get_Parent (E);

                  if Present (Super) then
                     Object := Create_Object;
                     Set_Label_And_Href (Object, Super);
                     Append (Inherits, Object);
                  end if;

                  for Progenitor of Get_Progenitors (E).all loop
                     Object := Create_Object;
                     Set_Label_And_Href (Object, Progenitor);
                     Append (Inherits, Object);
                  end loop;

                  if Inherits /= Empty_Array then
                     Entity_Entry.Set_Field ("inherits", Inherits);
                  end if;

                  --  Compute set of derived types

                  for Derived of Get_Direct_Derivations (E).all loop
                     Object := Create_Object;
                     Set_Label_And_Href (Object, Derived);
                     Append (Inherited, Object);
                  end loop;

                  if Inherited /= Empty_Array then
                     Entity_Entry.Set_Field ("inherited", Inherited);
                  end if;
               end;
            end if;

            Append (Aux, Entity_Entry);
         end loop;

         Entity_Kind_Entry.Set_Field ("entities", Aux);
         Entity_Kind_Entry.Set_Field ("label", Label);
         Append (Entity_Entries, Entity_Kind_Entry);
      end Build_Entity_Entries;

      Docs_Dir       : constant Virtual_File :=
        Get_Doc_Directory (Self.Context.Kernel).Create_From_Dir ("docs");
      File_Base_Name : constant String := To_Lower (Get_Full_Name (Entity));
      HTML_File_Name : constant String := File_Base_Name & ".html";
      JS_File_Name   : constant String := File_Base_Name & ".js";
      Documentation  : constant JSON_Value := Create_Object;
      Index_Entry    : constant JSON_Value := Create_Object;
      Summary        : JSON_Array;
      Description    : JSON_Array;
      Entity_Entries : JSON_Array;

   begin
      --  Extract package's "summary" and "description".

      Extract_Summary_And_Description (Entity, Summary, Description);
      Documentation.Set_Field ("summary", Summary);
      Documentation.Set_Field ("description", Description);

      --  Process entities

      if not Entities.Generic_Formals.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Generic formals", Entities.Generic_Formals);
      end if;

      if not Entities.Variables.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Constants and variables", Entities.Variables);
      end if;

      if not Entities.Simple_Types.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Simple types", Entities.Simple_Types);
      end if;

      if not Entities.Access_Types.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Access types", Entities.Access_Types);
      end if;

      if not Entities.Record_Types.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Record types", Entities.Record_Types);
      end if;

      if not Entities.Interface_Types.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Interface types", Entities.Interface_Types);
      end if;

      if not Entities.Tagged_Types.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Tagged types", Entities.Tagged_Types);
      end if;

      if not Entities.Tasks.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Tasks and task types", Entities.Tasks);
      end if;

      if not Entities.Subprgs.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Subprograms", Entities.Subprgs);
      end if;

      if not Entities.Methods.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Dispatching subprograms", Entities.Methods);
      end if;

      if not Entities.Pkgs.Is_Empty then
         declare
            Entity_Kind_Entry : constant JSON_Value := Create_Object;
            Entity_Entry      : JSON_Value;
            Aux               : JSON_Array;

         begin
            for E of Entities.Pkgs loop
               Extract_Summary_And_Description (E, Summary, Description);
               Entity_Entry := Create_Object;
               Entity_Entry.Set_Field ("label", Get_Short_Name (E));
               Entity_Entry.Set_Field ("href", "../" & Get_Docs_Href (E));
               Entity_Entry.Set_Field ("summary", Summary);
               Entity_Entry.Set_Field ("description", Description);
               Append (Aux, Entity_Entry);
            end loop;

            Entity_Kind_Entry.Set_Field ("entities", Aux);
            Entity_Kind_Entry.Set_Field ("label", "Nested packages");
            Append (Entity_Entries, Entity_Kind_Entry);
         end;
      end if;

      Documentation.Set_Field ("entities", Entity_Entries);

      --  Write JS data file

      declare
         Translation : Translate_Set;

      begin
         Insert
           (Translation,
            Assoc
              ("DOCUMENTATION_DATA",
               String'(Write (Documentation, False))));
         Write_To_File
           (Self.Context,
            Docs_Dir,
            Filesystem_String (JS_File_Name),
            Parse
              (+Self.Get_Template (Tmpl_Documentation_JS).Full_Name,
               Translation,
               Cached => True));
      end;

      --  Write HTML file

      declare
         Translation : Translate_Set;

      begin
         Insert (Translation, Assoc ("DOCUMENTATION_JS", JS_File_Name));
         Write_To_File
           (Self.Context,
            Get_Doc_Directory (Self.Context.Kernel).Create_From_Dir ("docs"),
            Filesystem_String (HTML_File_Name),
            Parse
              (+Self.Get_Template (Tmpl_Documentation_HTML).Full_Name,
               Translation,
               Cached => True));
      end;

      --  Construct documentation index entry for generated page

      Index_Entry.Set_Field ("label", Get_Full_Name (Entity));
      Index_Entry.Set_Field ("file", "docs/" & HTML_File_Name);
      Self.Doc_Files.Insert (Index_Entry);
   end Generate_Lang_Documentation;

   ------------------------
   -- Set_Label_And_Href --
   ------------------------

   procedure Set_Label_And_Href
     (Object : JSON_Value;
      Entity : Entity_Id) is
   begin
      Object.Set_Field ("label", Get_Short_Name (Entity));

      if Is_Decorated (Entity) then
         Object.Set_Field ("docHref", Get_Docs_Href (Entity));
      end if;
   end Set_Label_And_Href;

   -------------------
   -- Get_Docs_Href --
   -------------------

   function Get_Docs_Href (Entity : Entity_Id) return String is
      Parent : Entity_Id := Entity;

   begin
      while Get_Kind (Parent) /= E_Package loop
         Parent := Get_Scope (Parent);
      end loop;

      return
        "docs/"
        & To_Lower (Get_Full_Name (Parent))
        & ".html#L"
        & Trim (Natural'Image (LL.Get_Location (Entity).Line), Both)
        & "C"
        & Trim
        (Natural'Image (Natural (LL.Get_Location (Entity).Column)), Both);
   end Get_Docs_Href;

   ------------------------
   -- Get_Srcs_Base_Href --
   ------------------------

   function Get_Srcs_Base_Href (Entity : Entity_Id) return String is
   begin
      return
        "srcs/"
        & String (LL.Get_Location (Entity).File.Base_Name)
        & ".html";
   end Get_Srcs_Base_Href;

   -------------------
   -- Get_Scrs_Href --
   -------------------

   function Get_Srcs_Href (Entity : Entity_Id) return String is
   begin
      return
        Get_Srcs_Base_Href (Entity)
        & "#L"
        & Trim (Natural'Image (LL.Get_Location (Entity).Line), Both);
   end Get_Srcs_Href;

   ------------------
   -- Get_Template --
   ------------------

   function Get_Template
     (Self : HTML_Backend'Class;
      Kind : Template_Kinds) return GNATCOLL.VFS.Virtual_File is
   begin
      case Kind is
         when Tmpl_Documentation_HTML =>
            return Self.Get_Resource_File ("documentation.html.tmpl");
         when Tmpl_Documentation_JS =>
            return Self.Get_Resource_File ("documentation.js.tmpl");
         when Tmpl_Documentation_Index_JS =>
            return Self.Get_Resource_File ("documentation_index.js.tmpl");
         when Tmpl_Inheritance_Index_JS =>
            return Self.Get_Resource_File ("inheritance_index.js.tmpl");
         when Tmpl_Entities_Category_HTML =>
            return Self.Get_Resource_File ("entities_category.html.tmpl");
         when Tmpl_Entities_Category_JS =>
            return Self.Get_Resource_File ("entities_category.js.tmpl");
         when Tmpl_Entities_Categories_Index_JS =>
            return Self.Get_Resource_File
              ("entities_categories_index.js.tmpl");
         when Tmpl_Source_File_HTML =>
            return Self.Get_Resource_File ("source_file.html.tmpl");
         when Tmpl_Source_File_JS =>
            return Self.Get_Resource_File ("source_file.js.tmpl");
         when Tmpl_Source_File_Index_JS =>
            return Self.Get_Resource_File ("source_file_index.js.tmpl");
      end case;
   end Get_Template;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self    : in out HTML_Backend;
      Context : access constant Docgen_Context)
   is

      procedure Generate_Support_Files;
      --  Generate support files in destination directory

      procedure Create_Documentation_Directories;
      --  Creates root documentation directory and its subdirectories

      ----------------------------
      -- Generate_Support_Files --
      ----------------------------

      procedure Generate_Support_Files is
         Index_HTML       : constant Filesystem_String := "index.html";
         Inheritance_HTML : constant Filesystem_String :=
                              "inheritance_index.html";
         GNATdoc_JS       : constant Filesystem_String := "gnatdoc.js";
         GNATdoc_CSS      : constant Filesystem_String := "gnatdoc.css";

         Index_HTML_Src       : constant Virtual_File :=
           Self.Get_Resource_File (Index_HTML);
         Index_HTML_Dst       : constant Virtual_File :=
           Get_Doc_Directory
             (Self.Context.Kernel).Create_From_Dir (Index_HTML);
         Inheritance_HTML_Src : constant Virtual_File :=
           Self.Get_Resource_File (Inheritance_HTML);
         Inheritance_HTML_Dst : constant Virtual_File :=
           Get_Doc_Directory
             (Self.Context.Kernel).Create_From_Dir (Inheritance_HTML);
         GNATdoc_JS_Src       : constant Virtual_File :=
           Self.Get_Resource_File (GNATdoc_JS);
         GNATdoc_JS_Dst       : constant Virtual_File :=
           Get_Doc_Directory
             (Self.Context.Kernel).Create_From_Dir (GNATdoc_JS);
         GNATdoc_CSS_Src      : constant Virtual_File :=
           Self.Get_Resource_File (GNATdoc_CSS);
         GNATdoc_CSS_Dst      : constant Virtual_File :=
           Get_Doc_Directory
             (Self.Context.Kernel).Create_From_Dir (GNATdoc_CSS);

         Success : Boolean;

      begin
         Index_HTML_Src.Copy (Index_HTML_Dst.Full_Name, Success);
         pragma Assert (Success);
         Inheritance_HTML_Src.Copy (Inheritance_HTML_Dst.Full_Name, Success);
         pragma Assert (Success);
         GNATdoc_JS_Src.Copy (GNATdoc_JS_Dst.Full_Name, Success);
         pragma Assert (Success);
         GNATdoc_CSS_Src.Copy (GNATdoc_CSS_Dst.Full_Name, Success);
         pragma Assert (Success);
      end Generate_Support_Files;

      --------------------------------------
      -- Create_Documentation_Directories --
      --------------------------------------

      procedure Create_Documentation_Directories is
         Root_Dir : constant Virtual_File :=
           Get_Doc_Directory (Self.Context.Kernel);
         Srcs_Dir : constant Virtual_File := Root_Dir.Create_From_Dir ("srcs");
         Docs_Dir : constant Virtual_File := Root_Dir.Create_From_Dir ("docs");
         Ents_Dir : constant Virtual_File :=
           Root_Dir.Create_From_Dir ("entities");

      begin
         if not Root_Dir.Is_Directory then
            Root_Dir.Make_Dir;
         end if;

         if not Srcs_Dir.Is_Directory then
            Srcs_Dir.Make_Dir;
         end if;

         if not Docs_Dir.Is_Directory then
            Docs_Dir.Make_Dir;
         end if;

         if not Ents_Dir.Is_Directory then
            Ents_Dir.Make_Dir;
         end if;
      end Create_Documentation_Directories;

   begin
      GNATdoc.Backend.Base.Base_Backend (Self).Initialize (Context);

      --  Create documentation directory and its subdirectories

      Create_Documentation_Directories;

      --  Copy support files

      Generate_Support_Files;
   end Initialize;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : GNATCOLL.JSON.JSON_Value;
      Right : GNATCOLL.JSON.JSON_Value) return Boolean
   is
      L : constant String := Left.Get ("label");
      R : constant String := Right.Get ("label");

   begin
      return L < R;
   end Less;

   ----------
   -- Name --
   ----------

   overriding function Name (Self : HTML_Backend) return String is
      pragma Unreferenced (Self);

   begin
      return "html";
   end Name;

   ----------------------------
   -- To_JSON_Representation --
   ----------------------------

   function To_JSON_Representation
     (Text : Ada.Strings.Unbounded.Unbounded_String)
      return GNATCOLL.JSON.JSON_Array
   is
      Result      : JSON_Array;
      Delimiter   : Natural;
      Slice_First : Positive := 1;
      Slice_Last  : Positive;
      Paragraph   : JSON_Value;
      Span        : JSON_Value;
      Aux         : JSON_Array;

   begin
      while Slice_First <= Length (Text) loop
         Delimiter := Index (Text, ASCII.LF & ASCII.LF, Slice_First);

         if Delimiter = 0 then
            Slice_Last := Length (Text);

         else
            Slice_Last := Delimiter - 1;
         end if;

         Span := Create_Object;
         Span.Set_Field ("kind", "span");
         Span.Set_Field
           ("text", Slice (Text, Slice_First, Slice_Last));

         Paragraph := Create_Object;
         Paragraph.Set_Field ("kind", "paragraph");
         Aux := Empty_Array;
         Append (Aux, Span);
         Paragraph.Set_Field ("children", Aux);

         Append (Result, Paragraph);

         Slice_First := Slice_Last + 1;

         while Slice_First <= Length (Text) loop
            exit when Element (Text, Slice_First) /= ASCII.LF;
            Slice_First := Slice_First + 1;
         end loop;
      end loop;

      return Result;
   end To_JSON_Representation;

end GNATdoc.Backend.HTML;
