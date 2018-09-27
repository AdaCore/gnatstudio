------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

with Ada.Calendar.Formatting;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

with GNAT.Strings;                      use GNAT.Strings;
with GNATCOLL.JSON;                     use GNATCOLL.JSON;
with GNATCOLL.Traces;                   use GNATCOLL.Traces;

with Basic_Types;
with Language;                          use Language;
with Templates_Parser;                  use Templates_Parser;

with GNATdoc.Backend.HTML.JSON_Builder; use GNATdoc.Backend.HTML.JSON_Builder;
with GNATdoc.Backend.HTML.Source_Code;  use GNATdoc.Backend.HTML.Source_Code;
with GNATdoc.Backend.Text_Parser;       use GNATdoc.Backend.Text_Parser;
with GNATdoc.Comment;                   use GNATdoc.Comment;
with GNATdoc.Markup_Streams;            use GNATdoc.Markup_Streams;
with GNATdoc.Utils;                     use GNATdoc.Utils;

package body GNATdoc.Backend.HTML is
   Me : constant Trace_Handle := Create ("GNATdoc.1-HTML_Backend");

   type Template_Kinds is
     (Tmpl_Index_JS,                --  Main page data
      Tmpl_Documentation_HTML,      --  Documentation (HTML page)
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
     (Self        : HTML_Backend'Class;
      Entity      : Entity_Id;
      Summary     : out JSON_Array;
      Description : out JSON_Array);
   --  Extracts summary and description of the specified entity

   function Get_Template
     (Self : HTML_Backend'Class;
      Kind : Template_Kinds) return GNATCOLL.VFS.Virtual_File;
   --  Returns file name of the specified template.

   function To_JSON_Representation
     (Text    : Ada.Strings.Unbounded.Unbounded_String;
      Context : Docgen_Context) return GNATCOLL.JSON.JSON_Array;
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

   function Get_Srcs_Base_Href
     (Self   : HTML_Backend'Class;
      Entity : Entity_Id) return String;
   --  Returns href to source file where this entity is declared. Returned URI
   --  doesn't have fragment identifier.

   function Get_Srcs_Href
     (Self   : HTML_Backend'Class;
      Entity : Entity_Id) return String;
   --  Returns href to source file where this entity is declared. Returned URI
   --  includes fragment identifier to navigate to source code line.

   function Compilation_Unit_File_Basename (Entity : Entity_Id) return String;
   --  Returns basename to be used to construct filenames for enclosing
   --  compilation unit of given entity.

   function Get_Qualifier
     (Entity : Entity_Id; Include_Nested : Boolean := True) return String;
   --  Returns qualifier text for entity.

   procedure Print_Source_Code
     (Self       : HTML_Backend'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Buffer     : GNAT.Strings.String_Access;
      First_Line : Positive;
      Printer    : in out Source_Code.Source_Code_Printer'Class;
      Code       : out GNATCOLL.JSON.JSON_Value);

   function From_Spec
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Entity : Entity_Id) return Boolean;
   --  Returns True when given entity declared in specification.

   procedure Add_Instantiation_Information
     (Documentation : in out JSON_Value;
      Entity        : Entity_Id);
   --  Adds instantiation information for instantiation of generic package or
   --  generic subprogram.

   ---------
   -- "<" --
   ---------

   function "<" (Left : Entity_Id; Right : Entity_Id) return Boolean is

      use type Basic_Types.Visible_Column_Type;

      LN : constant String := To_Lower (Get_Short_Name (Left));
      LL : constant General_Location := Atree.LL.Get_Location (Left);
      RN : constant String := To_Lower (Get_Short_Name (Right));
      RL : constant General_Location := Atree.LL.Get_Location (Right);

   begin
      if LN < RN then
         return True;

      elsif LN = RN then
         if Get_Corresponding_Body (Left) = Right then
            return True;

         elsif LL.File < RL.File then
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

   -----------------------------------
   -- Add_Instantiation_Information --
   -----------------------------------

   procedure Add_Instantiation_Information
     (Documentation : in out JSON_Value;
      Entity        : Entity_Id) is
   begin
      if Present (LL.Get_Instance_Of (Entity)) then
         declare
            Instance_Of : constant Entity_Id :=
              Find_Unique_Entity
                (Get_Declaration (LL.Get_Instance_Of (Entity)).Loc);
            Object      : JSON_Value;

         begin
            if Present (Instance_Of) then
               Object := Create_Object;
               Set_Label_And_Href (Object, Instance_Of, True);
               Documentation.Set_Field ("instantiation", Object);
            end if;
         end;
      end if;
   end Add_Instantiation_Information;

   ------------------------------------
   -- Compilation_Unit_File_Basename --
   ------------------------------------

   function Compilation_Unit_File_Basename
     (Entity : Entity_Id) return String
   is
      Source : constant Virtual_File := LL.Get_Location (Entity).File;
      Parent : Entity_Id := Entity;
      Suffix : Unbounded_String;

   begin
      while Present (Get_Scope (Parent))
        and then LL.Get_Location (Get_Scope (Parent)).File = Source
      loop
         if Get_Kind (Parent) in E_Package | E_Generic_Package
           or else Is_Concurrent_Type_Or_Object (Parent)
         then
            Suffix := "___" & To_Lower (Get_Short_Name (Parent)) & Suffix;
         end if;

         Parent := Get_Scope (Parent);
      end loop;

      declare
         Name : constant String := To_Lower (Get_Full_Name (Parent));
         File : Unbounded_String;

      begin
         for C of Name loop
            if C = '.' then
               Append (File, "__");

            else
               Append (File, C);
            end if;
         end loop;

         Append (File, Suffix);

         if No (Get_Corresponding_Spec (Entity)) then
            return To_String (File) & "___spec";

         else
            return To_String (File) & "___body";
         end if;
      end;
   end Compilation_Unit_File_Basename;

   -------------------------------------
   -- Extract_Summary_And_Description --
   -------------------------------------

   procedure Extract_Summary_And_Description
     (Self        : HTML_Backend'Class;
      Entity      : Entity_Id;
      Summary     : out JSON_Array;
      Description : out JSON_Array) is
   begin
      Summary     := Empty_Array;
      Description := Empty_Array;

      --  In extensions mode process summary/description of subprogram
      --  specification before.

      if Self.Context.Options.Extensions_Enabled
        and then Is_Subprogram_Body (Entity)
        and then Present (Get_Corresponding_Spec (Entity))
        and then Present (Get_Comment (Get_Corresponding_Spec (Entity)))
      then
         declare
            Cursor : Tag_Cursor :=
              New_Cursor (Get_Comment (Get_Corresponding_Spec (Entity)));
            Tag    : Tag_Info_Ptr;

         begin
            while not At_End (Cursor) loop
               Tag := Get (Cursor);

               if Tag.Tag = "summary" then
                  Summary :=
                    To_JSON_Representation (Tag.Text, Self.Context.all);

               elsif Tag.Tag = "description"
                 or Tag.Tag = ""
               then
                  Description :=
                    To_JSON_Representation (Tag.Text, Self.Context.all);
               end if;

               Next (Cursor);
            end loop;
         end;
      end if;

      --  Process summary/description of entity itself

      if Present (Get_Comment (Entity)) then
         declare
            Cursor : Tag_Cursor := New_Cursor (Get_Comment (Entity));
            Tag    : Tag_Info_Ptr;

         begin
            while not At_End (Cursor) loop
               Tag := Get (Cursor);

               if Tag.Tag = "summary" then
                  Summary :=
                    To_JSON_Representation (Tag.Text, Self.Context.all);

               elsif Tag.Tag = "description"
                 or Tag.Tag = ""
               then
                  declare
                     Body_Description : constant JSON_Array :=
                       To_JSON_Representation (Tag.Text, Self.Context.all);

                  begin
                     for J in 1 .. Length (Body_Description) loop
                        Append (Description, Get (Body_Description, J));
                     end loop;
                  end;
               end if;

               Next (Cursor);
            end loop;
         end;
      end if;
   end Extract_Summary_And_Description;

   ---------------
   -- From_Spec --
   ---------------

   function From_Spec
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Entity : Entity_Id) return Boolean is
   begin
      return Is_Spec_File (Kernel, LL.Get_Location (Entity).File);
   end From_Spec;

   -----------------------
   -- Print_Source_Code --
   -----------------------

   procedure Print_Source_Code
     (Self       : HTML_Backend'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Buffer     : GNAT.Strings.String_Access;
      First_Line : Positive;
      Printer    : in out Source_Code.Source_Code_Printer'Class;
      Code       : out GNATCOLL.JSON.JSON_Value)
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

            when Aspect_Comment_Text =>
               Printer.Aspect_Comment_Text (Sloc_Start, Sloc_End, Continue);

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
      Printer.Start_File
        (File,
         Buffer,
         First_Line,
         Self.Context.Options.Show_Private,
         Continue);

      if Continue then
         Lang.Parse_Entities (Buffer.all, Callback'Unrestricted_Access);
         Printer.End_File (Code, Continue);
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
         Root_Types : in out EInfo_List.Vector)
      is
         Full_View : Entity_Id;
      begin
         if Get_IDepth_Level (Entity) = 0 then
            if not Root_Types.Contains (Entity) then
               Root_Types.Append (Entity);
            end if;

         else
            if Is_Partial_View (Entity) then
               Full_View := Get_Full_View (Entity);
            else
               Full_View := Entity;
            end if;

            Analyze_Inheritance_Tree (Get_Parent (Full_View), Root_Types);

            for Progenitor of Get_Progenitors (Full_View).all loop
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
      Printer    : Source_Code_Printer (Self.Context.Kernel);
      Sloc_First : Source_Location;
      Sloc_Last  : Source_Location;
      Code       : JSON_Value;
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

            when Aspect_Comment_Text =>
               Printer.Aspect_Comment_Text (Sloc_Start, Sloc_End, Continue);

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

      Src_Files : GNATdoc.Files.Files_List.Vector;

   begin
      --  Generate general information JSON data file.

      declare
         Object      : GNATCOLL.JSON.JSON_Value;
         Translation : Translate_Set;

      begin
         Object := GNATCOLL.JSON.Create_Object;
         Object.Set_Field
           ("project", Self.Context.Kernel.Registry.Tree.Root_Project.Name);
         Object.Set_Field
           ("timestamp", Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));

         Insert
           (Translation,
            Assoc ("INDEX_DATA", String'(Write (Object, False))));
         Write_To_File
           (Self.Context,
            Get_Doc_Directory (Self.Context.Kernel),
            "index.js",
            Parse (+Self.Get_Template (Tmpl_Index_JS).Full_Name, Translation));
      end;

      --  Generate annotated sources and compute index of source files.

      Src_Files := Self.Src_Files;
      Files_Vector_Sort.Sort (Src_Files);

      for File of Src_Files loop
         --  Generate source listing for spec files only, this is requirement
         --  for development under PC02-022.

         if Is_Spec_File (Self.Context.Kernel, File) then
            Trace
              (Me, "generate annotated source for " & String (File.Base_Name));

            Lang      :=
              Get_Language_From_File (Self.Context.Lang_Handler, File);
            Buffer    := Read_Source_File (Self.Context, File);
            Sloc_Last := (0, 0, 0);
            Printer.Start_File
              (File, Buffer, 1, Self.Context.Options.Show_Private, Continue);

            if Continue then
               Lang.Parse_Entities (Buffer.all, Callback'Unrestricted_Access);
               Printer.End_File (Code, Continue);
               Code.Set_Field ("label", String (File.Base_Name));

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
                          (+Self.Get_Template
                               (Tmpl_Source_File_HTML).Full_Name,
                           Translation,
                           Cached => True));
                  end;

                  --  Write JSON data file

                  declare
                     Translation : Translate_Set;

                  begin
                     Insert
                       (Translation,
                        Assoc
                          ("SOURCE_FILE_DATA", String'(Write (Code, False))));
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
                  Object.Set_Field ("label", String (File.Base_Name));
                  Object.Set_Field
                    ("srcHref", "srcs/" & String (File.Base_Name) & ".html");
                  Append (Sources, Object);
               end if;
            end if;

            Free (Buffer);
         end if;
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
         Translation  : Translate_Set;
         Result       : GNATCOLL.JSON.JSON_Array;
         Group_Items  : GNATCOLL.JSON.JSON_Array;
         Group_Object : GNATCOLL.JSON.JSON_Value;

      begin
         for Group of Self.Doc_Groups loop
            Clear (Group_Items);

            for Object of Group.Doc_Files loop
               Append (Group_Items, Object);
            end loop;

            Group_Object := Create_Object;
            Group_Object.Set_Field ("label", Group.Name);
            Group_Object.Set_Field ("items", Group_Items);

            Append (Result, Group_Object);
         end loop;

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
           (Self.Entities.Tasks,
            "Tasks & Task Types",
            "tasks",
            Categories_Index);
         Self.Generate_Entities_Category
           (Self.Entities.Protected_Objects,
            "Protected Objects & Protected Types",
            "protecteds",
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
         --  Copy entities to temporary ordered set to order them by names (or
         --  file name/location when name is not available) in generated
         --  documentation.

         for Entity of Entities loop
            Set.Insert (Entity);
         end loop;

         for Entity of Set loop
            Object := Create_Object;
            Set_Label_And_Href (Object, Entity);

            Scope := Get_Scope (Entity);
            Object.Set_Field ("declared", Get_Full_Name (Scope));
            Object.Set_Field ("declared_qualifier", Get_Qualifier (Entity));

            if Self.Get_Srcs_Href (Entity) /= "" then
               Object.Set_Field ("srcHref", Self.Get_Srcs_Href (Entity));
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

      use type EInfo_List.Vector;

      procedure Build_Entity_Entries
        (Entity_Entries : in out JSON_Array;
         Label          : String;
         Entities       : EInfo_List.Vector);

      function Entity_Data
        (Tag_Name : Unbounded_String;
         Entity   : Root_Entity'Class;
         Text     : Unbounded_String) return JSON_Value;
      --  Constructs description data for given tag

      procedure Process_Parameters_And_Return
        (Entity             : Entity_Id;
         Generic_Parameters : out JSON_Array;
         Parameters         : out JSON_Array;
         Returns            : out JSON_Value);
      --  Extract documentation for generic parameters/parameters/return of
      --  the given subprogram.

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
         Printer           : Source_Code_Printer (Self.Context.Kernel);
         Sorted_Entities   : Entity_Id_Ordered_Sets.Set;

      begin
         --  Copy entities into ordered set to sort them.

         for E of Entities loop
            Sorted_Entities.Insert (E);
         end loop;

         for E of Sorted_Entities loop
            Self.Extract_Summary_And_Description (E, Summary, Description);

            if Present (Get_Src (E))
              and not Is_Concurrent_Type_Or_Object (E)
            then
               --  Source code snippet doesn't generated when source code is
               --  not available. It is not generated for tasks & protecteds
               --  either.

               declare
                  Buffer : aliased String := To_String (Get_Src (E));
                  Code   : JSON_Value;

               begin
                  Self.Print_Source_Code
                    (Tree.File,
                     Buffer'Unchecked_Access,
                     LL.Get_Location (E).Line,
                     Printer,
                     Code);
                  Prepend (Description, Code);
               end;
            end if;

            Entity_Entry := Create_Object;
            Entity_Entry.Set_Field ("label", Get_Short_Name (E));
            Entity_Entry.Set_Field ("qualifier", Get_Qualifier (E));
            Entity_Entry.Set_Field ("line", LL.Get_Location (E).Line);
            Entity_Entry.Set_Field
              ("column", Integer (LL.Get_Location (E).Column));

            if Self.Get_Srcs_Base_Href (E) /= "" then
               Entity_Entry.Set_Field ("src", Self.Get_Srcs_Base_Href (E));
            end if;

            Entity_Entry.Set_Field ("summary", Summary);

            if Is_Concurrent_Type_Or_Object (E) then
               --  Description is not filled for tasks & protecteds

               Entity_Entry.Set_Field ("href", "../" & Get_Docs_Href (E));

            else
               Entity_Entry.Set_Field ("description", Description);
            end if;

            if Present (LL.Get_Instance_Of (E)) then
               Add_Instantiation_Information (Entity_Entry, E);

            elsif Is_Subprogram_Or_Entry (E)
              and then Present (Get_Comment (E))
            then
               if Self.Context.Options.Extensions_Enabled then
                  declare
                     Generic_Parameters : JSON_Array;
                     Parameters         : JSON_Array;
                     Returns            : JSON_Value;

                  begin
                     Process_Parameters_And_Return
                       (E, Generic_Parameters, Parameters, Returns);

                     if not Is_Empty (Parameters) then
                        Entity_Entry.Set_Field ("parameters", Parameters);
                     end if;

                     if Returns /= JSON_Null then
                        Entity_Entry.Set_Field ("returns", Returns);
                     end if;

                     if not Is_Empty (Generic_Parameters) then
                        Entity_Entry.Set_Field
                          ("generic_parameters", Generic_Parameters);
                     end if;
                  end;

               else
                  --  Extract parameters

                  declare
                     Cursor     : Tag_Cursor := New_Cursor (Get_Comment (E));
                     Tag        : Tag_Info_Ptr;
                     Parameters : JSON_Array;

                  begin
                     while not At_End (Cursor) loop
                        Tag := Get (Cursor);

                        if Tag.Tag = "param" then
                           Append
                             (Parameters,
                              Entity_Data
                                (Tag.Tag, Tag.Entity.Element, Tag.Text));
                        end if;

                        Next (Cursor);
                     end loop;

                     if Length (Parameters) /= 0 then
                        Entity_Entry.Set_Field ("parameters", Parameters);
                     end if;
                  end;

                  --  Extract return value

                  declare
                     Cursor : Tag_Cursor := New_Cursor (Get_Comment (E));
                     Tag    : Tag_Info_Ptr;

                  begin
                     while not At_End (Cursor) loop
                        Tag := Get (Cursor);

                        if Tag.Tag = "return" then
                           Entity_Entry.Set_Field
                             ("returns",
                              Entity_Data
                                (Tag.Tag, Tag.Entity.Element, Tag.Text));
                        end if;

                        Next (Cursor);
                     end loop;
                  end;
               end if;

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
                          ("description",
                           To_JSON_Representation
                             (Tag.Text, Self.Context.all));
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

            if Is_Record_Type (E)
              and then Present (Get_Comment (E))
            then
               --  Extract fields

               declare
                  Cursor : Tag_Cursor := New_Cursor (Get_Comment (E));
                  Tag    : Tag_Info_Ptr;
                  Fields : JSON_Array;

               begin
                  while not At_End (Cursor) loop
                     Tag := Get (Cursor);

                     if Tag.Tag = "field" then
                        Append
                          (Fields,
                           Entity_Data
                             (Tag.Tag, Tag.Entity.Element, Tag.Text));
                     end if;

                     Next (Cursor);
                  end loop;

                  if Length (Fields) /= 0 then
                     Entity_Entry.Set_Field ("fields", Fields);
                  end if;
               end;
            end if;

            if Get_Kind (E) = E_Enumeration_Type
              and then Present (Get_Comment (E))
            then
               --  Extract enumeration literals

               declare
                  Cursor   : Tag_Cursor := New_Cursor (Get_Comment (E));
                  Tag      : Tag_Info_Ptr;
                  Literals : JSON_Array;

               begin
                  while not At_End (Cursor) loop
                     Tag := Get (Cursor);

                     if Tag.Tag = "value" then
                        Append
                          (Literals,
                           Entity_Data
                             (Tag.Tag, Tag.Entity.Element, Tag.Text));
                     end if;

                     Next (Cursor);
                  end loop;

                  if Length (Literals) /= 0 then
                     Entity_Entry.Set_Field ("literals", Literals);
                  end if;
               end;
            end if;

            Append (Aux, Entity_Entry);
         end loop;

         Entity_Kind_Entry.Set_Field ("entities", Aux);
         Entity_Kind_Entry.Set_Field ("label", Label);
         Append (Entity_Entries, Entity_Kind_Entry);
      end Build_Entity_Entries;

      -----------------
      -- Entity_Data --
      -----------------

      function Entity_Data
        (Tag_Name : Unbounded_String;
         Entity   : Root_Entity'Class;
         Text     : Unbounded_String) return JSON_Value
      is
         Result      : JSON_Value;
         Declaration : Xref.General_Entity_Declaration;
         Type_Data   : JSON_Value;
         Entity_Type : Entity_Id;

      begin
         Result := Create_Object;

         if Tag_Name /= "return" then
            Declaration := Xref.Get_Declaration (Entity);
            Result.Set_Field ("label", Declaration.Name);
            Result.Set_Field ("line", Declaration.Loc.Line);
            Result.Set_Field ("column", Natural (Declaration.Loc.Column));

            if Tag_Name /= "value" and Tag_Name /= "gen_param" then
               --  Construct reference information to entity's type

               Declaration :=
                 Xref.Get_Declaration (Xref.Get_Type_Of (Entity));
               Entity_Type := Find_Unique_Entity (Declaration.Loc);

               Type_Data := Create_Object;

               if Present (Entity_Type) then
                  Type_Data.Set_Field
                    ("label", Get_Full_Name (Entity_Type));
                  Type_Data.Set_Field
                    ("docHref", Get_Docs_Href (Entity_Type));

               else
                  Type_Data.Set_Field ("label", Declaration.Name);
               end if;

               Result.Set_Field ("type", Type_Data);
            end if;
         end if;

         Result.Set_Field
           ("description",
            To_JSON_Representation (Text, Self.Context.all));

         return Result;
      end Entity_Data;

      -----------------------------------
      -- Process_Parameters_And_Return --
      -----------------------------------

      procedure Process_Parameters_And_Return
        (Entity             : Entity_Id;
         Generic_Parameters : out JSON_Array;
         Parameters         : out JSON_Array;
         Returns            : out JSON_Value)
      is
         Entity_Spec : constant Entity_Id :=
           (if Is_Subprogram (Entity)
                 and then Present (Get_Corresponding_Spec (Entity))
              then Get_Corresponding_Spec (Entity)
              else No_Entity);

         function Process_Parameters_And_Return
           (Entity      : Entity_Id;
            Scope_Level : Natural) return Traverse_Result;
         --  Extracts information about parameters (for all kinds
         --  of subprograms) and return value (for functions).

         -----------------------------------
         -- Process_Parameters_And_Return --
         -----------------------------------

         function Process_Parameters_And_Return
           (Entity      : Entity_Id;
            Scope_Level : Natural) return Traverse_Result
         is
            Entity_Documentation : constant Unbounded_String_Vectors.Vector :=
              Get_Doc (Entity).Text;
            Spec_Documentation   : Unbounded_String_Vectors.Vector;

            function Process_Spec_Parameters_And_Return
              (Spec_Entity : Entity_Id;
               Scope_Level : Natural) return Traverse_Result;
            --  Process entities of subprogram specification to lookup
            --  documentation for processed entity.

            ----------------------------------------
            -- Process_Spec_Parameters_And_Return --
            ----------------------------------------

            function Process_Spec_Parameters_And_Return
              (Spec_Entity : Entity_Id;
               Scope_Level : Natural) return Traverse_Result is
            begin
               if Get_Short_Name (Spec_Entity) = Get_Short_Name (Entity) then
                  Spec_Documentation := Get_Doc (Spec_Entity).Text;

                  return Skip;
               end if;

               return (if Scope_Level > 1 then Skip else OK);
            end Process_Spec_Parameters_And_Return;

         begin
            if Present (Entity_Spec) then
               Traverse_Tree
                 (Entity_Spec, Process_Spec_Parameters_And_Return'Access);
            end if;

            if Get_Kind (Entity) = E_Formal then
               Append
                 (Parameters,
                  Entity_Data
                    (To_Unbounded_String ("param"),
                     LL.Get_Entity (Entity),
                     Utils.To_Unbounded_String
                       (if Entity_Documentation.Is_Empty
                        then Spec_Documentation else Entity_Documentation)));

            elsif Get_Kind (Entity) = E_Return then
               Returns :=
                 Entity_Data
                   (To_Unbounded_String ("return"),
                    LL.Get_Entity (Entity),
                    Utils.To_Unbounded_String
                      (if Entity_Documentation.Is_Empty
                        then Spec_Documentation else Entity_Documentation));

            elsif Get_Kind (Entity) = E_Generic_Formal then
               Append
                 (Generic_Parameters,
                  Entity_Data
                    (To_Unbounded_String ("gen_param"),
                     LL.Get_Entity (Entity),
                     Utils.To_Unbounded_String
                       (if Entity_Documentation.Is_Empty
                        then Spec_Documentation else Entity_Documentation)));
            end if;

            return (if Scope_Level > 1 then Skip else OK);
         end Process_Parameters_And_Return;

      begin
         Generic_Parameters := Empty_Array;
         Parameters         := Empty_Array;
         Returns            := JSON_Null;

         Traverse_Tree (Entity, Process_Parameters_And_Return'Access);
      end Process_Parameters_And_Return;

      Docs_Dir       : constant Virtual_File :=
        Get_Doc_Directory (Self.Context.Kernel).Create_From_Dir ("docs");
      File_Base_Name : constant String :=
        Compilation_Unit_File_Basename (Entity);
      HTML_File_Name : constant String := File_Base_Name & ".html";
      JS_File_Name   : constant String := File_Base_Name & ".js";
      Documentation  : JSON_Value := Create_Object;
      Index_Entry    : constant JSON_Value := Create_Object;
      Summary        : JSON_Array;
      Description    : JSON_Array;
      Entity_Entries : JSON_Array;
      Default_Group  : Boolean := True;

   begin
      --  Skip separates.

      if Is_Separate_Unit (Entity) then
         return;
      end if;

      Documentation.Set_Field ("label", Get_Full_Name (Entity));
      Documentation.Set_Field ("qualifier", Get_Qualifier (Entity));

      --  Extract package's "summary" and "description".

      Self.Extract_Summary_And_Description (Entity, Summary, Description);
      Documentation.Set_Field ("summary", Summary);
      Documentation.Set_Field ("description", Description);

      --  Process entity specific information

      if Present (Get_Alias (Entity))
        and then (Is_Subprogram (Entity) or Is_Package (Entity))
      then
         declare
            Alias_Of : constant Entity_Id := Get_Alias (Entity);
            Object   : JSON_Value;

         begin
            if Present (Alias_Of) then
               Object := Create_Object;
               Set_Label_And_Href (Object, Alias_Of, True);
               Documentation.Set_Field ("renaming", Object);
            end if;
         end;
      end if;

      Add_Instantiation_Information (Documentation, Entity);

      --  Process entities

      declare
         Generic_Formals : EInfo_List.Vector;

      begin
         --  Extract generic formals for generic compilation unit itself

         for Parameter of Entities.Generic_Formals loop
            if Get_Parent (Parameter) = Entity then
               Generic_Formals.Append (Parameter);
            end if;
         end loop;

         if not Generic_Formals.Is_Empty then
            Build_Entity_Entries
              (Entity_Entries, "Generic formals", Generic_Formals);
         end if;
      end;

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

      if not Entities.Protected_Objects.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Protected objects and protected types",
            Entities.Protected_Objects);
      end if;

      if not Entities.Entries.Is_Empty then
         Build_Entity_Entries (Entity_Entries, "Entries", Entities.Entries);
      end if;

      if not Entities.Subprgs.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Subprograms", Entities.Subprgs);
      end if;

      if not Entities.Methods.Is_Empty then
         Build_Entity_Entries
           (Entity_Entries, "Dispatching subprograms", Entities.Methods);
      end if;

      if not Entities.Pkgs_Instances.Is_Empty
        or not Entities.Subprgs_Instances.Is_Empty
      then
         Build_Entity_Entries
           (Entity_Entries,
            "Generic instantiations",
            Entities.Pkgs_Instances & Entities.Subprgs_Instances);
      end if;

      if not Entities.Pkgs.Is_Empty then
         declare
            Entity_Kind_Entry : constant JSON_Value := Create_Object;
            Entity_Entry      : JSON_Value;
            Aux               : JSON_Array;

         begin
            for E of Entities.Pkgs loop
               Self.Extract_Summary_And_Description (E, Summary, Description);
               Entity_Entry := Create_Object;
               Entity_Entry.Set_Field ("label", Get_Short_Name (E));
               Entity_Entry.Set_Field ("href", "../" & Get_Docs_Href (E));
               Entity_Entry.Set_Field ("qualifier", Get_Qualifier (E, False));
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

      if not Is_Concurrent_Type_Or_Object (Entity) then
         --  Construct documentation index entry for generated page

         Index_Entry.Set_Field ("label", Get_Full_Name (Entity));
         Index_Entry.Set_Field ("qualifier", Get_Qualifier (Entity));
         Index_Entry.Set_Field ("file", "docs/" & HTML_File_Name);

         if Present (Get_Comment (Entity)) then
            declare
               Cursor : Tag_Cursor := New_Cursor (Get_Comment (Entity));
               Tag    : Tag_Info_Ptr;

            begin
               while not At_End (Cursor) loop
                  Tag := Get (Cursor);

                  if Tag.Tag = "group" then
                     Default_Group := False;

                     if not Self.Doc_Groups.Contains (Tag.Text) then
                        Self.Doc_Groups.Insert
                          (Tag.Text,
                           new Docs_Group'(Name => Tag.Text, Doc_Files => <>));
                     end if;

                     Self.Doc_Groups (Tag.Text).Doc_Files.Insert (Index_Entry);
                  end if;

                  Next (Cursor);
               end loop;
            end;
         end if;

         if Default_Group then
            Self.Doc_Files.Insert (Index_Entry);
         end if;
      end if;
   end Generate_Lang_Documentation;

   ------------------------
   -- Set_Label_And_Href --
   ------------------------

   procedure Set_Label_And_Href
     (Object    : JSON_Value;
      Entity    : Entity_Id;
      Full_Name : Boolean := False) is
   begin
      Object.Set_Field
        ("label",
         (if Full_Name
            then Get_Full_Name (Entity)
            else Get_Short_Name (Entity)));
      Object.Set_Field ("docHref", Get_Docs_Href (Entity));
   end Set_Label_And_Href;

   -------------------
   -- Get_Docs_Href --
   -------------------

   function Get_Docs_Href (Entity : Entity_Id) return String is
   begin
      return
        "docs/"
        & Compilation_Unit_File_Basename (Entity)
        & ".html#L"
        & Trim (Natural'Image (LL.Get_Location (Entity).Line), Both)
        & "C"
        & Trim
        (Natural'Image (Natural (LL.Get_Location (Entity).Column)), Both);
   end Get_Docs_Href;

   -------------------
   -- Get_Qualifier --
   -------------------

   function Get_Qualifier
     (Entity : Entity_Id; Include_Nested : Boolean := True) return String is
   begin
      if Present (LL.Get_Instance_Of (Entity)) then
         return "(generic instantiation)";

      elsif Include_Nested
        and then Get_Kind (Entity) in E_Package | E_Generic_Package
        and then Present (Get_Scope (Entity))
        and then LL.Get_Location (Entity).File
                   = LL.Get_Location (Get_Scope (Entity)).File
      then
         return
           (if Present (Get_Alias (Entity)) then "(renaming)"
            elsif No (Get_Corresponding_Spec (Entity))
            then "(nested)" else "(nested, body)");

      else
         return
           (if Present (Get_Alias (Entity)) then "(renaming)"
            elsif No (Get_Corresponding_Spec (Entity)) then "" else "(body)");
      end if;
   end Get_Qualifier;

   ------------------------
   -- Get_Srcs_Base_Href --
   ------------------------

   function Get_Srcs_Base_Href
     (Self   : HTML_Backend'Class;
      Entity : Entity_Id) return String is
   begin
      if From_Spec (Self.Context.Kernel, Entity) then
         return
           "srcs/"
           & String (LL.Get_Location (Entity).File.Base_Name)
           & ".html";

      else
         return "";
      end if;
   end Get_Srcs_Base_Href;

   -------------------
   -- Get_Srcs_Href --
   -------------------

   function Get_Srcs_Href
     (Self   : HTML_Backend'Class;
      Entity : Entity_Id) return String is
   begin
      if Self.Get_Srcs_Base_Href (Entity) /= "" then
         return
           Self.Get_Srcs_Base_Href (Entity)
           & "#L"
           & Trim (Natural'Image (LL.Get_Location (Entity).Line), Both);

      else
         return "";
      end if;
   end Get_Srcs_Href;

   ------------------
   -- Get_Template --
   ------------------

   function Get_Template
     (Self : HTML_Backend'Class;
      Kind : Template_Kinds) return GNATCOLL.VFS.Virtual_File is
   begin
      case Kind is
         when Tmpl_Index_JS =>
            return Self.Get_Resource_File ("templates/index.js.tmpl");
         when Tmpl_Documentation_HTML =>
            return Self.Get_Resource_File
              ("templates/documentation.html.tmpl");
         when Tmpl_Documentation_JS =>
            return Self.Get_Resource_File ("templates/documentation.js.tmpl");
         when Tmpl_Documentation_Index_JS =>
            return Self.Get_Resource_File
              ("templates/documentation_index.js.tmpl");
         when Tmpl_Inheritance_Index_JS =>
            return Self.Get_Resource_File
              ("templates/inheritance_index.js.tmpl");
         when Tmpl_Entities_Category_HTML =>
            return Self.Get_Resource_File
              ("templates/entities_category.html.tmpl");
         when Tmpl_Entities_Category_JS =>
            return Self.Get_Resource_File
              ("templates/entities_category.js.tmpl");
         when Tmpl_Entities_Categories_Index_JS =>
            return Self.Get_Resource_File
              ("templates/entities_categories_index.js.tmpl");
         when Tmpl_Source_File_HTML =>
            return Self.Get_Resource_File ("templates/source_file.html.tmpl");
         when Tmpl_Source_File_JS =>
            return Self.Get_Resource_File ("templates/source_file.js.tmpl");
         when Tmpl_Source_File_Index_JS =>
            return Self.Get_Resource_File
              ("templates/source_file_index.js.tmpl");
      end case;
   end Get_Template;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self    : in out HTML_Backend;
      Context : access constant Docgen_Context)
   is
      Source  : GNATCOLL.VFS.Virtual_File;
      Success : Boolean;

   begin
      GNATdoc.Backend.Base.Base_Backend (Self).Initialize (Context);

      --  Register user defined customization directory

      declare
         Project : constant GNATCOLL.Projects.Project_Type :=
           Self.Context.Kernel.Registry.Tree.Root_Project;
         Value   : constant String :=
           Project.Attribute_Value
             (Attribute_Pkg_String'(Build (Pkg_Name, HTML_Custom_Dir_Name)));

      begin
         if Value /= "" then
            Self.Resource_Dirs.Prepend
              (Project.Project_Path.Get_Parent.Create_From_Dir
                 (Filesystem_String (Value)));
         end if;
      end;

      --  Create directories and copy static resources.

      for Directory of reverse Self.Resource_Dirs loop
         Source := Directory.Create_From_Dir ("static");

         if Source.Is_Directory then
            Source.Copy
              (Get_Doc_Directory (Self.Context.Kernel).Full_Name, Success);

            if not Success then
               Trace
                 (Me,
                  "unable to copy static resources from "
                  & String (Source.Full_Name.all)
                  & " directory");
            end if;
         end if;
      end loop;
   end Initialize;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : GNATCOLL.JSON.JSON_Value;
      Right : GNATCOLL.JSON.JSON_Value) return Boolean
   is
      LL : constant String := Left.Get ("label");
      LQ : constant String := Left.Get ("qualifier");
      RL : constant String := Right.Get ("label");
      RQ : constant String := Right.Get ("qualifier");

   begin
      return LL < RL or (LL = RL and LQ < RQ);
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
     (Text    : Ada.Strings.Unbounded.Unbounded_String;
      Context : Docgen_Context) return GNATCOLL.JSON.JSON_Array is
   begin
      if not Context.Options.Disable_Markup then
         return
           To_JSON_Representation
             (Parse_Text (Split_Lines (To_String (Text))),
              Context.Kernel);

      else
         declare
            Stream     : GNATdoc.Markup_Streams.Event_Vectors.Vector;
            Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map;

         begin
            Attributes.Insert ("class", "preformatted");
            Stream.Append ((Start_Tag, To_Unbounded_String ("p"), Attributes));
            Stream.Append ((GNATdoc.Markup_Streams.Text, Text));
            Stream.Append ((End_Tag, To_Unbounded_String ("p")));

            return To_JSON_Representation (Stream, Context.Kernel);
         end;
      end if;
   end To_JSON_Representation;

end GNATdoc.Backend.HTML;
