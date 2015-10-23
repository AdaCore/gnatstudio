------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;            use GNAT.Case_Util;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;

with GPS.Intl;                  use GPS.Intl;
with GPS.Messages_Windows;      use GPS.Messages_Windows;

package body GPS.Project_Properties is

   procedure Parse_Attribute_Description
     (Kernel : access Core_Kernel_Record'Class;
      N      : Node_Ptr;
      A      : Attribute_Description_Access);
   --  Parse the attribute description from an XML node

   procedure Parse_Attribute_Type
     (Kernel : access Core_Kernel_Record'Class;
      Child  : Node_Ptr;
      Name   : String;
      A      : in out Attribute_Type);
   --  Parse an attribute type from an XML node

   procedure Register_New_Attribute
     (Kernel : access Core_Kernel_Record'Class;
      Attr   : Attribute_Description_Access);
   --  Register a new attribute in the project parser

   function Find_Editor_Page_By_Name
     (Module : access Base_Properties_Module;
      Name   : String) return Positive;
   --  Find the index in Module.Pages of the page Name.
   --  If this page doesn't exist yet, it is created as appropriate

   function Find_Editor_Section_By_Name
     (Module : access Base_Properties_Module;
      Page : Natural;
      Name : String) return Natural;
   --  Find the index of a specific attribute section in the given page

   function Find_Attribute_By_Name
     (Module    : access Base_Properties_Module;
      Page      : Natural;
      Section   : Natural;
      Name, Pkg : String;
      Indexed   : Boolean) return Attribute_Description_Access;
   --  Find the index of a specific attribute in a section. The attribute is
   --  created if necessary.

   procedure Free (Typ : in out Attribute_Type);
   --  Free the memory occupied by Typ

   procedure Free (Index : in out Indexed_Attribute_Type_List);
   --  Free the memory occupied by Index

   procedure Free (Attribute : in out Attribute_Description'Class);
   --  Free the memory occupied by Attribute

   procedure Free (Section : in out Attribute_Page_Section);
   --  Free the memory occupied by Section

   procedure Free (Page : in out Attribute_Page);
   --  Free the memory occupied by Page

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Indexed_Attribute_Type_Array, Indexed_Attribute_Type_List);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Description'Class, Attribute_Description_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Description_Array, Attribute_Description_List);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Page_Section_Array, Attribute_Page_Section_List);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Page_Array, Attribute_Page_List);

   ----------------------
   -- Attribute_Exists --
   ----------------------

   function Attribute_Exists
     (Attr            : access Attribute_Description'Class;
      Project         : Project_Type;
      Attribute_Index : String := "") return Boolean
   is
      Lower_Attribute_Index : String := Attribute_Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      return Project.Has_Attribute
        (Attribute_Pkg_String'
           (Build (Package_Name   => Attr.Pkg.all,
                   Attribute_Name => Attr.Name.all)),
         Index => Lower_Attribute_Index);
   end Attribute_Exists;

   --------------------
   -- Attribute_Name --
   --------------------

   function Attribute_Name (Attr : Attribute_Description) return String is
   begin
      if Attr.Pkg.all /= "" then
         return Attr.Pkg.all & "'" & Attr.Name.all;
      else
         return Attr.Name.all;
      end if;
   end Attribute_Name;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Base_Properties_Module;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
   begin
      if Node.Tag.all = "project_attribute" then
         declare
            Editor_Page    : constant Natural :=
              Find_Editor_Page_By_Name
                (Module,
                 Get_Attribute
                   (Node, "editor_page",
                    Default => ""));
            Editor_Section : constant Natural :=
              Find_Editor_Section_By_Name
                (Module,
                 Editor_Page,
                 Get_Attribute (Node, "editor_section"));
            Name           : String := Get_Attribute (Node, "name");
            Pkg            : String := Get_Attribute (Node, "package");
            Indexed        : constant Boolean := Node.Child /= null
              and then (Node.Child.Tag.all = "index"
                        or else Node.Child.Tag.all = "specialized_index");
            Attribute      : Attribute_Description_Access;
         begin
            To_Lower (Pkg);
            To_Lower (Name);

            if Name = "" then
               Module.Kernel.Messages_Window.Insert
                 (-"<project_attribute> must specify a ""name"" attribute",
                  Mode => Error);
            end if;

            Attribute := Find_Attribute_By_Name
              (Module, Editor_Page, Editor_Section, Name, Pkg, Indexed);
            Parse_Attribute_Description
              (Module.Kernel, Node, Attribute);
         end;
      end if;
   end Customize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Base_Properties_Module) is
   begin
      if Module.Pages /= null then
         for P in Module.Pages'Range loop
            Free (Module.Pages (P));
         end loop;
         Unchecked_Free (Module.Pages);
      end if;
   end Destroy;

   ------------------------------
   -- Find_Editor_Page_By_Name --
   ------------------------------

   function Find_Editor_Page_By_Name (Module : access Base_Properties_Module;
                                      Name   : String) return Positive is
      Tmp : Attribute_Page_List;
   begin
      if Module.Pages /= null then
         for P in Module.Pages'Range loop
            if Module.Pages (P).Name.all = Name then
               return P;
            end if;
         end loop;

         Tmp := Module.Pages;
         Module.Pages :=
           new Attribute_Page_Array (1 .. Tmp'Length + 1);
         Module.Pages (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      else
         Module.Pages := new Attribute_Page_Array (1 .. 1);
      end if;

      Module.Pages (Module.Pages'Last) :=
        (Name     => new String'(Name),
         Sections => null);

      return Module.Pages'Last;
   end Find_Editor_Page_By_Name;

   ---------------------------------
   -- Find_Editor_Section_By_Name --
   ---------------------------------

   function Find_Editor_Section_By_Name
     (Module : access Base_Properties_Module;
      Page   : Natural;
      Name   : String) return Natural
   is
      P   : Attribute_Page renames Module.Pages (Page);
      Tmp : Attribute_Page_Section_List;
   begin
      if P.Sections /= null then
         for S in P.Sections'Range loop
            if P.Sections (S).Name.all = Name then
               return S;
            end if;
         end loop;

         Tmp := P.Sections;
         P.Sections := new Attribute_Page_Section_Array (1 .. Tmp'Length + 1);
         P.Sections (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);

      else
         P.Sections := new Attribute_Page_Section_Array (1 .. 1);
      end if;

      P.Sections (P.Sections'Last) :=
        (Name       => new String'(Name),
         Attributes => null);
      return P.Sections'Last;
   end Find_Editor_Section_By_Name;

   ----------------------------
   -- Find_Attribute_By_Name --
   ----------------------------

   function Find_Attribute_By_Name
     (Module    : access Base_Properties_Module;
      Page      : Natural;
      Section   : Natural;
      Name, Pkg : String;
      Indexed   : Boolean) return Attribute_Description_Access
   is
      S   : Attribute_Page_Section renames
        Module.Pages (Page).Sections (Section);
      Tmp  : Attribute_Description_List;
      Self : Base_Properties_Module'Class renames
        Base_Properties_Module'Class (Module.all);
   begin
      if S.Attributes /= null then
         for A in S.Attributes'Range loop
            if S.Attributes (A).Name.all = Name
              and then S.Attributes (A).Pkg.all = Pkg
            then
               return S.Attributes (A);
            end if;
         end loop;

         Tmp := S.Attributes;
         S.Attributes := new Attribute_Description_Array (1 .. Tmp'Length + 1);
         S.Attributes (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      else
         S.Attributes := new Attribute_Description_Array (1 .. 1);
      end if;

      S.Attributes (S.Attributes'Last) :=
        Self.New_Attribute_Description (Indexed);
      S.Attributes (S.Attributes'Last).Name := new String'(Name);
      S.Attributes (S.Attributes'Last).Pkg  := new String'(Pkg);

      return S.Attributes (S.Attributes'Last);
   end Find_Attribute_By_Name;

   ---------------------------
   -- For_Each_Item_In_List --
   ---------------------------

   procedure For_Each_Item_In_List
     (Kernel   : access Core_Kernel_Record'Class;
      Attr     : Attribute_Type;
      Callback : List_Attribute_Callback)
   is
      Errors       : aliased Boolean;
      Script       : Scripting_Language;
      Start, Index : Natural;
   begin
      if Attr.Typ = Attribute_As_Dynamic_List then
         Script := Lookup_Scripting_Language
           (Kernel.Scripts, Attr.Dynamic_List_Lang.all);

         if Script = null then
            Kernel.Messages_Window.Insert
              (-"Unknown scripting language "
               & Attr.Dynamic_List_Lang.all
               & " used when defining a project attribute",
               Mode => Error);
            return;
         end if;

         declare
            List : constant String := GNATCOLL.Scripts.Execute_Command
              (Script,
               CL          => Parse_String
                 (Attr.Dynamic_List_Cmd.all, Command_Line_Treatment (Script)),
               Hide_Output => True,
               Errors      => Errors'Access);
         begin
            if Errors then
               Kernel.Messages_Window.Insert
                 (-"Couldn't execute the command """
                  & Attr.Dynamic_List_Cmd.all & """ when computing the"
                  & " valid values for a project attribute",
                  Mode => Error);
            end if;

            Start := List'First;

            while Start <= List'Last loop
               Index := Start + 1;

               while Index <= List'Last and then List (Index) /= ASCII.LF loop
                  Index := Index + 1;
               end loop;

               Callback (List (Start .. Index - 1),
                         List (Start .. Index - 1) = Attr.Dynamic_Default.all);

               Start := Index + 1;
            end loop;
         end;

      elsif Attr.Typ = Attribute_As_Static_List then
         if Attr.Static_List /= null then
            for V in Attr.Static_List'Range loop
               Callback (Attr.Static_List (V).all,
                         Attr.Static_Default (V));
            end loop;
         end if;
      end if;
   end For_Each_Item_In_List;

   ----------
   -- Free --
   ----------

   procedure Free (Attribute : in out Attribute_Description'Class) is
   begin
      Free (Attribute.Name);
      Free (Attribute.Pkg);
      Free (Attribute.Description);
      Free (Attribute.Label);
      Free (Attribute.Hide_In);
      Free (Attribute.Disable);
      if Attribute.Indexed then
         Free (Attribute.Index_Attribute);
         Free (Attribute.Index_Package);
         Free (Attribute.Index_Types);
      else
         Free (Attribute.Non_Index_Type);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Page : in out Attribute_Page) is
   begin
      Free (Page.Name);
      if Page.Sections /= null then
         for S in Page.Sections'Range loop
            Free (Page.Sections (S));
         end loop;
         Unchecked_Free (Page.Sections);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Section : in out Attribute_Page_Section) is
   begin
      Free (Section.Name);
      if Section.Attributes /= null then
         for A in Section.Attributes'Range loop
            Free (Section.Attributes (A).all);
            Unchecked_Free (Section.Attributes (A));
         end loop;
         Unchecked_Free (Section.Attributes);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Typ : in out Attribute_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Boolean_Array, Boolean_List);
   begin
      case Typ.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory =>
            Free (Typ.Default);
         when Attribute_As_Static_List =>
            Free (Typ.Static_List);
            Unchecked_Free (Typ.Static_Default);
         when Attribute_As_Dynamic_List =>
            Free (Typ.Dynamic_List_Lang);
            Free (Typ.Dynamic_List_Cmd);
            Free (Typ.Dynamic_Default);
      end case;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Index : in out Indexed_Attribute_Type_List) is
   begin
      if Index /= null then
         for J in Index'Range loop
            Free (Index (J).Index_Value);
            Free (Index (J).Typ);
         end loop;
         Unchecked_Free (Index);
      end if;
   end Free;

   -----------------------------------------
   -- Get_Attribute_Type_From_Description --
   -----------------------------------------

   function Get_Attribute_Type_From_Description
     (Attr : access Attribute_Description'Class; Index : String)
      return Attribute_Type
   is
      Pos : Integer := -1;
   begin
      if Attr.Indexed then
         if Attr.Index_Types /= null then
            for T in Attr.Index_Types'Range loop
               if Attr.Index_Types (T).Index_Value = null then
                  if Pos = -1 then
                     Pos := T;
                  end if;
               elsif Equal
                 (Attr.Index_Types (T).Index_Value.all, Index,
                  Case_Sensitive => Attr.Case_Sensitive_Index)
               then
                  Pos := T;
                  exit;
               end if;
            end loop;
         end if;

         if Pos = -1 then
            return Attr.Non_Index_Type;
         else
            return Attr.Index_Types (Pos).Typ;
         end if;
      else
         return Attr.Non_Index_Type;
      end if;
   end Get_Attribute_Type_From_Description;

   ----------------------------------
   -- Get_Attribute_Type_From_Name --
   ----------------------------------

   function Get_Attribute_Type_From_Name
     (Module : access Base_Properties_Module;
      Pkg    : String;
      Name   : String) return Attribute_Description_Access is
   begin
      if Module.Pages /= null then
         for P in Module.Pages'Range loop
            for S in Module.Pages (P).Sections'Range loop
               declare
                  Sect : Attribute_Page_Section renames
                    Module.Pages (P).Sections (S);
               begin
                  for A in Sect.Attributes'Range loop
                     if Sect.Attributes (A).Pkg.all = Pkg
                       and then Sect.Attributes (A).Name.all = Name
                     then
                        return Sect.Attributes (A);
                     end if;
                  end loop;
               end;
            end loop;
         end loop;
      end if;

      return null;
   end Get_Attribute_Type_From_Name;

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
     (Attr          : access Attribute_Description'Class;
      Index         : String) return String
   is
      Typ : constant Attribute_Type :=
        Get_Attribute_Type_From_Description (Attr, Index);
      Result : GNAT.Strings.String_Access;
   begin
      case Typ.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory =>
            Result := Typ.Default;

         when Attribute_As_Static_List =>
            for S in Typ.Static_Default'Range loop
               if Typ.Static_Default (S) then
                  Result := Typ.Static_List (S);
                  exit;
               end if;
            end loop;

         when Attribute_As_Dynamic_List =>
            Result := Typ.Dynamic_Default;
      end case;

      if Result = null then
         return "";
      else
         return Result.all;
      end if;
   end Get_Default_Value;

   ----------------------------
   -- Get_Value_From_Project --
   ----------------------------

   function Get_Value_From_Project
     (Project       : Project_Type;
      Attr          : access Attribute_Description'Class;
      Index         : String) return String
   is
      Default_Value : constant String := Get_Default_Value (Attr, Index);

      Lower_Attribute_Index : String := Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      return Project.Attribute_Value
        (Attribute => Attribute_Pkg_String'
           (Build (Package_Name   => Attr.Pkg.all,
                   Attribute_Name => Attr.Name.all)),
         Default   => Default_Value,
         Index     => Lower_Attribute_Index);
   end Get_Value_From_Project;

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
     (Kernel        : access Core_Kernel_Record'Class;
      Attr          : access Attribute_Description'Class;
      Index         : String := "") return String_List_Access
   is
      Result : String_List_Access;

      procedure Save_Value (Value : String; Is_Default : Boolean);
      --  Store the list of active values for Attr

      ----------------
      -- Save_Value --
      ----------------

      procedure Save_Value (Value : String; Is_Default : Boolean) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (GNAT.Strings.String_List, String_List_Access);
         Tmp : String_List_Access := Result;
      begin
         if Is_Default then
            if Result /= null then
               Result := new GNAT.Strings.String_List (1 .. Tmp'Length + 1);
               Result (Tmp'Range) := Tmp.all;
               Unchecked_Free (Tmp);

            else
               Result := new GNAT.Strings.String_List (1 .. 1);
            end if;

            Result (Result'Last) := new String'(Value);
         end if;
      end Save_Value;

      Attr_Type : constant Attribute_Type :=
        Get_Attribute_Type_From_Description (Attr, Index);
   begin
      case Attr_Type.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory =>

            if Attr_Type.Default.all = "" then
               return null;

            elsif Attr_Type.Default.all = "project source files" then
               return null;
            else
               --  Workaround fatal crash in GNAT
               declare
                  V : constant String_List_Access :=
                    new GNAT.Strings.String_List (1 .. 1);
               begin
                  V (1) := new String'(Attr_Type.Default.all);
                  return V;
               end;
            end if;

         when Attribute_As_Static_List
            | Attribute_As_Dynamic_List =>
            For_Each_Item_In_List
              (Kernel, Attr_Type, Save_Value'Unrestricted_Access);

            return Result;
      end case;
   end Get_Default_Value;

   ----------------------------
   -- Get_Value_From_Project --
   ----------------------------

   function Get_Value_From_Project
     (Kernel        : access Core_Kernel_Record'Class;
      Project       : Project_Type;
      Attr          : access Attribute_Description'Class;
      Index         : String := "") return String_List_Access
   is
      Attr_Type             : Attribute_Type;
      Lower_Attribute_Index : String := Index;

   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      --  Else lookup in the project or in the default values

      if Project = GNATCOLL.Projects.No_Project then
         return Get_Default_Value (Kernel, Attr, Index);
      else
         if Attr.Pkg.all = "" and then Attr.Name.all = "languages" then
            return new GNAT.Strings.String_List'
              (Project.Languages (Recursive => False));
         end if;

         declare
            Current : String_List_Access := Project.Attribute_Value
              (Attribute => Build (Package_Name   => Attr.Pkg.all,
                                   Attribute_Name => Attr.Name.all),
               Index     => Lower_Attribute_Index);
         begin
            if Current /= null and then Current'Length /= 0 then
               return Current;
            end if;
            Free (Current);
         end;
      end if;

      --  Else get the default value

      Attr_Type := Get_Attribute_Type_From_Description
        (Attr, Lower_Attribute_Index);

      case Attr_Type.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory =>

            if Attr_Type.Default.all = "project source files" then
               declare
                  Files  : File_Array_Access :=
                    Project.Source_Files (Recursive => False);
                  Result : constant String_List_Access :=
                    new GNAT.Strings.String_List (Files'Range);
               begin
                  for R in Result'Range loop
                     Result (R) :=
                       new String'(String (Full_Name (Files (R)).all));
                  end loop;
                  Unchecked_Free (Files);

                  return Result;
               end;
            end if;

         when Attribute_As_Static_List
            | Attribute_As_Dynamic_List =>

            null;
      end case;

      return Get_Default_Value (Kernel, Attr, Index);
   end Get_Value_From_Project;

   -------------------
   -- Is_Any_String --
   -------------------

   function Is_Any_String
     (Attr  : access Attribute_Description'Class;
      Index : String) return Boolean
   is
      Typ : constant Attribute_Type :=
              Get_Attribute_Type_From_Description (Attr, Index);
   begin
      case Typ.Typ is
         when Attribute_As_String    => return True;
         when Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory => return False;
         when Attribute_As_Static_List =>
            return Typ.Static_List = null
              and then Typ.Static_Allows_Any_String;
         when Attribute_As_Dynamic_List =>
            return False;
      end case;
   end Is_Any_String;

   -------------------------------
   -- New_Attribute_Description --
   -------------------------------

   function New_Attribute_Description
     (Module  : access Base_Properties_Module;
      Indexed : Boolean)
      return Attribute_Description_Access
   is
      pragma Unreferenced (Module);
   begin
      return new Attribute_Description (Indexed);
   end New_Attribute_Description;

   -----------
   -- Pages --
   -----------

   function Pages
     (Self : Base_Properties_Module)
      return Attribute_Page_List is
   begin
      return Self.Pages;
   end Pages;

   ---------------------------------
   -- Parse_Attribute_Description --
   ---------------------------------

   procedure Parse_Attribute_Description
     (Kernel : access Core_Kernel_Record'Class;
      N      : Node_Ptr;
      A      : Attribute_Description_Access)
   is
      Descr                : constant String :=
                               Get_Attribute (N, "description");
      Label                : constant String :=
                               Get_Attribute (N, "label", A.Name.all);
      Is_List              : constant String :=
                               Get_Attribute (N, "list", "false");
      Ordered              : constant String :=
                               Get_Attribute (N, "ordered", "false");
      Case_Sensitive_Index : constant String :=
                               Get_Attribute
                                 (N, "case_sensitive_index", "false");
      Omit                 : constant String :=
                               Get_Attribute (N, "omit_if_default", "true");
      Base                 : constant String :=
                               Get_Attribute (N, "base_name_only", "false");
      Indexed              : constant Boolean :=
                               N.Child /= null
                                   and then (N.Child.Tag.all = "index"
                                             or else N.Child.Tag.all =
                                               "specialized_index");
      Hide_In              : constant String := Get_Attribute (N, "hide_in");
      Disable_If_Not_Set   : constant String :=
                               Get_Attribute
                                 (N, "disable_if_not_set", "false");
      Disable              : constant String := Get_Attribute (N, "disable");
      Child                : Node_Ptr;

      procedure Parse_Indexed_Type (Value : String);
      --  Parse the type definition for an <index> or <specialized_index> node

      ------------------------
      -- Parse_Indexed_Type --
      ------------------------

      procedure Parse_Indexed_Type (Value : String) is
         Found : Boolean := False;
         Tmp   : Indexed_Attribute_Type_List;
      begin
         if A.Index_Types /= null then
            for T in A.Index_Types'Range loop
               if Value = "" and then A.Index_Types (T).Index_Value = null then
                  Kernel.Messages_Window.Insert
                    (-("General indexed type already defined for"
                     & " attribute ") & A.Name.all,
                     Mode => Error);
                  Found := True;
                  exit;

               elsif Value /= ""
                 and then A.Index_Types (T).Index_Value /= null
                 and then A.Index_Types (T).Index_Value.all = Value
               then
                  Kernel.Messages_Window.Insert
                    (-"Attribute type already defined for attribute"
                     & A.Name.all & (-" indexed by") & '"' & Value & '"',
                     Mode => Error);
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Tmp := A.Index_Types;
               A.Index_Types := new Indexed_Attribute_Type_Array
                 (1 .. Tmp'Length + 1);
               A.Index_Types (1 .. Tmp'Length) := Tmp.all;
               Unchecked_Free (Tmp);
            end if;
         else
            A.Index_Types := new Indexed_Attribute_Type_Array (1 .. 1);
         end if;

         if Value /= "" then
            A.Index_Types (A.Index_Types'Last).Index_Value :=
              new String'(Value);
         end if;

         Parse_Attribute_Type
           (Kernel, Child.Child, A.Name.all,
            A.Index_Types (A.Index_Types'Last).Typ);
      end Parse_Indexed_Type;

   begin
      if Descr /= "" and then A.Description = null then
         A.Description := new String'(Descr);
      end if;

      if Label /= "" and then A.Label = null then
         A.Label := new String'(Label);
      end if;

      A.Hide_In := new String'(Hide_In);
      A.Is_List := Is_List = "true" or else Is_List = "1";
      A.Ordered_List := Ordered = "true" or else Ordered = "1";
      A.Omit_If_Default := Omit = "true" or else Omit = "1";
      A.Base_Name_Only := Base = "true" or else Omit = "1";

      if Case_Sensitive_Index = "file" then
         A.Case_Sensitive_Index :=
           GNATCOLL.VFS_Utils.Is_Case_Sensitive (Local_Host);
      else
         A.Case_Sensitive_Index :=
           Case_Sensitive_Index = "true" or else Case_Sensitive_Index = "1";
      end if;

      A.Disable_If_Not_Set := Disable_If_Not_Set = "true"
        or else Disable_If_Not_Set = "1";
      A.Disable := new String'(Disable);

      if Indexed then
         Child := N.Child;
         while Child /= null loop
            if Child.Tag.all = "index" then
               A.Index_Attribute := new String'
                 (Get_Attribute (Child, "attribute"));
               To_Lower (A.Index_Attribute.all);
               A.Index_Package := new String'
                 (Get_Attribute (Child, "package"));
               To_Lower (A.Index_Package.all);
               Parse_Indexed_Type ("");

            elsif Child.Tag.all = "specialized_index" then
               Parse_Indexed_Type (Get_Attribute (Child, "value"));
            end if;

            Child := Child.Next;
         end loop;

      else
         Parse_Attribute_Type (Kernel, N.Child, A.Name.all, A.Non_Index_Type);
      end if;

      Register_New_Attribute (Kernel => Kernel, Attr => A);
   end Parse_Attribute_Description;

   --------------------------
   -- Parse_Attribute_Type --
   --------------------------

   procedure Parse_Attribute_Type
     (Kernel : access Core_Kernel_Record'Class;
      Child  : Node_Ptr;
      Name   : String;
      A      : in out Attribute_Type)
   is
      Child2       : Node_Ptr;
      Choice_Count : Natural;
   begin
      if Child = null then
         A := (Typ          => Attribute_As_String,
               Filter       => Filter_None,
               Allow_Empty  => True,
               Default      => null);

      elsif Child.Tag.all = "string" then
         declare
            Typ         : constant String :=
              Get_Attribute (Child, "type");
            Default     : constant String :=
              Get_Attribute (Child, "default");
            Allow_Empty : constant Boolean :=
              Boolean'Value
                (Get_Attribute (Child, "allow_empty", "True"));
            Filter_Attr : constant String :=
              Get_Attribute (Child, "filter", "none");
            Filter      : File_Filter := Filter_None;

         begin
            if Filter_Attr = "project" then
               Filter := Filter_From_Project;

            elsif Filter_Attr = "extended_project" then
               Filter := Filter_From_Extended;

            elsif Filter_Attr = "all_projects" then
               Filter := Filter_From_All_Projects;
            end if;

            if Typ = "file" then
               A := (Typ          => Attribute_As_Filename,
                     Filter       => Filter,
                     Allow_Empty  => Allow_Empty,
                     Default      => new String'(Default));
            elsif Typ = "directory" then
               A := (Typ          => Attribute_As_Directory,
                     Filter       => Filter,
                     Allow_Empty  => Allow_Empty,
                     Default      => new String'(Default));
            elsif Typ = "unit" then
               A := (Typ          => Attribute_As_Unit,
                     Filter       => Filter,
                     Allow_Empty  => Allow_Empty,
                     Default      => new String'(Default));
            else
               if Typ /= "" then
                  Kernel.Messages_Window.Insert
                    (-("Invalid value for ""type"" attribute"
                     & " for a <string> node"),
                     Mode => Error);
               end if;

               A := (Typ         => Attribute_As_String,
                     Filter      => Filter,
                     Allow_Empty => Allow_Empty,
                     Default     => new String'(Default));
            end if;
         end;

         if Child.Next /= null then
            Kernel.Messages_Window.Insert
              (-("<string> node must always appear only once,"
               & " and after all other type descriptions"),
               Mode => Error);
         end if;

      elsif Child.Tag.all = "choice" then
         Choice_Count := 1;
         Child2 := Child.Next;

         while Child2 /= null and then Child2.Tag.all = "choice" loop
            Choice_Count := Choice_Count + 1;
            Child2 := Child2.Next;
         end loop;

         if Child2 /= null
           and then (Child2.Tag.all /= "string"
                     or else Child2.Next /= null)
         then
            Kernel.Messages_Window.Insert
              (-("Only <string> can be specified in addition"
               & " to <choice> for the type of attributes"),
               Mode => Error);
         end if;

         A :=
           (Typ                      => Attribute_As_Static_List,
            Static_Allows_Any_String => Child2 /= null,
            Static_List              =>
            new GNAT.Strings.String_List (1 .. Choice_Count),
            Static_Default           => new Boolean_Array (1 .. Choice_Count));

         Child2 := Child;
         Choice_Count := 1;

         while Child2 /= null
           and then Child2.Tag.all = "choice"
         loop
            A.Static_List (Choice_Count) := new String'(Child2.Value.all);
            A.Static_Default (Choice_Count) :=
              Get_Attribute (Child2, "default") = "true"
              or else Get_Attribute (Child2, "default") = "1";
            Choice_Count := Choice_Count + 1;
            Child2 := Child2.Next;
         end loop;

      elsif Child.Tag.all = "shell" then
         A :=
           (Typ                       => Attribute_As_Dynamic_List,
            Dynamic_Allows_Any_String => Child.Next /= null
            and then Child.Next.Tag.all = "string",
            Dynamic_Default           =>
            new String'(Get_Attribute (Child, "default")),
            Dynamic_List_Lang         => new String'
              (Get_Attribute (Child, "lang", "shell")),
            Dynamic_List_Cmd          => new String'(Child.Value.all));

         if Child.Next /= null
           and then (Child.Next.Tag.all /= "string"
                     or else Child.Next.Next /= null)
         then
            Kernel.Messages_Window.Insert
              (-("Only <string> can be specified in addition"
               & " to <shell> for the type of attributes"),
               Mode => Error);
         end if;

      else
         Kernel.Messages_Window.Insert
           (-"Invalid type description for the attribute " & Name,
            Mode => Error);
      end if;
   end Parse_Attribute_Type;

   ----------------------------
   -- Register_New_Attribute --
   ----------------------------

   procedure Register_New_Attribute
     (Kernel : access Core_Kernel_Record'Class;
      Attr   : Attribute_Description_Access)
   is
      Msg : constant String := Register_New_Attribute
        (Name    => Attr.Name.all,
         Pkg     => Attr.Pkg.all,
         Is_List => Attr.Is_List,
         Indexed => Attr.Indexed,
         Case_Sensitive_Index => Attr.Case_Sensitive_Index);
   begin
      if Msg /= "" then
         Kernel.Messages_Window.Insert (Msg, Mode => Error);
      end if;
   end Register_New_Attribute;

end GPS.Project_Properties;
