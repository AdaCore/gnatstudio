------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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
      Name   : String) return Attribute_Page;
   --  Find the page assiciated with the given name.
   --  If this page doesn't exist yet, create it.

   function Find_Editor_Section_By_Name
     (Page               : Attribute_Page;
      Name               : String;
      Mutually_Exclusive : Boolean := False) return Attribute_Page_Section;
   --  Find the section associated with the given name.
   --  If this section doesn't exist yet, create it.

   function Find_Attribute_By_Name
     (Module             : access Base_Properties_Module;
      Section            : Attribute_Page_Section;
      Name, Pkg          : String;
      Indexed            : Boolean;
      Mutually_Exclusive : Boolean := False)
      return Attribute_Description_Access;
   --  Find the attribute associated with the given name.
   --  If this attribute doesn't exist yet, create it.

   procedure Free (Typ : in out Attribute_Type);
   --  Free the memory occupied by Typ

   procedure Free (Index : in out Indexed_Attribute_Type_List);
   --  Free the memory occupied by Index

   procedure Free (Attr : in out Attribute_Description_Access);
   --  Free the memory occupied by Attribute

   procedure Free (Section : in out Attribute_Page_Section);
   --  Free the memory occupied by Section

   procedure Free (Page : in out Attribute_Page);
   --  Free the memory occupied by Page

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Indexed_Attribute_Type_Array, Indexed_Attribute_Type_List);

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Section : not null access Attribute_Page_Section_Record'Class)
      return String
   is
     (if Section.Name /= null then Section.Name.all else "");

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Section : not null access Attribute_Page_Section_Record'Class)
      return String
   is
     (if not Section.Mutually_Exclusive or else Section.Description = null
      then
         ""
      else
         Section.Description.all);

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Page : not null access Attribute_Page_Record'Class) return String
   is
      (if Page.Name /= null then Page.Name.all else "");

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

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Attr : Attribute_Description) return String
   is
     (if Attr.Name /= null then Attr.Name.all else "");

   -------------
   -- Get_Pkg --
   -------------

   function Get_Pkg (Attr : Attribute_Description) return String
   is
      (if Attr.Pkg /= null then Attr.Pkg.all else "");

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name (Attr : Attribute_Description) return String
   is
      Pkg : constant String := Attr.Get_Pkg;
   begin
      if Pkg /= "" then
         return Pkg & "'" & Attr.Get_Name;
      else
         return Attr.Get_Name;
      end if;
   end Get_Full_Name;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Attr : Attribute_Description) return String
   is
     (if Attr.Label /= null then Attr.Label.all else "");

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Attr : Attribute_Description) return String
   is
     (if Attr.Description /= null then Attr.Description.all else "");

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

      procedure Parse_Mutually_Exclusive_Node (Parent : Node_Ptr);
      --  Used to parse a '<mutually_exclusive>' node

      function Parse_Project_Attribute_Node
        (Parent : Node_Ptr) return Attribute_Description_Access;
      procedure Parse_Project_Attribute_Node (Parent : Node_Ptr);
      --  Used to parse a '<project_attribute>' node

      -----------------------------------
      -- Parse_Mutually_Exclusive_Node --
      -----------------------------------

      procedure Parse_Mutually_Exclusive_Node (Parent : Node_Ptr) is
         Name      : constant String := Get_Attribute (Parent, "name");
         Desc      : constant String := Get_Attribute (Parent, "description");
         Page_Name : constant String := Get_Attribute (Parent, "editor_page");
         Page      : constant Attribute_Page :=
                       Find_Editor_Page_By_Name (Module, Page_Name);
         Section   : Attribute_Page_Section;
         Child     : Node_Ptr := Parent.Child;
         Attr      : Attribute_Description_Access;

         function Is_Child_Valid return Boolean;
         --  Check the validity of the '<mutually_exclusive>' children nodes.
         --  Display an error message if not valid.

         --------------------
         -- Is_Child_Valid --
         --------------------

         function Is_Child_Valid return Boolean is
         begin
            if Child.Tag.all /= "project_attribute" then
               Module.Kernel.Messages_Window.Insert
                 (-"<mutually_exclusive> Parent should only have "
                  & "<project_attribute> Parents as children",
                  Mode => Error);

               return False;
            elsif Get_Attribute (Child, "editor_page") /= Page_Name then
               Module.Kernel.Messages_Window.Insert
                 (-"<mutually_exclusive> children ""editor_page"" attribute "
                  & "should be equal to their parent's one",
                 Mode => Error);

               return False;
            end if;

            return True;
         end Is_Child_Valid;

      begin
         if Name = "" then
            Module.Kernel.Messages_Window.Insert
              (-"<mutually_exclusive> node must specify a ""name"" attribute",
               Mode => Error);
         end if;

         Section := Find_Editor_Section_By_Name
           (Page               => Page,
            Name               => Name,
            Mutually_Exclusive => True);

         --  Set the mutually exclusive section documentation
         if Desc /= "" then
            Section.Description := new String'(Desc);
         end if;

         while Child /= null and then Is_Child_Valid loop
            --  Parse the project attribute node and mark it as mutually
            --  exclusive.
            Attr := Parse_Project_Attribute_Node (Child);
            Attr.Mutually_Exclusive := True;
            Section.Attributes.Append (Attr);

            Child := Child.Next;
         end loop;

         --  If an error has occured during the parsing, delete the entire
         --  section from the page.
         if Child /= null then
            declare
               Section_Pos : Attribute_Page_Section_Lists.Cursor :=
                              Page.Sections.Find (Section);
            begin
               Page.Sections.Delete (Section_Pos);
            end;
         end if;
      end Parse_Mutually_Exclusive_Node;

      ----------------------------------
      -- Parse_Project_Attribute_Node --
      ----------------------------------

      function Parse_Project_Attribute_Node
        (Parent : Node_Ptr) return Attribute_Description_Access
      is
         Page      : constant Attribute_Page :=
                       Find_Editor_Page_By_Name
                         (Module,
                          Name => Get_Attribute (Parent, "editor_page"));
         Section   : constant Attribute_Page_Section :=
                       Find_Editor_Section_By_Name
                         (Page => Page,
                          Name => Get_Attribute (Parent, "editor_section"));
         Name      : String := Get_Attribute (Parent, "name");
         Pkg       : String := Get_Attribute (Parent, "package");
         Indexed   : constant Boolean := Parent.Child /= null
           and then (Parent.Child.Tag.all = "index"
                     or else Parent.Child.Tag.all = "specialized_index");
         Attribute : Attribute_Description_Access;
      begin
         To_Lower (Pkg);
         To_Lower (Name);

         if Name = "" then
            Module.Kernel.Messages_Window.Insert
              (-"<project_attribute> must specify a ""name"" attribute",
               Mode => Error);
         end if;

         Attribute := Find_Attribute_By_Name
           (Module, Section, Name, Pkg, Indexed);
         Parse_Attribute_Description
           (Module.Kernel, Parent, Attribute);

         return Attribute;
      end Parse_Project_Attribute_Node;

      ----------------------------------
      -- Parse_Project_Attribute_Node --
      ----------------------------------

      procedure Parse_Project_Attribute_Node (Parent : Node_Ptr) is
         Attr : constant Attribute_Description_Access :=
                  Parse_Project_Attribute_Node (Parent);
         pragma Unreferenced (Attr);
      begin
         null;
      end Parse_Project_Attribute_Node;

   begin
      if Node.Tag.all = "project_attribute" then
         Parse_Project_Attribute_Node (Node);
      elsif Node.Tag.all = "mutually_exclusive" then
         Parse_Mutually_Exclusive_Node (Node);
      end if;
   end Customize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Base_Properties_Module) is
   begin
      for Page of Module.Pages loop
         Free (Page);
      end loop;

      Module.Pages.Clear;
   end Destroy;

   ------------------------------
   -- Find_Editor_Page_By_Name --
   ------------------------------

   function Find_Editor_Page_By_Name (Module : access Base_Properties_Module;
                                      Name   : String) return Attribute_Page is
      New_Page : Attribute_Page;
   begin
      for Page of Module.Pages loop
         if Page.Name.all = Name then
            return Page;
         end if;
      end loop;

      New_Page := new Attribute_Page_Record'
        (Name     => new String'(Name),
         Sections => <>);
      Module.Pages.Append (New_Page);

      return New_Page;
   end Find_Editor_Page_By_Name;

   ---------------------------------
   -- Find_Editor_Section_By_Name --
   ---------------------------------

   function Find_Editor_Section_By_Name
     (Page               : Attribute_Page;
      Name               : String;
      Mutually_Exclusive : Boolean := False) return Attribute_Page_Section
   is
      New_Section : Attribute_Page_Section;
   begin
      for Section of Page.Sections loop
         if Section.Name.all = Name then
            return Section;
         end if;
      end loop;

      New_Section  := new Attribute_Page_Section_Record (Mutually_Exclusive);
      New_Section.Name := new String'(Name);

      Page.Sections.Append (New_Section);

      return New_Section;
   end Find_Editor_Section_By_Name;

   ----------------------------
   -- Find_Attribute_By_Name --
   ----------------------------

   function Find_Attribute_By_Name
     (Module             : access Base_Properties_Module;
      Section            : Attribute_Page_Section;
      Name, Pkg          : String;
      Indexed            : Boolean;
      Mutually_Exclusive : Boolean := False)
      return Attribute_Description_Access
   is

      Self     : Base_Properties_Module'Class renames
                   Base_Properties_Module'Class (Module.all);
      New_Attr : Attribute_Description_Access;
   begin
      for Attr of Section.Attributes loop
         if Attr.Name.all = Name and then Attr.Pkg.all = Pkg then
            return Attr;
         end if;
      end loop;

      New_Attr := Self.New_Attribute_Description (Indexed);
      New_Attr.Name := new String'(Name);
      New_Attr.Pkg  := new String'(Pkg);
      New_Attr.Mutually_Exclusive := Mutually_Exclusive;
      Section.Attributes.Append (New_Attr);

      return New_Attr;
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

   procedure Free (Attr : in out Attribute_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Description'Class, Attribute_Description_Access);
   begin
      if Attr /= null then
         Free (Attr.Name);
         Free (Attr.Pkg);
         Free (Attr.Description);
         Free (Attr.Label);
         Free (Attr.Hide_In);

         if Attr.Indexed then
            Free (Attr.Index_Attribute);
            Free (Attr.Index_Package);
            Free (Attr.Index_Types);
         else
            Free (Attr.Non_Index_Type);
         end if;

         Unchecked_Free (Attr);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Page : in out Attribute_Page) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Page_Record'Class, Attribute_Page);
   begin
      if Page /= null then
         Free (Page.Name);

         for Section of Page.Sections loop
            Free (Section);
         end loop;

         Page.Sections.Clear;
         Unchecked_Free (Page);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Section : in out Attribute_Page_Section) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Page_Section_Record'Class, Attribute_Page_Section);
   begin
      if Section /= null then
         Free (Section.Name);

         if Section.Mutually_Exclusive then
            Free (Section.Description);
         end if;

         for Attr of Section.Attributes loop
            Free (Attr);
         end loop;

         Section.Attributes.Clear;
         Unchecked_Free (Section);
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
      for Page of Module.Pages loop
         for Section of Page.Sections loop
            for Attr of Section.Attributes loop
               if Attr.Pkg.all = Pkg and then Attr.Name.all = Name then
                  return Attr;
               end if;
            end loop;
         end loop;
      end loop;

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
     (Project         : Project_Type;
      Attr            : access Attribute_Description'Class;
      Index           : String;
      Omit_If_Default : Boolean := False) return String
   is
      Default_Value : constant String :=
        (if Omit_If_Default and then Attr.Omit_If_Default
         then ""
         else Get_Default_Value (Attr, Index));

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
     (Kernel          : access Core_Kernel_Record'Class;
      Project         : Project_Type;
      Attr            : access Attribute_Description'Class;
      Index           : String := "";
      Omit_If_Default : Boolean := False) return String_List_Access
   is
      Attr_Type             : Attribute_Type;
      Lower_Attribute_Index : String := Index;

   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      --  Else lookup in the project or in the default values

      if Project = GNATCOLL.Projects.No_Project then
         if Omit_If_Default
           and then Attr.Omit_If_Default
         then
            return null;
         else
            return Get_Default_Value (Kernel, Attr, Index);
         end if;

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

      if Omit_If_Default
        and then Attr.Omit_If_Default
      then
         return null;
      end if;

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
     (Self : Base_Properties_Module) return Attribute_Page_Lists.List
   is
     (Self.Pages);

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
                               Get_Attribute (N, "omit_if_default", "false");
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
      Registered_Outside : constant Boolean := Attribute_Registered
        (Name => Attr.Name.all,
         Pkg  => Attr.Pkg.all);
   begin
      --  Register project attributes only if not already registered in
      --  GNATCOLL.

      if not Registered_Outside then
         declare
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
         end;
      end if;
   end Register_New_Attribute;

end GPS.Project_Properties;
