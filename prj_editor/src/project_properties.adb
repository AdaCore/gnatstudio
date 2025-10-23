------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2025, AdaCore                     --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                  use GNAT.OS_Lib;

with GNATCOLL.Scripts;             use GNATCOLL.Scripts;

with Gtk.Toggle_Button;            use Gtk.Toggle_Button;

with Dialog_Utils;                 use Dialog_Utils;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Kernel;                   use GPS.Kernel;
with GPS.Core_Kernels;             use GPS.Core_Kernels;
with GPS.Project_Properties;       use GPS.Project_Properties;
with Projects;                     use Projects;

package body Project_Properties is

   type Attribute_Editor_Record;
   type Attribute_Editor is access all Attribute_Editor_Record'Class;

   type Editable_Attribute_Description (Indexed : Boolean := False) is
     new Attribute_Description (Indexed) with
   record
      Editor : Attribute_Editor;
      --  The attribute editor widget that allows the user to change the
      --  value of an attribute.

      Active_Toggle_Button : Gtk_Toggle_Button;
      --  Set if the editor is not always activated, and this indicates the
      --  state of the editor in this case.
   end record;

   type Editable_Attribute_Description_Access is
     access all Editable_Attribute_Description;

   -----------------------
   -- Properties module --
   -----------------------

   type Properties_Module_ID_Record is new Base_Properties_Module
     with null record;

   overriding function New_Attribute_Description
     (Module  : access Properties_Module_ID_Record;
      Indexed : Boolean)
      return Attribute_Description_Access;

   type Properties_Module_ID_Access
     is access all Properties_Module_ID_Record'Class;
   Properties_Module_ID : Properties_Module_ID_Access;

   -----------------------
   -- Attribute editors --
   -----------------------

   type Attribute_Editor_Record is abstract new Root_Attribute_Editor_Record
   with record
      Kernel    : Kernel_Handle;
      Project   : Project_Type;
      Attribute : Editable_Attribute_Description_Access;
      --  Description of the attribute

      Group_Widget : Dialog_Group_Widget;
      --  Set if the editor is within a group.
      --  This is the case if a section has been specified for the editor's
      --  attribute. It is also the case for indexed attributes or if the
      --  editor is displayed as a list.

      Doc_Group_Widget : Dialog_Group_Widget;
      --  The documentation widget that can be associated with the editor but
      --  that not resides in the same group widget.
      --  This is the case for list and indexed attributes.
   end record;

   ---------------------------------
   -- Attribute editors (indexed) --
   ---------------------------------

   function Get_Current_Value
     (Project         : Project_Type;
      Attr            : Editable_Attribute_Description_Access;
      Index           : String;
      Omit_If_Default : Boolean := False) return String;
   function Get_Current_Value
     (Kernel          : access Core_Kernel_Record'Class;
      Project         : Project_Type;
      Attr            : Editable_Attribute_Description_Access;
      Index           : String := "";
      Omit_If_Default : Boolean := False) return String_List_Access;
   --  Get the current value for the given attribute. This value is extracted
   --  from one of three sources, in that order:
   --    - Either the current editor for that attribute. This reflects the
   --      changes that the user is currently doing
   --    - The value in the current project, if such project exists. This
   --      reflects the value this attribute had before the editor was started
   --    - The default value as specified in the attribute definition
   --  Do not return default value if Omit_If_Default is True

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String)
      return Editable_Attribute_Description_Access;
   --  Find the description of an attribute given its package and name

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Editor : access Root_Attribute_Editor_Record) return String
   is
      pragma Unreferenced (Editor);
   begin
      return "";
   end Is_Valid;

   ------------------------------------
   -- Create_Project_Command_Handler --
   ------------------------------------

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel        : constant Kernel_Handle := Get_Kernel (Data);
      Attribute_Cst : aliased constant String := "attribute";
      Package_Cst   : aliased constant String := "package";
      Index_Cst     : aliased constant String := "index";
      Tool_Cst      : aliased constant String := "tool";
      Value_Cst     : aliased constant String := "value";
      Recursive_Cst : aliased constant String := "recursive";
      Get_Attributes_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access);
      Tool_Parameters : constant Cst_Argument_List :=
        (1 => Tool_Cst'Unchecked_Access);
      Set_Attribute_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Add_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Remove_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Clear_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);

      procedure Set_Return_Attribute
        (Project           : Project_Type;
         Attr, Pkg, Index  : String;
         Attribute_Is_List : Boolean;
         As_List           : Boolean);
      --  Store in Data the value of a specific attribute.
      --  Attribute_Is_List indicates the type of the attribute. This is the
      --  first type that will be tested, although if no match is found the
      --  other type will also be tested.
      --  As_List indicates the format of the returned value

      procedure Set_Return_Attribute (List : Argument_List; As_List : Boolean);
      --  Sets the contents of List into the return value

      procedure Set_Return_Attribute
        (Value : String; As_List : Boolean);
      --  Sets the contents of Value into the return value

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (List : Argument_List; As_List : Boolean)
      is
         Result : Unbounded_String;
      begin
         if As_List then
            Set_Return_Value_As_List (Data);
            for L in List'Range loop
               Set_Return_Value (Data, List (L).all);
            end loop;

         else
            for L in List'Range loop
               Append (Result, List (L).all);

               if L /= List'Last then
                  Append (Result, " ");
               end if;
            end loop;
            Set_Return_Value (Data, To_String (Result));
         end if;
      end Set_Return_Attribute;

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (Value : String; As_List : Boolean) is
      begin
         if As_List then
            Set_Return_Value_As_List (Data);
         end if;
         Set_Return_Value (Data, Value);
      end Set_Return_Attribute;

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (Project           : Project_Type;
         Attr, Pkg, Index  : String;
         Attribute_Is_List : Boolean;
         As_List           : Boolean)
      is
         Descr  : constant Editable_Attribute_Description_Access :=
           Get_Attribute_Type_From_Name (Pkg, Attr);
      begin
         if Descr = null then
            --  Test whether the attribute is known anyway. Not all attributes
            --  are declared in projects.xml, in particular the predefined ones
            --  related to switches and naming, that have their own editor

            if Attribute_Is_List then
               declare
                  List : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index,
                     Use_Extended => True);
                  Var  : constant String := Project.Attribute_Value
                    (Attribute_Pkg_String'(Build (Pkg, Attr)),
                     Default => "", Index => Index,
                     Use_Extended => True);
               begin
                  if List = null
                    and then Var /= ""
                  then
                     --  Did we have a string attribute in fact ?
                     Set_Return_Attribute (Var, As_List);

                  else
                     if List = null then
                        List := new GNAT.Strings.String_List'(1 .. 0 => null);
                     end if;

                     Set_Return_Attribute (List.all, As_List);
                  end if;

                  Free (List);
               end;

            else
               declare
                  Val : constant String := Project.Attribute_Value
                    (Attribute_Pkg_String'(Build (Pkg, Attr)),
                     Default => "", Index => Index,
                     Use_Extended => True);
               begin
                  if Val = "" then
                     --  Did we have a list attribute in fact ?
                     declare
                        List : String_List_Access := Project.Attribute_Value
                          (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
                     begin
                        if List /= null then
                           Set_Return_Attribute (List.all, As_List);
                        else
                           Set_Return_Attribute (Val, As_List);
                        end if;
                        Free (List);
                     end;

                  else
                     Set_Return_Attribute (Val, As_List);
                  end if;
               end;
            end if;
            return;
         end if;

         --  Else use the description from projects.xml, which also provides
         --  the default value for attributes not declared in the project
         if Descr.Is_List then
            declare
               List : String_List_Access := Get_Current_Value
                 (Kernel, Project, Descr, Index, True);
            begin
               if List /= null then
                  Set_Return_Attribute (List.all, As_List);
               else
                  Set_Return_Attribute
                    (Argument_List'(1 .. 0 => null), As_List);
               end if;
               Free (List);
            end;
         else
            Set_Return_Attribute
              (Get_Current_Value (Project, Descr, Index, True), As_List);
         end if;
      end Set_Return_Attribute;

   begin
      if Command = "get_attribute_as_list"
        or else Command = "get_attribute_as_string"
      then
         Name_Parameters (Data, Get_Attributes_Parameters);
         Set_Return_Attribute
           (Project => Get_Data (Data, 1),
            Attr    => To_Lower (Nth_Arg (Data, 2)),
            Pkg     => To_Lower (Nth_Arg (Data, 3, "")),
            Index   => Nth_Arg (Data, 4, ""),
            Attribute_Is_List => Command = "get_attribute_as_list",
            As_List           => Command = "get_attribute_as_list");

      elsif Command = "get_tool_switches_as_list"
        or else Command = "get_tool_switches_as_string"
      then
         Name_Parameters (Data, Tool_Parameters);
         declare
            Tool  : constant String := Nth_Arg (Data, 2);
            Props : constant Tool_Properties :=
              Get_Tool_Properties (Kernel, Tool);
         begin
            if Props = null then
               Set_Error_Msg (Data, -"No such tool: " & Tool);

            else
               Set_Return_Attribute
                 (Project => Get_Data (Data, 1),
                  Attr    => To_Lower (To_String (Props.Project_Attribute)),
                  Pkg     => To_Lower (To_String (Props.Project_Package)),
                  Index   => To_String (Props.Project_Index),
                  Attribute_Is_List => True,
                  As_List => Command = "get_tool_switches_as_list");
            end if;
         end;

      elsif Command = "is_modified" then
         Name_Parameters (Data, (  --  1 => Self,
                                 2 => Recursive_Cst'Unchecked_Access));
         declare
            Project   : constant Project_Type := Get_Data (Data, 1);
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
         begin
            Set_Return_Value (Data, Project.Modified (Recursive));
         end;

      elsif Command = "get_extended_project" then
         declare
            Project : constant Project_Type := Get_Data (Data, 1);
            Ext     : Project_Type;
         begin
            Ext := Project.Extended_Project;
            if Ext /= No_Project then
               Set_Return_Value
                 (Data, Create_Project (Get_Script (Data), Ext));
            end if;
         end;

      elsif Command = "set_attribute_as_string" then
         Name_Parameters (Data, Set_Attribute_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Value          : constant String := Nth_Arg (Data, 5);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Project.Set_Attribute
                 (Attribute => Build (Package_Name, Attribute_Name),
                  Value     => Value,
                  Index     => Index);
            end if;
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "add_attribute_values" then
         Name_Parameters (Data, Add_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Values         : GNAT.OS_Lib.Argument_List
              (1 .. Number_Of_Arguments (Data) - 4);
            Attribute      : constant Attribute_Pkg_List :=
              Build (Package_Name, Attribute_Name);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for J in 5 .. Number_Of_Arguments (Data) loop
                  Values (J - 4) := new String'(Nth_Arg (Data, J));
               end loop;

               if Project.Has_Attribute (Attribute, Index) then
                  Project.Set_Attribute
                    (Attribute => Attribute,
                     Values    => Values,
                     Index     => Index,
                     Prepend   => True);
               else
                  Project.Set_Attribute
                    (Attribute => Attribute,
                     Values    => Values,
                     Index     => Index,
                     Prepend   => False);
               end if;

               for J in Values'Range loop
                  Free (Values (J));
               end loop;
            end if;
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "remove_attribute_values" then
         Name_Parameters (Data, Remove_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Values         : GNAT.OS_Lib.Argument_List
              (1 .. Number_Of_Arguments (Data) - 4);
            Attribute      : constant Attribute_Pkg_List :=
              Build (Package_Name, Attribute_Name);
            List           : String_List_Access := Project.Attribute_Value
              (Attribute, Index);
            Found          : Boolean := False;
            First_Added    : Boolean := False;
         begin
            if not Is_Editable (Project) or else List = null then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for J in 5 .. Number_Of_Arguments (Data) loop
                  Values (J - 4) := new String'(Nth_Arg (Data, J));
               end loop;

               Project.Delete_Attribute
                 (Attribute => Attribute,
                  Index     => Index);

               for J in reverse List'Range loop
                  Found := False;

                  for K in Values'Range loop
                     if List (J).all = Values (K).all then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Project.Set_Attribute
                       (Attribute => Attribute,
                        Values    => (1 => List (J)),
                        Index     => Index,
                        Prepend   => First_Added);

                     First_Added := True;
                  end if;
               end loop;

               for J in Values'Range loop
                  Free (Values (J));
               end loop;
            end if;

            Free (List);
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "clear_attribute_values" then
         Name_Parameters (Data, Clear_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3, "");
            Index          : constant String := Nth_Arg (Data, 4, "");
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Project.Delete_Attribute
                 (Attribute => Attribute_Pkg_String'(Build
                    (Package_Name, Attribute_Name)),
                  Index     => Index);
            end if;
         end;

         Recompute_View (Get_Kernel (Data));
      end if;
   end Create_Project_Command_Handler;

   ----------------------------
   -- Register_Module_Reader --
   ----------------------------

   procedure Register_Module_Reader
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Properties_Module_ID := new Properties_Module_ID_Record (Kernel);
      Kernel.Register_Module (Abstract_Module (Properties_Module_ID));

      --  Redefine command to take into account attribute descriptions from
      --  projects.xml, which also provides the default values for attributes
      Override_Command
        (Kernel.Scripts, "get_attribute_as_string",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Override_Command
        (Kernel.Scripts, "get_attribute_as_list",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      Register_Command
        (Kernel, "get_tool_switches_as_list",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "get_tool_switches_as_string",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "get_extended_project",
         Minimum_Args  => 0,
         Maximum_Args  => 0,
         Class         => Get_Project_Class (Kernel),
         Handler       => Create_Project_Command_Handler'Access);
   end Register_Module_Reader;

   ----------------------------
   -- Register_Module_Writer --
   ----------------------------

   procedure Register_Module_Writer
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Command
        (Kernel, "properties_editor",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      Register_Command
        (Kernel, "set_attribute_as_string",
         Minimum_Args => 4,
         Maximum_Args => 4,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "add_attribute_values",
         Minimum_Args => 4,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_attribute_values",
         Minimum_Args => 4,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "clear_attribute_values",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "is_modified",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Get_Project_Class (Kernel),
         Handler       => Create_Project_Command_Handler'Access);
   end Register_Module_Writer;

   ------------------------
   -- Paths_Are_Relative --
   ------------------------

   function Paths_Are_Relative (Project : Project_Type) return Boolean is
   begin
      case Get_Paths_Type (Project) is
         when Relative  => return True;
         when Absolute  => return False;
         when From_Pref => return Generate_Relative_Paths.Get_Pref;
      end case;
   end Paths_Are_Relative;

   ----------------------------------
   -- Get_Attribute_Type_From_Name --
   ----------------------------------

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String)
      return Editable_Attribute_Description_Access
   is
      Result : constant Attribute_Description_Access :=
        Properties_Module_ID.Get_Attribute_Type_From_Name (Pkg, Name);
   begin
      return Editable_Attribute_Description_Access (Result);
   end Get_Attribute_Type_From_Name;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Project         : Project_Type;
      Attr            : Editable_Attribute_Description_Access;
      Index           : String;
      Omit_If_Default : Boolean := False) return String is
   begin
      --  First choice: if the editor is being edited, use that value

      if Attr.Editor /= null then
         return Get_Value_As_String
                  (Attr.Editor,
                   (if Attr.Case_Sensitive_Index
                    then Index else To_Lower (Index)));
      end if;

      --  Otherwise, we'll have to look in the project, or use the default
      --  value if the attribute hasn't been specified otherwise.
      return Get_Value_From_Project (Project, Attr, Index, Omit_If_Default);
   end Get_Current_Value;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Kernel          : access Core_Kernel_Record'Class;
      Project         : Project_Type;
      Attr            : Editable_Attribute_Description_Access;
      Index           : String := "";
      Omit_If_Default : Boolean := False)
      return GNAT.Strings.String_List_Access
   is
   begin
      --  First choice: if the attribute is being edited, use that value

      if Attr.Editor /= null then
         return new GNAT.Strings.String_List'
                      (Get_Value_As_List
                         (Attr.Editor,
                          (if Attr.Case_Sensitive_Index
                           then Index
                           else To_Lower (Index))));
      else
         return Get_Value_From_Project
           (Kernel, Project, Attr, Index, Omit_If_Default);
      end if;
   end Get_Current_Value;

   -------------------------------
   -- New_Attribute_Description --
   -------------------------------

   overriding function New_Attribute_Description
     (Module  : access Properties_Module_ID_Record;
      Indexed : Boolean)
      return Attribute_Description_Access
   is
      pragma Unreferenced (Module);
   begin
      return new Editable_Attribute_Description (Indexed);
   end New_Attribute_Description;

end Project_Properties;
