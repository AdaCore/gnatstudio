------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with GNATCOLL.Any_Types; use GNATCOLL.Any_Types;
with Language_Handlers; use Language_Handlers;
with GPS.Scripts;
with GPS.Scripts.Files;

package body Language.Shell is
   pragma Warnings (Off);

   Construct_List_Class_Name : constant String := "__ConstructsList";
   Language_Class_Name       : constant String := "Language";

   type Construct_List_Properties_Record is new Instance_Property_Record
     with record
      CList : Construct_List;
      Lang : access Shell_Language;
   end record;

   type Construct_List_Properties
   is access all Construct_List_Properties_Record;

   function Create_Python_Constructs_List
     (Script : access Scripting_Language_Record'Class;
      Lang   : Shell_Language_Access) return Class_Instance;

   procedure Add_Construct
     (Data : in out Callback_Data'Class; Command : String);

   procedure Python_Add_Language
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Add_Construct
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      use GNATCOLL.Symbols;

      function Python_List_To_Sloc
        (L : List_Instance'Class) return Source_Location
      is
         (Source_Location'(L.Nth_Arg (1), L.Nth_Arg (2), L.Nth_Arg (3)));

      List_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data.Get_Script), Construct_List_Class_Name);

      CList : constant Construct_List_Properties :=
        Construct_List_Properties
          (Get_Data (Data.Nth_Arg (1, List_Class, True),
           Construct_List_Class_Name));

      Category : constant Integer := Data.Nth_Arg (2);
      Is_Declaration : constant Boolean := Data.Nth_Arg (3);
      Visibility : constant Integer := Data.Nth_Arg (4);

      Name : constant String := Data.Nth_Arg (5);
      Profile : constant String := Data.Nth_Arg (6);

      Sloc_Start : constant Source_Location :=
        Python_List_To_Sloc (Data.Nth_Arg (7));
      Sloc_End : constant Source_Location :=
        Python_List_To_Sloc (Data.Nth_Arg (8));
      Sloc_Entity : constant Source_Location :=
        Python_List_To_Sloc (Data.Nth_Arg (9));

      CInfo : constant Construct_Access := new Construct_Information'
        (Category => Language_Category'Val (Category),
         Category_Name => GNATCOLL.Symbols.Empty_String,
         Is_Declaration => Is_Declaration,
         Is_Generic_Spec => False,
         Visibility => Construct_Visibility'Val (Visibility),
         Name => (if Name /= "" then CList.Lang.Symbols.Find (Name)
                  else GNATCOLL.Symbols.No_Symbol),
         Profile => new String'(Profile),
         Sloc_Start => Sloc_Start,
         Sloc_End => Sloc_End,
         Sloc_Entity => Sloc_Entity,
         Prev => CList.CList.Last,
         Next => null,
         Attributes => (others => False));
   begin
      CList.CList.Last := CInfo;

      if CList.CList.First = null then
         CList.CList.First := CInfo;
      else
         CInfo.Prev.Next := CInfo;
      end if;

      CList.CList.Size := CList.CList.Size + 1;
   end Add_Construct;

   -----------------------------------
   -- Create_Python_Constructs_List --
   -----------------------------------

   function Create_Python_Constructs_List
     (Script : access Scripting_Language_Record'Class;
      Lang   : Shell_Language_Access)  return Class_Instance
   is
      List_Class : constant Class_Type :=
        New_Class (Get_Kernel (Script), Construct_List_Class_Name);
      Inst : constant Class_Instance := New_Instance (Script, List_Class);
   begin
      Set_Data (Inst, Construct_List_Class_Name,
                Construct_List_Properties_Record'(
                  CList => Construct_List'(Size => 0, others => null),
                  Lang => Lang));
      return Inst;
   end Create_Python_Constructs_List;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Shell_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return null;
   end Keywords;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return null;
   end Keywords;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access Shell_Language) return String is
   begin
      return To_String (Lang.Name);
   end Get_Name;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Shell_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return False;
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return (1 .. 0 => null);
   end Keywords;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Shell_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang, Name);
   begin
      return "";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang, Name, Index);
   begin
      return "";
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang, Name, Field);
   begin
      return "";
   end Record_Field_Name;

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

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access Shell_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Null_Context'Access;
   end Get_Language_Context;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang    : access Shell_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer  : UTF8_String;
      Result  : out Construct_List)
   is
      Sub    : constant Subprogram_Type :=
        Get_Method (Lang.Object, "parse_constructs");
   begin
      if Sub = null then
         Result := (null, null, null, 0);
         return;
      end if;

      declare
         Script : constant Scripting_Language  := Get_Script (Sub.all);
         Args   : Callback_Data'Class := Create (Script, 3);

         Python_CList : constant Class_Instance :=
           Create_Python_Constructs_List
             (Script, Shell_Language_Access (Lang));

         CList : constant Construct_List_Properties :=
           Construct_List_Properties
             (Get_Data (Python_CList, Construct_List_Class_Name));

      begin
         Args.Set_Nth_Arg (1, Python_CList);
         Args.Set_Nth_Arg (2, GPS.Scripts.Files.Create_File (Script, File));
         Args.Set_Nth_Arg (3, Buffer);
         declare
            Ignore : Any_Type := Execute (Sub, Args);
         begin
            null;
         end;
         Result := CList.CList;
      end;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access Shell_Language;
      Buffer   : String;
      Callback : Entity_Callback) is
   begin
      null;
   end Parse_Entities;

   ---------------------------
   -- Create_Shell_Language --
   ---------------------------

   procedure Register_Shell_Language
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Instance    : Class_Instance;
      Lang_Name   : String;
      Body_Suffix : String;
      Spec_Suffix : String := "";
      Obj_Suffix  : String := "";
      Indent      : Indentation_Kind := Simple)
   is
      Lang : constant access Language_Root'Class :=
        new Shell_Language'(Symbols => GNATCOLL.Symbols.Allocate,
                            Name => To_Unbounded_String (Lang_Name),
                            Object => Instance,
                            Indent_Params => Default_Indent_Parameters,
                            Indent_Style => Indent);
   begin
      Kernel.Lang_Handler.Register_Language (Lang, null);
      Get_Registry (Kernel).Environment.Register_Default_Language_Extension
        (Lang_Name, Spec_Suffix, Body_Suffix, Obj_Suffix);
   end Register_Shell_Language;

   procedure Python_Add_Language
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
   begin
      Register_Shell_Language
        (Get_Kernel (Data),
         Data.Nth_Arg (1),
         Data.Nth_Arg (2),
         Data.Nth_Arg (3),
         Data.Nth_Arg (4, ""),
         Data.Nth_Arg (5, ""),
         Indentation_Kind'Val (Integer'(Data.Nth_Arg (6, 1))));
   end Python_Add_Language;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Kernel : GPS.Kernel.Kernel_Handle)
   is
      List_Class : constant Class_Type :=
        New_Class (Kernel, Construct_List_Class_Name);
      Language_Class : constant Class_Type :=
        New_Class (Kernel, Language_Class_Name);
   begin
      Register_Command (Kernel.Scripts, "register",
                        (Param ("language_instance", False),
                         Param ("name", False),
                         Param ("body_suffix", False),
                         Param ("spec_suffix", True),
                         Param ("obj_suffix", True),
                         Param ("indentation_kind", True)),
                        Python_Add_Language'Access, Language_Class, True);

      Register_Command (Kernel.Scripts,
                        "add_construct",
                        (Param ("category", False),
                         Param ("is_declaration", False),
                         Param ("visibility", False),
                         Param ("name", False),
                         Param ("profile", False),
                         Param ("sloc_start", False),
                         Param ("sloc_end", False),
                         Param ("sloc_entity", False)),
                        Add_Construct'Access,
                        List_Class);
   end Setup;

end Language.Shell;
