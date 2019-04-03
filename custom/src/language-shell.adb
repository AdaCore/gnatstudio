------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;          use GNAT.Strings;
with GPS.Kernel.Scripts;    use GPS.Kernel.Scripts;
with GPS.Kernel.Project;    use GPS.Kernel.Project;
with GNATCOLL.Any_Types;    use GNATCOLL.Any_Types;
with Language_Handlers;     use Language_Handlers;
with GPS.Scripts;
with GPS.Scripts.Files;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Language.Shell is
   pragma Warnings (Off);
   Me : constant Trace_Handle := Create ("GPS.CUSTOM.LANGUAGE_SHELL");

   Construct_List_Class_Name : constant String := "ConstructsList";
   Language_Class_Name       : constant String := "Language";
   Construct_Class_Name      : constant String := "Construct";
   Semantic_Tree_Class_Name  : constant String := "SemanticTree";

   Null_Context : aliased Language_Context :=
     (Syntax                        => (Comment_Start                 => null,
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

   type Shell_Language is new Language_Root with record
      Object : Class_Instance;
      Name   : Unbounded_String;
   end record;
   type Shell_Language_Access is access all Shell_Language;
   overriding function Keywords
     (Lang : access Shell_Language) return Strings.String_Access is (null);
   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Expect.Pattern_Matcher_Access
     is (null);
   overriding function Get_Name (Lang : access Shell_Language) return String
     is (To_String (Lang.Name));
   overriding function Is_Simple_Type
     (Lang : access Shell_Language; Str : String) return Boolean is (False);
   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Strings.String_List
     is ((1 .. 0 => null));
   overriding function Dereference_Name
     (Lang : access Shell_Language;
      Name : String) return String is ("");
   overriding function Array_Item_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Index : String) return String is ("");
   overriding function Record_Field_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Field : String) return String is ("");
   overriding function Get_Language_Context
     (Lang : access Shell_Language) return Language_Context_Access
     is (Null_Context'Access);
   overriding procedure Parse_Constructs
     (Lang    : access Shell_Language;
      File    : GNATCOLL.VFS.Virtual_File;
      Buffer  : UTF8_String;
      Result  : out Construct_List);
   overriding function Should_Refresh_Constructs_Tree
     (Lang   : not null access Shell_Language;
      File   : GNATCOLL.VFS.Virtual_File)
      return Boolean;
   overriding function Clicked_On_Construct
     (Lang      : not null access Shell_Language;
      File      : GNATCOLL.VFS.Virtual_File;
      Construct : Semantic_Node_Info) return Boolean;
   overriding procedure Parse_Entities
     (Lang     : access Shell_Language;
      Buffer   : String;
      Callback : Entity_Callback);
   overriding function Get_Last_Selected_Construct_ID
     (Lang : not null access Shell_Language;
      File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.Symbols.Symbol;

   type Construct_List_Properties_Record is new Instance_Property_Record
     with record
      CList : Construct_List;
      Lang : access Shell_Language;
   end record;
   type Construct_List_Properties
      is access all Construct_List_Properties_Record;

   type Construct_Properties_Record is new Instance_Property_Record with record
      Info       : Semantic_Node_Info;
      File       : GNATCOLL.VFS.Virtual_File;
   end record;
   type Construct_Properties is access all Construct_Properties_Record;

   type Semantic_Tree_Properties_Record is new
     Instance_Property_Record
   with record
      --  Note: we cannot store a Semantic_Tree_Access in this class, since
      --  the data contained in a Semantic_Tree can be freed at any time.
      --  To provide safety, we query the tree from the file for every
      --  primitive.
      File : GNATCOLL.VFS.Virtual_File;
   end record;

   function Create_Python_Constructs_List
     (Script : access Scripting_Language_Record'Class;
      Lang   : Shell_Language_Access) return Class_Instance;

   procedure Add_Construct
      (Data : in out Callback_Data'Class; Command : String);

   procedure Language_Handler
      (Data : in out Callback_Data'Class; Command : String);
   --  Handlers for the python commands

   procedure Construct_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the GPS.Construct class

   procedure Semantic_Tree_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the GPS.SemanticTree class

   -----------------------
   -- Construct_Handler --
   -----------------------

   procedure Construct_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Prop : Construct_Properties;
      Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         Data.Set_Error_Msg ("Cannot create instances of Construct");
         return;
      end if;

      Inst := Data.Nth_Arg (1);
      Prop := Construct_Properties (Get_Data (Inst, Construct_Class_Name));

      if Command = "name" then
         Data.Set_Return_Value (Get (Prop.Info.Name).all);

      elsif Command = "file" then
         Data.Set_Return_Value
           (Create_File (Get_Script (Data), Prop.File));

      elsif Command = "start" then
         Data.Set_Return_Value_As_List (Size => 3);
         Data.Set_Return_Value (Prop.Info.Sloc_Start_No_Tab.Line);
         Data.Set_Return_Value (Integer (Prop.Info.Sloc_Start_No_Tab.Column));
         Data.Set_Return_Value (Integer (Prop.Info.Sloc_Start_No_Tab.Index));

      elsif Command = "id" then
         Data.Set_Return_Value (Get (Prop.Info.Unique_Id).all);
      end if;
   end Construct_Handler;

   ---------------------------
   -- Semantic_Tree_Handler --
   ---------------------------

   procedure Semantic_Tree_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant GPS.Kernel.Kernel_Handle := Get_Kernel (Data);

      function Get_Tree return Semantic_Tree'Class;
      --  Return the tree stored in the first argument to Data

      function Get_Tree return Semantic_Tree'Class is
         Ins : constant Class_Instance := Nth_Arg (Data, 1);
         R   : constant Instance_Property := Get_Data
           (Ins, Semantic_Tree_Class_Name);
      begin
         return Kernel.Get_Abstract_Tree_For_File
           ("SHELL", Semantic_Tree_Properties_Record (R.all).File);
      end Get_Tree;

   begin
      if Command = Constructor_Method then
         declare
            File : constant Class_Instance :=
              Nth_Arg (Data, 2,
                       GPS.Scripts.Files.Get_File_Class (Kernel),
                       Default    => No_Class_Instance,
                       Allow_Null => False);
            Instance : constant Class_Instance :=
              Nth_Arg (Data, 1,
                       New_Class (Kernel.Scripts, Semantic_Tree_Class_Name));
            R : Semantic_Tree_Properties_Record :=
              (File => GPS.Scripts.Files.Get_Data (File));
         begin
            Set_Data (Instance, Semantic_Tree_Class_Name, R);
         end;
      elsif Command = "is_ready" then
         Set_Return_Value (Data, Get_Tree.Is_Ready);
      elsif Command = "update" then
         declare
            Tree : Semantic_Tree'Class := Get_Tree;

         begin
            Tree.Update;
         end;
      end if;
   end Semantic_Tree_Handler;

   --------------------------
   -- Clicked_On_Construct --
   --------------------------

   overriding function Clicked_On_Construct
     (Lang      : not null access Shell_Language;
      File      : GNATCOLL.VFS.Virtual_File;
      Construct : Semantic_Node_Info) return Boolean
   is
      Sub    : constant Subprogram_Type :=
        Get_Method (Lang.Object, "clicked_on_construct");
   begin
      if Sub = null then
         return False;  --  Not handled
      end if;

      declare
         Script : constant Scripting_Language  := Get_Script (Sub.all);
         Inst   : Class_Instance;
         Args   : Callback_Data'Class := Create (Script, 1);
         Prop   : Construct_Properties;
      begin
         Trace (Me, "Running method " & Sub.Get_Name);
         Inst := Script.New_Instance
           (Script.Get_Repository.New_Class (Construct_Class_Name));
         Set_Data
           (Inst, Construct_Class_Name,
            Construct_Properties_Record'
              (Info       => Construct,
               File       => File));
         Args.Set_Nth_Arg (1, Inst);

         declare
            Result : constant Any_Type := Execute (Sub, Args);
         begin
            Free (Args);   --  Handled
            return True;
         end;
      end;
   end Clicked_On_Construct;

   ------------------------------------
   -- Get_Last_Selected_Construct_ID --
   ------------------------------------

   overriding function Get_Last_Selected_Construct_ID
     (Lang : not null access Shell_Language;
      File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.Symbols.Symbol
   is
      Sub : constant Subprogram_Type :=
        Get_Method (Lang.Object, "get_last_selected_construct_id");
   begin
      if Sub = null then
         return GNATCOLL.Symbols.No_Symbol;
      end if;

      declare
         Script : constant Scripting_Language  := Get_Script (Sub.all);
         Args   : Callback_Data'Class := Create (Script, 1);
      begin
         Args.Set_Nth_Arg (1, GPS.Scripts.Files.Create_File (Script, File));

         declare
            Result : constant String := Execute (Sub, Args);
         begin
            if Result /= "" then
               return Lang.Symbols.Find (Result);
            else
               return GNATCOLL.Symbols.No_Symbol;
            end if;
         end;
      end;
   end Get_Last_Selected_Construct_ID;

   -------------------
   -- Add_Construct --
   -------------------

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
      Id          : constant String := Data.Nth_Arg (10, "");

      CInfo : constant Construct_Access := new Construct_Information'
        (Info     =>
           (Category        => Language_Category'Val (Category),
            Category_Name   => GNATCOLL.Symbols.Empty_String,
            Is_Declaration  => Is_Declaration,
            Is_Generic_Spec => False,
            Visibility      => Construct_Visibility'Val (Visibility),
            Name            =>
              (if Name /= "" then CList.Lang.Symbols.Find (Name)
               else GNATCOLL.Symbols.No_Symbol),
            Profile         =>
              (if Profile /= "" then CList.Lang.Symbols.Find (Profile)
               else GNATCOLL.Symbols.No_Symbol),
            Unique_Id       =>
              (if Id /= "" then CList.Lang.Symbols.Find (Id)
               else GNATCOLL.Symbols.No_Symbol),
            Attributes      => (others => False),
            Sloc_Start      => Sloc_Start,
            Sloc_End        => Sloc_End,
            Sloc_Entity     => Sloc_Entity),
         Prev => CList.CList.Last,
         Next => null);
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

   ------------------------------------
   -- Should_Refresh_Constructs_Tree --
   ------------------------------------

   overriding function Should_Refresh_Constructs_Tree
     (Lang   : not null access Shell_Language;
      File   : GNATCOLL.VFS.Virtual_File)
      return Boolean
   is
      Sub    : constant Subprogram_Type :=
        Get_Method (Lang.Object, "should_refresh_constructs");
   begin
      if Sub = null then
         return False;  --  unless the file's timestamp has changed on disk
      end if;

      declare
         Script : constant Scripting_Language  := Get_Script (Sub.all);
         Args   : Callback_Data'Class := Create (Script, 1);
         Result : Boolean;
      begin
         Args.Set_Nth_Arg (1, GPS.Scripts.Files.Create_File (Script, File));
         Result := Execute (Sub, Args);
         Free (Args);
         return Result;
      end;
   end Should_Refresh_Constructs_Tree;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang    : access Shell_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer  : UTF8_String;
      Result  : out Construct_List)
   is
      Sub    : Subprogram_Type :=
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
         Free (Sub);
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

   -----------------------------
   -- Register_Shell_Language --
   -----------------------------

   procedure Register_Shell_Language
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Instance    : Class_Instance;
      Lang_Name   : String;
      Body_Suffix : String;
      Spec_Suffix : String := "";
      Obj_Suffix  : String := "";
      Indent      : Indentation_Kind := Simple)
   is
      Lang : constant not null Language_Access :=
        new Shell_Language'(Symbols       => GNATCOLL.Symbols.Allocate,
                            Name          => To_Unbounded_String (Lang_Name),
                            Object        => Instance,
                            Indent_Params => Default_Indent_Parameters,
                            Indent_Style  => Indent);

   begin
      Kernel.Lang_Handler.Register_Language (Lang, null);
      Get_Registry (Kernel).Environment.Register_Default_Language_Extension
        (Lang_Name, Spec_Suffix, Body_Suffix, Obj_Suffix);
   end Register_Shell_Language;

   ----------------------
   -- Language_Handler --
   ----------------------

   procedure Language_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Lang : Language_Access;
      Inst : Class_Instance;
   begin
      if Command = "register" then
         Register_Shell_Language
           (Get_Kernel (Data),
            Data.Nth_Arg (1),
            Data.Nth_Arg (2),
            Data.Nth_Arg (3),
            Data.Nth_Arg (4, ""),
            Data.Nth_Arg (5, ""),
            Indentation_Kind'Val (Integer'(Data.Nth_Arg (6, 1))));

      elsif Command = "get" then
         Data.Set_Return_Value
           (Create_Language_Info
              (Get_Script (Data),
               Get_Kernel (Data).Get_Language_Handler.Get_Language_By_Name
               (Data.Nth_Arg (1))));
      end if;
   end Language_Handler;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Kernel : GPS.Kernel.Kernel_Handle)
   is
      List_Class : constant Class_Type :=
        Kernel.Scripts.New_Class (Construct_List_Class_Name);
      Construct_Class : constant Class_Type :=
        Kernel.Scripts.New_Class (Construct_Class_Name);
      Language_Class : constant Class_Type :=
         Kernel.Scripts.New_Class (Language_Class_Name);
      Semantic_Tree_Class : constant Class_Type :=
        Kernel.Scripts.New_Class (Semantic_Tree_Class_Name);
   begin
      Kernel.Scripts.Register_Command
        ("register",
         (Param ("language_instance", False),
          Param ("name", False),
          Param ("body_suffix", False),
          Param ("spec_suffix", True),
          Param ("obj_suffix", True),
          Param ("indentation_kind", True)),
         Handler        => Language_Handler'Access,
         Class          => Language_Class,
         Static_Method  => True);
      Kernel.Scripts.Register_Command
        ("get",
         Params         => (1 => Param ("name")),
         Class          => Language_Class,
         Static_Method  => True,
         Handler        => Language_Handler'Access);

      --  Do not register the methods that are supposed to be overridden in
      --  plugins, like "clicked_on_construct" or "parse_constructs", since
      --  we would then no longer be able to detect whether they exist or not
      --  to fallback on the default behavior (as we do in
      --  Clicked_On_Construct).

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class     => Construct_Class,
         Handler   => Construct_Handler'Access);
      Kernel.Scripts.Register_Property
        ("name",
         Class     => Construct_Class,
         Getter    => Construct_Handler'Access);
      Kernel.Scripts.Register_Property
        ("file",
         Class     => Construct_Class,
         Getter    => Construct_Handler'Access);
      Kernel.Scripts.Register_Property
        ("start",
         Class     => Construct_Class,
         Getter    => Construct_Handler'Access);
      Kernel.Scripts.Register_Property
        ("id",
         Class     => Construct_Class,
         Getter    => Construct_Handler'Access);

      Kernel.Scripts.Register_Command
        ("add_construct",
         Params    => (Param ("category"),
                       Param ("is_declaration"),
                       Param ("visibility"),
                       Param ("name"),
                       Param ("profile"),
                       Param ("sloc_start"),
                       Param ("sloc_end"),
                       Param ("sloc_entity"),
                       Param ("id", Optional => True)),
         Handler  => Add_Construct'Access,
         Class     => List_Class);

      --  Tree class

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class       => Semantic_Tree_Class,
         Handler     => Semantic_Tree_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_ready",
         Class     => Semantic_Tree_Class,
         Handler   => Semantic_Tree_Handler'Access);
      Kernel.Scripts.Register_Command
        ("update",
         Class     => Semantic_Tree_Class,
         Handler   => Semantic_Tree_Handler'Access);
   end Setup;

end Language.Shell;
