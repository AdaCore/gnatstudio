------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with GNATCOLL.Any_Types;     use GNATCOLL.Any_Types;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.Utils;         use GNATCOLL.Utils;

with GNATdoc.Markup_Streams; use GNATdoc.Markup_Streams;

package body GNATdoc.Customization.Tag_Handlers.Shell is

   Inline_Tag_Handler_Class_Name : constant String := "InlineTagHandler";
   Markup_Generator_Class_Name   : constant String := "MarkupGenerator";

   Has_Parameter_Method          : constant String := "has_parameter";
   HTML_Method                   : constant String := "html";
   Start_List_Item_Method        : constant String := "start_list_item";
   Start_List_Method             : constant String := "start_list";
   Start_Paragraph_Method        : constant String := "start_paragraph";
   Text_Method                   : constant String := "text";
   To_Markup_Method              : constant String := "to_markup";
   Register_Tag_Handler_Method   : constant String := "register_tag_handler";

   Attributes_Parameter          : constant String := "attributes";
   Handler_Parameter             : constant String := "handler";
   HTML_Parameter                : constant String := "html";
   Name_Parameter                : constant String := "name";
   Text_Parameter                : constant String := "text";

   PE  : aliased constant String := "end_paragraph";
   LLE : aliased constant String := "end_list";
   LIE : aliased constant String := "end_list_item";
   GAP : aliased constant String := "generate_after_paragraph";
   GI  : aliased constant String := "generate_inline";

   type Parameterless_Method is
     (Emit_Paragraph_End,
      Emit_List_End,
      Emit_List_Item_End,
      Generate_After_Paragraph,
      Generate_Inline);

   Parameterless_Method_Name : constant array (Parameterless_Method)
     of Cst_String_Access :=
       (Emit_Paragraph_End       => PE'Access,
        Emit_List_End            => LLE'Access,
        Emit_List_Item_End       => LIE'Access,
        Generate_After_Paragraph => GAP'Access,
        Generate_Inline          => GI'Access);

   procedure Inline_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.InlineTagHandler commands

   procedure Markup_Generator_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.register_tag_handler command

   procedure Register_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.register_tag_handler command

   function Get_Inline_Tag_Handler_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Return class for InlineTagHandler

   function Get_Inline_Tag_Handler_Class
     (Data : Callback_Data'Class) return Class_Type;
   --  Return class for TagHandler

   function Get_Markup_Generator_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Return class for Markup_Generator

   function Get_Markup_Generator_Class
     (Data : Callback_Data'Class) return Class_Type;
   --  Return class for Markup_Generator

   type Markup_Generator_Access is access all Markup_Generator'Class;

   type Inline_Tag_Handler_Properties_Record is
     new Instance_Property_Record with record
      Object : Inline_Tag_Handler_Access;
      Name   : Unbounded_String;
   end record;

   type Inline_Tag_Handler_Properties_Access is
     access all Inline_Tag_Handler_Properties_Record'Class;

   type Markup_Generator_Properties_Record is
     new Instance_Property_Record with record
      Object : Markup_Generator_Access;
   end record;

   type Markup_Generator_Properties_Access is
     access all Markup_Generator_Properties_Record'Class;

   function Get_Inline_Tag_Handler_Properties
     (Object : Class_Instance) return Inline_Tag_Handler_Properties_Access;
   --  Returns properties of the object.

   function Get_Markup_Generator_Properties
     (Object : Class_Instance) return Markup_Generator_Properties_Access;
   --  Returns properties of the object.

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive) return Name_Value_Maps.Map;
   --  Returns value of nth argument as name-value map

   ----------------------------------
   -- Get_Inline_Tag_Handler_Class --
   ----------------------------------

   function Get_Inline_Tag_Handler_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Inline_Tag_Handler_Class_Name);
   end Get_Inline_Tag_Handler_Class;

   ----------------------------------
   -- Get_Inline_Tag_Handler_Class --
   ----------------------------------

   function Get_Inline_Tag_Handler_Class
     (Data : Callback_Data'Class) return Class_Type is
   begin
      return New_Class (Data.Get_Repository, Inline_Tag_Handler_Class_Name);
   end Get_Inline_Tag_Handler_Class;

   ---------------------------------------
   -- Get_Inline_Tag_Handler_Properties --
   ---------------------------------------

   function Get_Inline_Tag_Handler_Properties
     (Object : Class_Instance) return Inline_Tag_Handler_Properties_Access is
   begin
      return
        Inline_Tag_Handler_Properties_Access
          (Get_Data (Object, Inline_Tag_Handler_Class_Name));
   end Get_Inline_Tag_Handler_Properties;

   --------------------------------
   -- Get_Markup_Generator_Class --
   --------------------------------

   function Get_Markup_Generator_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Markup_Generator_Class_Name);
   end Get_Markup_Generator_Class;

   --------------------------------
   -- Get_Markup_Generator_Class --
   --------------------------------

   function Get_Markup_Generator_Class
     (Data : Callback_Data'Class) return Class_Type is
   begin
      return New_Class (Data.Get_Repository, Markup_Generator_Class_Name);
   end Get_Markup_Generator_Class;

   -------------------------------------
   -- Get_Markup_Generator_Properties --
   -------------------------------------

   function Get_Markup_Generator_Properties
     (Object : Class_Instance) return Markup_Generator_Properties_Access is
   begin
      return
        Markup_Generator_Properties_Access
          (Get_Data (Object, Markup_Generator_Class_Name));
   end Get_Markup_Generator_Properties;

   -------------------
   -- Has_Parameter --
   -------------------

   overriding function Has_Parameter
     (Self : Python_Inline_Tag_Handler) return Boolean
   is
      Method    : Subprogram_Type      :=
        Get_Method (Self.Instance, Has_Parameter_Method);
      Arguments :  Callback_Data'Class :=
        Create (Get_Script (Self.Instance), 0);

   begin
      return Aux : constant Boolean := Method.Execute (Arguments) do
         Free (Method);
         Free (Arguments);
      end return;
   end Has_Parameter;

   ----------------------------------------
   -- Inline_Tag_Handler_Command_Handler --
   ----------------------------------------

   procedure Inline_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Inline_Tag_Handler_Class (Data));

   begin
      if Command = Constructor_Method then
         declare
            Name    : constant String := Nth_Arg (Data, 2);
            Handler : constant Inline_Tag_Handler_Access :=
              new Python_Inline_Tag_Handler'
                (Abstract_Inline_Tag_Handler with Instance => Instance);

         begin
            Set_Data
              (Instance,
               Inline_Tag_Handler_Class_Name,
               Inline_Tag_Handler_Properties_Record'
                 (Object => Handler,
                  Name   => To_Unbounded_String (Name)));
         end;

      elsif Command = Has_Parameter_Method then
         Data.Set_Return_Value (False);
      end if;
   end Inline_Tag_Handler_Command_Handler;

   --------------------------------------
   -- Markup_Generator_Command_Handler --
   --------------------------------------

   procedure Markup_Generator_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Markup_Generator_Class (Data));
      Writer   : constant Markup_Generator_Access :=
        Get_Markup_Generator_Properties (Instance).Object;
      Method   : Parameterless_Method;

   begin
      if Command = Text_Method then
         Writer.Text
           (Text       => Nth_Arg (Data, 2),
            Attributes => Nth_Arg (Data, 3));

      elsif Command = HTML_Method then
         Writer.HTML (Nth_Arg (Data, 2));

      elsif Command = Start_Paragraph_Method then
         Writer.Start_Paragraph (Nth_Arg (Data, 2));

      elsif Command = Start_List_Method then
         Writer.Start_List (Nth_Arg (Data, 2));

      elsif Command = Start_List_Item_Method then
         Writer.Start_List_Item (Nth_Arg (Data, 2));

      else
         for J in Parameterless_Method_Name'Range loop
            if Command = Parameterless_Method_Name (J).all then
               Method := J;
               exit;
            end if;
         end loop;

         case Method is
            when Emit_Paragraph_End =>
               Writer.End_Paragraph;
            when Emit_List_End =>
               Writer.End_List;
            when Emit_List_Item_End =>
               Writer.End_List_Item;
            when Generate_After_Paragraph =>
               Writer.Switch_To_After_Stream;
            when Generate_Inline =>
               Writer.Switch_To_Inline_Stream;
         end case;
      end if;
   end Markup_Generator_Command_Handler;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Python_Inline_Tag_Handler) return String is
   begin
      return
        To_String (Get_Inline_Tag_Handler_Properties (Self.Instance).Name);
   end Name;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive) return Name_Value_Maps.Map
   is
      Result : Name_Value_Maps.Map;

   begin
      if Number_Of_Arguments (Data) >= N then
         declare
            Dict : constant Dictionary_Instance'Class := Nth_Arg (Data, N);
            Iter : aliased Dictionary_Iterator'Class := Dict.Iterator;

         begin
            while Iter.Next loop
               Result.Insert (Iter.Key, Iter.Value);
            end loop;
         end;
      end if;

      return Result;
   end Nth_Arg;

   ------------------------------------------
   -- Register_Tag_Handler_Command_Handler --
   ------------------------------------------

   procedure Register_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Register_Tag_Handler_Method then
         declare
            Instance : constant Class_Instance :=
              Nth_Arg (Data, 1, Get_Inline_Tag_Handler_Class (Data));

         begin
            GNATdoc.Customization.Tag_Handlers.Register
              (Tag_Handler_Access
                 (Get_Inline_Tag_Handler_Properties (Instance).Object));
         end;
      end if;
   end Register_Tag_Handler_Command_Handler;

   ---------------
   -- To_Markup --
   ---------------

   overriding procedure To_Markup
     (Self      : in out Python_Inline_Tag_Handler;
      Parameter : String;
      Writer    : in out Markup_Generator)
   is
      Method    : Subprogram_Type :=
        Get_Method (Self.Instance, To_Markup_Method);
      Arguments : Callback_Data'Class :=
        Create (Get_Script (Self.Instance), 2);
      Generator : Class_Instance;

   begin
      Generator :=
        New_Instance
          (Get_Script (Self.Instance),
           New_Class
             (Get_Repository (Get_Script (Self.Instance)),
              Markup_Generator_Class_Name));
      Set_Data
        (Generator,
         Markup_Generator_Class_Name,
         Markup_Generator_Properties_Record'
           (Object => Writer'Unchecked_Access));

      Set_Nth_Arg (Arguments, 1, Generator);
      Set_Nth_Arg (Arguments, 2, Parameter);

      declare
         Dummy : constant Any_Type := Method.Execute (Arguments);

      begin
         null;
      end;

      Free (Method);
      Free (Arguments);

      Get_Markup_Generator_Properties (Generator).Object := null;
   end To_Markup;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      --  InlineTagHandler

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Constructor_Method,
         Class   => Get_Inline_Tag_Handler_Class (Kernel),
         Params  => (1 => Param (Name_Parameter)),
         Handler => Inline_Tag_Handler_Command_Handler'Access);

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Has_Parameter_Method,
         Class   => Get_Inline_Tag_Handler_Class (Kernel),
         Params  => No_Params,
         Handler => Inline_Tag_Handler_Command_Handler'Access);

      --  MarkupGenerator

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Text_Method,
         Class   => Get_Markup_Generator_Class (Kernel),
         Params  => (1 => Param (Text_Parameter),
                     2 => Param (Attributes_Parameter, True)),
         Handler => Markup_Generator_Command_Handler'Access);

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => HTML_Method,
         Class   => Get_Markup_Generator_Class (Kernel),
         Params  => (1 => Param (HTML_Parameter)),
         Handler => Markup_Generator_Command_Handler'Access);

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Start_Paragraph_Method,
         Class   => Get_Markup_Generator_Class (Kernel),
         Params  => (1 => Param (Attributes_Parameter, True)),
         Handler => Markup_Generator_Command_Handler'Access);

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Start_List_Method,
         Class   => Get_Markup_Generator_Class (Kernel),
         Params  => (1 => Param (Attributes_Parameter, True)),
         Handler => Markup_Generator_Command_Handler'Access);

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Start_List_Item_Method,
         Class   => Get_Markup_Generator_Class (Kernel),
         Params  => (1 => Param (Attributes_Parameter, True)),
         Handler => Markup_Generator_Command_Handler'Access);

      for J of Parameterless_Method_Name loop
         Register_Command
           (Repo    => Kernel.Scripts,
            Command => J.all,
            Class   => Get_Markup_Generator_Class (Kernel),
            Params  => No_Params,
            Handler => Markup_Generator_Command_Handler'Access);
      end loop;

      --  "register_tag_handler"

      Register_Command
        (Repo    => Kernel.Scripts,
         Command => Register_Tag_Handler_Method,
         Params  => (1 => Param (Handler_Parameter)),
         Handler => Register_Tag_Handler_Command_Handler'Access);
   end Register_Commands;

end GNATdoc.Customization.Tag_Handlers.Shell;
