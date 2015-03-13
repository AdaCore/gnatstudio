------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2015, AdaCore                     --
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

with GNATCOLL.Any_Types; use GNATCOLL.Any_Types;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
with GNATCOLL.Utils;     use GNATCOLL.Utils;

package body GNATdoc.Customization.Tag_Handlers.Shell is

   Inline_Tag_Handler_Class_Name : constant String := "InlineTagHandler";
   Class_Cst                     : aliased constant String := "class";
   Item_Cst                      : aliased constant String := "item";
   Name_Cst                      : aliased constant String := "name";
   Text_Cst                      : aliased constant String := "text";
   String_Cst                    : aliased constant String := "string";

   Constructor_Parameters     : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);

   Emit_Text_Parameters       : constant Cst_Argument_List :=
     (1 => Text_Cst'Access, 2 => Class_Cst'Access);

   Emit_HTML_Parameters       : constant Cst_Argument_List :=
     (1 => String_Cst'Access);

   Register_Parameters        : constant Cst_Argument_List :=
     (1 => Item_Cst'Access);

   Has_Parameter_Method : constant String := "has_parameter";
   To_Markup_Method     : constant String := "to_markup";

   PS  : aliased constant String := "emit_paragraph_start";
   PE  : aliased constant String := "emit_paragraph_end";
   LS  : aliased constant String := "emit_list_start";
   LLE : aliased constant String := "emit_list_end";
   LIS : aliased constant String := "emit_list_item_start";
   LIE : aliased constant String := "emit_list_item_end";

   type Parameterless_Method is
     (Emit_Paragraph_Start,
      Emit_Paragraph_End,
      Emit_List_Start,
      Emit_List_End,
      Emit_List_Item_Start,
      Emit_List_Item_End);

   Parameterless_Method_Name : constant array (Parameterless_Method)
     of Cst_String_Access :=
       (Emit_Paragraph_Start => PS'Access,
        Emit_Paragraph_End   => PE'Access,
        Emit_List_Start      => LS'Access,
        Emit_List_End        => LLE'Access,
        Emit_List_Item_Start => LIS'Access,
        Emit_List_Item_End   => LIE'Access);

   procedure Inline_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.InlineTagHandler commands

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

   type Markup_Generator_Access is access all Markup_Generator'Class;

   type Inline_Tag_Handler_Properties_Record is
     new Instance_Property_Record with record
      Object : Inline_Tag_Handler_Access;
      Name   : Unbounded_String;
      Writer : Markup_Generator_Access;
   end record;

   type Inline_Tag_Handler_Properties_Access is
     access all Inline_Tag_Handler_Properties_Record'Class;

   function Get_Properties
     (Object : Class_Instance) return Inline_Tag_Handler_Properties_Access;
   --  Returns properties of the object.

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

   --------------------
   -- Get_Properties --
   --------------------

   function Get_Properties
     (Object : Class_Instance) return Inline_Tag_Handler_Properties_Access is
   begin
      return
        Inline_Tag_Handler_Properties_Access
          (Get_Data (Object, Inline_Tag_Handler_Class_Name));
   end Get_Properties;

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
      Method : Parameterless_Method;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Parameters);

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
                  Name   => To_Unbounded_String (Name),
                  Writer => null));
         end;

      elsif Command = "emit_text" then
         Name_Parameters (Data, Emit_Text_Parameters);
         Get_Properties (Instance).Writer.Text (Text  => Nth_Arg (Data, 2));

      elsif Command = "emit_html" then
         Name_Parameters (Data, Emit_HTML_Parameters);
         Get_Properties (Instance).Writer.HTML (Nth_Arg (Data, 2));

      elsif Command = Has_Parameter_Method then
         Data.Set_Return_Value (False);

      else
         for J in Parameterless_Method_Name'Range loop
            if Command = Parameterless_Method_Name (J).all then
               Method := J;
               exit;
            end if;
         end loop;

         declare
            Writer : constant Markup_Generator_Access :=
              Get_Properties (Instance).Writer;

         begin
            case Method is
               when Emit_Paragraph_Start =>
                  Writer.Start_Paragraph;
               when Emit_Paragraph_End =>
                  Writer.End_Paragraph;
               when Emit_List_Start =>
                  Writer.Start_List;
               when Emit_List_End =>
                  Writer.End_List;
               when Emit_List_Item_Start =>
                  Writer.Start_List_Item;
               when Emit_List_Item_End =>
                  Writer.End_List_Item;
            end case;
         end;
      end if;
   end Inline_Tag_Handler_Command_Handler;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Python_Inline_Tag_Handler) return String is
   begin
      return To_String (Get_Properties (Self.Instance).Name);
   end Name;

   ------------------------------------------
   -- Register_Tag_Handler_Command_Handler --
   ------------------------------------------

   procedure Register_Tag_Handler_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "register_tag_handler" then
         Name_Parameters (Data, Register_Parameters);

         declare
            Instance : constant Class_Instance :=
              Nth_Arg (Data, 1, Get_Inline_Tag_Handler_Class (Data));

         begin
            GNATdoc.Customization.Tag_Handlers.Register
              (Tag_Handler_Access (Get_Properties (Instance).Object));
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
        Create (Get_Script (Self.Instance), 1);

   begin
      Get_Properties (Self.Instance).Writer := Writer'Unchecked_Access;

      Set_Nth_Arg (Arguments, 1, Parameter);

      declare
         Dummy : constant Any_Type := Method.Execute (Arguments);

      begin
         null;
      end;

      Free (Method);
      Free (Arguments);

      Get_Properties (Self.Instance).Writer := null;
   end To_Markup;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Class         => Get_Inline_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Inline_Tag_Handler_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "emit_text",
         Class         => Get_Inline_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Inline_Tag_Handler_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "emit_html",
         Class         => Get_Inline_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Inline_Tag_Handler_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, Has_Parameter_Method,
         Class        => Get_Inline_Tag_Handler_Class (Kernel),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Inline_Tag_Handler_Command_Handler'Access);

      for J of Parameterless_Method_Name loop
         Register_Command
           (Kernel.Scripts, J.all,
            Class         => Get_Inline_Tag_Handler_Class (Kernel),
            Minimum_Args  => 0,
            Maximum_Args  => 0,
            Handler       => Inline_Tag_Handler_Command_Handler'Access);
      end loop;

      Register_Command
        (Kernel.Scripts, "register_tag_handler",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Register_Tag_Handler_Command_Handler'Access);
   end Register_Commands;

end GNATdoc.Customization.Tag_Handlers.Shell;
