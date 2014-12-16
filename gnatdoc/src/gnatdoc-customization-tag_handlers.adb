------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with GNATCOLL.Any_Types;              use GNATCOLL.Any_Types;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;

package body GNATdoc.Customization.Tag_Handlers is

   Tag_Handler_Class_Name     : constant String := "TagHandler";
   Class_Cst                  : aliased constant String := "class";
   Item_Cst                   : aliased constant String := "item";
   Tag_Cst                    : aliased constant String := "tag";
   Text_Cst                   : aliased constant String := "text";
   String_Cst                 : aliased constant String := "string";

   Constructor_Parameters     : constant Cst_Argument_List :=
     (1 => Tag_Cst'Access);

   Emit_Text_Parameters       : constant Cst_Argument_List :=
     (1 => Text_Cst'Access, 2 => Class_Cst'Access);

   Emit_HTML_Parameters       : constant Cst_Argument_List :=
     (1 => String_Cst'Access);

   Register_Parameters        : constant Cst_Argument_List :=
     (1 => Item_Cst'Access);

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

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.TagHandler commands

   function Get_Tag_Handler_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Return class for TagHandler

   function Get_Tag_Handler_Class
     (Data : Callback_Data'Class) return Class_Type;
   --  Return class for TagHandler

   type Markup_Generator_Access is access all Markup_Generator'Class;

   type Tag_Handler_Properties_Record is new Instance_Property_Record with
   record
      Tag    : Unbounded_String;
      Writer : Markup_Generator_Access;
   end record;

   function Get_Data (Instance : Class_Instance) return String;
   --  Return Tag corresponding to given Instance of TagHandle
   procedure Set_Data (Instance : Class_Instance; Tag : String);
   --  Set Tag to given Instance of TagHandle

   function Get_Writer
     (Instance : Class_Instance) return Markup_Generator_Access;
   --  Return Writer corresponding to given Instance of TagHandle
   procedure Set_Writer
     (Instance : Class_Instance;
      Writer   : Markup_Generator_Access);
   --  Set Writer to given Instance of TagHandle

   package Tag_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Class_Instance,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   Map : Tag_Maps.Map;
   --  Map tag name to corresponding python classes

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Method : Parameterless_Method;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_Tag_Handler_Class (Data));
            Tag_Name : constant String := Nth_Arg (Data, 2);
         begin
            Set_Data (Instance, Tag_Name);
         end;
      elsif Command = "emit_text" then
         Name_Parameters (Data, Emit_Text_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_Tag_Handler_Class (Data));
         begin
            Get_Writer (Instance).Text
              (Text  => Nth_Arg (Data, 2));
         end;

      elsif Command = "emit_html" then
         Name_Parameters (Data, Emit_HTML_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_Tag_Handler_Class (Data));
         begin
            Get_Writer (Instance).HTML (Nth_Arg (Data, 2));
         end;

      elsif Command = "register_tag_handler" then
         Name_Parameters (Data, Register_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_Tag_Handler_Class (Data));
            Tag      : constant String := Get_Data (Instance);
         begin
            Map.Include (Tag, Instance);
         end;
      else
         for J in Parameterless_Method_Name'Range loop
            if Command = Parameterless_Method_Name (J).all then
               Method := J;
               exit;
            end if;
         end loop;

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_Tag_Handler_Class (Data));
            Writer : constant Markup_Generator_Access := Get_Writer (Instance);
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
   end Command_Handler;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return String is
      Data : Instance_Property;
   begin
      if Instance /= No_Class_Instance then
         Data := Get_Data (Instance, Tag_Handler_Class_Name);
      end if;

      if Data = null then
         return "";
      else
         return To_String (Tag_Handler_Properties_Record (Data.all).Tag);
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Writer
     (Instance : Class_Instance) return Markup_Generator_Access
   is
      Data : Instance_Property;
   begin
      if Instance /= No_Class_Instance then
         Data := Get_Data (Instance, Tag_Handler_Class_Name);
      end if;

      if Data = null then
         return null;
      else
         return Tag_Handler_Properties_Record (Data.all).Writer;
      end if;
   end Get_Writer;

   ---------------------------
   -- Get_Tag_Handler_Class --
   ---------------------------

   function Get_Tag_Handler_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Tag_Handler_Class_Name);
   end Get_Tag_Handler_Class;

   ---------------------------
   -- Get_Tag_Handler_Class --
   ---------------------------

   function Get_Tag_Handler_Class
     (Data : Callback_Data'Class) return Class_Type is
   begin
      return New_Class (Data.Get_Repository, Tag_Handler_Class_Name);
   end Get_Tag_Handler_Class;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Is_Supported;
   --------------
   -- On_Match --
   --------------

   procedure On_Match
     (Writer : in out Markup_Generator;
      Tag    : String;
      Attrs  : String;
      Value  : String)
   is
      Pos  : constant Tag_Maps.Cursor := Map.Find (Tag);
      Inst : Class_Instance;
   begin
      if Tag_Maps.Has_Element (Pos) then
         Inst := Tag_Maps.Element (Pos);
      else
         return;
      end if;

      declare
         Args : Callback_Data'Class := Create
           (Get_Script (Inst), Arguments_Count => 2);
         Proc : Subprogram_Type := Get_Method (Inst, "on_match");
      begin
         Set_Writer (Inst, Writer'Unchecked_Access);
         Set_Nth_Arg (Args, 1, Attrs);
         Set_Nth_Arg (Args, 2, Value);

         declare
            Ignore : constant Any_Type := Proc.Execute (Args);
            pragma Unreferenced (Ignore);
         begin
            Free (Proc);
            Free (Args);
         end;
      end;
   end On_Match;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Class         => Get_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "emit_text",
         Class         => Get_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "emit_html",
         Class         => Get_Tag_Handler_Class (Kernel),
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Command_Handler'Access);

      for J of Parameterless_Method_Name loop
         Register_Command
           (Kernel.Scripts, J.all,
            Class         => Get_Tag_Handler_Class (Kernel),
            Minimum_Args  => 0,
            Maximum_Args  => 0,
            Handler       => Command_Handler'Access);
      end loop;

      Register_Command
        (Kernel.Scripts, "register_tag_handler",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Command_Handler'Access);
   end Register_Commands;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Instance : Class_Instance; Tag : String) is
   begin
      if not Is_Subclass (Instance, Tag_Handler_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, Tag_Handler_Class_Name,
         Tag_Handler_Properties_Record'(Tag => To_Unbounded_String (Tag),
                                        Writer => null));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Writer
     (Instance : Class_Instance;
      Writer   : Markup_Generator_Access)
   is
      Data : Instance_Property;
   begin
      if not Is_Subclass (Instance, Tag_Handler_Class_Name) then
         raise Invalid_Data;
      elsif Instance /= No_Class_Instance then
         Data := Get_Data (Instance, Tag_Handler_Class_Name);

         if Data /= null then
            Tag_Handler_Properties_Record (Data.all).Writer := Writer;
         end if;
      end if;
   end Set_Writer;

end GNATdoc.Customization.Tag_Handlers;
