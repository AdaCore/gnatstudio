-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System.OS_Lib;              use System.OS_Lib;
with GPS.Kernel;                 use GPS.Kernel;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Intl;                   use GPS.Intl;

package body Docgen2.Scripts is

   Me : constant Trace_Handle := Create ("Docgen2.Hooks");

   Docgen_Class_Name     : constant String := "Docgen";
   Docgen_Tag_Class_Name : constant String := "DocgenTagHandler";

   type Docgen_Property is new Instance_Property_Record with record
      Doc_Generator : Docgen_Object;
   end record;
   type Docgen_Property_Access is access all Docgen_Property'Class;

   type Custom_Tag_Property is new Instance_Property_Record with record
      Tag      : String_Access;
      Inst     : Class_Instance;
      On_Start : Subprogram_Type;
      On_Match : Subprogram_Type;
      On_Exit  : Subprogram_Type;
   end record;
   type Custom_Tag_Property_Access is access all Custom_Tag_Property'Class;

   package Custom_Tag_Vectors is new Ada.Containers.Vectors
     (Natural, Custom_Tag_Property);
   Custom_Tags  : Custom_Tag_Vectors.Vector;

   Custom_CSS_Files : Custom_CSS_File_Vectors.Vector;

   Tag_Cst      : aliased constant String := "tag";
   On_Start_Cst : aliased constant String := "on_start";
   On_Match_Cst : aliased constant String := "on_match";
   On_Exit_Cst  : aliased constant String := "on_exit";
   Handler_Cst  : aliased constant String := "handler";
   Name_Cst     : aliased constant String := "name";
   Filename_Cst : aliased constant String := "filename";
   Content_Cst  : aliased constant String := "content";

   Handler_Constructor_Args : constant Cst_Argument_List :=
                                (2  => Tag_Cst'Access,
                                 3  => On_Start_Cst'Access,
                                 4  => On_Match_Cst'Access,
                                 5  => On_Exit_Cst'Access);

   Register_Args            : constant Cst_Argument_List :=
                                (1 => Handler_Cst'Access);

   Index_Generator_Args     : constant Cst_Argument_List :=
                                (2 => Name_Cst'Access,
                                 3 => Filename_Cst'Access,
                                 4 => Content_Cst'Access);

   function Get_Docgen_Class (Script : Scripts_Repository) return Class_Type;
   --  Get the Docgen class type

   function Get_Docgen_Tag_Handler_Class
     (Script : Scripts_Repository) return Class_Type;
   --  Get the HandlerTag class type

   function Gen_New_Docgen_Inst
     (Script        : access Scripting_Language_Record'Class;
      Doc_Generator : Docgen_Object) return Class_Instance;
   --  Create a new GPS.Docgen instance

   procedure Docgen_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Main handler for Docgen class

   procedure Custom_Tag_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Main handler for Docgen.TagHandler class

   ----------------------
   -- Get_Docgen_Class --
   ----------------------

   function Get_Docgen_Class (Script : Scripts_Repository) return Class_Type is
   begin
      return New_Class (Script, Docgen_Class_Name);
   end Get_Docgen_Class;

   ----------------------------------
   -- Get_Docgen_Tag_Handler_Class --
   ----------------------------------

   function Get_Docgen_Tag_Handler_Class
     (Script : Scripts_Repository) return Class_Type is
   begin
      return New_Class (Script, Docgen_Tag_Class_Name);
   end Get_Docgen_Tag_Handler_Class;

   ------------------------
   -- Custom_Tag_Handler --
   ------------------------

   procedure Custom_Tag_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Handler_Constructor_Args);

         declare
            Inst            : constant Class_Instance :=
                                Nth_Arg
                                  (Data, 1,
                                   Get_Docgen_Tag_Handler_Class
                                     (Get_Repository (Data)));
            Handled_Tag     : constant String :=
                                Nth_Arg (Data, 2);
            Property        : Custom_Tag_Property;

         begin
            if Handled_Tag = "" then
               Set_Error_Msg (Data, -"Argument for tag cannot be empty");
               return;
            end if;

            Property :=
              (Tag      => new String'(Handled_Tag),
               Inst     => Inst,
               On_Start => Nth_Arg (Data, 3, null),
               On_Match => Nth_Arg (Data, 4, null),
               On_Exit  => Nth_Arg (Data, 5, null));
            Set_Data (Inst, Docgen_Tag_Class_Name, Property);
         end;
      end if;
   end Custom_Tag_Handler;

   -------------------------
   -- Gen_New_Docgen_Inst --
   -------------------------

   function Gen_New_Docgen_Inst
     (Script        : access Scripting_Language_Record'Class;
      Doc_Generator : Docgen_Object) return Class_Instance
   is
      Inst     : constant Class_Instance :=
                   New_Instance (Script,
                                 Get_Docgen_Class (Get_Repository (Script)));
      Property : Docgen_Property;

   begin
      Property := (Doc_Generator => Doc_Generator);
      Set_Data (Inst, Docgen_Class_Name, Property);
      return Inst;
   end Gen_New_Docgen_Inst;

   --------------------
   -- Docgen_Handler --
   --------------------

   procedure Docgen_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Prop   : Custom_Tag_Property_Access;
   begin
      if Command = "register_css" then
         declare
            Filename : constant String := Nth_Arg (Data, 1);
            File     : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Create (Filename);
         begin
            if not Is_Regular_File (File) then
               Set_Error_Msg
                 (Data, -"Could not find file " & File.Full_Name.all);
               return;
            end if;

            Custom_CSS_Files.Append (File);
         end;

      elsif Command = "register_tag_handler" then
         Name_Parameters (Data, Register_Args);
         declare
            Inst : constant Class_Instance :=
                     Nth_Arg
                       (Data, 1,
                        Get_Docgen_Tag_Handler_Class
                          (Get_Repository (Data)),
                        Allow_Null => True);
         begin
            if Inst = No_Class_Instance then
               Set_Error_Msg
                 (Data, -"handler object must be initialized");
               return;
            end if;

            Prop := Custom_Tag_Property_Access
              (GNATCOLL.Scripts.Get_Data (Inst, Docgen_Tag_Class_Name));

            for J in Custom_Tags.First_Index .. Custom_Tags.Last_Index loop
               if Custom_Tags.Element (J).Tag.all = Prop.Tag.all then
                  Custom_Tags.Replace_Element
                    (J, Custom_Tag_Property (Prop.all));
                  return;
               end if;
            end loop;

            Custom_Tags.Append (Custom_Tag_Property (Prop.all));
         end;

      elsif Command = "generate_index_file" then
         Name_Parameters (Data, Index_Generator_Args);
         declare
            Inst : constant Class_Instance :=
                     Nth_Arg
                       (Data, 1,
                        Get_Docgen_Class (Get_Repository (Data)),
                        Allow_Null => True);
            Prop : constant Docgen_Property_Access :=
                     Docgen_Property_Access
                       (Get_Data (Inst, Docgen_Class_Name));
         begin
            Generate_Custom_Docgen_File
              (Prop.Doc_Generator,
               Name     => Nth_Arg (Data, 2),
               Filename => Nth_Arg (Data, 3),
               Content  => Nth_Arg (Data, 4));
         end;

      elsif Command = "get_current_file" then
         declare
            Inst : constant Class_Instance :=
                     Nth_Arg
                       (Data, 1,
                        Get_Docgen_Class (Get_Repository (Data)),
                        Allow_Null => True);
            Prop : constant Docgen_Property_Access :=
                     Docgen_Property_Access
                       (Get_Data (Inst, Docgen_Class_Name));
         begin
            Set_Return_Value
              (Data,
               Create_File
                 (Get_Script (Data),
                  Get_Current_File (Prop.Doc_Generator)));
         end;
      end if;
   end Docgen_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Docgen_Class      : constant Class_Type :=
                            Get_Docgen_Class (Get_Scripts (Kernel));
      Tag_Handler_Class : constant Class_Type :=
                            Get_Docgen_Tag_Handler_Class
                              (Get_Scripts (Kernel));
   begin
      Register_Command
        (Kernel, "register_css",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Docgen_Class,
         Handler       => Docgen_Handler'Access,
         Static_Method => True);
      Register_Command
        (Kernel, "register_tag_handler",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Docgen_Class,
         Handler       => Docgen_Handler'Access,
         Static_Method => True);
      Register_Command
        (Kernel, "generate_index_file",
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => Docgen_Class,
         Handler       => Docgen_Handler'Access);
      Register_Command
        (Kernel, "get_current_file",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Docgen_Class,
         Handler      => Docgen_Handler'Access);
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 5,
         Class        => Tag_Handler_Class,
         Handler      => Custom_Tag_Handler'Access);
   end Register_Commands;

   --------------------------
   -- Get_Custom_CSS_Files --
   --------------------------

   function Get_Custom_CSS_Files return Custom_CSS_File_Vectors.Vector is
   begin
      return Custom_CSS_Files;
   end Get_Custom_CSS_Files;

   -------------------
   -- Is_Custom_Tag --
   -------------------

   function Is_Custom_Tag (Tag : String) return Boolean is
   begin
      --  Look into the registered custom tag handlers list to see if tag
      --  matches one of them.
      for J in Custom_Tags.First_Index .. Custom_Tags.Last_Index loop
         if Custom_Tags.Element (J).Tag.all = Tag then
            return True;
         end if;
      end loop;

      return False;
   end Is_Custom_Tag;

   ----------------------------
   -- On_Documentation_Start --
   ----------------------------

   procedure On_Documentation_Start (Object : Docgen_Object)
   is
      D : Custom_Tag_Property;
   begin
      --  For each registered custom tag handler
      for J in Custom_Tags.First_Index .. Custom_Tags.Last_Index loop
         --  If on_start is defined, run it.
         if Custom_Tags.Element (J).On_Start /= null then
            D := Custom_Tags.Element (J);

            declare
               Inst : constant Class_Instance :=
                        Gen_New_Docgen_Inst
                          (Get_Script (D.Inst), Object);
               C    : Callback_Data'Class :=
                        Create (Get_Script (D.Inst), Arguments_Count => 1);
               Tmp  : Boolean;
               pragma Unreferenced (Tmp);
            begin
               Set_Nth_Arg (C, 1, Inst);
               Tmp := Execute (D.On_Start, C);
               Free (C);
            end;
         end if;
      end loop;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Documentation_Start;

   -------------------------------
   -- On_Documentation_Finished --
   -------------------------------

   procedure On_Documentation_Finished (Object : Docgen_Object)
   is
      D : Custom_Tag_Property;
   begin
      --  For each registered custom tag handler
      for J in Custom_Tags.First_Index .. Custom_Tags.Last_Index loop
         --  If on_exit is defined, run it.
         if Custom_Tags.Element (J).On_Exit /= null then
            D := Custom_Tags.Element (J);

            declare
               Inst : constant Class_Instance :=
                        Gen_New_Docgen_Inst
                          (Get_Script (D.Inst), Object);
               C : Callback_Data'Class := Create
                 (Get_Script (D.Inst), Arguments_Count => 1);
               Tmp  : Boolean;
               pragma Unreferenced (Tmp);
            begin
               Set_Nth_Arg (C, 1, Inst);
               Tmp := Execute (D.On_Exit, C);
               Free (C);
            end;
         end if;
      end loop;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Documentation_Finished;

   -------------------
   -- On_Custom_Tag --
   -------------------

   function On_Custom_Tag
     (Object      : Docgen_Object;
      Tag         : String;
      Attrs       : String;
      Value       : String;
      Entity_Name : String;
      Entity_Href : String) return String
   is
      D : Custom_Tag_Property;
   begin
      for J in Custom_Tags.First_Index .. Custom_Tags.Last_Index loop
         if Custom_Tags.Element (J).Tag.all = Tag
           and then Custom_Tags.Element (J).On_Match /= null
         then
            D := Custom_Tags.Element (J);

            declare
               Inst : constant Class_Instance :=
                        Gen_New_Docgen_Inst
                          (Get_Script (D.Inst), Object);
               C : Callback_Data'Class := Create
                 (Get_Script (D.Inst), Arguments_Count => 5);
            begin
               Set_Nth_Arg (C, 1, Inst);
               Set_Nth_Arg (C, 2, Attrs);
               Set_Nth_Arg (C, 3, Value);
               Set_Nth_Arg (C, 4, Entity_Name);
               Set_Nth_Arg (C, 5, Entity_Href);

               declare
                  Tmp : constant String :=
                          Execute (D.On_Match, C);
               begin
                  Free (C);

                  return Tmp;
               end;
            end;
         end if;
      end loop;

      return "";

   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end On_Custom_Tag;

end Docgen2.Scripts;
