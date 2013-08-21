------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;

with GNAT.OS_Lib;                     use GNAT.OS_Lib;

with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with Docgen3;                         use Docgen3;

with XML_Utils;                       use XML_Utils;
with XML_Parsers;

with GPS.Customizable_Modules;        use GPS.Customizable_Modules;
with GPS.Scripts;                     use GPS.Scripts;
with GPS.Scripts.Projects;            use GPS.Scripts.Projects;

package body GPS.CLI_Scripts is

   Xml_Cst               : aliased constant String := "xml";
   Xml_Custom_Parameters : constant Cst_Argument_List := (1 => Xml_Cst'Access);
   Docgen_Class_Name     : constant String := "Docgen";
   Skip_C_Files_Cst      : aliased constant String := "skip_c_files";
   Report_Errors_Cst     : aliased constant String := "report_errors";
   Tree_Output_Cst       : aliased constant String := "tree_output";
   With_Comments_Cst     : aliased constant String := "with_comments";
   Attribute_Cst         : aliased constant String := "attribute";
   Package_Cst           : aliased constant String := "package";
   Index_Cst             : aliased constant String := "index";
   Process_Parameters    : constant Cst_Argument_List :=
     (1 => Skip_C_Files_Cst'Access,
      2 => Report_Errors_Cst'Access,
      3 => Tree_Output_Cst'Access,
      4 => With_Comments_Cst'Access);
   Get_Attributes_Parameters : constant Cst_Argument_List :=
     (1 => Attribute_Cst'Unchecked_Access,
      2 => Package_Cst'Unchecked_Access,
      3 => Index_Cst'Unchecked_Access);

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for misc GPS.* commands

   function Get_Docgen_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Return class for Docgen

   procedure Docgen_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.Docgen.process_project command

   procedure Project_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.Project.get_attribute_as_* command

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
   begin
      if Command = "get_share_dir" then
         Set_Return_Value (Data, +Kernel.Get_Share_Dir.Full_Name);

      elsif Command = "parse_xml" then
         Name_Parameters (Data, Xml_Custom_Parameters);

         declare
            File : constant Filesystem_String :=
              +Current_Script (Get_Script (Data));
            Node : Node_Ptr;
            Err  : GNAT.OS_Lib.String_Access;
         begin
            XML_Parsers.Parse_Buffer
              (Buffer     => Nth_Arg (Data, 1),
               From_File  => File,
               Start_Line => 1,
               Tree       => Node,
               Error      => Err);

            if Node /= null then
               Execute_Customization_String
                 (Kernel => Kernel,
                  File   => Create (File),
                  Node   => Node.Child,
                  Level  => Hard_Coded);

            elsif Err.all /= "" then
               Set_Error_Msg (Data, Err.all);
            end if;

            exception
               when others =>
                  Set_Error_Msg (Data, "Error while executing parse_xml()");
         end;
      end if;
   end Command_Handler;

   ----------------------------
   -- Docgen_Command_Handler --
   ----------------------------

   procedure Docgen_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
   begin
      if Command = "process_project" then
         Name_Parameters (Data, Process_Parameters);

         declare
            Skip_C_Files  : constant Boolean := Nth_Arg (Data, 1, False);
            Report_Errors : constant String := Nth_Arg (Data, 2, "None");
            Tree_Output   : constant String := Nth_Arg (Data, 3, "None");
            With_Comments : constant Boolean := Nth_Arg (Data, 4, False);

            Options : constant Docgen3.Docgen_Options :=
              (Comments_Filter => null,
               Report_Errors   => Report_Errors_Kind'Value (Report_Errors),
               Skip_C_Files    => Skip_C_Files,
               Tree_Output     => (Tree_Output_Kind'Value (Tree_Output),
                                   With_Comments),
               Display_Time    => False,
               Process_Bodies  => False);
         begin
            Docgen3.Process_Project_Files
              (Kernel    => Kernel,
               Options   => Options,
               Project   => Kernel.Registry.Tree.Root_Project,
               Recursive => False);
         exception
            when others =>
               Set_Error_Msg (Data, "Error while executing "  & Command);
         end;
      end if;
   end Docgen_Command_Handler;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "get_attribute_as_list" then
         Name_Parameters (Data, Get_Attributes_Parameters);
         declare
            Project : constant Project_Type := Get_Data (Data, 1);
            Attr    : constant String := Nth_Arg (Data, 2);
            Pkg     : constant String := Nth_Arg (Data, 3, "");
            Index   : constant String := Nth_Arg (Data, 4, "");
            List    : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
            Value   : constant String := Project.Attribute_Value
                   (Attribute_Pkg_String'(Build (Pkg, Attr)),
                   Default => "", Index => Index);
         begin
            Set_Return_Value_As_List (Data);

            if List = null and then Value /= "" then
               Set_Return_Value (Data, Value);
            elsif List /= null then
               for L in List'Range loop
                  Set_Return_Value (Data, List (L).all);
               end loop;
            end if;

            Free (List);
         end;
      elsif Command = "get_attribute_as_string" then
         Name_Parameters (Data, Get_Attributes_Parameters);
         declare
            Project : constant Project_Type := Get_Data (Data, 1);
            Attr    : constant String := Nth_Arg (Data, 2);
            Pkg     : constant String := Nth_Arg (Data, 3, "");
            Index   : constant String := Nth_Arg (Data, 4, "");
            Value   : constant String := Project.Attribute_Value
                   (Attribute_Pkg_String'(Build (Pkg, Attr)),
                   Default => "", Index => Index);
         begin
            if Value = "" then
               declare
                  Result : Unbounded_String;
                  List   : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
               begin
                  if List /= null then
                     for L in List'Range loop
                        Append (Result, List (L).all);

                        if L /= List'Last then
                           Append (Result, " ");
                        end if;
                     end loop;

                     Free (List);
                  end if;

                  Set_Return_Value (Data, To_String (Result));
               end;
            else
               Set_Return_Value (Data, Value);
            end if;
         end;
      end if;
   end Project_Command_Handler;

   ----------------------
   -- Get_Docgen_Class --
   ----------------------

   function Get_Docgen_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Docgen_Class_Name);
   end Get_Docgen_Class;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, "get_share_dir",
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "parse_xml",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "process_project",
         Class         => Get_Docgen_Class (Kernel),
         Static_Method => True,
         Minimum_Args  => 0,
         Maximum_Args  => 4,
         Handler       => Docgen_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "get_attribute_as_string",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "get_attribute_as_list",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
   end Register_Commands;

end GPS.CLI_Scripts;
