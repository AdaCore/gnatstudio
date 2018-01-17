------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada;    use GNATCOLL.Scripts.Gtkada;

with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with Histories;
with Switches_Chooser.Gtkada;    use Switches_Chooser.Gtkada;
with Switches_Parser;            use Switches_Parser;
with XML_Utils;                  use XML_Utils;

package body Switches_Chooser.Scripts is

   Name_Cst     : aliased constant String := "name";
   Xml_Data_Cst : aliased constant String := "xmldata";

   function Dummy_Other_Config_Finder
     (Name : String) return Switches_Editor_Config;
   --  Dummy procedure for cross-config finder (unsupported for scripts)

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle commands for the SwitchesParser class

   -------------------------------
   -- Dummy_Other_Config_Finder --
   -------------------------------

   function Dummy_Other_Config_Finder
     (Name : String) return Switches_Editor_Config
   is
      pragma Unreferenced (Name);
   begin
      return null;
   end Dummy_Other_Config_Finder;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Class    : constant Class_Type :=
                   New_Class (Get_Kernel (Data), "SwitchesChooser");
      Inst     : constant Class_Instance := Nth_Arg (Data, 1, Class);
      Config   : Switches_Editor_Config;
      Editor   : Switches_Chooser.Gtkada.Switches_Editor;
      Error    : Unbounded_String;
      Xml      : XML_Utils.Node_Ptr;

   begin
      if Command = Constructor_Method then
         Name_Parameters
           (Data,
            ( --  1 => Self,
             2 => Name_Cst'Access,
             3 => Xml_Data_Cst'Access));

         Xml := XML_Utils.Parse_Buffer (Nth_Arg (Data, 3));
         Parse_Switches_Node
           (Current_Tool_Name   => Nth_Arg (Data, 2),
            Current_Tool_Config => Config,
            Error_Message       => Error,
            Finder              => Dummy_Other_Config_Finder'Access,
            Node                => Xml);
         Switches_Chooser.Gtkada.Gtk_New
           (Editor, Config,
            Use_Native_Dialogs => Use_Native_Dialogs.Get_Pref,
            Read_Only          => False,
            History            => null,
            Key                => Histories.No_Key,
            Cmd_Line_Tooltip   => Command_Line_Editor_Tooltip_Text);

         GNATCOLL.Scripts.Gtkada.Set_Data
           (Inst, GObject (Editor));

      else
         Editor := Switches_Chooser.Gtkada.Switches_Editor
           (GNATCOLL.Scripts.Gtkada.Get_Data (Inst));

         if Command = "set_cmd_line" then
            Editor.Set_Command_Line (Nth_Arg (Data, 2));

         elsif Command = "get_cmd_line" then
            declare
               Cmd_Line : constant String_List_Access :=
                            Editor.Get_Command_Line (False);
               Ret      : Unbounded_String;
            begin
               for J in Cmd_Line'Range loop
                  if J = Cmd_Line'First then
                     Ret := To_Unbounded_String (Cmd_Line (J).all);
                  else
                     Ret := Ret & " " & Cmd_Line (J).all;
                  end if;
               end loop;

               Set_Return_Value (Data, To_String (Ret));
            end;
         end if;
      end if;
   end Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class
        (Kernel, "SwitchesChooser", Get_GUI_Class (Kernel));
   begin
      Register_Command
        (Kernel, Constructor_Method, 2, 2, Class => Class,
         Handler                                 => Command_Handler'Access);
      Register_Command
        (Kernel, "set_cmd_line", 1, 1, Class => Class,
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel, "get_cmd_line", 0, 0, Class => Class,
         Handler => Command_Handler'Access);
   end Register_Module;

end Switches_Chooser.Scripts;
