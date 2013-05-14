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

with GNAT.OS_Lib;                     use GNAT.OS_Lib;

with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with XML_Utils;                       use XML_Utils;
with XML_Parsers;

with GPS.Customizable_Modules;        use GPS.Customizable_Modules;
with GPS.Scripts;                     use GPS.Scripts;

package body GPS.CLI_Scripts is

   Xml_Cst               : aliased constant String := "xml";
   Xml_Custom_Parameters : constant Cst_Argument_List := (1 => Xml_Cst'Access);

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
   begin
      if Command = "parse_xml" then
         Name_Parameters (Data, Xml_Custom_Parameters);

         declare
            File : constant Filesystem_String :=
              +Current_Script (Get_Script (Data));
            Node : Node_Ptr;
            Err  : String_Access;
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

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, "parse_xml",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Command_Handler'Access);
   end Register_Commands;

end GPS.CLI_Scripts;
