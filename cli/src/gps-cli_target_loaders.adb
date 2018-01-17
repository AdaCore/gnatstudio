------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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
with Build_Command_Utils;             use Build_Command_Utils;
with Build_Configurations;            use Build_Configurations;

package body GPS.CLI_Target_Loaders is

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Self   : access Target_Loader;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File);

      Target    : Target_Access;
      pragma Unreferenced (Target);
      Mode      : Mode_Record;
      pragma Unreferenced (Mode);

      From_User : constant Boolean := Level = User_Specific;
      Builder   : constant Builder_Context :=
        Builder_Context (Self.Kernel.Module (Builder_Context_Record'Tag));
   begin
      if Node.Tag.all = "target" then
         Target := Load_Target_From_XML (Builder.Registry, Node, From_User);

      elsif Node.Tag.all = "target-model" then
         Create_Model_From_XML (Builder.Registry, Node);

      elsif Node.Tag.all = "builder-mode" then
         Mode := Load_Mode_From_XML (Builder.Registry, Node);
      end if;
   end Customize;

end GPS.CLI_Target_Loaders;
