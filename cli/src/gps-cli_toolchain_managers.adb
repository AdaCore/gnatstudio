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

with GNAT.Expect;

with GNATCOLL.Arg_Lists;              use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with Remote;                          use Remote;

package body GPS.CLI_Toolchain_Managers is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self              : Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False)
      return String
   is
      pragma Unreferenced (Timeout_MS);
      pragma Unreferenced (Handle_GUI_Events);

      Result : aliased Unbounded_String;
      Parser : constant Tools_Output_Parser_Access := new Output_To_String'
        (Child => null, Result => Result'Unchecked_Access);

      Success : Boolean;
      Args    : constant Arg_List :=
                  GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args);

   begin
      Self.Kernel.Process_Launcher.Launch_Process
        (CL                   => Args,
         Server               => Build_Server,
         Output_Parser        => Parser,
         Success              => Success);

      if Success then
         return To_String (Result);
      else
         raise GNAT.Expect.Process_Died;
      end if;
   end Execute;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Output_To_String;
      Item    : String;
      Command : Command_Access)
   is
      pragma Unreferenced (Command);
   begin
      Append (Self.Result.all, Item);
   end Parse_Standard_Output;

end GPS.CLI_Toolchain_Managers;
