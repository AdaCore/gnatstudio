------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2012-2019, AdaCore                   --
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

with GNATCOLL.Arg_Lists;     use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GPS.Kernel.Console;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Timeout;     use GPS.Kernel.Timeout;
with GPS.Kernel;
with GPS.Scripts.Commands;   use GPS.Scripts.Commands;
with Gtkada.MDI;
with Src_Editor_Module;

package body Src_Printing.Command_Printer is

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (This       : Printer;
      Editor     : Src_Editor_Box.Source_Editor_Box;
      From       : Editable_Line_Type := 1;
      To         : Editable_Line_Type := Editable_Line_Type'Last)
   is
      pragma Unreferenced (From, To);

      Print_Helper : constant String :=
        Ada.Strings.Unbounded.To_String (This.Command);

      Kernel  : constant GPS.Kernel.Kernel_Handle := Editor.Get_Kernel;
      Child   : constant Gtkada.MDI.MDI_Child :=
        Src_Editor_Module.Find_Current_Editor (Kernel);
      Success   : Boolean;
      Scheduled : Scheduled_Command_Access;
      CL        : Arg_List;
   begin
      if GPS.Kernel.MDI.Save_MDI_Children
        (Kernel,
         Children => (1 => Child),
         Force    => Auto_Save.Get_Pref)
      then
         CL := Parse_String (Print_Helper, Separate_Args);
         Append_Argument
           (CL, +Full_Name (Editor.Get_Filename), One_Arg);

         Launch_Process
           (Kernel    => Kernel,
            CL        => CL,
            Console   => GPS.Kernel.Console.Get_Console (Kernel),
            Scheduled => Scheduled,
            Success   => Success);
      end if;
   end Print;

   ------------
   -- Create --
   ------------

   function Create (Command : String) return Printer is
      use Ada.Strings.Unbounded;
   begin
      return (Abstract_Printer with Command => To_Unbounded_String (Command));
   end Create;

end Src_Printing.Command_Printer;
