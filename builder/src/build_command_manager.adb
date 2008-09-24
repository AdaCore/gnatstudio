-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Intl;           use GPS.Intl;

with Traces;             use Traces;

package body Build_Command_Manager is

   Me : constant Debug_Handle := Create ("Build_Command_Manager");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   function Expand_Command_Line
     (Kernel : GPS.Kernel.Kernel_Handle;
      CL     : Argument_List) return Argument_List_Access;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.

   function Expand_Arg
     (Kernel : GPS.Kernel.Kernel_Handle;
      Arg    : String) return Argument_List;
   --  Expand macros contained in Arg.
   --  Caller must free the result.

   ----------------
   -- Expand_Arg --
   ----------------

   function Expand_Arg
     (Kernel : GPS.Kernel.Kernel_Handle;
      Arg    : String) return Argument_List is
      pragma Unreferenced (Kernel);
   begin
      --  ??? this is a dummy implementation, connect to the proper module!
      return (1 => new String'(Arg));
   end Expand_Arg;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Kernel : GPS.Kernel.Kernel_Handle;
      CL     : Argument_List) return Argument_List_Access
   is
      Result : Argument_List_Access := new Argument_List (1 .. CL'Length * 2);

      Index : Natural := 1;
      --  Index of the next free element in Result.

   begin

      for J in CL'Range loop
         if CL (J) = null then
            --  This should not happen
            Insert (Kernel, (-"Invalid command line"));
            Free (Result);
            return null;
         end if;

         declare
            Expanded : constant Argument_List :=
              Expand_Arg (Kernel, CL (J).all);
         begin
            --  Expand the result if needed
            if Result'Last - Index < Expanded'Length then
               declare
                  New_Result : constant Argument_List_Access :=
                    new Argument_List
                      (1 .. (Result'Length + Expanded'Length) * 2);
               begin
                  for K in 1 .. Index - 1 loop
                     New_Result (K) := Result (K);
                  end loop;
                  Unchecked_Free (Result);
                  Result := New_Result;
               end;
            end if;

            Result (Index .. Index + Expanded'Length - 1) := Expanded;

            Index := Index + Expanded'Length;
         end;
      end loop;

      declare
         Real_Result : Argument_List_Access;
      begin
         Real_Result := new Argument_List (1 .. Index - 1);
         Real_Result (1 .. Index - 1) := Result (1 .. Index - 1);
         Unchecked_Free (Result);
         return Real_Result;
      end;
   end Expand_Command_Line;

   -------------------
   -- Launch_Target --
   -------------------

   procedure Launch_Target
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String)
   is
      T    : Target_Access;
      Full : Argument_List_Access;
   begin
      --  Get the target

      T := Get_Target_From_Name (Registry, Target_Name);

      if T = null then
         --  This should never happen
         Insert
           (Kernel, (-"Build target not found in registry: ") & Target_Name);
         return;
      end if;

      --  Get the unexpanded command line

      declare
         --  ??? the mode string is left empty, as support for modes is not
         --  implemented yet
         CL : constant Argument_List :=
           Get_Command_Line_Unexpanded (Registry, "", T);
      begin
         --  Sanity check that the command line contains at least one element
         --  (the command itself). It can happen that this is not the case
         --  if the user has modified the command by hand.

         if CL'Length = 0 then
            Insert
              (Kernel, -"Command line is empty for target: " & Target_Name);
            return;
         end if;

         --  Expand the command line

         Full := Expand_Command_Line (Kernel, CL);
      end;

      --  Trace the command line, for debug purposes
      if Full = null then
         Trace (Me, "Macro expansion resulted in empty command line");
      else
         for J in Full'Range loop
            Trace (Me, "Arg: """ & Full (J).all & """");
         end loop;
      end if;

      --  Create the build command

      --  Launch the build command
   end Launch_Target;

end Build_Command_Manager;
