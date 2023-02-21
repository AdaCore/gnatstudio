------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPS.Kernel;

package DAP.Modules.Variables.Items is

   type Value_Format is (Default_Format, Decimal, Binary, Hexadecimal, Octal);

   type Item_Info is tagged record
      Varname      : Unbounded_String;       --  tree display varname
      Cmd_Name     : Unbounded_String;       --  tree display command name
      Cmd          : Unbounded_String;       --  tree display `cmd`
--      Entity       : GVD_Type_Holder;        --  parsed type info
      Split_Lines  : Boolean;                --  for commands
      Auto_Refresh : Boolean := True;
      Format       : Value_Format := Default_Format;
   end record;

   No_Item_Info : constant Item_Info :=
     Item_Info'(Varname      => Null_Unbounded_String,
                Cmd_Name     => Null_Unbounded_String,
                Cmd          => Null_Unbounded_String,
--                Entity       => <>,
                Split_Lines  => False,
                Auto_Refresh => False,
                Format       => <>);

   type Context_Item_Info is new GPS.Kernel.Context_Item with record
      Info : Item_Info;
   end record;
   type Context_Item_Info_Access is access all Context_Item_Info'Class;

end DAP.Modules.Variables.Items;
