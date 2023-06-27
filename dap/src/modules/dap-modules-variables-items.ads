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

with Ada.Containers.Vectors;
with VSS.Strings;                 use VSS.Strings;

with GPS.Kernel;

with DAP.Tools;                   use DAP.Tools;

package DAP.Modules.Variables.Items is

   type Item_ID is new Natural;
   Unknown_Id : constant Item_ID := 0;

   type Value_Format is (Default, Hexadecimal);

   Default_Format : constant DAP.Tools.ValueFormat :=
     DAP.Tools.ValueFormat'(others => False);

   function Image (Format : DAP.Tools.ValueFormat) return String;
   function Convert (Format : DAP.Tools.ValueFormat) return Value_Format;
   function Convert (Format : Value_Format) return DAP.Tools.ValueFormat;

   type Item_Info is tagged record
      Id           : Item_ID;   --  unique id
      Varname      : Virtual_String; --  tree display varname
      Cmd_Name     : Virtual_String; --  tree display command name
      Cmd          : Virtual_String; --  tree display `cmd`
      Split_Lines  : Boolean;        --  for commands
      Auto_Refresh : Boolean := True;
      Format       : DAP.Tools.ValueFormat := Default_Format;
   end record;

   function Get_Name (Self : Item_Info) return Virtual_String;
   function Get_Name (Self : Item_Info) return String;
   --  Return the display name for this item

   function Is_Same (Info : Item_Info; Name : Virtual_String) return Boolean;

   No_Item_Info : constant Item_Info :=
     Item_Info'(Id           => Unknown_Id,
                Varname      => Empty_Virtual_String,
                Cmd_Name     => Empty_Virtual_String,
                Cmd          => Empty_Virtual_String,
                Split_Lines  => False,
                Auto_Refresh => False,
                Format       => <>);

   package Item_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Item_Info);

   type Context_Item_Info is new GPS.Kernel.Context_Item with record
      Info : Item_Info;
   end record;
   type Context_Item_Info_Access is access all Context_Item_Info'Class;

end DAP.Modules.Variables.Items;
