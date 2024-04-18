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

package DAP.Modules.Variables.Items.Commands is

   type Command_Item_Info is new Item_Info with record
      Cmd         : Virtual_String; --  tree display `cmd`
      Split_Lines : Boolean;
   end record;

   overriding function Get_Name
     (Self : Command_Item_Info) return Virtual_String;

   overriding function Is_Command (Info : Command_Item_Info) return Boolean;

   overriding procedure Find_DAP_Item
     (Info  : Command_Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean);

   overriding procedure Store
     (Info  : Command_Item_Info;
      Value : in out GNATCOLL.JSON.JSON_Value);

   function Load (Value : GNATCOLL.JSON.JSON_Value) return Item_Info'Class;

   function Create
     (Command     : VSS.Strings.Virtual_String;
      Split_Lines : Boolean;
      Format      : DAP.Tools.ValueFormat)
      return Item_Info'Class;

end DAP.Modules.Variables.Items.Commands;
