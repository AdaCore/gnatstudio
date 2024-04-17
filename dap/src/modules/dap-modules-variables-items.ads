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

with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

with GNATCOLL.JSON;

with VSS.Strings;                 use VSS.Strings;

with DAP.Types;                   use DAP.Types;
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

   type Item_Info is abstract tagged record
      Id           : Item_ID;   --  unique id
      Auto_Refresh : Boolean := True;
      Format       : DAP.Tools.ValueFormat := Default_Format;
   end record;
   type Item_Info_Access is access all Item_Info'Class;

   function Get_Name (Self : Item_Info) return Virtual_String is abstract;
   function Get_Name (Self : Item_Info'Class) return String;
   --  Return the display name for this item

   function Get_Format (Self : Item_Info) return String;
   --  Returns the format name

   function Is_Same_Name
     (Info : Item_Info'Class;
      Name : Virtual_String)
      return Boolean;
   --  Returns True when the Item's name equal the Name

   function Is_Command (Info : Item_Info) return Boolean;
   --  Returns True if the item corresponds to a command

   function Is_Arguments (Info : Item_Info) return Boolean;
   --  Returns True if the item corresponds to "arguments" item that is used
   --  for "display arguments" action

   function Is_No_Item (Info : Item_Info) return Boolean;
   --  Returns True if the item has no data and used to indicate that
   --  it should not be processed.

   procedure Find_DAP_Item
     (Info  : Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean) is abstract;
   --  Find the corresponding DAP item and returns a cursor to it.
   --  Returns No_Element if Item or parent are not found. Found is set to
   --  True if exact element is found and to False if parent element is found.

   procedure Store
     (Info  : Item_Info;
      Value : in out GNATCOLL.JSON.JSON_Value) is abstract;
   --  Store internal values

   function Restore (Value : GNATCOLL.JSON.JSON_Value) return Item_Info'Class;
   --  Load internal values and return Item

   function Create
     (Variable    : VSS.Strings.Virtual_String := "";
      Command     : VSS.Strings.Virtual_String := "";
      Split_Lines : Boolean := False;
      Arguments   : Boolean := False;
      Format      : DAP.Tools.ValueFormat := Default_Format)
      return Item_Info'Class;
   --  Returns corresponding Item_Info

   -----------------
   -- Item_Holder --
   -----------------

   type Item_Holder is new Ada.Finalization.Controlled with record
      Info : Item_Info_Access := null;
   end record;
   --  To store Item_Info'Class instance in the context

   overriding procedure Adjust (Object : in out Item_Holder);
   overriding procedure Finalize (Object : in out Item_Holder);

   procedure Set (Self : in out Item_Holder; Item : Item_Info'Class);
   --  Set Item to the Holder

   -----------------------
   -- Item_Info_Vectors --
   -----------------------

   package Item_Info_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Item_Info'Class);

   ------------------
   -- No_Item_Info --
   ------------------

   type No_Item_Info is new Item_Info with null record;

   overriding function Get_Name
     (Self : No_Item_Info) return Virtual_String is ("");

   overriding procedure Find_DAP_Item
     (Info  : No_Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean);

   overriding function Get_Format (Self : No_Item_Info) return String;

   overriding function Is_No_Item (Info : No_Item_Info) return Boolean;

   overriding procedure Store
     (Info  : No_Item_Info;
      Value : in out GNATCOLL.JSON.JSON_Value) is null;

   No_Item : constant No_Item_Info :=
     No_Item_Info'(Id           => Unknown_Id,
                   Auto_Refresh => False,
                   Format       => <>);

end DAP.Modules.Variables.Items;
