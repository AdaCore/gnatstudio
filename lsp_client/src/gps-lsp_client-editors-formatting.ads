------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2023, AdaCore                  --
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

--  Integration with GNAT Studio's source editor formatting

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with GPS.Editors;                use GPS.Editors;
with GPS.Kernel;                 use GPS.Kernel;
with Src_Editor_Buffer.Formatters;

package GPS.LSP_Client.Editors.Formatting is

   procedure Register_Module (Kernel : Kernel_Handle);
   --  The support can be deactivated for Ada by setting For_Ada to False.

   type LSP_Editor_Formatting_Provider is
     new Src_Editor_Buffer.Formatters.Formatting_Provider
   with private;

private

   package Triggers_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (String,
        Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set);
   --  Map of language to trigger character set

   type LSP_Editor_Formatting_Provider is
     new Src_Editor_Buffer.Formatters.Formatting_Provider
   with record
      Kernel                : Kernel_Handle;
      Lang_To_Trigger_Chars : Triggers_Maps.Map;
   end record;

   overriding function On_Range_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural;
      Cursor_Move : in out Integer)
      return Boolean;

   overriding function On_Type_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural)
      return Boolean;

end GPS.LSP_Client.Editors.Formatting;
