-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
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

with Gtk.Widget; use Gtk.Widget;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gtk.Enums;  use Gtk.Enums;

package body Language.Icons is

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics (Widget : Gtk_Widget) is

      function R (Id : String) return Gdk_Pixbuf;
      --  Convenience function: create the Gdk_Pixbuf from stock Id

      function Predefined_Array (Suffix : String) return Cat_Array;
      --  Convenience function to produce the predefined entity graphics

      -------
      -- R --
      -------

      function R (Id : String) return Gdk_Pixbuf is
      begin
         return Render_Icon (Widget, Id, Icon_Size_Menu);
      end R;

      ----------------------
      -- Predefined_Array --
      ----------------------

      function Predefined_Array (Suffix : String) return Cat_Array is
      begin
         return
           (Cat_Unknown | Cat_With
            | Cat_Use   | Cat_Include
            | Construct_Category | Cat_Exception_Handler =>
              R ("gps-entity-generic" & Suffix),
            Cat_Package | Cat_Namespace | Cat_Custom =>
              R ("gps-entity-package" & Suffix),
            Cat_Task        | Cat_Procedure   | Cat_Function
            | Cat_Method    | Cat_Constructor | Cat_Destructor
            | Cat_Protected | Cat_Entry =>
              R ("gps-entity-subprogram" & Suffix),
            Cat_Class | Cat_Structure | Cat_Union |
            Cat_Type  | Cat_Subtype | Cat_Case_Inside_Record =>
              R ("gps-entity-type" & Suffix),
            Cat_Variable    | Cat_Local_Variable
            | Cat_Parameter | Cat_Field
            | Cat_Literal   | Cat_Representation_Clause =>
              R ("gps-entity-variable" & Suffix));
      end Predefined_Array;

   begin
      --  If initialization has already been done, exit
      if Entity_Icons (False, Visibility_Public) (Cat_Unknown) /= null then
         return;
      end if;

      Entity_Icons (False, Visibility_Public) := Predefined_Array ("");
      Entity_Icons (False, Visibility_Protected) :=
        Predefined_Array ("-protected");
      Entity_Icons (False, Visibility_Private) :=
        Predefined_Array ("-private");

      Entity_Icons (True, Visibility_Public) := Predefined_Array ("-spec");
      Entity_Icons (True, Visibility_Protected) :=
        Predefined_Array ("-protected-spec");
      Entity_Icons (True, Visibility_Private) := Predefined_Array
        ("-private-spec");
   end Init_Graphics;

end Language.Icons;
