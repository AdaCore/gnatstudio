------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

--  This package provides an Entity View widget

with Gtk.Dialog; use Gtk.Dialog;
with Gtk.GEntry; use Gtk.GEntry;

with XML_Utils;  use XML_Utils;
with Gtkada.MDI; use Gtkada.MDI;
with Gtk.Paned;  use Gtk.Paned;

with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Ada_Semantic_Tree; use Ada_Semantic_Tree;

package Completion_Window.Entity_Views is

   type Entity_View_Record is new Gtk_Vbox_Record with private;
   type Entity_View_Access is access all Entity_View_Record'Class;

   procedure Gtk_New
     (View       : out Entity_View_Access;
      Kernel     : Kernel_Handle;
      Initial    : Glib.UTF8_String;
      Visibility : Visibility_Context);
   --  Create a new Completion_Explorer

   procedure Initialize
     (View     : access Entity_View_Record'Class;
      Kernel   : Kernel_Handle;
      Initial  : Glib.UTF8_String;
      Visibility : Visibility_Context);
   --  Internal initialization procedure

   function Get_Entry
     (View : access Entity_View_Record'Class) return Gtk_Entry;
   --  Return the entry

   function Save_Desktop
     (View : access Entity_View_Record'Class) return Node_Ptr;
   function Load_Desktop
     (Kernel : Kernel_Handle;
      Node   : Node_Ptr;
      Module : Module_ID) return MDI_Child;
   --  Desktop functions

   procedure Set_Dialog (Explorer : Entity_View_Access; Dialog : Gtk_Dialog);
   --  Set the Explorer in Dialog mode: in this mode, Dialog will be quit
   --  right after jumping to an entry.

private

   type Entity_View_Record is new Gtk_Vbox_Record with record
      Explorer : Completion_Explorer_Access;

      Visibility : Visibility_Context;
      Ent      : Gtk_Entry;
      Pane     : Gtk_Paned;
      Notes_Scroll : Gtk_Scrolled_Window;

      Is_Horizontal : Boolean := True;

      Is_New        : Boolean := True;
      --  True when the entity view was just created

      Dialog        : Gtk_Dialog;

      Vertical_Position   : Gint := -1;
      Horizontal_Position : Gint := -1;
      --  Record the horizontal and vertical positions of the paned.
      --  -1 indicates that there is no recorded value.
   end record;

end Completion_Window.Entity_Views;
