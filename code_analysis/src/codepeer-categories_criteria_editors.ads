------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

--  This package contains implementation of the CodePeer's message categories
--  filter criteria editor. It is used by Summary Report form.

with Glib;
with Gtk.Widget;

private with Gtk.Check_Button;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_View;

private with CodePeer.Categories_Criteria_Models;

package CodePeer.Categories_Criteria_Editors is

   type Categories_Criteria_Editor_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Categories_Criteria_Editor is
     access all Categories_Criteria_Editor_Record'Class;

   procedure Gtk_New
     (Editor         : in out Categories_Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set);

   procedure Initialize
     (Self           : not null access Categories_Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set);

   function Get_Visible_Categories
     (Self : access Categories_Criteria_Editor_Record'Class)
      return CodePeer.Message_Category_Sets.Set;
   --  Returns a set of selected message categories

   Signal_Criteria_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the editor in the case of the criteria change

private

   type Categories_Criteria_Editor_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Model  : Categories_Criteria_Models.Categories_Criteria_Model;
         View   : Gtk.Tree_View.Gtk_Tree_View;
         Toggle : Gtk.Check_Button.Gtk_Check_Button;
      end record;

   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria-changed";

end CodePeer.Categories_Criteria_Editors;
