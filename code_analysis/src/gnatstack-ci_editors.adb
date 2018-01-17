------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;

with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Label;
with Glib.Properties;
with Glib.Values;

with GNATCOLL.VFS;
with GPS.Intl;

package body GNATStack.CI_Editors is

   use GPS.Intl;

   procedure On_CI_Edited
     (Self   : access CI_Editor_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when subprogram's stack size is changed in CI file view.

   procedure On_Unassigned_Edited
     (Self   : access CI_Editor_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when subprogram's stack size is changed in unassigned view.

   package CI_Editor_Callbacks is new Gtk.Handlers.Callback (CI_Editor_Record);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item : out CI_Editor;
      Data : not null access GNATStack.Data_Model.Analysis_Information) is
   begin
      Item := new CI_Editor_Record;
      Initialize (Item, Data);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access CI_Editor_Record'Class;
      Data : not null access GNATStack.Data_Model.Analysis_Information)
   is
      Notebook : Gtk.Notebook.Gtk_Notebook;
      View     : Gtk.Tree_View.Gtk_Tree_View;
      Label    : Gtk.Label.Gtk_Label;
      Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Dummy    : Glib.Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk.Box.Initialize_Vbox (Self);
      Self.Data := Data;

      Gtk.Notebook.Gtk_New (Notebook);
      Self.Pack_Start (Notebook);

      Gtk.Label.Gtk_New
        (Label,
         String
           (GNATCOLL.VFS.Create
              (GNATCOLL.VFS.Filesystem_String
                 (Ada.Strings.Unbounded.To_String
                    (Self.Data.CIs.First_Element.File_Name))).Base_Name));
      GNATStack.CI_Models.Gtk_New
        (Self.CI_Model, Self.Data.CIs.First_Element.Subprograms);
      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Dummy := Notebook.Insert_Page (Scrolled, Label, 0);

      Gtk.Tree_View.Gtk_New (View, Self.CI_Model);
      Scrolled.Add (View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Subprogram");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", CI_Models.Name_Column);
      Dummy := View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Stack size");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Glib.Properties.Set_Property
        (Renderer, Gtk.Cell_Renderer_Text.Editable_Property, True);
      CI_Editor_Callbacks.Object_Connect
        (Renderer, "edited", On_CI_Edited'Access, Self);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", CI_Models.Bytes_Column);
      Dummy := View.Append_Column (Column);

      --  External subprograms with undefined stack usage view.

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      GNATStack.CI_Models.Gtk_New
        (Self.Unassigned_Model, Self.Data.External_Set);
      Gtk.Tree_View.Gtk_New (View, Self.Unassigned_Model);
      Scrolled.Add (View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Subprogram");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", CI_Models.Name_Column);
      Dummy := View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Stack size");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Glib.Properties.Set_Property
        (Renderer, Gtk.Cell_Renderer_Text.Editable_Property, True);
      CI_Editor_Callbacks.Object_Connect
        (Renderer, "edited", On_Unassigned_Edited'Access, Self);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", CI_Models.Bytes_Column);
      Dummy := View.Append_Column (Column);
   end Initialize;

   ------------------
   -- On_CI_Edited --
   ------------------

   procedure On_CI_Edited
     (Self   : access CI_Editor_Record'Class;
      Params : Glib.Values.GValues)
   is
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Value      : constant String :=
        Glib.Values.Get_String (Glib.Values.Nth (Params, 2));
      Size       : Integer;
      Subprogram : Data_Model.Subprogram_Information_Access;

   begin
      Gtk.Tree_Model.Gtk_New
        (Path, Glib.Values.Get_String (Glib.Values.Nth (Params, 1)));
      Subprogram :=
        Self.CI_Model.Subprogram_At (Self.CI_Model.Get_Iter (Path));

      begin
         Size := Integer'Value (Value);

         if Size < 0 then
            Size := -2;
         end if;

      exception
         when Constraint_Error =>
            Size := Integer'First;
      end;

      if Size >= 0 then
         Subprogram.Local_Usage.Size := Size;

      elsif Size /= Integer'First then
         Subprogram.Local_Usage.Size := -2;
         Self.CI_Model.Removed (Subprogram);
         Self.Data.CIs.First_Element.Subprograms.Delete (Subprogram);
         Self.Data.External_Set.Insert (Subprogram);
         Self.Unassigned_Model.Inserted (Subprogram);
      end if;

      Gtk.Tree_Model.Path_Free (Path);
   end On_CI_Edited;

   --------------------------
   -- On_Unassigned_Edited --
   --------------------------

   procedure On_Unassigned_Edited
     (Self   : access CI_Editor_Record'Class;
      Params : Glib.Values.GValues)
   is
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Value      : constant String :=
        Glib.Values.Get_String (Glib.Values.Nth (Params, 2));
      Size       : Integer;
      Subprogram : Data_Model.Subprogram_Information_Access;

   begin
      Gtk.Tree_Model.Gtk_New
        (Path, Glib.Values.Get_String (Glib.Values.Nth (Params, 1)));
      Subprogram :=
        Self.Unassigned_Model.Subprogram_At
          (Self.Unassigned_Model.Get_Iter (Path));

      begin
         Size := Integer'Value (Value);

         if Size < 0 then
            Size := -2;
         end if;

      exception
         when Constraint_Error =>
            Size := -1;
      end;

      if Size >= 0 then
         Subprogram.Local_Usage.Size := Size;
         Self.Unassigned_Model.Removed (Subprogram);
         Self.Data.External_Set.Delete (Subprogram);
         Self.Data.CIs.First_Element.Subprograms.Insert (Subprogram);
         Self.CI_Model.Inserted (Subprogram);
      end if;

      Gtk.Tree_Model.Path_Free (Path);
   end On_Unassigned_Edited;

end GNATStack.CI_Editors;
