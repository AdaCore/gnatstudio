------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Default_Preferences;       use Default_Preferences;
with Generic_Views;             use Generic_Views;

with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Label;                 use Gtk.Label;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with Language;                  use Language;

package body GPS.Kernel.Preferences_Views is

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   type Preferences_Editor_Record is new Generic_Views.View_Record with record
      View              : Gtk_Tree_View;
      Model             : Gtk_Tree_Store;
      Current_Selection : Gtk_Widget;
   end record;
   --  Preferences editor view record. This record only encapsulates needed
   --  for callbacks (e.g: Selection_Changed).

   type Preferences_Editor is access all Preferences_Editor_Record'Class;

   function Initialize
     (Self : access Preferences_Editor_Record'Class) return Gtk_Widget;
   --  Initialize and add all the widgets needed for the
   --  Preferences_Editor_Record view given in parameter.

   package Preferences_Editor_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Preferences",
      View_Name                 => "Preferences",
      Formal_View_Record        => Preferences_Editor_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Reuse_If_Exist            => True,
      Local_Toolbar             => False,
      Local_Config              => False,
      Group                     => Group_Default,
      Areas                     => Gtkada.MDI.Both,
      Default_Width             => 700,
      Default_Height            => 700,
      Commands_Category         => -"/Edit/Preferences",
      Add_Close_Button_On_Float => True,
      MDI_Flags                 =>
         All_Buttons or Float_To_Main or Always_Destroy_Float,
      Position                  => Position_Float,
      Initialize                => Initialize);
   use Preferences_Editor_Views;
   --  Instantiation of the Generic_Views.Simple_Views package with
   --  the parameters we want for our Preferences editor views.

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the selected page has changed.

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class) is
      Pref_View : constant Preferences_Editor := Preferences_Editor (Widget);
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;
   begin
      if Pref_View.Current_Selection /= null then
         Set_Child_Visible (Pref_View.Current_Selection, False);
         Pref_View.Current_Selection := null;
      end if;

      Get_Selected (Get_Selection (Pref_View.View), M, Iter);

      if Iter /= Null_Iter then
         Pref_View.Current_Selection :=
           Gtk_Widget (Get_Object (Pref_View.Model, Iter, 1));
         Set_Child_Visible (Pref_View.Current_Selection, True);
      end if;
   end Selection_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference) is
      pragma Unreferenced (Self, Kernel);
   begin
      Pref.Update_On_Pref_Changed (Get_GObject_To_Update (Pref));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Preferences_Editor_Record'Class) return Gtk_Widget
   is
      Manager  : constant GPS_Preferences :=
                   GPS_Preferences (Self.Kernel.Preferences);
      Filename : constant Virtual_File := Self.Kernel.Preferences_File;

      Main_Table        : Gtk_Table;

      procedure Select_First_Page;
      --  Select the first page of preferences to be active in the Tree_View.
      --  This procedure is called right after the initialization.

      function Find_Or_Create_Page
        (Name : String; Widget : Gtk_Widget) return Gtk_Widget;
      --  Return the iterator in Model matching Name.
      --  If no such page already exists, then eithe Widget (if non null) is
      --  inserted for it, or a new table is created and inserted

      -----------------------
      -- Select_First_Page --
      -----------------------

      procedure Select_First_Page is
         First_Iter      : Gtk_Tree_Iter;
         First_Page_Path : Gtk_Tree_Path;
      begin
         First_Iter := Get_Iter_First (Self.Model);

         if First_Iter /= Null_Iter then
            First_Page_Path := Self.Model.Get_Path (First_Iter);

            Set_Cursor (Self.View, First_Page_Path, null, False);
            Self.Current_Selection
              := Gtk_Widget (Get_Object (Self.Model, First_Iter, 1));
         end if;
      end Select_First_Page;

      -------------------------
      -- Find_Or_Create_Page --
      -------------------------

      function Find_Or_Create_Page
        (Name : String; Widget : Gtk_Widget) return Gtk_Widget
      is
         Current     : Gtk_Tree_Iter := Null_Iter;
         Child       : Gtk_Tree_Iter;
         First, Last : Integer := Name'First;
         Table       : Gtk_Table;
         W           : Gtk_Widget;

      begin
         while First <= Name'Last loop
            Last := First;

            while Last <= Name'Last
              and then Name (Last) /= '/'
            loop
               Last := Last + 1;
            end loop;

            if Current = Null_Iter then
               Child := Get_Iter_First (Self.Model);
            else
               Child := Children (Self.Model, Current);
            end if;

            while Child /= Null_Iter
              and then
                Get_String (Self.Model, Child, 0) /= Name (First .. Last - 1)
            loop
               Next (Self.Model, Child);
            end loop;

            if Child = Null_Iter then
               if Widget = null then
                  Gtk_New (Table, Rows => 0, Columns => 2,
                           Homogeneous => False);
                  Set_Row_Spacings (Table, 1);
                  Set_Col_Spacings (Table, 5);
                  W := Gtk_Widget (Table);

               else
                  W := Widget;
               end if;

               Append (Self.Model, Child, Current);
               Set (Self.Model, Child, 0, Name (First .. Last - 1));
               Set (Self.Model, Child, 1, GObject (W));

               Attach (Main_Table, W, 1, 2, 2, 3,
                       Ypadding => 0, Xpadding => 10);
               Set_Child_Visible (W, False);
            end if;

            Current := Child;

            First := Last + 1;
         end loop;

         return Gtk_Widget (Get_Object (Self.Model, Current, 1));
      end Find_Or_Create_Page;

      Frame          : Gtk_Frame;
      Table          : Gtk_Table;
      Col            : Gtk_Tree_View_Column;
      Render         : Gtk_Cell_Renderer_Text;
      Num            : Gint;
      Scrolled       : Gtk_Scrolled_Window;
      Pref           : Preference;
      Row            : Guint;
      Backup_Created : Boolean;
      Widget         : Gtk_Widget;
      Event          : Gtk_Event_Box;
      Label          : Gtk_Label;
      Separator      : Gtk_Separator;
      C              : Default_Preferences.Cursor;
      Tmp            : Gtk_Widget;
      Backup_File    : constant Virtual_File :=
                         Create (Full_Filename => Filename.Full_Name & ".bkp");

      pragma Unreferenced (Tmp, Num);

   begin
      Filename.Copy (Backup_File.Full_Name, Success => Backup_Created);

      Initialize_Vbox (Self);
      Self.Set_Name ("Preferences");  --  for testsuite

      Manager.Set_Editor (Self);

      Gtk_New (Main_Table, Rows => 3, Columns => 2, Homogeneous => False);
      Self.Pack_Start (Main_Table);

      Gtk_New (Frame);
      Main_Table.Attach (Frame, 0, 1, 0, 3);

      Gtk_New_Hseparator (Separator);
      Main_Table.Attach (Separator, 1, 2, 1, 2, Yoptions => 0, Ypadding => 1);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Self.Model, (0 => GType_String, 1 => GType_Object));
      Gtk_New (Self.View, Self.Model);
      Scrolled.Add (Self.View);
      Unref (Self.Model);
      Self.View.Set_Headers_Visible (False);

      Gtk_New (Col);
      Num := Self.View.Append_Column (Col);
      Gtk_New (Render);
      Col.Pack_Start (Render, Expand => True);
      Col.Add_Attribute (Render, "text", 0);

      Widget_Callback.Object_Connect
        (Get_Selection (Self.View), Gtk.Tree_Selection.Signal_Changed,
         Selection_Changed'Unrestricted_Access,
         Self);

      C := Manager.Get_First_Reference;
      loop
         Pref := Get_Pref (C);
         exit when Pref = null;

         if Pref.Get_Page /= "" then
            Table := Gtk_Table (Find_Or_Create_Page (Pref.Get_Page, null));
            Row := Get_Property (Table, N_Rows_Property);
            Resize (Table, Rows => Row + 1, Columns => 2);

            if Pref.Editor_Needs_Label then
               Gtk_New (Event);
               Gtk_New (Label, Pref.Get_Label);
               Event.Add (Label);
               Event.Set_Tooltip_Text (Pref.Get_Doc);
               Label.Set_Alignment (0.0, 0.5);
               Table.Attach (Event, 0, 1, Row, Row + 1,
                             Xoptions => Fill, Yoptions => 0);

               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);

               if Widget /= null then
                  Table.Attach (Widget, 1, 2, Row, Row + 1, Yoptions => 0);
               end if;

            else
               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);
               Widget.Set_Tooltip_Text (Pref.Get_Doc);

               if Widget /= null then
                  Table.Attach (Widget, 0, 2, Row, Row + 1, Yoptions => 0);
               end if;
            end if;
         end if;

         Manager.Next (C);
      end loop;

      Self.Set_Can_Focus (True);

      --  Show all pages for more convenient access
      Self.View.Expand_All;

      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      Select_First_Page;

      return Gtk_Widget (Self);
   end Initialize;

   -------------------------------
   -- Register_Preferences_Menu --
   -------------------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Preferences_Editor_Views.Register_Module (Kernel);
   end Register_Module;

end GPS.Kernel.Preferences_Views;
