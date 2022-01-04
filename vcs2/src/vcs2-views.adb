------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2022, AdaCore                     --
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

with Interactive_Consoles;   use Interactive_Consoles;
with GNATCOLL.Traces;        use GNATCOLL.Traces;

with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Intl;               use GPS.Intl;
with Glib.Object;            use Glib.Object;
with Gtkada.MDI;             use Gtkada.MDI;
with Gtk.Box;                use Gtk.Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Link_Button;        use Gtk.Link_Button;
with Gtk.Label;              use Gtk.Label;
with Gtk.Style_Context;      use Gtk.Style_Context;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Style;           use Gtkada.Style;
with Histories;              use Histories;
with Pango.Layout;           use Pango.Layout;
with Project_Properties;

package body VCS2.Views is
   Me : constant Trace_Handle := Create ("GPS.VCS.VIEWS");

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : access Base_VCS_View_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type Open_VCS_Page is new Hyper_Link_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding procedure On_Click (Link : access Open_VCS_Page; Text : String);
   --  Callback opening the Project Properties to the VCS page

   procedure On_Selection_Changed (Self : access GObject_Record'Class);
   --  Called when the selection changes in the tree

   procedure On_Project_Properties_Link_Clicked
     (Self : access GObject_Record'Class);
   --  Called when the link button to the VCS page of the Project Properties
   --  has been clicked. This link is displayed when no VCS repo has been
   --  found.

   procedure No_VCS_Help (Self : not null access Base_VCS_View_Record'Class);
   --  If no VCS engine was found when creating the view, inform the user

   type Refresh_On_Terminate_Visitor is new Task_Visitor with record
      Kernel    : Kernel_Handle;
   end record;
   overriding procedure On_Terminate
     (Self     : not null access Refresh_On_Terminate_Visitor;
      VCS      : access VCS_Engine'Class);
   --  Refreshes all VCS views on terminate

   type On_Active_VCS_Changed is new Simple_Hooks_Function with record
      View : Base_VCS_View;
   end record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      procedure Show_Child
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

      ----------------
      -- Show_Child --
      ----------------

      procedure Show_Child
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class) is
      begin
         Widget.Set_No_Show_All (False);
         Widget.Show_All;
      end Show_Child;

   begin
      Self.View.No_VCS_Help.Destroy;
      Self.View.Foreach (Show_Child'Unrestricted_Access);
   end Execute;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Base_VCS_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => Histories.History_Key
           (To_String (View.Filter_Hist_Prefix)),
         Tooltip     => -"Filter the contents",
         Placeholder => -"filter",
         Options     => View.Filter_Options);
   end Create_Toolbar;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Self    : not null access Base_VCS_View_Record;
      Pref    : Preference)
   is
   begin
      Set_Font_And_Colors (Self.Tree, Fixed_Font => True, Pref => Pref);

      if (Pref = null
          or else Pref = Preference (Show_Ellipsis)
          or else Pref = Preference (Default_Style))
        and then Self.Text_Render /= null
      then
         Set_Property
           (Self.Text_Render,
            Gtk.Cell_Renderer_Text.Ellipsize_Property,
            (if Show_Ellipsis.Get_Pref
             then Ellipsize_Middle else Ellipsize_None));
         Self.Queue_Resize;
         Self.Tree.Queue_Draw;
      end if;
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      Self.View.On_Preferences_Changed (Pref);
   end Execute;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click (Link : access Open_VCS_Page; Text : String)
   is
      pragma Unreferenced (Text);
   begin
      Project_Properties.Edit_Properties
        (Get_Project (Link.Kernel), Link.Kernel, Name => "Version Control");
   end On_Click;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Self : access GObject_Record'Class) is
      View  : constant Base_VCS_View := Base_VCS_View (Self);
      MDI   : constant MDI_Window := Get_MDI (View.Kernel);
      Child : constant MDI_Child := Find_MDI_Child_From_Widget (View);
   begin
      --  Might be null during refresh
      if Child /= null
        and then Child = MDI.Get_Focus_Child
      then
         View.Kernel.Context_Changed (GPS_MDI_Child (Child).Build_Context);
      end if;
   end On_Selection_Changed;

   ----------------------------------------
   -- On_Project_Properties_Link_Clicked --
   ----------------------------------------

   procedure On_Project_Properties_Link_Clicked
     (Self : access GObject_Record'Class)
   is
      View   : constant Base_VCS_View := Base_VCS_View (Self);
      Kernel : constant Kernel_Handle := View.Kernel;
   begin
      Project_Properties.Edit_Properties
        (Get_Project (Kernel), Kernel, Name => "Version Control");
   end On_Project_Properties_Link_Clicked;

   -----------------
   -- No_VCS_Help --
   -----------------

   procedure No_VCS_Help (Self : not null access Base_VCS_View_Record'Class)
   is
      Kernel      : constant Kernel_Handle := Self.Kernel;
      VCS         : constant VCS_Engine_Access := Active_VCS (Kernel);
      Label       : Gtk_Label;
      Vbox        : Gtk_Vbox;
      Link_Button : Gtk_Link_Button;

      procedure Hide_Child
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);

      ----------------
      -- Hide_Child --
      ----------------

      procedure Hide_Child
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class) is
      begin
         Widget.Hide;
         Widget.Set_No_Show_All (True);
      end Hide_Child;

   begin
      if VCS = null or else VCS.Name = "unknown" then

         --  Hide all the VCS view children...

         Self.Foreach (Hide_Child'Unrestricted_Access);

         --  ... And display a label to warn the user that no VCS has been
         --  found for the current project, with a link button to the VCS
         --  page of the Project Properties.

         Gtk_New (Self.No_VCS_Help);
         Self.No_VCS_Help.Set_Policy (Policy_Automatic, Policy_Automatic);
         Gtk_New_Vbox (Vbox, Homogeneous => False);
         Self.No_VCS_Help.Add (Vbox);
         Self.Pack_Start (Self.No_VCS_Help);

         Gtk_New (Label, "No VCS repository found: you can set one via the ");
         Label.Set_Selectable (True);
         Vbox.Pack_Start (Label);
         Label.Set_Alignment (0.5, 1.0);

         Gtk_New_With_Label
           (Link_Button,
            "Project Properties",
            "Project Properties");
         Link_Button.On_Clicked
           (On_Project_Properties_Link_Clicked'Access,
            Slot => Self);
         Link_Button.Set_Alignment (0.5, 0.0);
         Vbox.Pack_Start (Link_Button);

         Self.No_VCS_Help.Show_All;
      end if;
   end No_VCS_Help;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self  : not null access Base_VCS_View_Record;
      Child : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class)
   is
      pragma Unreferenced (Child);

      P : Preferences_Hooks_Function_Access;
   begin
      Get_Style_Context (Self).Add_Class ("gps-vcs-view");

      No_VCS_Help (Self);
      Vcs_Active_Changed_Hook.Add
        (new On_Active_VCS_Changed'(Hook_Function
         with View => Base_VCS_View (Self)),
         Watch => Self);

      if Self.Tree /= null then
         Self.Tree.Get_Selection.On_Changed
           (On_Selection_Changed'Access, Self);
      end if;

      P := new On_Pref_Changed'(Preferences_Hooks_Function with View => Self);
      Preferences_Changed_Hook.Add (Obj => P, Watch => Self);
      P.Execute (Self.Kernel, null);   --   initial setup
   end On_Create;

   -----------------------------
   -- Get_Section_Title_Color --
   -----------------------------

   function Get_Section_Title_Color
     (Self : not null access Base_VCS_View_Record) return Gdk_RGBA
   is
      pragma Unreferenced (Self);
   begin
      return Shade_Or_Lighten (Default_Style.Get_Pref_Fg, 0.2);
   end Get_Section_Title_Color;

   ------------------
   -- On_Terminate --
   ------------------

   overriding procedure On_Terminate
     (Self     : not null access Refresh_On_Terminate_Visitor;
      VCS      : access VCS_Engine'Class)
   is
      pragma Unreferenced (VCS);
   begin
      Trace (Me, "Refreshing all VCS views");
      Self.Kernel.VCS.Invalidate_All_Caches;
      Vcs_Refresh_Hook.Run (Self.Kernel, Is_File_Saved => False);
   end On_Terminate;

   --------------------------
   -- Refresh_On_Terminate --
   --------------------------

   function Refresh_On_Terminate
      (Kernel    : not null access Kernel_Handle_Record'Class)
       return not null access Task_Visitor'Class
   is
      Aux : constant not null Task_Visitor_Access :=
              new Refresh_On_Terminate_Visitor'
                (Task_Visitor with
                 Kernel => Kernel_Handle (Kernel));

   begin
      return Aux;
   end Refresh_On_Terminate;

end VCS2.Views;
