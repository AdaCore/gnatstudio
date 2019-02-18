------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Intl;                    use GPS.Intl;
with Glib.Object;                 use Glib.Object;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Widget;                  use Gtk.Widget;
with Histories;                   use Histories;
with Pango.Layout;                use Pango.Layout;

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

   procedure On_Selection_Changed (Self : access GObject_Record'Class);
   --  Called when the selection changes in the tree

   type Refresh_On_Terminate_Visitor is new Task_Visitor with record
      Kernel    : Kernel_Handle;
   end record;
   overriding procedure On_Terminate
     (Self     : not null access Refresh_On_Terminate_Visitor;
      VCS      : access VCS_Engine'Class);
   --  Refreshes all VCS views on terminate

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

      if (Pref = null or else Pref = Preference (Show_Ellipsis))
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
      if Self.Tree /= null then
         Self.Tree.Get_Selection.On_Changed
           (On_Selection_Changed'Access, Self);
      end if;

      P := new On_Pref_Changed'(Preferences_Hooks_Function with View => Self);
      Preferences_Changed_Hook.Add (Obj => P, Watch => Self);
      P.Execute (Self.Kernel, null);   --   initial setup
   end On_Create;

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
      return not null access Task_Visitor'Class is
   begin
      return new Refresh_On_Terminate_Visitor'
         (Task_Visitor with
          Kernel    => Kernel_Handle (Kernel));
   end Refresh_On_Terminate;

end VCS2.Views;
