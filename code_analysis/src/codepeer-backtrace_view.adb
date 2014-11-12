------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Glib;                   use Glib;
with Gtk.Box;                use Gtk.Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.MDI;             use Gtkada.MDI;

with Basic_Types;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;    use GNATCOLL.VFS.GtkAda;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;

with BT;                     use BT;
with BT.Xml.Reader;
with CodePeer.Module;

package body CodePeer.Backtrace_View is

   type Backtrace_View is access all Backtrace_View_Record'Class;

   function Initialize
     (Self : access Backtrace_View_Record'Class)
      return Gtk_Widget;

   package Backtrace_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Backtrace_View_Record",
      View_Name          => "Backtrace",
      Formal_View_Record => Backtrace_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Position           => Position_Right,
      Group              => Group_Debugger_Stack,
      Commands_Category  => "",
      Areas              => Sides_Only);

   Text_Column   : constant := 0;
   File_Column   : constant := 1;
   Line_Column   : constant := 2;
   Column_Column : constant := 3;

   package Backtrace_View_Callbacks is
     new Gtk.Handlers.Callback (Backtrace_View_Record);

   procedure On_Activated
     (Self   : access Backtrace_View_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : Gtk_Tree_View_Column);
   --  Opens source editor for clicked location.

   ---------------------------
   -- Close_Backtraces_View --
   ---------------------------

   procedure Close_Backtraces_View
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Backtrace_Views.Close (Kernel);
   end Close_Backtraces_View;

   ------------------------
   -- Display_Backtraces --
   ------------------------

   procedure Display_Backtraces
     (Kernel     : access Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Subprogram : String;
      Set        : Natural_Sets.Set)
   is
      Mode    : constant String := Kernel.Get_Build_Mode;
      View    : constant Backtrace_View :=
        Backtrace_View (Backtrace_Views.Get_Or_Create_View (Kernel, False));
      Found   : Boolean;
      Info    : BT.BT_Info_Seqs.Vector;
      Vn_Iter : Gtk_Tree_Iter;
      Bt_Iter : Gtk_Tree_Iter;

   begin
      View.Store.Clear;

      if Set.Is_Empty then
         return;
      end if;

      Kernel.Set_Build_Mode ("codepeer");
      BT.Xml.Reader.Read_File_Backtrace_Xml
        (String
           (CodePeer.Module.Codepeer_Output_Directory
                (GPS.Kernel.Project.Get_Project (Kernel)).Full_Name.all),
         String (File.Base_Name),
         Found);
      Kernel.Set_Build_Mode (Mode);

      if not Found then
         Kernel.Insert
           ("There is no backtrace information file for "
            & File.Display_Base_Name & ".",
            Mode => GPS.Kernel.Error);

         return;
      end if;

      for Vn of Set loop
         View.Store.Append (Vn_Iter, Null_Iter);
         View.Store.Set (Vn_Iter, Text_Column, "vn" & Integer'Image (Vn));
         Set_File (View.Store, Vn_Iter, File_Column, No_File);
         View.Store.Set (Vn_Iter, Line_Column, 0);
         View.Store.Set (Vn_Iter, Column_Column, 0);
         Info.Clear;
         BT.Xml.Reader.Get_Vn_Backtraces (Subprogram, Vn, Info);

         for Location of Info loop
            View.Store.Append (Bt_Iter, Vn_Iter);
            View.Store.Set
              (Bt_Iter,
               Text_Column,
               File.Display_Base_Name
               & ':' & Image (Location.Sloc.Line, 1)
               & ':' & Image (Location.Sloc.Column, 1)
               & " - " & To_String (Location.Text));
            Set_File (View.Store, Bt_Iter, File_Column, File);
            View.Store.Set (Bt_Iter, Line_Column, Gint (Location.Sloc.Line));
            View.Store.Set
              (Bt_Iter, Column_Column, Gint (Location.Sloc.Column));
         end loop;
      end loop;

      View.View.Expand_All;
   end Display_Backtraces;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Backtrace_View_Record'Class)
      return Gtk_Widget
   is
      Types : constant GType_Array :=
        (Text_Column   => GType_String,
         File_Column   => Get_Virtual_File_Type,
         Line_Column   => GType_Int,
         Column_Column => GType_Int);

      Scrolled : Gtk_Scrolled_Window;
      Column   : Gtk_Tree_View_Column;
      Renderer : Gtk_Cell_Renderer_Text;
      Dummy    : Gint;

   begin
      Initialize_Vbox (Self, False);

      Gtk_New (Self.Store, Types);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, True, True);

      Gtk_New (Self.View, Self.Store);
      Self.View.Set_Activate_On_Single_Click (True);
      Scrolled.Add (Self.View);

      Gtk_New (Column);
      Column.Set_Resizable (True);
      Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", Text_Column);
      Dummy := Self.View.Append_Column (Column);

      Backtrace_View_Callbacks.Object_Connect
        (Self.View,
         Gtk.Tree_View.Signal_Row_Activated,
         Backtrace_View_Callbacks.To_Marshaller (On_Activated'Access),
         Self);

      return Gtk_Widget (Self.View);
   end Initialize;

   ------------------
   -- On_Activated --
   ------------------

   procedure On_Activated
     (Self   : access Backtrace_View_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : Gtk_Tree_View_Column)
   is
      pragma Unreferenced (Column);

      Iter : constant Gtk_Tree_Iter := Self.Store.Get_Iter (Path);
      File : constant Virtual_File :=
        Get_File (Self.Store, Iter, File_Column);
      Line : constant Natural :=
        Natural (Self.Store.Get_Int (Iter, Line_Column));
      Col  : constant Basic_Types.Visible_Column_Type :=
        Basic_Types.Visible_Column_Type
          (Self.Store.Get_Int (Iter, Column_Column));

   begin
      if File /= No_File then
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Self.Kernel,
            File,
            Self.Kernel.Get_Project_Tree.Root_Project,
            Line,
            Col);
      end if;
   end On_Activated;

end CodePeer.Backtrace_View;
