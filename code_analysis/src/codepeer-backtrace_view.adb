------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Glib;                   use Glib;
with Glib_Values_Utils;      use Glib_Values_Utils;

with Gtk.Box;                use Gtk.Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.MDI;             use Gtkada.MDI;

with Basic_Types;            use Basic_Types;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;    use GNATCOLL.VFS.GtkAda;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;

with BT;                     use BT;
with BT.Xml.Reader;

package body CodePeer.Backtrace_View is

   Me : constant Trace_Handle := Create ("GPS.CODEPEER.CODEPEER-BT");

   type Backtrace_View is access all Backtrace_View_Record'Class;

   function Initialize
     (Self : access Backtrace_View_Record'Class)
      return Gtk_Widget;

   package Backtrace_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Backtrace_View",
      View_Name          => "Backtraces",
      Formal_View_Record => Backtrace_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Position           => Position_Right,
      Group              => Group_Debugger_Stack,
      Commands_Category  => "CodePeer",
      Areas              => Sides_Only);

   Text_Column   : constant := 0;
   File_Column   : constant := 1;
   Line_Column   : constant := 2;
   Column_Column : constant := 3;

   Column_Types : GType_Array (0 .. 3);

   package Backtrace_View_Callbacks is
     new Gtk.Handlers.Callback (Backtrace_View_Record);

   procedure On_Activated
     (Self   : access Backtrace_View_Record'Class;
      Path   : Gtk_Tree_Path;
      Column : Gtk_Tree_View_Column);
   --  Opens source editor for clicked location.

   ------------------------
   -- Display_Backtraces --
   ------------------------

   procedure Display_Backtraces
     (Kernel           : access Kernel_Handle_Record'Class;
      Output_Directory : GNATCOLL.VFS.Virtual_File;
      File             : GNATCOLL.VFS.Virtual_File;
      Message          : GPS.Kernel.Messages.Message_Access;
      Subprogram       : String;
      Set              : Natural_Sets.Set)
   is
      View     : constant Backtrace_View :=
        Backtrace_View (Backtrace_Views.Retrieve_View (Kernel));
      Found    : Boolean := False;
      Info     : BT.BT_Info_Seqs.Vector;
      Vn_Iter  : Gtk_Tree_Iter;
      Bt_Iter  : Gtk_Tree_Iter;
      Src_File : GNATCOLL.VFS.Virtual_File;

   begin
      if View = null then
         --  There is no Backtraces view open.

         return;

      end if;

      --  Ignore change of backtraces information when change of source
      --  location was requested by Backtraces view.

      if View.Activated then
         View.Activated := False;

         return;
      end if;

      View.Store.Clear;

      if not Set.Is_Empty then
         BT.Xml.Reader.Read_File_Backtrace_Xml
           (String (Output_Directory.Full_Name.all),
            String (File.Full_Name.all),
            Found);

         if not Found then
            Trace
              (Me,
               "There is no backtrace information file for "
               & File.Display_Full_Name & ".");
         end if;
      end if;

      if Set.Is_Empty or else not Found then
         View.Store.Append (Vn_Iter, Null_Iter);
         View.Store.Set (Vn_Iter, Text_Column, "<no backtrace info>");

         return;
      end if;

      for Vn of Set loop
         View.Store.Append (Vn_Iter, Null_Iter);

         Set_All_And_Clear
           (View.Store, Vn_Iter,
            (0 => As_String (Message.Get_File.Display_Base_Name & ':' &
               Trim (Natural'Image (Message.Get_Line), Both) & ':' &
               Trim (Visible_Column_Type'Image (Message.Get_Column), Both) &
               ": " & To_String (Message.Get_Text)),
             1 => As_File   (No_File),
             2 => As_Int    (0),
             3 => As_Int    (0)));

         Info.Clear;
         BT.Xml.Reader.Get_Vn_Backtraces
           (Subprogram,
            Vn,
            (Message.Get_Line, Integer (Message.Get_Column)),
            Info);

         for Location of Info loop
            Src_File :=
              GPS.Kernel.Create
                (GNATCOLL.VFS.Filesystem_String
                   (BT.Xml.Reader.Get_BT_File_Name (Location.Bt_Id)),
                 Kernel);

            View.Store.Append (Bt_Iter, Vn_Iter);

            Set_All_And_Clear
              (View.Store, Bt_Iter,
               (0 => As_String (Src_File.Display_Base_Name & ':' &
                  Image (Location.Sloc.Line, 1) & ':' &
                  Image (Location.Sloc.Column, 1) & " - " &
                  To_String (Location.Text)),
                1 => As_File (Src_File),
                2 => As_Int  (Gint (Location.Sloc.Line)),
                3 => As_Int  (Gint (Location.Sloc.Column))));
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
      Scrolled : Gtk_Scrolled_Window;
      Column   : Gtk_Tree_View_Column;
      Renderer : Gtk_Cell_Renderer_Text;
      Dummy    : Gint;

   begin
      Initialize_Vbox (Self, False);

      Gtk_New (Self.Store, Column_Types);

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
         Self.Activated := True;
         Open_File_Action_Hook.Run
           (Kernel  => Self.Kernel,
            File    => File,
            Project => Self.Kernel.Get_Project_Tree.Root_Project,
            Line    => Line,
            Column  => Col);
      end if;
   end On_Activated;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Column_Types :=
        (Text_Column   => GType_String,
         File_Column   => Get_Virtual_File_Type,
         Line_Column   => GType_Int,
         Column_Column => GType_Int);

      Backtrace_Views.Register_Module (Kernel);
   end Register_Module;

end CodePeer.Backtrace_View;
