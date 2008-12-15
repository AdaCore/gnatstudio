-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Interfaces.C.Strings;
with System;

with Glib.Object;
with Gdk.Event;
with Gtk.Cell_Renderer_Text;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Object;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model.Utils;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;

package body Code_Peer.Summary_Reports is

   use type Gtk.Tree_Model.Gtk_Tree_Path;

   package Report_Return_Cb is new Gtk.Handlers.User_Return_Callback
     (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Summary_Report);

   package Summary_Report_Handler is new Gtk.Handlers.Callback
     (Summary_Report_Record);

   procedure On_Destroy (Self : access Summary_Report_Record'Class);

   procedure Context_Func
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Summary_Report) return Boolean;

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Activated)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out GPS.Kernel.Selection_Context;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Menu, Kernel, Event_Widget);

      use type Code_Analysis.File_Access;
      use type Code_Analysis.Project_Access;
      use type Code_Analysis.Subprogram_Access;

      Self       : constant Summary_Report := Summary_Report (Object);
      X          : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_X (Event));
      Y          : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_Y (Event));
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell_X     : Glib.Gint;
      Cell_Y     : Glib.Gint;
      Column     : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Found      : Boolean;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Project    : Code_Analysis.Project_Access;
      File       : Code_Analysis.File_Access;
      Subprogram : Code_Analysis.Subprogram_Access;

   begin
      Self.Analysis_View.Get_Path_At_Pos
        (X, Y, Path, Column, Cell_X, Cell_Y, Found);

      if Path /= null then
         Self.Analysis_View.Get_Selection.Select_Path (Path);
         Iter       := Self.Analysis_Model.Get_Iter (Path);
         Project    := Self.Analysis_Model.Project_At (Iter);
         File       := Self.Analysis_Model.File_At (Iter);
         Subprogram := Self.Analysis_Model.Subprogram_At (Iter);

         if Subprogram /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.Name,
               Files   => (1 => File.Name));
            GPS.Kernel.Contexts.Set_Entity_Information
              (Context     => Context,
               Entity_Name => Subprogram.Name.all);

         elsif File /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context,
               Project => Project.Name,
               Files   => (1 => File.Name));

         elsif Project /= null then
            GPS.Kernel.Contexts.Set_File_Information
              (Context => Context, Project => Project.Name);
         end if;
      end if;
   end Context_Func;

   -----------------------
   -- Get_Selected_File --
   -----------------------

   function Get_Selected_File
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.File_Access
   is
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);

      return Self.Analysis_Model.File_At (Iter);
   end Get_Selected_File;

   --------------------------
   -- Get_Selected_Project --
   --------------------------

   function Get_Selected_Project
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Project_Access
   is
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);

      return Self.Analysis_Model.Project_At (Iter);
   end Get_Selected_Project;

   -----------------------------
   -- Get_Selected_Subprogram --
   -----------------------------

   function Get_Selected_Subprogram
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Subprogram_Access
   is
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Gtk.Tree_Selection.Get_Selected
        (Self.Analysis_View.Get_Selection, Model, Iter);

      return Self.Analysis_Model.Subprogram_At (Iter);
   end Get_Selected_Subprogram;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Report : out Summary_Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree) is
   begin
      Report := new Summary_Report_Record;
      Initialize (Report, Kernel, Module, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : access Summary_Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree)
   is
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy    : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Box.Initialize_Vbox (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerSummaryReport",
         Signal_Parameters);
      Summary_Report_Handler.Connect
        (Self,
         Gtk.Object.Signal_Destroy,
         Summary_Report_Handler.To_Marshaller (On_Destroy'Access));

      Self.Kernel := Kernel;
      Self.Tree   := Tree;

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Self.Pack_Start (Scrolled);

      Code_Peer.Summary_Models.Gtk_New (Self.Analysis_Model, Tree);
      Gtk.Tree_View.Gtk_New
        (Self.Analysis_View,
         Gtk.Tree_Model.Gtk_Tree_Model (Self.Analysis_Model));
      Scrolled.Add (Self.Analysis_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Entity");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", 0);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"High");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_End (Renderer, False);
      Column.Add_Attribute
        (Renderer, "text", Code_Peer.Summary_Models.High_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Medium");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_End (Renderer, False);
      Column.Add_Attribute
        (Renderer, "text", Code_Peer.Summary_Models.Medium_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Low");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_End (Renderer, False);
      Column.Add_Attribute
        (Renderer, "text", Code_Peer.Summary_Models.Low_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Self.Pack_Start (Scrolled);

      Code_Peer.Entity_Messages_Models.Gtk_New
        (Self.Messages_Model,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).Categories);
      Gtk.Tree_View.Gtk_New (Self.Messages_View, Self.Messages_Model);
      Scrolled.Add (Self.Messages_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Message category");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, False);
      Column.Add_Attribute
        (Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Category_Name_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"High");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, False);
      Column.Add_Attribute
        (Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.High_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Medium");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, False);
      Column.Add_Attribute
        (Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Medium_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Low");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, False);
      Column.Add_Attribute
        (Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Low_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Report_Return_Cb.Connect
        (Self.Analysis_View,
         Gtk.Widget.Signal_Button_Press_Event,
         Report_Return_Cb.To_Marshaller (On_Analysis_Click'Access),
         Summary_Report (Self),
         False);

      --

      GPS.Kernel.Modules.Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Self.Analysis_View,
         Object          => Self,
         ID              => Module,
         Context_Func    => Context_Func'Access);
   end Initialize;

   -----------------------
   -- On_Analysis_Click --
   -----------------------

   function On_Analysis_Click
     (View  : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Self  : Summary_Report) return Boolean
   is
      pragma Unreferenced (View);

      use type Glib.Guint;
      use type Glib.Signal_Name;
      use type Gdk.Event.Gdk_Event_Type;

      use type Code_Analysis.Project_Access;
      use type Code_Analysis.File_Access;
      use type Code_Analysis.Subprogram_Access;

      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      X      : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_X (Event));
      Y      : constant Glib.Gint := Glib.Gint (Gdk.Event.Get_Y (Event));
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell_X : Glib.Gint;
      Cell_Y : Glib.Gint;
      Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Found  : Boolean;

   begin
      if Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press
      then
         --  When the callback is called the tree selection contains old
         --  selection, so we need to calculate and update selection.

         Self.Analysis_View.Get_Path_At_Pos
           (X, Y, Path, Column, Cell_X, Cell_Y, Found);

         if Path /= null then
            Self.Analysis_View.Get_Selection.Select_Path (Path);
            Iter := Self.Analysis_Model.Get_Iter (Path);

            declare
               Project_Node    : constant Code_Analysis.Project_Access :=
                 Self.Analysis_Model.Project_At (Iter);
               File_Node       : constant Code_Analysis.File_Access :=
                 Self.Analysis_Model.File_At (Iter);
               Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                 Self.Analysis_Model.Subprogram_At (Iter);

            begin
               if Subprogram_Node /= null then
                  Self.Messages_Model.Set (Subprogram_Node);

               elsif File_Node /= null then
                  Self.Messages_Model.Set (File_Node);

               elsif Project_Node /= null then
                  Self.Messages_Model.Set (Project_Node);

               elsif not Gtk.Tree_Model.Utils.Is_Null (Iter) then
                  Self.Messages_Model.Set (Self.Tree);
               end if;
            end;
         end if;

      elsif Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2button_Press
      then
         Emit_By_Name (Self.Get_Object, Signal_Activated & ASCII.NUL);
      end if;

      return False;
   end On_Analysis_Click;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Summary_Report_Record'Class) is
   begin
      Self.Messages_Model.Clear;
      --  Entity messages model internal data must be cleaned before the code
      --  analysis date is cleaned, because entity messages model catch
      --  direct references to the code analysis data.
   end On_Destroy;

end Code_Peer.Summary_Reports;
