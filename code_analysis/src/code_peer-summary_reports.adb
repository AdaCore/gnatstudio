-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
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

with Ada.Characters.Latin_1;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Glib.Values;
with Gdk.Event;
with Gdk.Pixbuf;
with Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Object;
with Gtk.Scrolled_Window;
with Gtk.Toggle_Button;
with Gtk.Tree_Model.Utils;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;
with Code_Analysis_GUI;

package body Code_Peer.Summary_Reports is

   use type Gtk.Tree_Model.Gtk_Tree_Path;

   package Tree_View_Report_Return_Boolean_Callbacks is
     new Gtk.Handlers.User_Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean, Summary_Report);

   package Summary_Report_Callbacks is new Gtk.Handlers.Callback
     (Summary_Report_Record);

   package Cell_Renderer_Toggle_Report_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
            Summary_Report);

   package Cell_Renderer_Toggle_Report_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Report_Callbacks.Marshallers.Generic_Marshaller
           (Interfaces.C.Strings.chars_ptr, Glib.Values.Get_Chars);

   package Check_Button_Report_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk.Check_Button.Gtk_Check_Button_Record, Summary_Report);

   procedure On_Destroy (Self : access Summary_Report_Record'Class);

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
      Self   : Summary_Report);

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report);

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
      Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Toggle_Renderer : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
      Report_Pane     : Gtk.Paned.Gtk_Vpaned;
      Filter_Box      : Gtk.Box.Gtk_Vbox;
      Check           : Gtk.Check_Button.Gtk_Check_Button;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Paned.Initialize_Hpaned (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerSummaryReport",
         Signal_Parameters);
      Summary_Report_Callbacks.Connect
        (Self,
         Gtk.Object.Signal_Destroy,
         Summary_Report_Callbacks.To_Marshaller (On_Destroy'Access));

      Self.Kernel := Kernel;
      Self.Tree   := Tree;

      Project_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.Prj_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      File_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.File_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Subprogram_Icon :=
        Gtk.Widget.Gtk_Widget
          (Kernel.Get_Main_Window).Render_Icon
          (Code_Analysis_GUI.Subp_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);

      Gtk.Paned.Gtk_New_Vpaned (Report_Pane);
      Self.Pack1 (Report_Pane, Resize => True);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Report_Pane.Pack1 (Scrolled, Resize => True);

      Code_Peer.Summary_Models.Gtk_New
        (Self.Analysis_Model,
         Tree,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories,
         Project_Icon,
         File_Icon,
         Subprogram_Icon);
      Gtk.Tree_View.Gtk_New
        (Self.Analysis_View,
         Gtk.Tree_Model.Gtk_Tree_Model (Self.Analysis_Model));
      Scrolled.Add (Self.Analysis_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Entity");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Column.Pack_Start (Pixbuf_Renderer, False);
      Column.Add_Attribute
        (Pixbuf_Renderer,
         "pixbuf",
         Code_Peer.Summary_Models.Entity_Icon_Column);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer, "text", Code_Peer.Summary_Models.Entity_Name_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Lifeage");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, True);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Entity_Lifeage_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("High" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.High_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.High_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Medium" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Medium_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.Medium_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "base"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Base_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-("Low" & Ada.Characters.Latin_1.LF & "deltas"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Deltas_Count_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-(Ada.Characters.Latin_1.LF & "now"));
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Summary_Models.Low_Current_Count_Column);
      Column.Add_Attribute
        (Text_Renderer,
         "cell_background",
         Code_Peer.Summary_Models.Low_Current_Color_Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Dummy := Self.Analysis_View.Append_Column (Column);

      --  Message category view

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 200);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Report_Pane.Pack2 (Scrolled);

      Code_Peer.Entity_Messages_Models.Gtk_New
        (Self.Messages_Model,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories);
      Gtk.Tree_View.Gtk_New (Self.Messages_View, Self.Messages_Model);
      Scrolled.Add (Self.Messages_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Message category");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Category_Name_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"High");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.High_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Medium");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Medium_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Low");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Entity_Messages_Models.Low_Count_Column);
      Dummy := Self.Messages_View.Append_Column (Column);

      Tree_View_Report_Return_Boolean_Callbacks.Connect
        (Self.Analysis_View,
         Gtk.Widget.Signal_Button_Press_Event,
         Tree_View_Report_Return_Boolean_Callbacks.To_Marshaller
           (On_Analysis_Click'Access),
         Summary_Report (Self),
         False);

      --  Filter view

      Gtk.Box.Gtk_New_Vbox (Filter_Box);
      Filter_Box.Set_Size_Request (Width => 250);
      Self.Pack2 (Filter_Box);

      Gtk.Check_Button.Gtk_New (Check, -"Show all subprograms");
--      Filter_Box.Pack_Start (Check, False);
--  This check button is not displayed by default, see H519-028 discussion
      Check_Button_Report_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Report_Callbacks.To_Marshaller
           (On_Show_All_Subprograms_Toggled'Access),
         Summary_Report (Self));

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Filter_Box.Pack_Start (Scrolled);

      Code_Peer.Messages_Filter_Models.Gtk_New
        (Self.Hide_Model,
         Code_Peer.Project_Data'Class
           (Code_Analysis.Get_Or_Create
              (Tree,
               GPS.Kernel.Project.Get_Project
                 (Kernel)).Analysis_Data.Code_Peer_Data.all).
                    Message_Categories);

      Gtk.Tree_View.Gtk_New (Self.Hide_View, Self.Hide_Model);
      Scrolled.Add (Self.Hide_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle_Renderer);
      Column.Pack_End (Toggle_Renderer, False);
      Column.Add_Attribute
        (Toggle_Renderer,
         "active",
         Code_Peer.Messages_Filter_Models.Active_Column);
      Dummy := Self.Hide_View.Append_Column (Column);
      Cell_Renderer_Toggle_Report_Callbacks.Connect
        (Toggle_Renderer,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Report_Callbacks_Marshallers.To_Marshaller
           (On_Toggle_Category_Visibility'Access),
         Summary_Report (Self),
         True);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Message categories");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_End (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Messages_Filter_Models.Name_Column);
      Dummy := Self.Hide_View.Append_Column (Column);

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
      --  Models' internal data must be cleaned before the code analysis data
      --  is cleaned, because models catch direct references to the code
      --  analysis data.

      Self.Analysis_Model.Clear;
      Self.Messages_Model.Clear;
      Self.Hide_Model.Clear;
   end On_Destroy;

   -------------------------------------
   -- On_Show_All_Subprograms_Toggled --
   -------------------------------------

   procedure On_Show_All_Subprograms_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Summary_Report)
   is
   begin
      Self.Analysis_Model.Set_Show_All_Subprograms (Object.Get_Active);
   end On_Show_All_Subprograms_Toggled;

   -----------------------------------
   -- On_Toggle_Category_Visibility --
   -----------------------------------

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
      Self   : Summary_Report)
   is
      Iter  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                         Self.Hide_Model.Get_Iter_From_String
                           (Interfaces.C.Strings.Value (Path));

   begin
      if Object.Get_Active then
         Self.Hide_Model.Hide (Self.Hide_Model.Category_At (Iter));

      else
         Self.Hide_Model.Show (Self.Hide_Model.Category_At (Iter));
      end if;

      Self.Analysis_Model.Set_Visible_Message_Categories
        (Self.Hide_Model.Get_Visible_Categories);
   end On_Toggle_Category_Visibility;

end Code_Peer.Summary_Reports;
