------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Glib.Properties;
with Gtk.Cell_Renderer;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Window;                 use Gtk.Window;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item;    use Gtk.Separator_Menu_Item;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Image;                  use Gtk.Image;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Progress; use Gtk.Cell_Renderer_Progress;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtkada.Handlers;            use Gtkada.Handlers;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Messages.View;   use GPS.Kernel.Messages.View;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                   use GPS.Intl;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with Traces;                     use Traces;
with Basic_Types;                use Basic_Types;
with Code_Analysis_Tree_Model;   use Code_Analysis_Tree_Model;
with Coverage_GUI;               use Coverage_GUI;

package body Code_Analysis_GUI is

   ---------------------------
   -- Build_Analysis_Report --
   ---------------------------

   function Build_Analysis_Report
     (Kernel   : Kernel_Handle;
      Name     : String_Access;
      Projects : Code_Analysis_Tree;
      Binary   : Boolean) return Code_Analysis_View
   is
      View        : constant Code_Analysis_View :=
                      new Code_Analysis_View_Record;
      Scrolled    : Gtk_Scrolled_Window;
      Text_Render : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Bar_Render  : Gtk_Cell_Renderer_Progress;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);
      --  Warning board widgets
      Warning_Image    : Gtk_Image;
      Board_Label      : Gtk_Label;
      Full_Tree_Button : Gtk_Button;
      Label_And_Button : Gtk_Vbox;
      Button_Box       : Gtk_Hbox;
   begin
      View.Binary_Mode       := Binary;
      View.Icons.Prj_Pixbuf  := Render_Icon
        (Get_Main_Window (Kernel), Prj_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      View.Icons.File_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel), File_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      View.Icons.Subp_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel), Subp_Pixbuf_Cst, Gtk.Enums.Icon_Size_Menu);
      Initialize_Vbox (View, False, 0);
      View.Projects := Projects;
      Gtk_New (View.Model, GType_Array'
          (Pix_Col     => Gdk.Pixbuf.Get_Type,
           Name_Col    => GType_String,
           Node_Col    => GType_Pointer,
           File_Col    => GType_Pointer,
           Prj_Col     => GType_Pointer,
           Cov_Col     => GType_String,
           Cov_Sort    => GType_Int,
           Cov_Bar_Txt => GType_String,
           Cov_Bar_Val => GType_Int));
      Gtk_New (View.Tree, Gtk_Tree_Model (View.Model));
      --  Ideally, we should have used Set_Name on the view itself, but for
      --  historical reasons we set it on View.Tree. This name can be retrieved
      --  using the Name function below.
      Set_Name (View.Tree, Name.all); --  testsuite

      ------------------
      --  Error_Board --
      ------------------

      Gtk_New_Hbox (View.Error_Board, False, 7);
      Set_Name (View.Error_Board, Name.all & "_Error_Board"); --  testsuite
      Gtk_New_Vbox (Label_And_Button, False, 7);
      Gtk_New_Hbox (Button_Box);
      Gtk_New
        (Warning_Image, Stock_Dialog_Warning, Icon_Size_Dialog);
      Gtk_New
        (Board_Label,
         -"This coverage report is empty. You can populate it with the "
         & '"' & (-"Load data..." & '"' &
           (-" entries of the /Tools/Coverage menu or the button below."
             )));
      Set_Line_Wrap (Board_Label, True);
      Set_Justify (Board_Label, Justify_Left);
      Gtk_New (View.Load_Button, -"Load data for all projects");
      Set_Name (View.Load_Button, Name.all & "_Load_Button"); --  testsuite
      Pack_Start (View.Error_Board, Warning_Image, False, False, 7);
      Pack_Start (Label_And_Button, Board_Label, False, True, 7);
      Pack_Start (Button_Box, View.Load_Button, False, False, 0);
      Pack_Start (Label_And_Button, Button_Box, False, True, 7);
      Pack_Start (View.Error_Board, Label_And_Button, False, True, 0);
      Pack_Start (View, View.Error_Board, False, True, 0);
      Set_No_Show_All (View.Error_Board, True);

      ------------------
      --  Empty_Board --
      ------------------

      Gtk_New_Hbox (View.Empty_Board, False, 7);
      Gtk_New_Vbox (Label_And_Button, False, 7);
      Gtk_New_Hbox (Button_Box);
      Gtk_New
        (Warning_Image, Stock_Dialog_Warning, Icon_Size_Dialog);
      Gtk_New
        (Board_Label,
         -"There is nothing in this flat view. You should try to display the "
         & ("full tree."));
      Set_Line_Wrap (Board_Label, True);
      Set_Justify (Board_Label, Justify_Left);
      Gtk_New (Full_Tree_Button, -"Show full tree");
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Full_Tree_Button, Gtk.Button.Signal_Clicked,
         Show_Full_Tree'Access, View);
      Pack_Start (View.Empty_Board, Warning_Image, False, False, 7);
      Pack_Start (Label_And_Button, Board_Label, False, True, 7);
      Pack_Start (Button_Box, Full_Tree_Button, False, False, 0);
      Pack_Start (Label_And_Button, Button_Box, False, True, 7);
      Pack_Start (View.Empty_Board, Label_And_Button, False, True, 0);
      Pack_Start (View, View.Empty_Board, False, True, 0);
      Set_No_Show_All (View.Empty_Board, True);

      ------------------
      --  Node column --
      ------------------

      Gtk_New (View.Node_Column);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (View.Node_Column, Pixbuf_Rend, False);
      Add_Attribute (View.Node_Column, Pixbuf_Rend, "pixbuf", Pix_Col);
      Gtk_New (Text_Render);
      Pack_Start (View.Node_Column, Text_Render, False);
      Add_Attribute (View.Node_Column, Text_Render, "text", Name_Col);
      Dummy := Append_Column (View.Tree, View.Node_Column);
      Set_Title (View.Node_Column, -"Entities");
      Set_Resizable (View.Node_Column, True);
      Set_Reorderable (View.Node_Column, True);
      Set_Sort_Column_Id (View.Node_Column, Name_Col);

      -----------------------
      --  Coverage columns --
      -----------------------

      Gtk_New (View.Cov_Column);
      Dummy := Append_Column (View.Tree, View.Cov_Column);
      Gtk_New (Text_Render);
      Pack_Start (View.Cov_Column, Text_Render, False);
      Add_Attribute (View.Cov_Column, Text_Render, "text", Cov_Col);
      Set_Title (View.Cov_Column, -"Coverage");
      Set_Reorderable (View.Cov_Column, True);
      Set_Resizable (View.Cov_Column, True);
      Set_Sort_Column_Id (View.Cov_Column, Cov_Sort);

      Gtk_New (View.Cov_Percent);
      Dummy := Append_Column (View.Tree, View.Cov_Percent);
      Gtk_New (Bar_Render);
      Glib.Properties.Set_Property
        (Bar_Render,
         Gtk.Cell_Renderer.Width_Property,
         Progress_Bar_Width_Cst);
      Pack_Start (View.Cov_Percent, Bar_Render, False);
      Add_Attribute (View.Cov_Percent, Bar_Render, "text", Cov_Bar_Txt);
      Add_Attribute (View.Cov_Percent, Bar_Render, "value", Cov_Bar_Val);
      Set_Title (View.Cov_Percent, -"Percentage");
      Set_Resizable (View.Cov_Percent, True);
      Set_Reorderable (View.Cov_Percent, True);
      Set_Sort_Column_Id (View.Cov_Percent, Cov_Bar_Val);
      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Add (Scrolled, View.Tree);
      Add (View, Scrolled);

      return View;
   end Build_Analysis_Report;

   ----------
   -- Name --
   ----------

   function Name (View : access Code_Analysis_View_Record'Class) return String
   is
   begin
      return Get_Name (View.Tree);
   end Name;

   -----------
   -- Clear --
   -----------

   procedure Clear (View : access Code_Analysis_View_Record'Class) is
   begin
      View.Model.Clear;
   end Clear;

   ----------------------------
   -- Expand_All_From_Report --
   ----------------------------

   procedure Expand_All_From_Report
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
   begin
      Expand_All (View.Tree);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Expand_All_From_Report;

   ------------------------------
   -- Collapse_All_From_Report --
   ------------------------------

   procedure Collapse_All_From_Report
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
   begin
      Collapse_All (View.Tree);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Collapse_All_From_Report;

   --------------------
   -- Show_Full_Tree --
   --------------------

   procedure Show_Full_Tree (Object : access Gtk_Widget_Record'Class) is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
      Path : Gtk_Tree_Path;
   begin
      Hide_All (View.Empty_Board);
      Clear (View.Model);
      Fill_Iter
        (View.Model, Iter, View.Projects, View.Binary_Mode, View.Icons);
      Iter := Get_Iter_First (View.Model);
      --  can't be null as the corresponding menu entry is not displayed when
      --  there nothing in the Model.
      Path := Get_Path (View.Model, Iter);
      Collapse_All (View.Tree);
      Expand_To_Path (View.Tree, Path);
      Select_Path (Get_Selection (View.Tree), Path);
      Path_Free (Path);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Full_Tree;

   -----------------------------
   -- Show_Flat_List_Of_Files --
   -----------------------------

   procedure Show_Flat_List_Of_Files
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
   begin
      Clear (View.Model);
      Fill_Iter_With_Files
        (View.Model, Iter, View.Projects, View.Binary_Mode, View.Icons);

      if Get_Iter_First (View.Model) = Null_Iter then
         --  Show the empty flat view warning board
         if Get_No_Show_All (View.Empty_Board) then
            Set_No_Show_All (View.Empty_Board, False);
         end if;

         Show_All (View.Empty_Board);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Flat_List_Of_Files;

   -----------------------------------
   -- Show_Flat_List_Of_Subprograms --
   -----------------------------------

   procedure Show_Flat_List_Of_Subprograms
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter : Gtk_Tree_Iter := Get_Iter_First (View.Model);
   begin
      Clear (View.Model);
      Fill_Iter_With_Subprograms
        (View.Model, Iter, View.Projects, View.Binary_Mode, View.Icons);
      if Get_Iter_First (View.Model) = Null_Iter then
         --  Show the empty flat view warning board
         if Get_No_Show_All (View.Empty_Board) then
            Set_No_Show_All (View.Empty_Board, False);
         end if;

         Show_All (View.Empty_Board);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Flat_List_Of_Subprograms;

   ---------------------
   -- On_Double_Click --
   ---------------------

   function On_Double_Click
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Kernel : Kernel_Handle) return Boolean
   is
      use Code_Analysis_Tree_Model.File_Set;
      View  : constant Code_Analysis_View := Code_Analysis_View (Object);
      Tree  : constant Gtk_Tree_View := View.Tree;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Get_Selected (Get_Selection (Tree), Model, Iter);

         if Iter /= Null_Iter then
            declare
               Node : constant Node_Access := Code_Analysis.Node_Access
                 (Node_Set.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
            begin
               if Node.all in Code_Analysis.Project'Class then
                  --  So we are on a project node
                  null;
               elsif Node.all in Code_Analysis.File'Class then
                  --  So we are on a file node
                  Open_File_Editor_On_File (Kernel, View, Iter);
               elsif Node.all in Code_Analysis.Subprogram'Class then
                  --  So we are on a subprogram node
                  Open_File_Editor_On_Subprogram (Kernel, View, Iter);
               end if;
            end;
            return True;
         end if;
      end if;

      return False;
   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Double_Click;

   ----------------------
   -- Open_File_Editor --
   ----------------------

   procedure Open_File_Editor
     (Kernel    : Kernel_Handle;
      View      : Code_Analysis_View;
      File_Node : Code_Analysis.File_Access;
      Quiet     : Boolean;
      Line      : Natural := 1;
      Column    : Natural := 1)
   is
      Marker : MDI_Location_Marker;
   begin
      Do_Not_Goto_First_Location (Kernel);
      List_File_Uncovered_Lines (Kernel, File_Node, Quiet);

      Marker := Create_MDI_Marker (View.Name & (-" Report"));
      Push_Marker_In_History (Kernel, Marker);

      Open_File_Editor
        (Kernel, File_Node.Name, Line,
         Basic_Types.Visible_Column_Type (Column));
      Add_File_Coverage_Annotations (Kernel, File_Node);
   end Open_File_Editor;

   ------------------------------
   -- Open_File_Editor_On_File --
   ------------------------------

   procedure Open_File_Editor_On_File
     (Kernel : Kernel_Handle;
      View   : Code_Analysis_View;
      Iter   : Gtk_Tree_Iter)
   is
      File_Node : constant File_Access := File_Access
        (File_Set.Get (View.Model, Iter, Node_Col));
   begin
      Open_File_Editor (Kernel, View, File_Node, False);
   end Open_File_Editor_On_File;

   ------------------------------------
   -- Open_File_Editor_On_Subprogram --
   ------------------------------------

   procedure Open_File_Editor_On_Subprogram
     (Kernel : Kernel_Handle;
      View   : Code_Analysis_View;
      Iter   : Gtk_Tree_Iter)
   is
      File_Node : constant File_Access := File_Access
        (File_Set.Get (View.Model, Iter, File_Col));
      Subp_Node : constant Subprogram_Access := Subprogram_Access
        (Subprogram_Set.Get (View.Model, Iter, Node_Col));
   begin
      Open_File_Editor
        (Kernel, View, File_Node, True, Subp_Node.Start, Subp_Node.Column);
   end Open_File_Editor_On_Subprogram;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);
      use Project_Maps;
      View      : constant Code_Analysis_View := Code_Analysis_View (Object);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Prj_Node  : Code_Analysis.Project_Access;
      File_Node : Code_Analysis.File_Access;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Item      : Gtk_Menu_Item;
      Sep       : Gtk_Separator_Menu_Item;
      Iter      : Gtk_Tree_Iter := Get_Iter_First (View.Model);

   begin

      Get_Path_At_Pos (View.Tree, Gint (X), Gint (Y), Path, Column,
                       Buffer_X, Buffer_Y, Row_Found);

      --------------------------------------------------------
      --  Report of Coverage # specific contextual entries  --
      --------------------------------------------------------

      if First (View.Projects.all) /= No_Element then
         Gtk_New (Item, -"Show flat list of files");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Show_Flat_List_Of_Files'Access, View);
         Append (Menu, Item);
         Gtk_New (Item, -"Show flat list of subprograms");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Show_Flat_List_Of_Subprograms'Access, View);
         Append (Menu, Item);

         if not Has_Child (View.Model, Iter) then
            Gtk_New (Item, -"Show full tree");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Show_Full_Tree'Access, View);
            Append (Menu, Item);
         else
            Gtk_New (Item, -"Expand all");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Expand_All_From_Report'Access, View);
            Append (Menu, Item);
            Gtk_New (Item, -"Collapse all");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Item, Gtk.Menu_Item.Signal_Activate,
               Collapse_All_From_Report'Access, View);
            Append (Menu, Item);
         end if;
      end if;

      ----------------------------------
      --  Set up context information  --
      ----------------------------------

      if Path /= null then
         Gtk_New (Sep);
         Append (Menu, Sep);
         Select_Path (Get_Selection (View.Tree), Path);
         Iter := Get_Iter (Gtk_Tree_Model (View.Model), Path);

         declare
            Node : constant Node_Access := Code_Analysis.Node_Access
              (Node_Set.Get (View.Model, Iter, Node_Col));
         begin
            if Node.all in Code_Analysis.Project'Class then
               --  So we are on a project node
               --  Context receive project information
               Set_File_Information
                 (Context, Project => Project_Access (Node).Name);

            elsif Node.all in Code_Analysis.File'Class then
               --  So we are on a file node
               --  Context receive project and file information
               Prj_Node := Project_Access
                 (Project_Set.Get (View.Model, Iter, Prj_Col));
               Set_File_Information
                 (Context,
                  Files   => (1 => Code_Analysis.File_Access (Node).Name),
                  Project => Prj_Node.Name);

            elsif Node.all in Code_Analysis.Subprogram'Class then
               --  So we are on a subprogram node
               --  Context receive project, file and entity information
               File_Node := Code_Analysis.File_Access
                 (File_Set.Get (View.Model, Iter, File_Col));
               Prj_Node  := Project_Access
                 (Project_Set.Get (View.Model, Iter, Prj_Col));
               Set_File_Information
                 (Context, (1 => File_Node.Name), Prj_Node.Name);
               Set_Entity_Information
                 (Context, Kernel.Symbols.Find
                    (Subprogram_Access (Node).Name.all));
            end if;
         end;
      end if;
   end Context_Func;

   -------------------------
   -- Initialize_Graphics --
   -------------------------

   procedure Initialize_Graphics (Kernel : Kernel_Handle) is
   begin
      if Covered_Line_Pixbuf /= null then
         return;
      end if;

      --  Constants here correspond to definitions in icons.xml

      Covered_Line_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel),
         "gps-gcov-covered", Gtk.Enums.Icon_Size_Menu);

      Uncovered_Line_Pixbuf := Render_Icon
        (Get_Main_Window (Kernel),
         "gps-gcov-non-covered", Gtk.Enums.Icon_Size_Menu);
   end Initialize_Graphics;

end Code_Analysis_GUI;
