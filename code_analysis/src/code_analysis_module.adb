-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;                      use Glib;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Window;                use Gtk.Window;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Code_Coverage;             use Code_Coverage;
with Code_Analysis_Tree_Model;  use Code_Analysis_Tree_Model;

with VFS;                       use VFS;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Styles;         use GPS.Kernel.Styles;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Location_View;         use GPS.Location_View;

package body Code_Analysis_Module is

   Src_File_Cst : aliased constant String := "src";
   --  Constant string that represents the name of the source file parameter
   --  of the GPS.CodeAnalysis.add_gcov_info subprogram
   Cov_File_Cst : aliased constant String := "cov";
   --  Constant string that represents the name of the .gcov file parameter
   --  of the GPS.CodeAnalysis.add_gcov_info subprogram

   ------------
   -- Create --
   ------------

   procedure Create
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property : constant Code_Analysis_Class
        := new Code_Analysis_Class_Record;
      Instance : Class_Instance;
   begin
      Instance          := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property.Projects := new Project_Maps.Map;
      GPS.Kernel.Scripts.Set_Property
        (Instance, Code_Analysis_Cst_Str,
         Instance_Property_Record (Property.all));
   end Create;

   -------------------
   -- Add_Gcov_Info --
   -------------------

   procedure Add_Gcov_Info
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property      : Code_Analysis_Class_Record;
      Instance      : Class_Instance;
      File_Contents : GNAT.OS_Lib.String_Access;
      Project_Name  : Project_Type;
      Project_Node  : Project_Access;
      Src_File      : Class_Instance;
      Cov_File      : Class_Instance;
      VFS_Src_File  : VFS.Virtual_File;
      VFS_Cov_File  : VFS.Virtual_File;
      File_Node     : Code_Analysis.File_Access;
   begin
      Instance      := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property      := Code_Analysis_Class_Record
        (Get_Property (Instance, Code_Analysis_Cst_Str));

      Name_Parameters (Data, (2 => Src_File_Cst'Access,
                              3 => Cov_File_Cst'Access));

      Src_File := Nth_Arg
           (Data, 2, Get_File_Class (Code_Analysis_Module_ID.Kernel),
            Default => No_Class_Instance, Allow_Null => True);

      if Src_File = No_Class_Instance then
         VFS_Src_File := VFS.No_File;
      else
         VFS_Src_File := Get_Data (Src_File);
      end if;

      if not Is_Regular_File (VFS_Src_File) then
         Set_Error_Msg (Data, "The name given for 'src' file is wrong");
         return;
      end if;

      Cov_File := Nth_Arg
           (Data, 3, Get_File_Class (Code_Analysis_Module_ID.Kernel),
            Default => No_Class_Instance, Allow_Null => True);

      if Cov_File = No_Class_Instance then
         VFS_Cov_File := VFS.No_File;
      else
         VFS_Cov_File := Get_Data (Cov_File);
      end if;

      if not Is_Regular_File (VFS_Cov_File) then
         Set_Error_Msg (Data, "The name given for 'cov' file is wrong");
         return;
      end if;

      Project_Name  := Get_Project_From_File
        (Get_Registry (Code_Analysis_Module_ID.Kernel).all, VFS_Src_File);
      Project_Node  := Get_Or_Create (Property.Projects, Project_Name);

      File_Node     := Get_Or_Create (Project_Node, VFS_Src_File);
      File_Node.Analysis_Data.Coverage_Data := new Node_Coverage;
      File_Contents := Read_File (VFS_Cov_File);
      Read_Gcov_Info (File_Node, File_Contents,
                      Node_Coverage
                        (File_Node.Analysis_Data.Coverage_Data.all).Children,
                      File_Node.Analysis_Data.Coverage_Data.Coverage);
      Compute_Project_Coverage (Project_Node);
      Free (File_Contents);
      GPS.Kernel.Scripts.Set_Property (Instance, Code_Analysis_Cst_Str,
                                       Instance_Property_Record (Property));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Add_Gcov_Info;

   ----------------------------
   -- List_Not_Covered_Lines --
   ----------------------------

   procedure List_Not_Covered_Lines
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      use Project_Maps;
      Property : Code_Analysis_Class_Record;
      Instance : Class_Instance;
      Map_Cur  : Project_Maps.Cursor;
   begin
      Instance := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property := Code_Analysis_Class_Record
        (Get_Property (Instance, Code_Analysis_Cst_Str));
      declare
         Sort_Arr : Project_Array (1 .. Integer (Property.Projects.Length));
      begin
         Map_Cur  := Property.Projects.First;

         for J in Sort_Arr'Range loop
            Sort_Arr (J) := Element (Map_Cur);
            Next (Map_Cur);
         end loop;

         Sort_Projects (Sort_Arr);

         for J in Sort_Arr'Range loop
            List_Not_Covered_Lines_In_Project (Sort_Arr (J));
         end loop;
      end;
   end List_Not_Covered_Lines;

   --------------------
   -- Show_Tree_View --
   --------------------

   procedure Show_Tree_View
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Property     : Code_Analysis_Class_Record;
      Instance     : Class_Instance;
      Project_Node : Project_Access;
      Scrolled     : Gtk_Scrolled_Window;
      Text_Render  : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend  : Gtk_Cell_Renderer_Pixbuf;
      Num_Col      : Gint;
      pragma Unreferenced (Num_Col, Project_Node);
   begin
      Instance      := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property      := Code_Analysis_Class_Record
        (Get_Property (Instance, Code_Analysis_Cst_Str));

      if Property.View = null then
         Property.View := new Code_Analysis_View_Record;
         Initialize_Hbox (Property.View);

         Property.View.Instance := Instance;

         if Code_Analysis_Module_ID.Project_Pixbuf = null then
            Code_Analysis_Module_ID.Project_Pixbuf := Render_Icon
              (Get_Main_Window (Code_Analysis_Module_ID.Kernel),
               "gps-project-closed", Gtk.Enums.Icon_Size_Menu);
            Code_Analysis_Module_ID.File_Pixbuf := Render_Icon
              (Get_Main_Window (Code_Analysis_Module_ID.Kernel), "gps-file",
               Gtk.Enums.Icon_Size_Menu);
            Code_Analysis_Module_ID.Subp_Pixbuf := Render_Icon
              (Get_Main_Window (Code_Analysis_Module_ID.Kernel), "gps-box",
               Gtk.Enums.Icon_Size_Menu);
            Code_Analysis_Module_ID.Warn_Pixbuf := Render_Icon
              (Get_Main_Window (Code_Analysis_Module_ID.Kernel),
               "gps-warning", Gtk.Enums.Icon_Size_Menu);
         end if;

         Gtk_New (Property.View.Model, GType_Array'
             (Pix_Col  => Gdk.Pixbuf.Get_Type,
              Name_Col => GType_String,
              Node_Col => GType_Pointer,
              Cov_Col  => GType_String,
              Sort_Col => GType_Int));
         Gtk_New (Property.View.Tree, Gtk_Tree_Model (Property.View.Model));

         -----------------
         -- Node column --
         -----------------

         Gtk_New (Property.View.Node_Column);
         Gtk_New (Pixbuf_Rend);
         Pack_Start (Property.View.Node_Column, Pixbuf_Rend, False);
         Add_Attribute
           (Property.View.Node_Column, Pixbuf_Rend, "pixbuf", Pix_Col);
         Gtk_New (Text_Render);
         Pack_Start (Property.View.Node_Column, Text_Render, False);
         Add_Attribute
           (Property.View.Node_Column, Text_Render, "text", Name_Col);
         Num_Col := Append_Column
           (Property.View.Tree, Property.View.Node_Column);
         Set_Title (Property.View.Node_Column, -"Entities");
         Set_Resizable (Property.View.Node_Column, True);
         Set_Sort_Column_Id (Property.View.Node_Column, Name_Col);

         ----------------------
         -- Coverage columns --
         ----------------------

         Gtk_New (Property.View.Cov_Column);
         Num_Col :=
           Append_Column (Property.View.Tree, Property.View.Cov_Column);

         Gtk_New (Text_Render);
         Pack_Start (Property.View.Cov_Column, Text_Render, False);
         Add_Attribute
           (Property.View.Cov_Column, Text_Render, "text", Cov_Col);

         Set_Title (Property.View.Cov_Column, -"Coverage");
         Set_Sort_Column_Id (Property.View.Cov_Column, Sort_Col);

         Gtk_New (Scrolled);
         Set_Policy
           (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Add (Scrolled, Property.View.Tree);
         Add (Property.View, Scrolled);
         Property.View.Iter := Get_Iter_First
           (Gtk_Tree_Model (Property.View.Model));

         GPS.Kernel.MDI.Gtk_New (Property.Child,
                                 Property.View,
                                 Group  => Group_VCS_Explorer,
                                 Module => Code_Analysis_Module_ID);
         Set_Title (Property.Child, -"Code Analysis Report");

         Register_Contextual_Menu
           (Code_Analysis_Module_ID.Kernel,
            Event_On_Widget => Property.View.Tree,
            Object          => Property.View,
            ID              => Module_ID (Code_Analysis_Module_ID),
            Context_Func    => Context_Func'Access);

         Widget_Callback.Connect (Property.View, "destroy", On_Destroy'Access);
         Gtkada.Handlers.Return_Callback.Object_Connect
           (Property.View.Tree,
            "button_press_event",
            Gtkada.Handlers.Return_Callback.To_Marshaller
              (On_Double_Click'Access),
            Property.View,
            After => False);

         Put (Get_MDI (Code_Analysis_Module_ID.Kernel), Property.Child);
         GPS.Kernel.Scripts.Set_Property
           (Instance, Code_Analysis_Cst_Str,
            Instance_Property_Record (Property));
      end if;

      Fill_Iter
        (Property.View.Model, Property.View.Iter, Property.Projects);
      Raise_Child (Property.Child);
   end Show_Tree_View;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Instance : Class_Instance;
      Property : Code_Analysis_Class_Record;
   begin
      Instance      := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property      := Code_Analysis_Class_Record
        (Get_Property (Instance, Code_Analysis_Cst_Str));
      Free_Code_Analysis (Property.Projects);
      Unchecked_Free (Property.View);
   end Destroy;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V        : constant Code_Analysis_View := Code_Analysis_View (View);
      Property : Code_Analysis_Class_Record;
   begin
      Property := Code_Analysis_Class_Record
        (Get_Property (V.Instance, Code_Analysis_Cst_Str));
      Property.View := null;
      GPS.Kernel.Scripts.Set_Property (V.Instance, Code_Analysis_Cst_Str,
                                       Instance_Property_Record (Property));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

   ---------------------
   -- On_Double_Click --
   ---------------------

   function On_Double_Click (View  : access Gtk_Widget_Record'Class;
                             Event : Gdk_Event) return Boolean
   is
      V         : constant Code_Analysis_View := Code_Analysis_View (View);
      Iter      : Gtk_Tree_Iter;
      Model     : Gtk_Tree_Model;
      Path      : Gtk_Tree_Path;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Get_Selected (Get_Selection (V.Tree), Model, Iter);
         Path := Get_Path (Model, Iter);

         if Get_Depth (Path) = 2 then
            File_Node := Code_Analysis.File_Access
              (GType_File.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
            Open_File_Editor (Code_Analysis_Module_ID.Kernel, File_Node.Name);
         elsif Get_Depth (Path) = 3 then
            File_Node := Code_Analysis.File_Access
              (GType_File.Get
                 (Gtk_Tree_Store (Model), Parent (Model, Iter), Node_Col));
            Subp_Node := Subprogram_Access
              (GType_Subprogram.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
            Open_File_Editor
              (Code_Analysis_Module_ID.Kernel,
               File_Node.Name,
               Subp_Node.Body_Line);
         end if;

         return True;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Double_Click;

   ------------------------------
   -- Add_Coverage_Annotations --
   ------------------------------

   procedure Add_Coverage_Annotations
     (Object : access Gtk_Widget_Record'Class)
   is
      View       : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter       : Gtk_Tree_Iter;
      Model      : Gtk_Tree_Model;
      Path       : Gtk_Tree_Path;
      File_Node  : Code_Analysis.File_Access;
      Subp_Node  : Subprogram_Access;
      Line_Info  : Line_Information_Data;
      Line_Icons : Line_Information_Data;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Path := Get_Path (Model, Iter);

      if Get_Depth (Path) = 2 then
         File_Node := Code_Analysis.File_Access
           (GType_File.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
         Open_File_Editor (Code_Analysis_Module_ID.Kernel, File_Node.Name);

      elsif Get_Depth (Path) = 3 then
         File_Node := Code_Analysis.File_Access
           (GType_File.Get
              (Gtk_Tree_Store (Model), Parent (Model, Iter), Node_Col));
         Subp_Node := Subprogram_Access
           (GType_Subprogram.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
         Open_File_Editor
           (Code_Analysis_Module_ID.Kernel,
            File_Node.Name,
            Subp_Node.Body_Line);
      end if;

      Line_Info  := new Line_Information_Array (File_Node.Lines'Range);
      Line_Icons := new Line_Information_Array (File_Node.Lines'Range);

      for J in File_Node.Lines'Range loop
         if File_Node.Lines (J) /= Null_Line then
            Line_Info (J).Text := Line_Coverage_Info
              (File_Node.Lines (J).Analysis_Data.Coverage_Data);

            if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage
              = 0 then
               Line_Icons (J).Image := Code_Analysis_Module_ID.Warn_Pixbuf;
            end if;
         end if;
      end loop;

      Create_Line_Information_Column
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name, "Coverage Icons");
      Add_Line_Information
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name, "Coverage Icons",
         Line_Icons);
      Create_Line_Information_Column
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name, "Coverage Analysis");
      Add_Line_Information
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name, "Coverage Analysis",
         Line_Info);
      Unchecked_Free (Line_Info);
      Unchecked_Free (Line_Icons);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Add_Coverage_Annotations;

   ---------------------------------
   -- Remove_Coverage_Annotations --
   ---------------------------------

   procedure Remove_Coverage_Annotations
     (Object : access Gtk_Widget_Record'Class)
   is
      View      : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter      : Gtk_Tree_Iter;
      Model     : Gtk_Tree_Model;
      Path      : Gtk_Tree_Path;
      File_Node : Code_Analysis.File_Access;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Path := Get_Path (Model, Iter);

      if Get_Depth (Path) = 2 then
         File_Node := Code_Analysis.File_Access
           (GType_File.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
         Open_File_Editor (Code_Analysis_Module_ID.Kernel, File_Node.Name);
      elsif Get_Depth (Path) = 3 then
         File_Node := Code_Analysis.File_Access
           (GType_File.Get
              (Gtk_Tree_Store (Model), Parent (Model, Iter), Node_Col));
      end if;

      Remove_Line_Information_Column
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name,
         "Coverage Icons");
      Remove_Line_Information_Column
        (Code_Analysis_Module_ID.Kernel,
         File_Node.Name,
         "Coverage Analysis");
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Remove_Coverage_Annotations;

   --------------------------------------------
   -- Menu_List_Not_Covered_Lines_In_Project --
   --------------------------------------------

   procedure Menu_List_Not_Covered_Lines_In_Project
     (Object : access Gtk_Widget_Record'Class)
   is
      View  : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Project_Node : Project_Access;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Project_Node := Project_Access
        (GType_Project.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
      List_Not_Covered_Lines_In_Project (Project_Node);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Menu_List_Not_Covered_Lines_In_Project;

   -----------------------------------------
   -- Menu_List_Not_Covered_Lines_In_File --
   -----------------------------------------

   procedure Menu_List_Not_Covered_Lines_In_File
     (Object : access Gtk_Widget_Record'Class)
   is
      View      : constant Code_Analysis_View := Code_Analysis_View (Object);
      Iter      : Gtk_Tree_Iter;
      Model     : Gtk_Tree_Model;
      File_Node : Code_Analysis.File_Access;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      File_Node := Code_Analysis.File_Access
        (GType_File.Get (Gtk_Tree_Store (Model), Iter, Node_Col));
      Open_File_Editor (Code_Analysis_Module_ID.Kernel, File_Node.Name);
      List_Not_Covered_Lines_In_File (File_Node);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Menu_List_Not_Covered_Lines_In_File;

   ---------------------------------------
   -- List_Not_Covered_Lines_In_Project --
   ---------------------------------------

   procedure List_Not_Covered_Lines_In_Project
     (Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur  : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr : Code_Analysis.File_Array
        (1 .. Integer (Project_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         List_Not_Covered_Lines_In_File (Sort_Arr (J));
      end loop;
   end List_Not_Covered_Lines_In_Project;

   ------------------------------------
   -- List_Not_Covered_Lines_In_File --
   ------------------------------------

   procedure List_Not_Covered_Lines_In_File
     (File_Node : Code_Analysis.File_Access) is
   begin
      for J in File_Node.Lines'Range loop
         if File_Node.Lines (J) /= Null_Line then
            if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage
              = 0 then
               Insert_Location
                 (Kernel    => Code_Analysis_Module_ID.Kernel,
                  Category  => Coverage_Category,
                  File      => File_Node.Name,
                  Text      => File_Node.Lines (J).Contents.all,
                  Line      => J,
                  Column    => 1,
                  Highlight => True,
                  Highlight_Category => Builder_Warnings_Style);
            end if;
         end if;
      end loop;
   end List_Not_Covered_Lines_In_File;

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
      pragma Unreferenced (Context, Kernel, Event_Widget);
      View      : constant Code_Analysis_View := Code_Analysis_View (Object);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Mitem     : Gtk_Menu_Item;
   begin

      Get_Path_At_Pos
        (View.Tree,
         Gint (X),
         Gint (Y),
         Path,
         Column,
         Buffer_X,
         Buffer_Y,
         Row_Found);

      if Path = null then
         return;
      end if;

      Select_Path (Get_Selection (View.Tree), Path);

      if Get_Depth (Path) > 1 then
         Gtk_New (Mitem, -"View with coverage annotations");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Add_Coverage_Annotations'Access,
            View, After => False);
         Append (Menu, Mitem);
         Gtk_New (Mitem, -"Remove coverage annotations");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Remove_Coverage_Annotations'Access,
            View, After => False);
         Append (Menu, Mitem);
      end if;

      if Get_Depth (Path) = 1 then
         Gtk_New (Mitem, -"List not covered lines");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Menu_List_Not_Covered_Lines_In_Project'Access,
            View, After => False);
         Append (Menu, Mitem);
      end if;

      if Get_Depth (Path) = 2 then
         Gtk_New (Mitem);
         Append (Menu, Mitem);
         Gtk_New (Mitem, -"List not covered lines");
         Gtkada.Handlers.Widget_Callback.Object_Connect
           (Mitem, "activate", Menu_List_Not_Covered_Lines_In_File'Access,
            View, After => False);
         Append (Menu, Mitem);
      end if;

   end Context_Func;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Code_Analysis_Class : constant Class_Type :=
        New_Class (Kernel, Code_Analysis_Cst_Str);
   begin
      Code_Analysis_Module_ID := new Code_Analysis_Module_ID_Record;
      Code_Analysis_Module_ID.Kernel := Kernel_Handle (Kernel);
      Code_Analysis_Module_ID.Class  := Code_Analysis_Class;
      Register_Module
        (Module      => Code_Analysis_Module_ID,
         Kernel      => Kernel,
         Module_Name => Code_Analysis_Cst_Str);

      Register_Command
        (Kernel, Constructor_Method,
         Class         => Code_Analysis_Class,
         Handler       => Create'Access);
      Register_Command
        (Kernel, "add_gcov_info",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Code_Analysis_Class,
         Handler       => Add_Gcov_Info'Access);
      Register_Command
        (Kernel, "list_not_covered_lines",
         Class         => Code_Analysis_Class,
         Handler       => List_Not_Covered_Lines'Access);
      Register_Command
        (Kernel, "show_tree_view",
         Class         => Code_Analysis_Class,
         Handler       => Show_Tree_View'Access);
      Register_Command
        (Kernel, Destructor_Method,
         Class         => Code_Analysis_Class,
         Handler       => Destroy'Access);
   end Register_Module;

end Code_Analysis_Module;
