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

with Ada.Exceptions;           use Ada.Exceptions;
with GNAT.OS_Lib;              use GNAT.OS_Lib;

with Glib;                     use Glib;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Window;               use Gtk.Window;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtkada.Handlers;          use Gtkada.Handlers;

with Code_Coverage;            use Code_Coverage;
with Code_Analysis_Tree_Model; use Code_Analysis_Tree_Model;

with VFS;                      use VFS;
with Projects;                 use Projects;
with Projects.Registry;        use Projects.Registry;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;

package body Code_Analysis_Module is

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
      VFS_File_Name : VFS.Virtual_File;
      Cov_File_Name : VFS.Virtual_File;
      File_Contents : GNAT.OS_Lib.String_Access;
      Project_Name  : Project_Type;
      Project_Node  : Project_Access;
      File_Name     : constant String := Nth_Arg (Data, 2);
      File_Node     : Code_Analysis.File_Access;
   begin
      Instance      := Nth_Arg (Data, 1, Code_Analysis_Module_ID.Class);
      Property      := Code_Analysis_Class_Record
        (Get_Property (Instance, Code_Analysis_Cst_Str));
      Cov_File_Name := Create (File_Name);
      VFS_File_Name := Create (File_Name
                               (File_Name'First .. File_Name'Last - 5));
      Project_Name  := Get_Project_From_File
        (Get_Registry (Code_Analysis_Module_ID.Kernel).all, VFS_File_Name);
      Project_Node  := Get_Or_Create (Property.Projects, Project_Name);

      File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
      File_Node.Analysis_Data.Coverage_Data := new Node_Coverage;
      File_Contents := Read_File (Cov_File_Name);

      Read_Gcov_Info (File_Node, File_Contents,
                      Node_Coverage
                        (File_Node.Analysis_Data.Coverage_Data.all).Children,
                      File_Node.Analysis_Data.Coverage_Data.Covered);
      Free (File_Contents);
      GPS.Kernel.Scripts.Set_Property (Instance, Code_Analysis_Cst_Str,
         Instance_Property_Record (Property));
   end Add_Gcov_Info;

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

         Widget_Callback.Connect (Property.View, "destroy", On_Destroy'Access);

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
         end if;

         Gtk_New (Property.View.Model, GType_Array'
             (Pix_Col  => Gdk.Pixbuf.Get_Type,
              Node_Col => GType_String,
              Cov_Col  => GType_String));
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
           (Property.View.Node_Column, Text_Render, "text", Node_Col);
         Num_Col := Append_Column
           (Property.View.Tree, Property.View.Node_Column);
         Set_Title (Property.View.Node_Column, -"Entities");
         Set_Resizable (Property.View.Node_Column, True);

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

         Gtk_New (Scrolled);
         Set_Policy
           (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Add (Scrolled, Property.View.Tree);
         Add (Property.View, Scrolled);
         Property.View.Iter := Get_Iter_First
           (Gtk_Tree_Model (Property.View.Model));
         Fill_Iter
           (Property.View.Model, Property.View.Iter, Property.Projects.First);

         GPS.Kernel.MDI.Gtk_New (Property.Child,
                                 Property.View,
                                 Module => Code_Analysis_Module_ID);
         Set_Title (Property.Child, -"Code Analysis Report");
         Put (Get_MDI (Code_Analysis_Module_ID.Kernel), Property.Child);
         GPS.Kernel.Scripts.Set_Property
           (Instance, Code_Analysis_Cst_Str,
            Instance_Property_Record (Property));
      end if;

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
      V : constant Code_Analysis_View := Code_Analysis_View (View);
      Property : Code_Analysis_Class_Record;
   begin
      Property := Code_Analysis_Class_Record
        (Get_Property (V.Instance, Code_Analysis_Cst_Str));
      Property.View := null;
      GPS.Kernel.Scripts.Set_Property
           (V.Instance, Code_Analysis_Cst_Str,
            Instance_Property_Record (Property));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

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
         Module_Name => Code_Analysis_Cst_Str,
         Priority    => Default_Priority);
      Register_Command
        (Kernel, Constructor_Method,
         Class         => Code_Analysis_Class,
         Handler       => Create'Access);
      Register_Command
        (Kernel, "add_gcov_info",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Code_Analysis_Class,
         Handler       => Add_Gcov_Info'Access);
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
