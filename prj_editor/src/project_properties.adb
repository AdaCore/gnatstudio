-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;               use Glib.Object;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Text;                  use Gtk.Text;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Widget;                use Gtk.Widget;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Prj_API;                   use Prj_API;
with String_Utils;              use String_Utils;

package body Project_Properties is

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
      record
         Name        : Gtk.GEntry.Gtk_Entry;
         Path        : Gtk.GEntry.Gtk_Entry;
         Executables : Gtk.Text.Gtk_Text;
         Prefix      : Gtk.GEntry.Gtk_Entry;
         Convert     : Gtk.Check_Button.Gtk_Check_Button;
      end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Gtk_New
     (Editor       : out Properties_Editor;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class);
   --  Create a new properties editor

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class);
   --  Internal initialization function


   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor       : out Properties_Editor;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class) is
   begin
      Editor := new Properties_Editor_Record;
      Initialize (Editor, Project_View, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor       : access Properties_Editor_Record'Class;
      Project_View : Prj.Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Button  : Gtk_Widget;
      Button2 : Gtk_Button;
      Label   : Gtk_Label;
      Table   : Gtk_Table;
   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for "
           & Project_Name (Project_View),
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);

      Gtk_New (Table, Rows => 5, Columns => 3, Homogeneous => False);
      Pack_Start
        (Get_Vbox (Editor), Table, Expand => True, Fill => True);

      Gtk_New (Label, -"Name:");
      Attach (Table, Label, 0, 1, 0, 1, Xoptions => 0);
      Gtk_New (Editor.Name);
      Attach (Table, Editor.Name, 1, 3, 0, 1);

      Set_Text (Editor.Name, Project_Name (Project_View));

      Gtk_New (Label, -"Path:");
      Attach (Table, Label, 0, 1, 1, 2, Xoptions => 0);
      Gtk_New (Editor.Path);
      Attach (Table, Editor.Path, 1, 2, 1, 2);
      Set_Width_Chars (Editor.Path, 40);
      Gtk_New (Button2, -"Browse");
      Attach (Table, Button2, 2, 3, 1, 2, Xoptions => 0);

      Gtk_New (Editor.Convert, "Convert paths to absolute");
      Attach (Table, Editor.Convert, 1, 2, 2, 3);

      Set_Active (Editor.Convert, True);
      Set_Text (Editor.Path, Dir_Name (Project_Path (Project_View)));

      Gtk_New (Label, -"Main files:");
      Attach (Table, Label, 0, 1, 3, 4, Xoptions => 0);
      Gtk_New (Editor.Executables);
      Attach (Table, Editor.Executables, 1, 3, 3, 4);

      Set_Sensitive (Editor.Executables, False);
      Set_Sensitive (Label, False);

      Gtk_New (Label, -"Tools prefix:");
      Attach (Table, Label, 0, 1, 4, 5, Xoptions => 0);
      Gtk_New (Editor.Prefix);
      Attach (Table, Editor.Prefix, 1, 3, 4, 5);

      Set_Sensitive (Editor.Prefix, False);
      Set_Sensitive (Label, False);

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
   end Initialize;

   ---------------------
   -- Edit_Properties --
   ---------------------

   procedure Edit_Properties
     (Project_View : Prj.Project_Id;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Editor  : Properties_Editor;
      Changed : Boolean := False;
      Project : Project_Node_Id;
   begin
      Gtk_New (Editor, Project_View, Kernel);
      Show_All (Editor);

      if Run (Editor) = Gtk_Response_OK then
         declare
            New_Name : constant String := Get_Text (Editor.Name);
            New_Path : constant String :=
              Name_As_Directory (Get_Text (Editor.Path));

         begin
            Project := Get_Project_From_View (Project_View);

            if New_Name /= Project_Name (Project_View)
              or else New_Path /= Dir_Name (Project_Path (Project_View))
            then
               if Get_Active (Editor.Convert) then
                  --  The with statements will be automatically updated by the
                  --  call to Rename_And_Move.
                  Convert_Paths_To_Absolute (Project, False);
               end if;
               Rename_And_Move
                 (Root_Project => Get_Project (Kernel),
                  Project      => Project,
                  New_Name     => New_Name,
                  New_Path     => New_Path);
               Changed := True;
            end if;
         end;

         if Changed then
            Recompute_View (Kernel);
         end if;
      end if;

      Destroy (Editor);
   end Edit_Properties;

   -----------------------------
   -- Edit_Project_Properties --
   -----------------------------

   procedure Edit_Project_Properties
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      C : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Edit_Properties
        (Project_Information (C), Get_Kernel (Context));
   end Edit_Project_Properties;

end Project_Properties;
