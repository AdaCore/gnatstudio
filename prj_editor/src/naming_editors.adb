-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with Ada_Naming_Editors;      use Ada_Naming_Editors;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Basic_Types;             use Basic_Types;
with Foreign_Naming_Editors;  use Foreign_Naming_Editors;
with Glide_Intl;              use Glide_Intl;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Project;    use Glide_Kernel.Project;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Label;               use Gtk.Label;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Namet;                   use Namet;
with Prj;                     use Prj;
with Prj_API;                 use Prj_API;
with String_Utils;            use String_Utils;

package body Naming_Editors is

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Called when the naming editor is destroyed.

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Naming : Naming_Editor := Naming_Editor (Widget);
   begin
      for P in Naming.Pages'Range loop
         Free (Naming.Pages (P).Language);
         if Naming.Pages (P).Ada_Naming /= null then
            Destroy (Naming.Pages (P).Ada_Naming);
         else
            Destroy (Naming.Pages (P).Foreign_Naming);
         end if;
      end loop;
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Languages : Argument_List)
   is
      Label : Gtk_Label;
   begin
      Editor := new Naming_Editor_Record;
      Gtk.Notebook.Initialize (Editor);

      --  Default is to have a single language, Ada
      if Languages'Length = 0 then
         Editor.Pages := new Language_Naming_Array (1 .. 1);
         Editor.Pages (1).Language := new String' (Ada_String);
         Mixed_Case (Editor.Pages (1).Language.all);
         Gtk_New (Label, Editor.Pages (1).Language.all);
         Gtk_New (Editor.Pages (1).Ada_Naming);
         Append_Page
           (Editor, Get_Window (Editor.Pages (1).Ada_Naming), Label);

      else
         Editor.Pages := new Language_Naming_Array (Languages'Range);
         for L in Languages'Range loop
            Editor.Pages (L).Language := new String' (Languages (L).all);
            Mixed_Case (Editor.Pages (L).Language.all);
            Gtk_New (Label, Editor.Pages (L).Language.all);

            if To_Lower (Languages (L).all) = Ada_String then
               Gtk_New (Editor.Pages (L).Ada_Naming);
               Append_Page
                 (Editor, Get_Window (Editor.Pages (L).Ada_Naming), Label);

            else
               Name_Len := Languages (L)'Length;
               Name_Buffer (1 .. Name_Len) := Languages (L).all;
               Gtk_New (Editor.Pages (L).Foreign_Naming, Name_Find);
               Append_Page
                 (Editor, Get_Window (Editor.Pages (L).Foreign_Naming), Label);
            end if;
         end loop;
      end if;

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Project_View : Prj.Project_Id)
   is
      Languages : Argument_List := Get_Languages (Project_View);
   begin
      Gtk_New (Editor, Languages);
      Free (Languages);
   end Gtk_New;

   ------------------------
   -- Edit_Naming_Scheme --
   ------------------------

   function Edit_Naming_Scheme
     (Parent       : access Gtk.Window.Gtk_Window_Record'Class;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project_View : Prj.Project_Id) return Boolean
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Editor : Naming_Editor;
   begin
      Gtk_New (Dialog,
               Title => -"Edit naming scheme for project "
                 & Project_Name (Project_View),
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent);

      Gtk_New (Editor, Project_View);
      Pack_Start (Get_Vbox (Dialog), Editor);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_Project_Settings (Editor, Project_View);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Create_Project_Entry
           (Editor, Kernel, Get_Project_From_View (Project_View));
         Destroy (Dialog);
         return True;
      end if;

      Destroy (Dialog);
      return False;
   end Edit_Naming_Scheme;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   procedure Create_Project_Entry
     (Editor  : access Naming_Editor_Record;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project : Prj.Tree.Project_Node_Id)
   is
      Changed : Boolean := False;
   begin
      --  Each package will update its appropriate attributes in the project,
      --  so we do not need to remove the naming package. In fact, this allows
      --  each of the naming scheme editor to check whether the naming scheme
      --  has actually changed.

      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Ada_Naming /= null then
            Changed := Changed or Create_Project_Entry
              (Editor.Pages (P).Ada_Naming, Kernel, Project);
         else
            Changed := Changed or Create_Project_Entry
              (Editor.Pages (P).Foreign_Naming, Kernel, Project);
         end if;
      end loop;

      if Changed then
         Set_Project_Modified (Kernel, Project, True);
         Recompute_View (Kernel);
      end if;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor : access Naming_Editor_Record; Project_View : Prj.Project_Id) is
   begin
      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Ada_Naming /= null then
            Show_Project_Settings (Editor.Pages (P).Ada_Naming, Project_View);
         else
            Show_Project_Settings
              (Editor.Pages (P).Foreign_Naming, Project_View);
         end if;
      end loop;
   end Show_Project_Settings;

end Naming_Editors;
