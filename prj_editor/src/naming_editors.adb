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

with Glib; use Glib;
with Ada_Naming_Editors;      use Ada_Naming_Editors;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Basic_Types;             use Basic_Types;
with Foreign_Naming_Editors;  use Foreign_Naming_Editors;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Gtk.Label;               use Gtk.Label;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Namet;                   use Namet;
with Projects;                use Projects;
with String_Utils;            use String_Utils;
with Ada.Unchecked_Deallocation;

package body Naming_Editors is

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Called when the naming editor is destroyed.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Naming_Array, Language_Naming_Array_Access);

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Naming : constant Naming_Editor := Naming_Editor (Widget);
   begin
      for P in Naming.Pages'Range loop
         Free (Naming.Pages (P).Language);
      end loop;
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Languages : Argument_List) is
   begin
      Editor := new Naming_Editor_Record;
      Gtk.Notebook.Initialize (Editor);
      Set_Visible_Pages (Editor, Kernel, Languages, No_Project);

      Set_Current_Page (Editor, 0);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor       : out Naming_Editor;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Project_Type)
   is
      Languages : Argument_List := Get_Languages (Project);
   begin
      Gtk_New (Editor, Kernel, Languages);
      Free (Languages);
   end Gtk_New;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor       : access Naming_Editor_Record;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Languages    : GNAT.OS_Lib.Argument_List;
      Project      : Projects.Project_Type)
   is
      procedure Create_Page (Name : String);
      --  Create a page for the language Name

      -----------------
      -- Create_Page --
      -----------------

      procedure Create_Page (Name : String) is
         Label  : Gtk_Label;
         Old    : Language_Naming_Array_Access;
         Last   : Natural;
         Exists : Boolean := False;
      begin
         --  Has the page already been created ?

         if Editor.Pages /= null then
            for P in Editor.Pages'Range loop
               if To_Lower (Editor.Pages (P).Language.all) =
                 To_Lower (Name)
               then
                  Exists := True;
                  if Editor.Pages (P).Ada_Naming /= null then
                     Show (Get_Window (Editor.Pages (P).Ada_Naming));
                  else
                     Show (Get_Window (Editor.Pages (P).Foreign_Naming));
                  end if;
                  Editor.Pages (P).Is_Visible := True;

                  exit;
               end if;
            end loop;
         end if;

         if Exists then
            return;
         end if;

         --  Extend the array that stores all the editors
         if Editor.Pages = null then
            Editor.Pages := new Language_Naming_Array (1 .. 1);
         else
            Old := Editor.Pages;
            Editor.Pages := new Language_Naming_Array
              (Old'First .. Old'Last + 1);
            Editor.Pages (Old'Range) := Old.all;
            Unchecked_Free (Old);
         end if;

         --  Create the new page

         Last := Editor.Pages'Last;
         Editor.Pages (Last).Language := new String'(Name);
         Mixed_Case (Editor.Pages (Last).Language.all);

         Gtk_New (Label, Editor.Pages (Last).Language.all);

         if To_Lower (Name) = Ada_String then
            Gtk_New (Editor.Pages (Last).Ada_Naming);
            Append_Page
              (Editor, Get_Window (Editor.Pages (Last).Ada_Naming), Label);
            Show_All (Get_Window (Editor.Pages (Last).Ada_Naming));

            if Project /= No_Project then
               Show_Project_Settings
                 (Editor.Pages (Last).Ada_Naming, Project, True);
            end if;

         else
            Name_Len := Name'Length;
            Name_Buffer (1 .. Name_Len) := Name;
            Gtk_New (Editor.Pages (Last).Foreign_Naming, Name_Find);
            Append_Page
              (Editor, Get_Window (Editor.Pages (Last).Foreign_Naming),
               Label);
            Show_All (Get_Window (Editor.Pages (Last).Foreign_Naming));

            Show_Project_Settings
              (Editor.Pages (Last).Foreign_Naming,
               Kernel, Project, True);
         end if;

         Editor.Pages (Last).Is_Visible := True;
      end Create_Page;

      Current : constant Gint := Get_Current_Page (Editor);
   begin
      if Editor.Pages /= null then
         for P in Editor.Pages'Range loop
            if Editor.Pages (P).Ada_Naming /= null then
               Hide (Get_Window (Editor.Pages (P).Ada_Naming));
            else
               Hide (Get_Window (Editor.Pages (P).Foreign_Naming));
            end if;
            Editor.Pages (P).Is_Visible := False;
         end loop;
      end if;

      for L in Languages'Range loop
         Create_Page (Languages (L).all);
      end loop;

      if Languages'Length = 0 then
         Create_Page (Ada_String);
      end if;

      --  Work around an apparent bug in gtk+: when the contents of a page is
      --  hidden, and the shown again, it is always displayed on top of the
      --  current page in the notebook. We thus see the contents of two or more
      --  pages at the same time...
      Set_Current_Page (Editor, Current);
   end Set_Visible_Pages;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor          : access Naming_Editor_Record;
      Project         : Projects.Project_Type;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      Changed : Boolean := False;
   begin
      --  Each package will update its appropriate attributes in the project,
      --  so we do not need to remove the naming package. In fact, this allows
      --  each of the naming scheme editor to check whether the naming scheme
      --  has actually changed.

      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Ada_Naming /= null
           and then Editor.Pages (P).Is_Visible
         then
            Changed := Changed or Create_Project_Entry
              (Editor.Pages (P).Ada_Naming, Project, Scenario_Variables);
         elsif Editor.Pages (P).Foreign_Naming /= null
           and then Editor.Pages (P).Is_Visible
         then
            Changed := Changed or Create_Project_Entry
              (Editor.Pages (P).Foreign_Naming, Project, Scenario_Variables);
         end if;
      end loop;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor       : access Naming_Editor_Record;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Display_Exceptions : Boolean := True)
   is
      Languages : Argument_List := Get_Languages (Project);
   begin
      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Ada_Naming /= null then
            Show_Project_Settings
              (Editor.Pages (P).Ada_Naming, Project, Display_Exceptions);
         else
            Show_Project_Settings
              (Editor.Pages (P).Foreign_Naming, Kernel,
               Project, Display_Exceptions);
         end if;
      end loop;

      Free (Languages);
   end Show_Project_Settings;

end Naming_Editors;
