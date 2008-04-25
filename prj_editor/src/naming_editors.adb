-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2008, AdaCore             --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

with Glib; use Glib;
with Gtk.Label;               use Gtk.Label;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Object;              use Gtk.Object;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Handlers;         use Gtkada.Handlers;
with GPS.Kernel;              use GPS.Kernel;
with Language_Handlers;       use Language_Handlers;
with Projects;                use Projects;
with Project_Viewers;         use Project_Viewers;
with Case_Handling;           use Case_Handling;

package body Naming_Editors is

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Called when the naming editor is destroyed.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Naming_Array, Language_Naming_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Naming_Editor_Record'Class, Language_Naming_Editor);

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Naming : constant Naming_Editor := Naming_Editor (Widget);
   begin
      for P in Naming.Pages'Range loop
         Destroy (Naming.Pages (P).Naming);
         Unchecked_Free (Naming.Pages (P).Naming);
         Free (Naming.Pages (P).Language);
      end loop;
      Unchecked_Free (Naming.Pages);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Languages : String_List)
   is
      Supported : String_List := Known_Languages
        (Get_Language_Handler (Kernel));
      Naming    : Language_Naming_Editor;
      Old       : Language_Naming_Array_Access;
      Last      : Natural;
      Label     : Gtk_Label;

   begin
      Editor := new Naming_Editor_Record;
      Gtk.Notebook.Initialize (Editor);

      --  Create all possible pages. Creating on the fly seems to hit some gtk+
      --  bugs: when a new page is added, it is not immediately selectable by
      --  the user, until a page change has occurred. The performance hit when
      --  creating the pages at once is minimal, since the pages are not
      --  filled.

      for L in Supported'Range loop
         Naming := Get_Naming_Scheme_Page (Kernel, Supported (L).all);

         if Naming /= null then
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
            Editor.Pages (Last).Language := new String'(Supported (L).all);
            Mixed_Case (Editor.Pages (Last).Language.all);
            Editor.Pages (Last).Naming := Naming;

            Gtk_New (Label, Editor.Pages (Last).Language.all);
            Append_Page
              (Editor, Get_Window (Editor.Pages (Last).Naming), Label);
         end if;
      end loop;

      Show_All (Editor);

      Set_Current_Page (Editor, 0);
      Set_Visible_Pages (Editor, Kernel, Languages, No_Project);

      Widget_Callback.Connect (Editor, Signal_Destroy, On_Destroy'Access);

      Free (Supported);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor       : out Naming_Editor;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Project_Type)
   is
      Languages : String_List := Get_Languages (Project);
   begin
      Gtk_New (Editor, Kernel, Languages);
      Set_Visible_Pages (Editor, Kernel, Languages, Project);
      Free (Languages);
   end Gtk_New;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor       : access Naming_Editor_Record;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Languages    : String_List;
      Project      : Projects.Project_Type)
   is
      Current : constant Gint := Get_Current_Page (Editor);
      Exists  : Boolean;
   begin
      for P in Editor.Pages'Range loop
         Exists := Languages'Length = 0
           and then To_Lower (Editor.Pages (P).Language.all) = "ada";

         for L in Languages'Range loop
            if Equal
              (Editor.Pages (P).Language.all, Languages (L).all, False)
            then
               Exists := True;
               exit;
            end if;
         end loop;

         Editor.Pages (P).Is_Visible := Exists;

         if Exists then
            Show_All (Get_Window (Editor.Pages (P).Naming));
         else
            Hide_All (Get_Window (Editor.Pages (P).Naming));
         end if;

         if Exists then
            Show_Project_Settings
              (Editor.Pages (P).Naming, Kernel, Project, True);
         end if;
      end loop;

      --  Work around an apparent bug in gtk+: when the contents of a page is
      --  hidden, and the shown again, it is always displayed on top of the
      --  current page in the notebook. We thus see the contents of two or more
      --  pages at the same time...
      for Num in 0 .. Gint'Last loop
         exit when Get_Nth_Page (Editor, Num) = null;
         Set_Current_Page (Editor, Num);
      end loop;
      Set_Current_Page (Editor, Current);
   end Set_Visible_Pages;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor             : access Naming_Editor_Record;
      Project            : Projects.Project_Type;
      Languages          : String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      Changed : Boolean := False;
   begin
      --  Each package will update its appropriate attributes in the project,
      --  so we do not need to remove the naming package. In fact, this allows
      --  each of the naming scheme editor to check whether the naming scheme
      --  has actually changed.

      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Is_Visible then
            Changed := Changed or Create_Project_Entry
              (Editor.Pages (P).Naming,
               Project, Languages, Scenario_Variables);
         end if;
      end loop;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor       : access Naming_Editor_Record;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Display_Exceptions : Boolean := True)
   is
      Languages : String_List := Get_Languages (Project);
   begin
      if Editor.Pages /= null then
         for P in Editor.Pages'Range loop
            Show_Project_Settings
              (Editor.Pages (P).Naming, Kernel, Project, Display_Exceptions);
         end loop;
      end if;

      Free (Languages);
   end Show_Project_Settings;

end Naming_Editors;
