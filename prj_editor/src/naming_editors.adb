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
with Glide_Kernel;            use Glide_Kernel;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Gtk.Label;               use Gtk.Label;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Widget;              use Gtk.Widget;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Namet;                   use Namet;
with Prj;                     use Prj;
with Prj_API;                 use Prj_API;
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
         if not Naming.Pages (P).Is_Visible then
            if Naming.Pages (P).Ada_Naming /= null then
               Unref (Naming.Pages (P).Ada_Naming);
            else
               Unref (Naming.Pages (P).Foreign_Naming);
            end if;
         end if;
      end loop;
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Languages : Argument_List) is
   begin
      Editor := new Naming_Editor_Record;
      Gtk.Notebook.Initialize (Editor);
      Set_Visible_Pages (Editor, Languages, No_Project);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor       : out Naming_Editor;
      Project_View : Prj.Project_Id)
   is
      Languages : Argument_List := Get_Languages (Project_View);
   begin
      Gtk_New (Editor, Languages);
      Free (Languages);
   end Gtk_New;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor       : access Naming_Editor_Record;
      Languages    : GNAT.OS_Lib.Argument_List;
      Project_View : Prj.Project_Id)
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

                  --  If the page has already been created, but is not visible,
                  --  then show it.

                  if not Editor.Pages (P).Is_Visible then
                     Editor.Pages (P).Is_Visible := True;
                     Gtk_New (Label, Editor.Pages (P).Language.all);

                     if Editor.Pages (P).Ada_Naming /= null then
                        Append_Page
                          (Editor,
                           Get_Window (Editor.Pages (P).Ada_Naming),
                           Label);
                        Unref (Editor.Pages (P).Ada_Naming);

                     else
                        Append_Page
                          (Editor,
                           Get_Window (Editor.Pages (P).Foreign_Naming),
                           Label);
                        Unref (Editor.Pages (P).Foreign_Naming);
                     end if;
                  end if;

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
         Editor.Pages (Last).Language := new String' (Name);
         Mixed_Case (Editor.Pages (Last).Language.all);
         Editor.Pages (Last).Is_Visible := True;

         Gtk_New (Label, Editor.Pages (Last).Language.all);

         if To_Lower (Name) = Ada_String then
            Gtk_New (Editor.Pages (Last).Ada_Naming);
            Append_Page
              (Editor, Get_Window (Editor.Pages (Last).Ada_Naming), Label);

            if Project_View /= No_Project then
               Show_Project_Settings
                 (Editor.Pages (Last).Ada_Naming, Project_View, True);
            end if;

         else
            Name_Len := Name'Length;
            Name_Buffer (1 .. Name_Len) := Name;
            Gtk_New (Editor.Pages (Last).Foreign_Naming, Name_Find);
            Append_Page
              (Editor, Get_Window (Editor.Pages (Last).Foreign_Naming),
               Label);

            if Project_View /= No_Project then
               Show_Project_Settings
                 (Editor.Pages (Last).Foreign_Naming, Project_View, True);
            end if;
         end if;
      end Create_Page;

   begin
      if Editor.Pages /= null then
         for P in Editor.Pages'Range loop
            if Editor.Pages (P).Is_Visible
              and then not Contains
                (Languages, Editor.Pages (P).Language.all, False)
            then
               Editor.Pages (P).Is_Visible := False;

               if Editor.Pages (P).Ada_Naming /= null then
                  Ref (Editor.Pages (P).Ada_Naming);
                  Remove_Page
                    (Editor,
                     Page_Num (Editor,
                               Get_Window (Editor.Pages (P).Ada_Naming)));
               else
                  Ref (Editor.Pages (P).Foreign_Naming);
                  Remove_Page
                    (Editor,
                     Page_Num (Editor,
                               Get_Window (Editor.Pages (P).Foreign_Naming)));
               end if;
            end if;
         end loop;
      end if;

      for L in Languages'Range loop
         Create_Page (Languages (L).all);
      end loop;

      if Languages'Length = 0 then
         Create_Page (Ada_String);
      end if;

      Show_All (Editor);
   end Set_Visible_Pages;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor          : access Naming_Editor_Record;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project         : Prj.Tree.Project_Node_Id;
      Project_View    : Prj.Project_Id;
      Ignore_Scenario : Boolean := False) return Boolean
   is
      Changed : Boolean := False;
   begin
      --  Each package will update its appropriate attributes in the project,
      --  so we do not need to remove the naming package. In fact, this allows
      --  each of the naming scheme editor to check whether the naming scheme
      --  has actually changed.

      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Is_Visible then
            if Editor.Pages (P).Ada_Naming /= null then
               Changed := Changed or Create_Project_Entry
                 (Editor.Pages (P).Ada_Naming, Kernel,
                  Project, Project_View, Ignore_Scenario);
            else
               Changed := Changed or Create_Project_Entry
                 (Editor.Pages (P).Foreign_Naming, Kernel, Project,
                  Project_View, Ignore_Scenario);
            end if;
         end if;
      end loop;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor : access Naming_Editor_Record;
      Project_View : Prj.Project_Id;
      Display_Exceptions : Boolean := True) is
   begin
      for P in Editor.Pages'Range loop
         if Editor.Pages (P).Ada_Naming /= null then
            Show_Project_Settings
              (Editor.Pages (P).Ada_Naming, Project_View, Display_Exceptions);
         else
            Show_Project_Settings
              (Editor.Pages (P).Foreign_Naming,
               Project_View, Display_Exceptions);
         end if;
      end loop;
   end Show_Project_Settings;

end Naming_Editors;
