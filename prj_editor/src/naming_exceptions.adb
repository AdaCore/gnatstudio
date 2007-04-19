-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
--                              AdaCore                              --
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

with GNAT.OS_Lib;            use GNAT.OS_Lib;

with Glib;                   use Glib;
with Gdk.Event;              use Gdk.Event;
with Gdk.Types.Keysyms;      use Gdk.Types, Gdk.Types.Keysyms;
with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Object;             use Gtk.Object;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Handlers;        use Gtkada.Handlers;

with Projects;               use Projects;
with Projects.Editor;        use Projects.Editor;
with GPS.Intl;               use GPS.Intl;
with Basic_Types;
with GUI_Utils;              use GUI_Utils;

package body Naming_Exceptions is

   Empty_Filename : constant String := "<filename>";

   procedure Destroy_Editor (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor is destroyed

   procedure On_Add (Editor : access Gtk_Widget_Record'Class);
   --  Called when a new exception is added

   function On_Edit_Filename
     (Editor : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user starts editing the filename

   function Delete_Exception
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when the user presses a key in the list of exceptions

   procedure Exception_Edited (Editor : access Gtk_Widget_Record'Class);
   --  Called when an existing exception has been edited

   --------------------
   -- Destroy_Editor --
   --------------------

   procedure Destroy_Editor (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Exceptions_Editor := Exceptions_Editor (Editor);
   begin
      Free (Ed.Language);
   end Destroy_Editor;

   ----------------------
   -- Exception_Edited --
   ----------------------

   procedure Exception_Edited (Editor : access Gtk_Widget_Record'Class) is
      Ed    : constant Exceptions_Editor := Exceptions_Editor (Editor);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Ed.Exceptions_List), Model, Iter);
      if Iter /= Null_Iter then
         if Get_String (Ed.Exceptions, Iter, 0) = "" then
            Remove (Ed.Exceptions, Iter);
         end if;
      end if;
   end Exception_Edited;

   ----------------------
   -- Delete_Exception --
   ----------------------

   function Delete_Exception
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Ed    : constant Exceptions_Editor := Exceptions_Editor (Editor);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      if Get_Event_Type (Event) = Key_Press
        and then (Get_Key_Val (Event) = GDK_BackSpace
                  or else Get_Key_Val (Event) = GDK_Delete)
      then
         Get_Selected (Get_Selection (Ed.Exceptions_List), Model, Iter);
         if Iter /= Null_Iter then
            Remove (Ed.Exceptions, Iter);
            return True;
         end if;
      end if;
      return False;
   end Delete_Exception;

   ------------
   -- On_Add --
   ------------

   procedure On_Add (Editor : access Gtk_Widget_Record'Class) is
      Ed       : constant Exceptions_Editor := Exceptions_Editor (Editor);
      Filename : constant String := Get_Text (Ed.Filename_Entry);
      Iter     : Gtk_Tree_Iter;
      Found    : Boolean := False;
   begin
      if Filename /= -Empty_Filename then
         --  Check if there is already an entry for this file
         Iter := Get_Iter_First (Ed.Exceptions);
         while Iter /= Null_Iter loop
            if Get_String (Ed.Exceptions, Iter, 0) = Filename then
               Found := True;
               exit;
            end if;
            Next (Ed.Exceptions, Iter);
         end loop;

         if not Found then
            Append (Ed.Exceptions, Iter, Null_Iter);
            Set (Ed.Exceptions, Iter, 0, Filename);
            Set (Ed.Exceptions, Iter, 1, True);
            Set_Text (Ed.Filename_Entry, -Empty_Filename);
         end if;

         Scroll_To_Cell
           (Ed.Exceptions_List,
            Get_Path (Ed.Exceptions, Iter),
            Column => Get_Column (Ed.Exceptions_List, 0),
            Use_Align => False,
            Row_Align => 0.0,
            Col_Align => 0.0);
      end if;
   end On_Add;

   ----------------------
   -- On_Edit_Filename --
   ----------------------

   function On_Edit_Filename
     (Editor : access Gtk_Widget_Record'Class) return Boolean
   is
      Ed       : constant Exceptions_Editor := Exceptions_Editor (Editor);
      Filename : constant String := Get_Text (Ed.Filename_Entry);
   begin
      if Filename = -Empty_Filename then
         Set_Text (Ed.Filename_Entry, "");
      end if;
      return False;
   end On_Edit_Filename;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor   : out Exceptions_Editor;
      Language : String)
   is
      Render   : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Num      : Gint;
      Box      : Gtk_Box;
      Button   : Gtk_Button;
      Scrolled : Gtk_Scrolled_Window;
      pragma Unreferenced (Num);
   begin
      Editor := new Exceptions_Editor_Record;
      Editor.Language := new String'(Language);
      Initialize_Vbox (Editor, Homogeneous => False);
      Widget_Callback.Connect (Editor, Signal_Destroy, Destroy_Editor'Access);

      Gtk_New (Scrolled);
      Pack_Start (Editor, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Editor.Exceptions, (0 => GType_String, 1 => GType_Boolean));
      Gtk_New (Editor.Exceptions_List, Editor.Exceptions);
      Add (Scrolled, Editor.Exceptions_List);
      Set_Mode (Get_Selection (Editor.Exceptions_List), Selection_Single);
      Return_Callback.Object_Connect
        (Editor.Exceptions_List, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Delete_Exception'Access), Editor);

      Gtk_New (Render);

      Gtk_New (Col);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, 0);
      Num := Append_Column (Editor.Exceptions_List, Col);
      Set_Title (Col, -"Filename");
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", 0);
      Add_Attribute (Col, Render, "editable", 1);
      Set_Editable_And_Callback (Editor.Exceptions, Render, 0);
      Widget_Callback.Object_Connect
        (Render, Signal_Edited, Exception_Edited'Access, Editor);

      Clicked (Col);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Editor, Box, Expand => False);

      Gtk_New (Editor.Filename_Entry);
      Set_Text (Editor.Filename_Entry, Empty_Filename);
      Pack_Start (Box, Editor.Filename_Entry, Expand => True, Fill => True);
      Return_Callback.Object_Connect
        (Editor.Filename_Entry, Signal_Key_Press_Event,
         On_Edit_Filename'Access, Editor);
      Widget_Callback.Object_Connect
        (Editor.Filename_Entry,
         Gtk.GEntry.Signal_Activate, On_Add'Access, Editor);

      Gtk_New (Button, -"Add");
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Add'Access, Editor);
   end Gtk_New;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor             : access Exceptions_Editor_Record;
      Project            : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array)
      return Boolean
   is
      Num_Rows : constant Gint := N_Children (Editor.Exceptions);
      Bodies   : Argument_List (1 .. Integer (Num_Rows));
      Changed  : Boolean := False;
      Iter     : Gtk_Tree_Iter := Get_Iter_First (Editor.Exceptions);

   begin
      for J in 0 .. Num_Rows - 1 loop
         Bodies (Integer (J + 1)) := new String'
           (Get_String (Editor.Exceptions, Iter, 0));
         Next (Editor.Exceptions, Iter);
      end loop;

      if Project = No_Project then
         Changed := True;
      else
         declare
            Old_Exceptions : Argument_List := Get_Attribute_Value
              (Project   => Project,
               Attribute => Impl_Exception_Attribute,
               Index     => Editor.Language.all);
         begin
            Changed := not Basic_Types.Is_Equal (Bodies, Old_Exceptions);
            Basic_Types.Free (Old_Exceptions);
         end;
      end if;

      if Changed then
         if Num_Rows /= 0 then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Impl_Exception_Attribute,
               Values             => Bodies,
               Attribute_Index    => Editor.Language.all);
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Impl_Exception_Attribute,
               Attribute_Index    => Editor.Language.all);
         end if;

         Changed := True;
      end if;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Exceptions_Editor_Record;
      Project            : Projects.Project_Type)
   is
      Iter   : Gtk_Tree_Iter := Null_Iter;
      Freeze : Gint;
   begin
      Clear (Editor.Exceptions);

      if Project /= No_Project then
         declare
            Bodies : Argument_List := Get_Attribute_Value
              (Project,
               Attribute => Impl_Exception_Attribute,
               Index     => Editor.Language.all);
         begin
            Freeze := Freeze_Sort (Editor.Exceptions);
            for B in Bodies'Range loop
               Append (Editor.Exceptions, Iter, Null_Iter);
               Set (Editor.Exceptions, Iter, 0, Bodies (B).all);
               Set (Editor.Exceptions, Iter, 1, True);
            end loop;

            Thaw_Sort (Editor.Exceptions, Freeze);
            Basic_Types.Free (Bodies);
         end;
      end if;
   end Show_Project_Settings;

end Naming_Exceptions;
