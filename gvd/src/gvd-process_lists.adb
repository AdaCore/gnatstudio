------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Strings.Fixed;         use Ada.Strings, Ada.Strings.Fixed;
with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;
with Glib_Values_Utils;         use Glib_Values_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Main_Window;           use GPS.Main_Window;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk;                       use Gtk;
with GVD.Proc_Utils;            use GVD.Proc_Utils;

package body GVD.Process_Lists is

   Column_Pid     : constant := 0;
   Column_Command : constant := 1;

   procedure On_Select_Row (List : access GObject_Record'Class);
   --  Called when the user selects a new row in the list

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self    : out Process_List;
      Process : not null access Visual_Debugger_Record'Class)
   is
      Success        : Boolean;
      Info           : Process_Info;
      Hbox           : Gtk_Hbox;
      Scrolled       : Gtk_Scrolled_Window;
      T              : Gtk_Cell_Renderer_Text;
      C              : Gtk_Tree_View_Column;
      Dummy          : Gint;
      Dummy_Button   : Gtk_Widget;
      Iter           : Gtk_Tree_Iter;

   begin
      Self := new Process_List_Record;
      Gtk.Dialog.Initialize
        (Self,
         Title         => -"Select the process to attach to",
         Parent        => Process.Kernel.Get_Main_Window,
         Flags         => Modal or Destroy_With_Parent);
      Set_Default_Size_From_History
        (Self, "gvd-processes", Process.Kernel, 500, 250);

      Gtk_New_Hbox (Hbox, False);
      Self.Get_Content_Area.Pack_Start (Hbox, True, True);

      --  The list of processes

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Hbox.Pack_Start (Scrolled, True, True);

      Gtk_New (Self.Tree_Model,
               (Column_Pid     => GType_String,
                Column_Command => GType_String));
      Gtk_New (Self.Tree_View, Self.Tree_Model);
      Self.Tree_Model.Unref;   --  owned by the tree view

      Scrolled.Add (Self.Tree_View);
      Self.Tree_View.Set_Headers_Visible (True);
      Self.Tree_View.Get_Selection.On_Changed (On_Select_Row'Access, Self);

      Gtk_New (C);
      C.Set_Title ("Pid");
      Dummy := Self.Tree_View.Append_Column (C);
      C.Set_Sort_Column_Id (Column_Pid);

      Gtk_New (T);
      Pack_Start (C, T, False);
      Add_Attribute (C, T, "text", Column_Pid);

      Gtk_New (C);
      C.Set_Title ("Command");
      Dummy := Self.Tree_View.Append_Column (C);
      C.Set_Sort_Column_Id (Column_Command);

      Gtk_New (T);
      Pack_Start (C, T, True);
      Add_Attribute (C, T, "text", Column_Command);

      --  The field to manually enter text

      Gtk_New_Hbox (Hbox, False, 0);
      Self.Get_Content_Area.Pack_Start (Hbox, False, False);

      Gtk_New (Self.Ent);
      Hbox.Pack_Start (Self.Ent, True, True);

      --  Action buttons

      Dummy_Button := Self.Add_Button (Stock_Ok, Gtk_Response_OK);
      Dummy_Button.Grab_Default;
      Dummy_Button := Self.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

      --  Show the list of processes

      Process.Debugger.Open_Processes;
      loop
         Process.Debugger.Next_Process (Info, Success);
         exit when not Success;

         Self.Tree_Model.Append (Iter, Null_Iter);
         Set_And_Clear
           (Self.Tree_Model, Iter,
            (Column_Pid     => As_String (Info.Id),
             Column_Command => As_String (Info.Info)));
      end loop;
      Process.Debugger.Close_Processes;

      Self.Tree_View.Columns_Autosize;
   end Gtk_New;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Self  : not null access Process_List_Record)
      return String
   is
   begin
      Self.Show_All;

      if Self.Run = Gtk_Response_OK then
         return Self.Ent.Get_Text;
      else
         return "";
      end if;
   end Get_Selection;

   -------------------
   -- On_Select_Row --
   -------------------

   procedure On_Select_Row (List : access GObject_Record'Class) is
      Self  : constant Process_List := Process_List (List);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Self.Tree_View.Get_Selection.Get_Selected (Model, Iter);
      Self.Ent.Set_Text (Trim (Get_String (Model, Iter, Column_Pid), Left));
   end On_Select_Row;

end GVD.Process_Lists;
