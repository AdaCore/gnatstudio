-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Unchecked_Deallocation;
with Basic_Types;
with Glide_Kernel;           use Glide_Kernel;
with Glide_Intl;             use Glide_Intl;
with VFS;                    use VFS;

with Glib;                  use Glib;
with Gtk.Box;               use Gtk.Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;
with Gtk.Widget;            use Gtk.Widget;

package body Refactoring is

   -------------------
   -- Confirm_Files --
   -------------------

   function Confirm_Files
     (Kernel              : access Kernel_Handle_Record'Class;
      No_LI_List          : File_Array;
      Stale_LI_List       : File_Array) return Boolean
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Label  : Gtk_Label;
      pragma Unreferenced (Button);
      Result : Boolean;
   begin
      if No_LI_List'Length = 0 and then Stale_LI_List'Length = 0 then
         return True;
      end if;

      Gtk_New (Dialog,
               Title  => -"Missing cross-reference",
               Parent => Get_Main_Window (Kernel),
               Flags  => Destroy_With_Parent or Modal);

      Gtk_New (Label,
               -"Errors in cross-references. Continue refactoring anyway?");
      Set_Alignment (Label, 0.0, 0.0);
      Set_Padding (Label, 0, 20);
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      if No_LI_List'Length /= 0 then
         Gtk_New (Label, -"The following files might contain references"
                  & " to the entity," & ASCII.LF
                  & "but no cross-reference information was found for them");
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Vbox (Dialog), Label, Expand => False);
         Pack_Start (Get_Vbox (Dialog), Create_File_List (No_LI_List));
      end if;

      if Stale_LI_List'Length /= 0 then
         Gtk_New (Label, -"The following files contain references to the"
                  & " entity, but the" & ASCII.LF
                  & "cross-reference information is not up-to-date and replace"
                  & " will fail");
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Vbox (Dialog), Label, Expand => False);
         Pack_Start (Get_Vbox (Dialog), Create_File_List (Stale_LI_List));
      end if;

      Grab_Default (Add_Button (Dialog, Stock_Execute, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);
      Result := Run (Dialog) = Gtk_Response_OK;
      Destroy (Dialog);

      return Result;
   end Confirm_Files;

   ----------------------
   -- Create_File_List --
   ----------------------

   function Create_File_List (List : File_Array) return Gtk_Scrolled_Window is
      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Col_Number : Gint;
      Iter       : Gtk_Tree_Iter := Null_Iter;
      View       : Gtk_Tree_View;
      Model      : Gtk_Tree_Store;
      Scrolled   : Gtk_Scrolled_Window;
      pragma Unreferenced (Col_Number);
   begin
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Model, (0 => GType_String, 1 => GType_String));
      Gtk_New (View, Model);
      Add (Scrolled, View);
      Set_Headers_Visible (View, True);
      Set_Mode (Get_Selection (View), Selection_None);

      Gtk_New (Render);

      Gtk_New (Col);
      Set_Title (Col, -"Name");
      Set_Sort_Column_Id (Col, 0);
      Col_Number := Append_Column (View, Col);
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", 0);

      Gtk_New (Col);
      Set_Title (Col, -"Directory");
      Set_Sort_Column_Id (Col, 1);
      Col_Number := Append_Column (View, Col);
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", 1);

      for F in List'Range loop
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, 0, Base_Name (List (F)));
         Set (Model, Iter, 1, Dir_Name (List (F)).all);
      end loop;

      return Scrolled;
   end Create_File_List;

end Refactoring;
