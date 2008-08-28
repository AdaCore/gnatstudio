-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2008, AdaCore              --
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

with GPS.Kernel;           use GPS.Kernel;
with GPS.Intl;             use GPS.Intl;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with Entities;               use Entities;

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

with GPS.Kernel.MDI;      use GPS.Kernel.MDI;

package body Refactoring is

   use File_Arrays;
   use Location_Arrays;

   ------------
   -- Dialog --
   ------------

   function Dialog
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title  : String;
      Msg    : String;
      Files  : File_Arrays.Instance;
      Execute_Label : String := Stock_Execute;
      Cancel_Label  : String := Stock_Cancel) return Boolean
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Label  : Gtk_Label;
      Result : Boolean;
   begin
      if Length (Files) > 0 then
         Gtk_New (Dialog,
                  Title  => Title,
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Destroy_With_Parent or Modal);
         Set_Default_Size (Dialog, -1, 350);

         Gtk_New (Label, Msg);
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Get_Vbox (Dialog), Label, Expand => False, Padding => 10);
         Pack_Start (Get_Vbox (Dialog), Create_File_List (Files));

         Button := Add_Button (Dialog, Execute_Label, Gtk_Response_OK);
         Grab_Default (Button);
         Grab_Focus (Button);
         Button := Add_Button (Dialog, Cancel_Label, Gtk_Response_Cancel);

         Show_All (Dialog);
         Result := Run (Dialog) = Gtk_Response_OK;
         --  No need to Destroy(Dialog), this is already taken care of by Run

         return Result;
      end if;
      return True;
   end Dialog;

   -------------------
   -- Confirm_Files --
   -------------------

   function Confirm_Files
     (Kernel          : access Kernel_Handle_Record'Class;
      Read_Only_Files : File_Arrays.Instance;
      No_LI_List      : File_Arrays.Instance;
      Stale_LI_List   : File_Arrays.Instance) return Boolean is
   begin
      return Dialog
        (Kernel,
         -"Read-only files",
         -("The following files are not writable, and will not be updated."
           & ASCII.LF
           & "Do you want to refactor the other files anyway ?"),
         Files => Read_Only_Files)

      and then Dialog
        (Kernel,
         -"Missing cross-references",
         -("The following files might contain references to the entity,"
           & ASCII.LF
           & "but no cross-reference information was found for them"),
         No_LI_List)

      and then Dialog
        (Kernel,
         -"Cross-references not up-to-date",
         -("The following files contain references to the entity, but the"
            & ASCII.LF
            & "cross-reference information is not up-to-date and replace"
            & " might fail"),
         Stale_LI_List);
   end Confirm_Files;

   ----------------------
   -- Create_File_List --
   ----------------------

   function Create_File_List
     (List : File_Arrays.Instance) return Gtk_Scrolled_Window
   is
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

      for F in File_Arrays.First .. Last (List) loop
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, 0,
              Display_Base_Name (Get_Filename (List.Table (F))));
         Set (Model, Iter, 1,
              Display_Dir_Name (Get_Filename (List.Table (F))));
      end loop;

      return Scrolled;
   end Create_File_List;

end Refactoring;
