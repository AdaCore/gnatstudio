-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Object;               use Glib.Object;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Contexts;     use Glide_Kernel.Contexts;
with Glide_Kernel.Hooks;        use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Intl;                use Glide_Intl;
with Diff_Utils;                use Diff_Utils;
with Vdiff_Pkg;                 use Vdiff_Pkg;
with Vdiff_Utils;               use Vdiff_Utils;
with OS_Utils;                  use OS_Utils;

with Ada.Exceptions;            use Ada.Exceptions;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body Vdiff_Module is

   Me : constant Debug_Handle := Create (Vdiff_Module_Name);

   function Diff_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : Hooks_Data'Class) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->Compare->Two Files...

   function Compare_Two_Files
     (Kernel : access Kernel_Handle_Record'Class;
      File1, File2 : VFS.Virtual_File;
      Title1, Title2 : String;
      Result : Diff_Occurrence_Link) return MDI_Child;
   --  Compare two files.
   --  Return null if there are no differences

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Return the default context factory for Vdiff widgets.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   -----------------------
   -- Compare_Two_Files --
   -----------------------

   function Compare_Two_Files
     (Kernel : access Kernel_Handle_Record'Class;
      File1, File2 : VFS.Virtual_File;
      Title1, Title2 : String;
      Result : Diff_Occurrence_Link) return MDI_Child
   is
      R      : Diff_Occurrence_Link := Result;
      Vdiff  : Vdiff_Access;
      Child  : MDI_Child;
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
   begin
      if Result = null then
         Button := Message_Dialog
           (Msg     => -"No differences found.",
            Buttons => Button_OK,
            Parent  => Get_Main_Window (Kernel));
         return null;
      end if;

      Gtk_New (Vdiff);
      Set_Text (Vdiff.File_Label1, Title1);
      Set_Text (Vdiff.File_Label2, Title2);
      Vdiff.File1 := File1;
      Vdiff.File2 := File2;
      Fill_Diff_Lists
        (Kernel, Vdiff.Clist1, Vdiff.Clist2, File1, File2, Result);
      Show_All (Vdiff);
      Child := Put
        (Kernel, Vdiff,
         Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
         Default_Height => Get_Pref (Kernel, Default_Widget_Height),
         Module         => Vdiff_Module_ID);
      Set_Focus_Child (Child);
      Set_Title (Child, -"Visual Comparison");

      Free (R);

      return Child;
   end Compare_Two_Files;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Title1, Title2 : Glib.String_Ptr;
      File1, File2 : VFS.Virtual_File;
   begin
      if Node.Tag.all = "Vdiff" then
         Title1 := Get_Field (Node, "Title1");
         Title2 := Get_Field (Node, "Title2");
         File1  := Create (Full_Filename => Get_Field (Node, "File1").all);
         File2  := Create (Full_Filename => Get_Field (Node, "File2").all);

         return Compare_Two_Files
           (User, File1, File2, Title1.all, Title2.all,
            Diff (User, File1, File2));
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Node_Ptr
   is
      N, N2 : Node_Ptr;
   begin
      if Widget.all in Vdiff_Record'Class then
         N := new Node;
         N.Tag := new String'("Vdiff");

         N2 := new Node;
         N2.Tag := new String'("Title1");
         N2.Value := new String'(Get_Text (Vdiff_Access (Widget).File_Label1));
         Add_Child (N, N2);

         N2 := new Node;
         N2.Tag := new String'("Title2");
         N2.Value := new String'(Get_Text (Vdiff_Access (Widget).File_Label2));
         Add_Child (N, N2);

         N2 := new Node;
         N2.Tag := new String'("File1");
         N2.Value := new String'(Full_Name (Vdiff_Access (Widget).File1).all);
         Add_Child (N, N2);

         N2 := new Node;
         N2.Tag := new String'("File2");
         N2.Value := new String'(Full_Name (Vdiff_Access (Widget).File2).all);
         Add_Child (N, N2);

         return N;
      end if;

      return null;
   end Save_Desktop;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      File1  : constant Virtual_File :=
        Select_File
          (Title             => -"Select First File",
           Parent            => Get_Main_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));
      File2 : Virtual_File;
      Child  : MDI_Child;
      pragma Unreferenced (Widget, Child);

   begin
      if File1 = VFS.No_File then
         return;
      end if;

      File2 := Select_File
        (Title             => -"Select Second File",
         Parent            => Get_Main_Window (Kernel),
         Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
         Kind              => Unspecified,
         History           => Get_History (Kernel));

      if File2 = VFS.No_File then
         return;
      end if;

      Child := Compare_Two_Files
        (Kernel, File1, File2,
         Full_Name (File1).all, Full_Name (File2).all,
         Diff (Kernel, File1, File2));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   ---------------
   -- Diff_Hook --
   ---------------

   function Diff_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : Hooks_Data'Class) return Boolean
   is
      D       : constant Diff_Hooks_Args := Diff_Hooks_Args (Data);
      Child   : MDI_Child;
      pragma Unreferenced (Child);
   begin
      if D.Orig_File = VFS.No_File then
         if D.New_File = VFS.No_File then
            return False;
         end if;

         declare
            Base     : constant String := Base_Name (D.New_File);
            Ref_File : constant Virtual_File :=
              Create (Full_Filename => Get_Tmp_Dir & Base & "$ref");
         begin
            Child := Compare_Two_Files
              (Kernel, Ref_File, D.New_File,
               Base & (-" <reference>"), Full_Name (D.New_File).all,
               Diff (Kernel, Ref_File, D.New_File, D.Diff_File,
                     Revert => True));
            Delete (Ref_File);
         end;

      elsif D.New_File = VFS.No_File then
         if D.Orig_File = VFS.No_File then
            return False;
         end if;

         declare
            Base     : constant String := Base_Name (D.Orig_File);
            Ref_File : constant Virtual_File :=
              Create (Full_Filename => Get_Tmp_Dir & Base & "$ref");
         begin
            Child := Compare_Two_Files
              (Kernel, D.Orig_File, Ref_File,
               Full_Name (D.Orig_File).all, Base & (-" <reference>"),
               Diff (Kernel, D.Orig_File, Ref_File, D.Diff_File));
            Delete (Ref_File);
         end;

      else
         --  All arguments are specified

         Child := Compare_Two_Files
           (Kernel, D.Orig_File, D.New_File,
            Full_Name (D.Orig_File).all,
            Full_Name (D.New_File).all,
            Diff (Kernel, D.Orig_File, D.New_File, D.Diff_File));
      end if;

      return True;
   end Diff_Hook;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      Vdiff   : constant Vdiff_Access := Vdiff_Access (Child);
      Context : File_Selection_Context_Access;
   begin
      if Vdiff = null then
         return null;
      end if;

      declare
         Label_1 : constant Virtual_File :=
           Create (Full_Filename => Get_Text (Vdiff.File_Label1));
         Label_2 : constant Virtual_File :=
           Create (Full_Filename => Get_Text (Vdiff.File_Label2));
      begin
         Context := new File_Selection_Context;

         Set_Context_Information
           (Context => Context,
            Kernel  => Kernel,
            Creator => Vdiff_Module_ID);

         if Is_Regular_File (Label_1) then
            Set_File_Information (Context, File => Label_1);
         elsif Is_Regular_File (Label_2) then
            Set_File_Information (Context, File => Label_2);
         end if;

         return Selection_Context_Access (Context);
      end;
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools") & '/' & (-"Compare") & '/';
   begin
      Register_Module
        (Module                  => Vdiff_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Vdiff_Module_Name,
         Priority                => Default_Priority,
         Default_Context_Factory => Default_Factory'Access);
      Add_Hook (Kernel, Diff_Action_Hook, Diff_Hook'Access);
      Register_Menu
        (Kernel, Tools, -"Two Files...", "", On_Compare_Two_Files'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

end Vdiff_Module;
