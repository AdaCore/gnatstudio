-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with Diff_Utils;                use Diff_Utils;
with Vdiff_Pkg;                 use Vdiff_Pkg;
with Vdiff_Utils;               use Vdiff_Utils;

with Traces;                    use Traces;
with GNATCOLL.Filesystem;       use GNATCOLL.Filesystem;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Vdiff_Module is

   type Vdiff_Module_Record is new Module_ID_Record with null record;

   procedure Default_Context_Factory
     (Module  : access Vdiff_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget);
   --  See inherited documentation

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->Compare->Two Files...

   function Compare_Two_Files
     (Kernel         : access Kernel_Handle_Record'Class;
      File1, File2   : GNATCOLL.VFS.Virtual_File;
      Title1, Title2 : String;
      Result         : Diff_Occurrence_Link) return MDI_Child;
   --  Compare two files.
   --  Return null if there are no differences

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   -----------------------
   -- Compare_Two_Files --
   -----------------------

   function Compare_Two_Files
     (Kernel         : access Kernel_Handle_Record'Class;
      File1, File2   : GNATCOLL.VFS.Virtual_File;
      Title1, Title2 : String;
      Result         : Diff_Occurrence_Link) return MDI_Child
   is
      R     : Diff_Occurrence_Link := Result;
      Vdiff : Vdiff_Access;
      Child : GPS_MDI_Child;
   begin
      if Result = null then
         Insert (Kernel, -"No differences found for: " &
                 Base_Name (File1) & ", " & Base_Name (File2));
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
      Gtk_New (Child, Vdiff,
               Default_Width  => Get_Pref (Default_Widget_Width),
               Default_Height => Get_Pref (Default_Widget_Height),
               Module         => Vdiff_Module_ID);
      Set_Title (Child, -"Visual Comparison");
      Put (Get_MDI (Kernel), Child);
      Set_Focus_Child (Child);

      Free (R);

      return MDI_Child (Child);
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
      File1, File2   : GNATCOLL.VFS.Virtual_File;
   begin
      if Node.Tag.all = "Vdiff" then
         Title1 := Get_Field (Node, "Title1");
         Title2 := Get_Field (Node, "Title2");
         File1  := Create (Full_Filename => Get_Field (Node, "File1").all);
         File2  := Create (Full_Filename => Get_Field (Node, "File2").all);

         return Compare_Two_Files
           (User, File1, File2, Title1.all, Title2.all,
            Diff (File1, File2));
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
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
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
           Pattern_Name      => -"All files;Ada files;C/C++ files",
           History           => Get_History (Kernel));
      File2  : Virtual_File;
      Child  : MDI_Child;
      pragma Unreferenced (Widget, Child);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return;
      end if;

      File2 := Select_File
        (Title             => -"Select Second File",
         Parent            => Get_Current_Window (Kernel),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
         Kind              => Open_File,
         File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
         Pattern_Name      => -"All files;Ada files;C/C++ files",
         History           => Get_History (Kernel));

      if File2 = GNATCOLL.VFS.No_File then
         return;
      end if;

      Child := Compare_Two_Files
        (Kernel, File1, File2,
         Full_Name (File1).all, Full_Name (File2).all,
         Diff (File1, File2));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Compare_Two_Files;

   ---------------
   -- Diff_Hook --
   ---------------

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D       : constant Diff_Hooks_Args := Diff_Hooks_Args (Data.all);
      Child   : MDI_Child;
      pragma Unreferenced (Child);
      Success : Boolean;
      Tmp_Dir : constant String := Get_Local_Filesystem.Get_Tmp_Directory;
   begin
      if D.Orig_File = GNATCOLL.VFS.No_File then
         if D.New_File = GNATCOLL.VFS.No_File then
            return False;
         end if;

         declare
            Base     : constant String := Base_Name (D.New_File);
            Ref_File : constant Virtual_File :=
              Create (Full_Filename => Tmp_Dir & Base & "$ref");
         begin
            Child := Compare_Two_Files
              (Kernel, Ref_File, D.New_File,
               Base & (-" <reference>"), Full_Name (D.New_File).all,
               Diff (Kernel, Ref_File, D.New_File, D.Diff_File,
                     Revert => True));
            Delete (Ref_File, Success);
         end;

      elsif D.New_File = GNATCOLL.VFS.No_File then
         if D.Orig_File = GNATCOLL.VFS.No_File then
            return False;
         end if;

         declare
            Base     : constant String := Base_Name (D.Orig_File);
            Ref_File : constant Virtual_File :=
              Create (Full_Filename => Tmp_Dir & Base & "$ref");
         begin
            Child := Compare_Two_Files
              (Kernel, D.Orig_File, Ref_File,
               Full_Name (D.Orig_File).all, Base & (-" <reference>"),
               Diff (Kernel, D.Orig_File, Ref_File, D.Diff_File));
            Delete (Ref_File, Success);
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

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   procedure Default_Context_Factory
     (Module  : access Vdiff_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget)
   is
      Vdiff : constant Vdiff_Access := Vdiff_Access (Child);
   begin
      if Vdiff /= null then
         declare
            Label_1 : constant Virtual_File :=
              Create (Full_Filename => Get_Text (Vdiff.File_Label1));
            Label_2 : constant Virtual_File :=
              Create (Full_Filename => Get_Text (Vdiff.File_Label2));
         begin
            Set_Context_Information
              (Context => Context,
               Kernel  => Get_Kernel (Module.all),
               Creator => Abstract_Module_ID (Module));

            if Is_Regular_File (Label_1) then
               Set_File_Information (Context, Files => (1 => Label_1));
            elsif Is_Regular_File (Label_2) then
               Set_File_Information (Context, Files => (1 => Label_2));
            end if;
         end;
      end if;
   end Default_Context_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools") & '/' & (-"C_ompare");
   begin
      Vdiff_Module_ID := new Vdiff_Module_Record;
      Register_Module
        (Module      => Vdiff_Module_ID,
         Kernel      => Kernel,
         Module_Name => Vdiff_Module_Name,
         Priority    => Default_Priority);
      Add_Hook (Kernel, Diff_Action_Hook,
                Wrapper (Diff_Hook'Access), Name => "vidff.diff");
      Register_Menu
        (Kernel, '/' & (-"Tools") & '/', (-"C_ompare"),
         Callback   => null,
         Ref_Item   => -"Consoles",
         Add_Before => False);
      Register_Menu
        (Kernel, Tools, -"_Two Files...", "", On_Compare_Two_Files'Access,
         Ref_Item => -"Browsers", Add_Before => False);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Diff_Context_Length := Param_Spec_Int (Gnew_Int
        (Name    => "Diff_Utils-Context-Length",
         Minimum => -1,
         Maximum => Gint'Last,
         Default => 5,
         Blurb   => -("The number of lines displayed before and after each"
                      & " chunk of differences. -1 to display the whole file"),
         Nick    => -"Context length"));
      Register_Property
        (Kernel, Param_Spec (Diff_Context_Length), -"Visual diff");
   end Register_Module;

end Vdiff_Module;
