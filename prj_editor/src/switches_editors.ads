with Gtk.Widget;
with GNAT.OS_Lib;
with Switches_Editor_Pkg; use Switches_Editor_Pkg;

package Switches_Editors is

   type Switches_Edit_Record is new Switches_Editor_Record with private;
   type Switches_Edit is access all Switches_Edit_Record'Class;

   type Page_Filter is mod 2 ** 4;
   Gnatmake_Page : constant Page_Filter := 2 ** 0;
   Compiler_Page : constant Page_Filter := 2 ** 1;
   Binder_Page   : constant Page_Filter := 2 ** 2;
   Linker_Page   : constant Page_Filter := 2 ** 3;
   All_Pages     : constant Page_Filter :=
     Gnatmake_Page or Compiler_Page or Binder_Page or Linker_Page;

   type Tool_Names is (Gnatmake, Compiler, Binder, Linker);

   procedure Gtk_New (Editor : out Switches_Edit);
   --  Create a new switches editor

   procedure Destroy_Pages
     (Editor : access Switches_Edit_Record; Pages : Page_Filter);
   --  Destroy specific pages in the editor, and remove them from the display

   function Get_Window
     (Editor : access Switches_Edit_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the window to use to insert the editor in a parent container.
   --  You should not use Editor itself, which is a top-level window.
   --  Likewise, you shouldn't call Show_All on the editor itself, but rather
   --  on the window.

   function Get_Switches
     (Editor : access Switches_Edit_Record; Tool : Tool_Names)
      return GNAT.OS_Lib.Argument_List;
   --  Return the switches set in the editor for one of the specific tools.
   --  It is your responsability to free the strings.

   procedure Free (Switches : in out GNAT.OS_Lib.Argument_List);
   procedure Free (Switches : in out GNAT.OS_Lib.Argument_List_Access);
   --  Free all the strings in Switches

   procedure Set_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : GNAT.OS_Lib.Argument_List);
   --  Set the initial value for the switches, for a specific tool

   procedure Filter_Switches
     (Editor   : access Switches_Edit_Record;
      Tool     : Tool_Names;
      Switches : in out GNAT.OS_Lib.Argument_List);
   --  Remove from Switches all the switches that can be set directly from
   --  the GUI. As a result, on exit Switches will only contain non-null
   --  values for the switches that were set manually by the user

private
   type Switches_Edit_Record is new Switches_Editor_Record with record
      Pages : Page_Filter := All_Pages;
   end record;

end Switches_Editors;
