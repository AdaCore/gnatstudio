with Gtk.Frame;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Switches_Editor_Pkg;

package Switches_Editors is

   type Switches_Editor_Record is new Gtk.Frame.Gtk_Frame_Record with private;
   type Switches_Editor is access all Switches_Editor_Record'Class;

   type Switches_Array is array (Positive range <>) of String_Access;
   --  Array that contains the switches set in the editor.

   type Page_Filter is mod 2 ** 4;
   Gnatmake_Page : constant Page_Filter := 2 ** 0;
   Compiler_Page : constant Page_Filter := 2 ** 1;
   Binder_Page   : constant Page_Filter := 2 ** 2;
   Linker_Page   : constant Page_Filter := 2 ** 3;
   All_Pages     : constant Page_Filter :=
     Gnatmake_Page or Compiler_Page or Binder_Page or Linker_Page;

   procedure Gtk_New
     (Editor : out Switches_Editor;
      Pages  : Page_Filter := All_Pages);
   --  Creates a new switches editor.

   procedure Initialize
     (Editor : access Switches_Editor_Record'Class;
      Pages  : Page_Filter := All_Pages);
   --  Internal function used to create the editor

   function Get_Switches (Editor : access Switches_Editor_Record)
      return Switches_Array;
   --  Return the switches set in the editor.
   --  If several pages were displayed in the explorer, the syntax used is
   --  gnatmake's syntax, ie the groups are separated with -largs, ...
   --  However, if a single page is displayed (for instance the compiler), then
   --  the switches are returned as is.
   --  Note: *Do not free the array or modify the strings* since the strings
   --  point to internal copies.

   procedure Set_Switches
     (Editor   : access Switches_Editor_Record;
      Switches : Switches_Array);
   --  Set the initial value for the switches.
   --  If more than one page is displayed in the editor, you need to separate
   --  each group with "-largs", ... appropriately.


private
   type Switches_Editor_Record is new Gtk.Frame.Gtk_Frame_Record with record
      Full  : Switches_Editor_Pkg.Switches_Editor_Access;
      Pages : Page_Filter;
      Num_Pages : Natural;
   end record;

end Switches_Editors;
