
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Gtkada.File_Selector; use Gtkada.File_Selector;

package Test_File_Selector is

   type Filter_Show_Txt is new File_Filter_Record (new String'("*.txt"))
     with null record;
   type Filter_Show_Txt_Access is access all Filter_Show_Txt'Class;

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Txt;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access);

end Test_File_Selector;
