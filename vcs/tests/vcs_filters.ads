--  This is a very dirty package used to test the VCS system.

with VCS;     use VCS;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Gtkada.File_Selector; use Gtkada.File_Selector;

package VCS_Filters is

   type Filter_VCS is new File_Filter_Record (new String'("CVS Files"))
   with record
      Query_Unknown : Boolean := False;

      Current_Directory : String_Access;
      Current_Status    : File_Status_List.List;

      VCS_Id : VCS_Access;

      Unknown_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Unknown_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Not_Registered_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Not_Registered_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Up_To_Date_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Up_To_Date_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Removed_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Removed_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Modified_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Modified_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Needs_Merge_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Needs_Merge_Bitmap : Gdk.Bitmap.Gdk_Bitmap;

      Needs_Update_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Needs_Update_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
   end record;

   type Filter_VCS_Access is access all Filter_VCS'Class;

   procedure Use_File_Filter
     (Filter    : access Filter_VCS;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : String;
      File      : String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access);

end VCS_Filters;
