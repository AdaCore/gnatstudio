
package body Test_File_Selector is

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Txt;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access)
   is
   begin
      if File'Length >= 4
        and then File (File'Last - 3 .. File'Last) = ".txt"
      then
         State := Normal;
         Text := new String'("This is a text file");
      else
         State := Invisible;
         Text := new String'("");
      end if;
      Mask := Gdk.Bitmap.Null_Bitmap;
      Pixmap := Gdk.Pixmap.Null_Pixmap;
   end Use_File_Filter;

end Test_File_Selector;
