package body VCS_View_Pixmaps is

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics is
   begin
      if Status_Unknown_Pixbuf = Null_Pixbuf then
         Status_Unknown_Pixbuf
           := Gdk_New_From_Xpm_Data (Unknown_Xpm);
         Status_Up_To_Date_Pixbuf
           := Gdk_New_From_Xpm_Data (Up_To_Date_Xpm);
         Status_Modified_Pixbuf
           := Gdk_New_From_Xpm_Data (Modified_Xpm);
         Status_Needs_Merge_Pixbuf
           := Gdk_New_From_Xpm_Data (Needs_Merge_Xpm);
         Status_Needs_Update_Pixbuf
           := Gdk_New_From_Xpm_Data (Needs_Update_Xpm);
         Status_Removed_Pixbuf
           := Gdk_New_From_Xpm_Data (Removed_Xpm);
         Status_Not_Registered_Pixbuf
           := Gdk_New_From_Xpm_Data (Not_Registered_Xpm);
      end if;
   end Init_Graphics;

end VCS_View_Pixmaps;
