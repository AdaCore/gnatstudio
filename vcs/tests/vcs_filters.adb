with VCS;     use VCS;

package body VCS_Filters is

   use VCS.String_List;
   use File_Status_List;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_VCS;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : String;
      File      : String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access)
   is
      Files  : VCS.String_List.List;
      Result : File_Status_Record;

      Temp_Status_List : File_Status_List.List;
   begin

      if Filter.Current_Directory = null
        or else Filter.Current_Directory.all /= Dir then
         if Filter.Current_Directory /= null then
            Free (Filter.Current_Directory);
         end if;

         Filter.Current_Directory := new String'(Dir);

         Free (Filter.Current_Status);
         declare
            Status_List : File_Status_List.List;
            File_List   : VCS.String_List.List;
         begin
            Append (Files, Dir & File);
            Status_List := Local_Get_Status (Filter.VCS_Id, Files);
            Free (Files);

            Temp_Status_List := Status_List;

            while not Is_Empty (Temp_Status_List) loop
               Prepend (File_List, Head (Head (Temp_Status_List).File_Name));
               Temp_Status_List := Next (Temp_Status_List);
            end loop;

            Filter.Current_Status := Get_Status (Filter.VCS_Id, File_List);

            Free (File_List);
            Free (Status_List);
         end;

         Use_File_Filter (Filter, Win, Dir, File, State, Pixmap, Mask, Text);

      else

         --  This directory has already been cached.

         --  Try to find the file information in the current status.
         Temp_Status_List := Filter.Current_Status;

         while not Is_Empty (Temp_Status_List)
           and then not Is_Empty (Head (Temp_Status_List).File_Name)
           and then Head (Head (Temp_Status_List).File_Name) /= Dir & File
         loop
            Temp_Status_List := Next (Temp_Status_List);
         end loop;

         if not Is_Empty (Temp_Status_List)
           and then not Is_Empty (Head (Temp_Status_List).File_Name)
           and then Head (Head (Temp_Status_List).File_Name) = Dir & File
         then
            Result := Head (Temp_Status_List);
         else
            if Filter.Query_Unknown then
               Append (Files, Dir & File);
               Result := Head (Get_Status (Filter.VCS_Id, Files));
               Free (Files);
            else
               Result.Status := Unknown;
            end if;
         end if;

         State := Normal;

         case Result.Status is
            when Unknown =>
               Pixmap := Filter.Unknown_Pixmap;
               Mask   := Filter.Unknown_Bitmap;

         when Not_Registered =>
            Pixmap := Filter.Not_Registered_Pixmap;
            Mask   := Filter.Not_Registered_Bitmap;

            when Up_To_Date =>
               Pixmap := Filter.Up_To_Date_Pixmap;
               Mask   := Filter.Up_To_Date_Bitmap;

            when Removed =>
               Pixmap := Filter.Removed_Pixmap;
               Mask   := Filter.Removed_Bitmap;

            when Modified =>
               Pixmap := Filter.Modified_Pixmap;
               Mask   := Filter.Modified_Bitmap;

            when Needs_Merge =>
               Pixmap := Filter.Needs_Merge_Pixmap;
               Mask   := Filter.Needs_Merge_Bitmap;
               State := Highlighted;

            when Needs_Update =>
               Pixmap := Filter.Needs_Update_Pixmap;
               Mask   := Filter.Needs_Update_Bitmap;
         end case;

         Text := new String'(File_Status'Image (Result.Status));
      end if;

   end Use_File_Filter;


end VCS_Filters;
