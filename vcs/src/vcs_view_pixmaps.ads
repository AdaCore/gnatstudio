with Glib; use Glib;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Basic_Types; use Basic_Types;

package VCS_View_Pixmaps is

   Status_Unknown_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Not_Registered_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Status_Up_To_Date_Pixbuf     : Gdk_Pixbuf := Null_Pixbuf;
   Status_Removed_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Modified_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Merge_Pixbuf    : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Update_Pixbuf   : Gdk_Pixbuf := Null_Pixbuf;

   procedure Init_Graphics;
   --  Initialize the pixbufs.

private

   Unknown_Xpm : aliased Pixmap_Array;
   pragma Import (C, Unknown_Xpm, "unknown_xpm");

   Removed_Xpm : aliased Pixmap_Array;
   pragma Import (C, Removed_Xpm, "unknown_xpm");

   Not_Registered_Xpm : aliased Pixmap_Array;
   pragma Import (C, Not_Registered_Xpm, "unknown_xpm");

   Modified_Xpm : aliased Pixmap_Array;
   pragma Import (C, Modified_Xpm, "modified_xpm");

   Up_To_Date_Xpm : aliased Pixmap_Array;
   pragma Import (C, Up_To_Date_Xpm, "up_to_date_xpm");

   Needs_Merge_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Merge_Xpm, "needs_merge_xpm");

   Needs_Update_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Update_Xpm, "needs_update_xpm");

end VCS_View_Pixmaps;
