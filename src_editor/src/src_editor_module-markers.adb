-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2005-2008, AdaCore                --
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

with Ada.Unchecked_Conversion;
with System;                    use System;
with System.Address_Image;

with Glib.Object;               use Glib.Object;

with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;

with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;

package body Src_Editor_Module.Markers is

   Me : constant Debug_Handle := Create ("Markers");

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, File_Marker);
   function Convert is new Ada.Unchecked_Conversion
     (File_Marker, System.Address);

   procedure On_Destroy_Mark
     (Marker : System.Address; Text_Mark : System.Address);
   pragma Convention (C, On_Destroy_Mark);
   --  Called when a gtk_text_mark is destroyed. At this point, the mark is
   --  no longer attached to a buffer. This is called only when the mark
   --  is explicitly removed from the buffer.

   procedure Update_Marker_Location
     (Marker : access File_Marker_Record'Class);
   --  Update the location stored in Marker according to the one stored in the
   --  associated Gtk_Text_Mark

   procedure Register_Persistent_Marker
     (Marker  : access File_Marker_Record'Class);
   --  Register Marker as a permanent marker. This means that if the associated
   --  file is closed, reopened, modified,... the marker will always point to
   --  the same location

   procedure Create_Text_Mark
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : access File_Marker_Record'Class);
   --  Create a text mark that will keep the location of the marker even when
   --  the text is changed

   procedure Dump_Persistent_Markers;
   pragma Unreferenced (Dump_Persistent_Markers);
   --  Debug procedure to dump the list of persistent markers

   -----------------------------
   -- Dump_Persistent_Markers --
   -----------------------------

   procedure Dump_Persistent_Markers is
      use Marker_List;
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      Node   : List_Node;

   begin
      Trace (Me, "List of persistent markers");

      if Module = null then
         return;
      end if;

      Node := First (Module.Stored_Marks);

      while Node /= Null_Node loop
         Trace (Me, "     marker="
                & System.Address_Image
                  (Convert (File_Marker (Data (Node))))
                & " " & To_String (Data (Node)));
         Node := Next (Node);
      end loop;
   end Dump_Persistent_Markers;

   --------------------------------
   -- Register_Persistent_Marker --
   --------------------------------

   procedure Register_Persistent_Marker
     (Marker : access File_Marker_Record'Class)
   is
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Marker.Id            := Module.Next_Mark_Id;
      Module.Next_Mark_Id  := Module.Next_Mark_Id + 1;
      Marker_List.Append (Module.Stored_Marks,  Location_Marker (Marker));
   end Register_Persistent_Marker;

   ---------------
   -- Find_Mark --
   ---------------

   function Find_Mark (Id : Natural) return File_Marker is
      use type Marker_List.List_Node;
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      Marker : File_Marker;
      Node   : Marker_List.List_Node;

   begin
      if Module = null then
         return null;
      end if;

      Node := Marker_List.First (Module.Stored_Marks);
      while Node /= Marker_List.Null_Node loop
         Marker := File_Marker (Marker_List.Data (Node));

         if Marker.Id = Id then
            return Marker;
         end if;

         Node := Marker_List.Next (Node);
      end loop;

      return null;
   end Find_Mark;

   ----------------------------
   -- Reset_Markers_For_File --
   ----------------------------

   procedure Reset_Markers_For_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      use Marker_List;
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      Node   : List_Node;
      Marker : File_Marker;

   begin
      if Module = null then
         return;
      end if;

      Node := First (Module.Stored_Marks);
      while Node /= Null_Node loop
         Marker := File_Marker (Data (Node));

         if Marker.File = File then
            Create_Text_Mark (Kernel, Marker);
         end if;

         Node := Next (Node);
      end loop;
   end Reset_Markers_For_File;

   ----------------------------
   -- Update_Marker_Location --
   ----------------------------

   procedure Update_Marker_Location
     (Marker : access File_Marker_Record'Class)
   is
      Iter : Gtk_Text_Iter;
   begin
      if Marker.Mark /= null then
         Get_Iter_At_Mark (Marker.Buffer, Iter, Marker.Mark);
         Get_Iter_Position
           (Source_Buffer (Marker.Buffer), Iter,
            Marker.Line,
            Marker.Column);
      end if;
   end Update_Marker_Location;

   ---------------------
   -- On_Destroy_Mark --
   ---------------------

   procedure On_Destroy_Mark
     (Marker : System.Address; Text_Mark : System.Address)
   is
      pragma Unreferenced (Text_Mark);
      M : constant File_Marker := Convert (Marker);
   begin
      --  No need to remove the weak_ref on the mark since it has been
      --  cancelled automatically by gtk+ prior to calling On_Destroy_Mark.

      M.Mark   := null;
      M.Buffer := null;
   end On_Destroy_Mark;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Marker : in out File_Marker_Record) is
      use Marker_List;
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      M1     : constant File_Marker := Marker'Unchecked_Access;
      M      : File_Marker;
      Node   : Marker_List.List_Node;
      Prev   : Marker_List.List_Node := Marker_List.Null_Node;

   begin
      if Module = null then
         return;
      end if;

      Node := Marker_List.First (Module.Stored_Marks);

      if Marker.Mark /= null then
         --  We explicitly remove the weak_unref on the mark, even though
         --  it is likely it will be destroyed as a result of Delete_Mark.
         --  However, if someone happened to have a Ref on it, we want to be
         --  sure that On_Destroy_Mark will never be call with the dangling
         --  File_Marker pointer.

         Weak_Unref (Marker.Mark,
                   On_Destroy_Mark'Access, Convert (M1));

         if not In_Destruction_Is_Set (Source_Buffer (Marker.Buffer)) then
            Delete_Mark (Marker.Buffer, Marker.Mark);
         end if;

         Marker.Mark := null;
      end if;

      while Node /= Marker_List.Null_Node loop
         M := File_Marker (Marker_List.Data (Node));
         if M = M1 then
            Remove_Nodes (Module.Stored_Marks, Prev, Node);
            exit;
         end if;

         Prev := Node;
         Node := Marker_List.Next (Node);
      end loop;
   end Destroy;

   ----------------------
   -- Create_Text_Mark --
   ----------------------

   procedure Create_Text_Mark
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : access File_Marker_Record'Class)
   is
      Child  : constant MDI_Child := Find_Editor (Kernel, Marker.File);
      Source : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
   begin
      if Marker.Mark = null and then Source /= null then
         Marker.Buffer :=
           Gtk_Text_Buffer (Source_Buffer'(Get_Buffer (Source)));
         Marker.Mark := Create_Mark
           (Get_Buffer (Source), Marker.Line,
            Visible_Column_Type'Max (1, Marker.Column));

         --  This mark can be destroyed in three different contexts:
         --    1 - explicitly (call to Destroy for File_Marker)
         --    2 - explicitly (call to Delete_Mark on the gtk+ side)
         --    3 - implicitly (when the buffer itself is destroyed)
         --  The last two cases need to be protected, since the File_Marker
         --  itself isn't destroyed then, and we need to update its location).

         --  Protect against case 3: this results in the destruction of the
         --  text_mark anyway, so nothing to do.

         --  Protect against case 2.
         --  Connect on the mark itself. If it is removed from the buffer
         --  directly, it will be destroyed anyway, so we need to be aware of
         --  that.

         Weak_Ref (Marker.Mark,
                   On_Destroy_Mark'Access, Convert (File_Marker (Marker)));
      end if;
   end Create_Text_Mark;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Marker : access File_Marker_Record'Class) return Virtual_File is
   begin
      return Marker.File;
   end Get_File;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
     (Marker : access File_Marker_Record'Class)
      return Gtk.Text_Mark.Gtk_Text_Mark is
   begin
      return Marker.Mark;
   end Get_Mark;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Marker : access File_Marker_Record'Class) return Integer is
   begin
      return Marker.Id;
   end Get_Id;

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      Length : Natural := 0) return File_Marker
   is
      Marker : File_Marker;
   begin
      Marker := new File_Marker_Record'
        (Location_Marker_Record with
         Id      => Natural'Last,
         File    => File,
         Line    => Line,
         Column  => Column,
         Length  => Length,
         Buffer  => null,
         Mark    => null,
         Kernel  => Kernel_Handle (Kernel));
      Create_Text_Mark (Kernel, Marker);
      Register_Persistent_Marker (Marker);
      return Marker;
   end Create_File_Marker;

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark) return File_Marker
   is
      Marker : File_Marker;
   begin
      Marker := new File_Marker_Record'
        (Location_Marker_Record with
         Id      => Natural'Last,
         File    => File,
         Line    => 0,
         Column  => 1,
         Length  => 0,
         Buffer  => Get_Buffer (Mark),
         Mark    => Mark,
         Kernel  => Kernel_Handle (Kernel));
      Update_Marker_Location (Marker);
      Register_Persistent_Marker (Marker);
      return Marker;
   end Create_File_Marker;

   ---------------------------------------------
   -- Push_Current_Editor_Location_In_History --
   ---------------------------------------------

   procedure Push_Current_Editor_Location_In_History
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Box    : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if Box /= null then
         Get_Cursor_Position (Get_Buffer (Box), Line, Column);
         Push_Marker_In_History
           (Kernel  => Kernel,
            Marker  => Create_File_Marker
              (Kernel => Kernel,
               File   => Get_Filename (Box),
               Line   => Line,
               Column => Column));
      end if;
   end Push_Current_Editor_Location_In_History;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean
   is
      Child : constant MDI_Child := Find_Editor (Kernel, Marker.File);
      Box   : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
   begin
      if Marker.Mark /= null then
         Raise_Child (Child);
         Set_Focus_Child (Child);
         Grab_Focus (Box);
         Scroll_To_Mark (Box, Marker.Mark, Marker.Length);

      else
         Open_File_Editor
           (Kernel,
            Marker.File,
            Integer (Marker.Line),
            Marker.Column,
            Marker.Column + Visible_Column_Type (Marker.Length),
            --  ??? This is incorrect if there is an ASCII.HT before
            --  column+length
            Enable_Navigation => False,
            New_File          => False);
      end if;
      return True;
   end Go_To;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Marker : access File_Marker_Record'Class) return Editable_Line_Type is
   begin
      Update_Marker_Location (Marker);
      return Marker.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Marker : access File_Marker_Record'Class) return Visible_Column_Type is
   begin
      Update_Marker_Location (Marker);
      return Marker.Column;
   end Get_Column;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : access File_Marker_Record) return String
   is
      function Get_Subprogram_Name return String;
      --  Returns the subprogram name at the marker position

      -------------------------
      -- Get_Subprogram_Name --
      -------------------------

      function Get_Subprogram_Name return String is
         Box : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI
                   (Find_Editor (Marker.Kernel, Marker.File));
      begin
         if Box /= null then
            return Get_Subprogram_Name (Box, Editable_Line_Type (Marker.Line));
         else
            return "";
         end if;
      end Get_Subprogram_Name;

   begin
      Update_Marker_Location (Marker);

      declare
         Location : constant String := Base_Name (Marker.File)
           & ":"  & Image (Integer (Marker.Line))
           & ":"  & Image (Integer (Marker.Column));
         Name     : constant String := Get_Subprogram_Name;
      begin
         if Name = "" then
            return Location;
         else
            return Name & " (" & Location & ")";
         end if;
      end;
   end To_String;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Marker : access File_Marker_Record) return Glib.Xml_Int.Node_Ptr
   is
      Node : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Update_Marker_Location (Marker);
      Node.Tag := new String'("file_marker");
      Set_Attribute (Node, "file", Full_Name (Marker.File).all);
      Set_Attribute (Node, "line", Editable_Line_Type'Image (Marker.Line));
      Set_Attribute (Node, "column", Visible_Column_Type'Image
                     (Marker.Column));
      return Node;
   end Save;

   ----------
   -- Load --
   ----------

   function Load
     (Kernel   : access Kernel_Handle_Record'Class;
      From_XML : Glib.Xml_Int.Node_Ptr := null) return Location_Marker
   is
      Source : Source_Editor_Box;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if From_XML /= null then
         if From_XML.Tag.all = "file_marker" then
            return Location_Marker
              (Create_File_Marker
                 (Kernel => Kernel,
                  File   => Create (Get_Attribute (From_XML, "file")),
                  Line   => Editable_Line_Type'Value
                   (Get_Attribute (From_XML, "line")),
                  Column =>
                    Visible_Column_Type'Value
                      (Get_Attribute (From_XML, "column"))));
         end if;

      else
         Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
         if Source /= null then
            Get_Cursor_Position (Get_Buffer (Source), Line, Column);
            return Location_Marker
              (Create_File_Marker
                 (Kernel => Kernel,
                  File   => Get_Filename (Source),
                  Line   => Line,
                  Column => Column));
         end if;
      end if;
      return null;
   end Load;

end Src_Editor_Module.Markers;
