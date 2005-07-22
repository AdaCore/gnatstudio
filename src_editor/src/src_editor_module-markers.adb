-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                              AdaCore                              --
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

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with VFS;                       use VFS;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with String_Utils;              use String_Utils;
with System;                    use System;
with System.Address_Image;
with Ada.Unchecked_Conversion;
with Traces;                    use Traces;

package body Src_Editor_Module.Markers is
   Me : constant Debug_Handle := Create ("Markers");

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, File_Marker);
   function Convert is new Ada.Unchecked_Conversion
     (File_Marker, System.Address);

   procedure On_Destroy_Buffer
     (Marker : System.Address; Text_Mark : System.Address);
   pragma Convention (C, On_Destroy_Buffer);

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
     (Marker  : access File_Marker_Record'Class)
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
      File   : VFS.Virtual_File)
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
      Iter   : Gtk_Text_Iter;
   begin
      if Marker.Mark /= null then
         Get_Iter_At_Mark (Get_Buffer (Marker.Mark), Iter, Marker.Mark);
         Marker.Line   := Integer (Get_Line (Iter)) + 1;
         Marker.Column := Integer (Get_Line_Offset (Iter)) + 1;
      end if;
   end Update_Marker_Location;

   -----------------------
   -- On_Destroy_Buffer --
   -----------------------

   procedure On_Destroy_Buffer
     (Marker : System.Address; Text_Mark : System.Address)
   is
      pragma Unreferenced (Text_Mark);
      M    : constant File_Marker := Convert (Marker);
   begin
      Update_Marker_Location (M);
      M.Mark   := null;
   end On_Destroy_Buffer;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Marker : in out File_Marker_Record) is
      use Marker_List;
      Module : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      M1    : constant File_Marker := Marker'Unchecked_Access;
      M     : File_Marker;
      Node  : Marker_List.List_Node;
      Prev  : Marker_List.List_Node := Marker_List.Null_Node;

   begin
      if Module = null then
         return;
      end if;

      Node := Marker_List.First (Module.Stored_Marks);

      if Marker.Mark /= null then
         Weak_Unref (Get_Buffer (Marker.Mark),
                     On_Destroy_Buffer'Access, Convert (M1));
         Delete_Mark (Get_Buffer (Marker.Mark), Marker.Mark);
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
         Marker.Mark := Create_Mark
           (Get_Buffer (Source), Editable_Line_Type (Marker.Line),
            Integer'Max (1, Marker.Column));

         --  We cannot connect to the mark, since it is destroyed *after* the
         --  buffer itself. Strange decision from the gtk+ developers
         Weak_Ref (Get_Buffer (Source),
                   On_Destroy_Buffer'Access, Convert (File_Marker (Marker)));
      end if;
   end Create_Text_Mark;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Marker : access File_Marker_Record'Class) return VFS.Virtual_File is
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
      File   : VFS.Virtual_File;
      Line   : Natural;
      Column : Natural;
      Length : Natural := 0) return File_Marker
   is
      Marker : File_Marker;
   begin
      Marker := new File_Marker_Record'
        (Location_Marker_Record with
         Id     => Natural'Last,
         File   => File,
         Line   => Line,
         Column => Column,
         Length => Length,
         Mark   => null);
      Create_Text_Mark (Kernel, Marker);
      Register_Persistent_Marker (Marker);
      return Marker;
   end Create_File_Marker;

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (File   : VFS.Virtual_File;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark) return File_Marker
   is
      Marker : File_Marker;
   begin
      Marker := new File_Marker_Record'
        (Location_Marker_Record with
         Id     => Natural'Last,
         File   => File,
         Line   => 0,
         Column => 1,
         Length => 0,
         Mark   => Mark);
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
      Child : constant MDI_Child := Find_Current_Editor (Kernel);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
      Line, Column : Integer;
   begin
      if Box /= null then
         Get_Cursor_Location (Box, Line, Column);
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

   function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean
   is
      Child  : constant MDI_Child := Find_Editor (Kernel, Marker.File);
      Box    : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
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
            Marker.Line,
            Marker.Column,
            Marker.Column + Marker.Length,
            Enable_Navigation => False,
            New_File          => False);
      end if;
      return True;
   end Go_To;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Marker : access File_Marker_Record'Class) return Integer is
   begin
      Update_Marker_Location (Marker);
      return Marker.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Marker : access File_Marker_Record'Class) return Integer is
   begin
      Update_Marker_Location (Marker);
      return Marker.Column;
   end Get_Column;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Marker : access File_Marker_Record) return String is
   begin
      Update_Marker_Location (Marker);
      return Base_Name (Marker.File)
        & ":"  & Image (Marker.Line)
        & ":"  & Image (Marker.Column);
   end To_String;

   ----------
   -- Save --
   ----------

   function Save
     (Marker : access File_Marker_Record) return Glib.Xml_Int.Node_Ptr
   is
      Node : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Update_Marker_Location (Marker);
      Node.Tag := new String'("file_marker");
      Set_Attribute (Node, "file", Full_Name (Marker.File).all);
      Set_Attribute (Node, "line", Integer'Image (Marker.Line));
      Set_Attribute (Node, "column", Integer'Image (Marker.Column));
      return Node;
   end Save;

   ----------
   -- Load --
   ----------

   function Load
     (Kernel   : access Kernel_Handle_Record'Class;
      From_XML : Glib.Xml_Int.Node_Ptr := null) return Location_Marker
   is
      Source  : Source_Editor_Box;
      Line, Column : Integer;
   begin
      if From_XML /= null then
         if From_XML.Tag.all = "file_marker" then
            return Location_Marker
              (Create_File_Marker
                 (Kernel => Kernel,
                  File   => Create (Get_Attribute (From_XML, "file")),
                  Line   => Integer'Value (Get_Attribute (From_XML, "line")),
                  Column =>
                    Integer'Value (Get_Attribute (From_XML, "column"))));
         end if;

      else
         Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
         if Source /= null then
            Get_Cursor_Location (Source, Line, Column);
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
