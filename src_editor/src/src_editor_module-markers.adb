------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;                    use System;
with Glib.Object;               use Glib.Object;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with GNATCOLL.Xref;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Buffer;         use Src_Editor_Buffer;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with String_Utils;              use String_Utils;

package body Src_Editor_Module.Markers is
   use type GNATCOLL.Xref.Visible_Column;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, File_Marker);
   function Convert is new Ada.Unchecked_Conversion
     (File_Marker, System.Address);

   package Markers_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, File_Marker);

   procedure On_Destroy_Buffer
     (Marker : System.Address; Buffer : System.Address);
   pragma Convention (C, On_Destroy_Buffer);
   --  Called when a gtk_text_mark is destroyed. At this point, the mark is
   --  no longer attached to a buffer. This is called only when the mark
   --  is explicitly removed from the buffer.

   procedure On_Closed_Or_Changed
     (Buffer  : access GObject_Record'Class;
      Marker : File_Marker);
   --  Called when buffer is about to close

   procedure Update_Marker_Location
     (Marker : not null access File_Marker_Data'Class);
   --  Update the location stored in Marker according to the one stored in the
   --  associated Gtk_Text_Mark

   procedure Register_Persistent_Marker (Marker  : Location_Marker)
     with Pre => Marker.Get.Element.all in File_Marker_Data'Class;
   --  Register Marker as a permanent marker. This means that if the associated
   --  file is closed, reopened, modified,... the marker will always point to
   --  the same location

   procedure Create_Text_Mark
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : not null access File_Marker_Data'Class;
      Box    : Source_Editor_Box := null);
   --  Create a text mark that will keep the location of the marker even when
   --  the text is changed
   --  If the mark already exists, update its location.

   --------------------------------
   -- Register_Persistent_Marker --
   --------------------------------

   procedure Register_Persistent_Marker (Marker : Location_Marker) is
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
   begin
      Marker_List.Append (Module.Stored_Marks,  Marker.Weak);
   end Register_Persistent_Marker;

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
      F      : File_Marker;
      M      : Location_Marker;
      W      : Weak_Location_Marker;
      C, C2  : Marker_List.Cursor;
      Box    : Source_Editor_Box;
   begin
      if Module /= null then
         C := Module.Stored_Marks.First;
         while Has_Element (C) loop
            W := Marker_List.Element (C);
            C2 := Next (C);
            if W.Was_Freed then
               --  Cleanup the list to keep it short
               Module.Stored_Marks.Delete (C);
            else
               if Box = null then
                  Box := Get_Source_Box_From_MDI
                    (Find_Editor
                       (Kernel, File,
                        Project => GNATCOLL.Projects.No_Project));
               end if;

               if Box /= null then
                  M.Set (W);
                  F := File_Marker (M.Unchecked_Get);
                  if F.File = File then
                     Create_Text_Mark (Kernel, F, Box => Box);
                  end if;
               end if;
            end if;
            C := C2;
         end loop;
      end if;
   end Reset_Markers_For_File;

   ----------------------------
   -- Update_Marker_Location --
   ----------------------------

   procedure Update_Marker_Location
     (Marker : not null access File_Marker_Data'Class)
   is
      Iter : Gtk_Text_Iter;
   begin
      if Marker.Mark /= null
        and then not Get_Deleted (Marker.Mark)
      then
         Get_Iter_At_Mark (Marker.Buffer, Iter, Marker.Mark);
         Get_Iter_Position
           (Source_Buffer (Marker.Buffer), Iter, Marker.Line, Marker.Column);
      end if;
   end Update_Marker_Location;

   -----------------------
   -- On_Destroy_Buffer --
   -----------------------

   procedure On_Destroy_Buffer
     (Marker : System.Address; Buffer : System.Address)
   is
      pragma Unreferenced (Buffer);
      M : constant File_Marker := Convert (Marker);
   begin
      M.Mark   := null;
      M.Buffer := null;
   end On_Destroy_Buffer;

   --------------------------
   -- On_Closed_Or_Changed --
   --------------------------

   procedure On_Closed_Or_Changed
     (Buffer : access GObject_Record'Class;
      Marker : File_Marker)
   is
      pragma Unreferenced (Buffer);
   begin
      Update_Marker_Location (Marker);
   end On_Closed_Or_Changed;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : not null access File_Marker_Data'Class) is
   begin
      if Self.Mark /= null then
         if not Self.Mark.Get_Deleted then
            Self.Buffer.Delete_Mark (Self.Mark);
            Disconnect (Self.Buffer, Self.Cid);
            Self.Buffer.Weak_Unref (On_Destroy_Buffer'Access, Convert (Self));
            Self.Mark   := null;
            Self.Buffer := null;
         end if;
      end if;
   end Delete;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Marker : in out File_Marker_Data) is
      use Marker_List;
      Module : constant Source_Editor_Module :=
                 Source_Editor_Module (Src_Editor_Module_Id);
      M1     : constant File_Marker := Marker'Unchecked_Access;

   begin
      if Module = null then
         return;
      end if;

      Free (Marker.Instances);

      if Marker.Mark /= null then
         --  We explicitly remove the weak_unref on the mark, even though
         --  it is likely it will be destroyed as a result of Delete_Mark.
         --  However, if someone happened to have a Ref on it, we want to be
         --  sure that On_Destroy_Mark will never be call with the dangling
         --  File_Marker pointer.

         if not Source_Buffer (Marker.Buffer).In_Destruction then
            Disconnect (Marker.Buffer, Marker.Cid);
            Weak_Unref (Marker.Buffer,
                        On_Destroy_Buffer'Access, Convert (M1));
         end if;

         --  Remove the mark from the buffer, but do not Unref it (since we do
         --  not own a reference, the buffer does).

         --  What we really want is to destroy the mark in gtk+ if no other
         --  file_marker or python class instance references it. However, none
         --  of those is ref'ing it, so we can't know that. If we do delete the
         --  mark, we end up with the following wrong scenario:
         --       GPS.EditorBuffer.add_special_line creates a file_marker, and
         --          a python class. Then the refcount of the file_marker goes
         --          to 0, and this Destroy is called, thus the mark is deleted
         --          and python instance no longer references a mark.
         --  So we cannot remove the mark explicitly from the buffer, and it
         --  will be destroyed when the buffer is destroyed. This might mean
         --  extra memory use over time, until the buffer is closed.
         --
         --  One solution would be to have a boolean that indicates whether
         --  there is a python class for the mark (in fact we can check that
         --  directly), and delete if there is none. That would at least
         --  remove the cases where we leave the mark
         --  Scenario:
         --      a = GPS.EditorBuffer.add_special_line
         --      # no more file_marker, but a exists, so don't delete mark
         --      # a then goes out of scope. Destructor of EditorMark deletes
         --      # the mark

         Marker.Mark := null;
         Marker.Buffer := null;
      end if;
   end Destroy;

   ----------------------
   -- Create_Text_Mark --
   ----------------------

   procedure Create_Text_Mark
     (Kernel : access Kernel_Handle_Record'Class;
      Marker : not null access File_Marker_Data'Class;
      Box    : Source_Editor_Box := null)
   is
      Source : Source_Editor_Box := Box;
   begin
      if Source = null then
         Source := Get_Source_Box_From_MDI
           (Find_Editor (Kernel, Marker.File, Marker.Project));
      end if;

      if Source /= null then
         if Marker.Mark = null
           or else Marker.Mark.Get_Deleted
         then
            Marker.Buffer :=
              Gtk_Text_Buffer (Source_Buffer'(Get_Buffer (Source)));

            --  Creates the mark. Its reference is owned by the buffer, and we
            --  do not need to have our own, since there would be no point in
            --  having the mark live longer than the buffer (we are monitoring
            --  life cycles anyway

            Marker.Mark := Create_Mark
              (Get_Buffer (Source), Marker.Line,
               Visible_Column_Type'Max (1, Marker.Column));

            --  This mark can be destroyed in three different contexts:
            --    1 - explicitly (call to Destroy for File_Marker)
            --    2 - explicitly (call to Delete_Mark on the gtk+ side)
            --    3 - implicitly (when the buffer itself is destroyed)
            --  The last two cases need to be protected, since the File_Marker
            --  itself isn't destroyed then, and we need to update its
            --  location).

            --  Protect against case 3: this results in the destruction of the
            --  text_mark anyway, so nothing to do.

            --  Protect against case 2.
            --  Connect on the mark itself. If it is removed from the buffer
            --  directly, it will be destroyed anyway, so we need to be aware
            --  of that.

            Weak_Ref
              (Marker.Buffer, On_Destroy_Buffer'Access, Convert (Marker));

            Marker.Cid := Markers_Callback.Connect
              (Source_Buffer (Marker.Buffer), Signal_Closed,
               Markers_Callback.To_Marshaller (On_Closed_Or_Changed'Access),
               File_Marker (Marker));

         else
            --  The mark already exists: simply move it to its intended
            --  location. This can happen for instance when reloading a file
            --  editor.

            declare
               Buffer : constant Source_Buffer := Get_Buffer (Source);
               Iter   : Gtk_Text_Iter;
               Line   : Editable_Line_Type;
               Column : Visible_Column_Type;

            begin
               Buffer.Get_Iter_At_Mark (Iter, Marker.Mark);
               Get_Iter_Position (Buffer, Iter, Line, Column);

               if Marker.Line /= Line or Marker.Column /= Column then
                  Buffer.Get_Iter_At_Screen_Position
                    (Iter   => Iter,
                     Line   => Marker.Line,
                     Column => Visible_Column_Type'Max (1, Marker.Column));
                  Buffer.Move_Mark (Marker.Mark, Iter);
               end if;
            end;
         end if;
      end if;
   end Create_Text_Mark;

   --------------
   -- Get_File --
   --------------

   overriding function Get_File
     (Marker : not null access File_Marker_Data) return Virtual_File is
   begin
      return Marker.File;
   end Get_File;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
     (Marker : not null access File_Marker_Data'Class)
      return Gtk.Text_Mark.Gtk_Text_Mark is
   begin
      return Marker.Mark;
   end Get_Mark;

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Length  : Natural := 0) return Location_Marker
   is
   begin
      return M : Location_Marker do
         M.Set (File_Marker_Data'
                  (File     => File,
                   Project  => Project,
                   Line     => Line,
                   Column   => Column,
                   Length   => Length,
                   Buffer   => null,
                   Mark     => null,
                   Cid      => <>,
                   Instances => <>,
                   Kernel   => Kernel_Handle (Kernel)));
         Register_Persistent_Marker (M);
         Create_Text_Mark (Kernel, File_Marker (M.Unchecked_Get), Box => null);
      end return;
   end Create_File_Marker;

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Mark    : Gtk.Text_Mark.Gtk_Text_Mark) return Location_Marker
   is
      Module : constant Source_Editor_Module :=
        Source_Editor_Module (Src_Editor_Module_Id);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (Mark));
      D : File_Marker;
   begin
      --  Check whether there is already a Location_Marker for this mark
      for Node of Module.Stored_Marks loop
         if not Node.Was_Freed then
            declare
               M : Location_Marker;
            begin
               M.Set (Node);
               if File_Marker (M.Unchecked_Get).Mark = Mark then
                  return M;
               end if;
            end;
         end if;
      end loop;

      return M : Location_Marker do
         M.Set (File_Marker_Data'
                  (File     => File,
                   Project  => Project,
                   Line     => 0,
                   Column   => 1,
                   Length   => 0,
                   Buffer   => Get_Buffer (Mark),
                   Mark     => Mark,
                   Cid      => <>,
                   Instances => <>,
                   Kernel   => Kernel_Handle (Kernel)));

         D := File_Marker (M.Unchecked_Get);

         D.Cid := Markers_Callback.Connect
           (Buffer, Signal_Closed,
            Markers_Callback.To_Marshaller (On_Closed_Or_Changed'Access), D);
         Weak_Ref (Buffer, On_Destroy_Buffer'Access, Convert (D));

         Update_Marker_Location (D);
         Register_Persistent_Marker (M);
      end return;
   end Create_File_Marker;

   ----------
   -- Move --
   ----------

   procedure Move
     (Marker : not null access File_Marker_Data'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark)
   is
   begin
      if Marker.Mark /= null then
         Weak_Unref
           (Marker.Buffer, On_Destroy_Buffer'Access,
            Convert (File_Marker (Marker)));
      end if;

      Marker.Buffer := Get_Buffer (Mark);
      Marker.Mark   := Mark;

      Weak_Ref
        (Marker.Buffer, On_Destroy_Buffer'Access,
         Convert (File_Marker (Marker)));
      Update_Marker_Location (Marker);
   end Move;

   ---------------------------------------------
   -- Push_Current_Editor_Location_In_History --
   ---------------------------------------------

   procedure Push_Current_Editor_Location_In_History
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Child  : constant MDI_Child := Find_Current_Editor (Kernel);
      Box    : Source_Editor_Box;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if not Is_Source_Box (Child) then
         return;
      end if;

      Box := Get_Source_Box_From_MDI (Child);

      if Box /= null then
         Get_Cursor_Position (Get_Buffer (Box), Line, Column);
         Push_Marker_In_History
           (Kernel  => Kernel,
            Marker  => Create_File_Marker
              (Kernel  => Kernel,
               File    => Get_Filename (Box),
               Project => Get_Project (Box),
               Line    => Line,
               Column  => Column));
      end if;
   end Push_Current_Editor_Location_In_History;

   -----------
   -- Go_To --
   -----------

   overriding function Go_To
     (Marker : not null access File_Marker_Data) return Boolean
   is
      Child : constant MDI_Child := Find_Editor
        (Marker.Kernel, Marker.File, Marker.Project);
      Box   : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
   begin
      if Child /= null
        and then Marker.Mark /= null
      then
         Raise_Child (Child);
         Set_Focus_Child (Child);
         Grab_Focus (Box);
         Scroll_To_Mark (Box, Marker.Mark, Marker.Length);

      else
         Open_File_Action_Hook.Run
           (Marker.Kernel,
            File      => Marker.File,
            Project   => Marker.Project,
            Line      => Integer (Marker.Line),
            Column    => Marker.Column,
            Column_End => Marker.Column + Visible_Column_Type (Marker.Length),
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

   overriding function Get_Line
     (Marker : not null access File_Marker_Data)
     return Editable_Line_Type is
   begin
      Update_Marker_Location (Marker);
      return Marker.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   overriding function Get_Column
     (Marker : not null access File_Marker_Data)
     return Visible_Column_Type is
   begin
      Update_Marker_Location (Marker);
      return Marker.Column;
   end Get_Column;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Marker : not null access File_Marker_Data) return String
   is
      function Get_Subprogram_Name return String;
      --  Returns the subprogram name at the marker position

      -------------------------
      -- Get_Subprogram_Name --
      -------------------------

      function Get_Subprogram_Name return String is
         Box : constant Source_Editor_Box :=
                 Get_Source_Box_From_MDI
                   (Find_Editor (Marker.Kernel, Marker.File, Marker.Project));
      begin
         if Box /= null then
            return Get_Subprogram_Name (Box, Marker.Line);
         else
            return "";
         end if;
      end Get_Subprogram_Name;

   begin
      Update_Marker_Location (Marker);

      declare
         Location : constant String := +Base_Name (Marker.File)
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
     (Marker : not null access File_Marker_Data) return XML_Utils.Node_Ptr
   is
      Node : constant Node_Ptr := new XML_Utils.Node;
   begin
      Update_Marker_Location (Marker);
      Node.Tag := new String'("file_marker");
      Add_File_Child (Node, "file", Marker.File);
      Add_File_Child (Node, "project", Marker.Project.Project_Path);
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
      From_XML : XML_Utils.Node_Ptr := null) return Location_Marker
   is
      Source : Source_Editor_Box;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
   begin
      if From_XML /= null then
         if From_XML.Tag.all = "file_marker" then
            return Create_File_Marker
              (Kernel  => Kernel,
               File    => Get_File_Child (From_XML, "file"),
               Project => Get_Registry (Kernel).Tree.Project_From_Path
               (Get_File_Child (From_XML, "project")),
               Line    => Editable_Line_Type'Value
                 (Get_Attribute (From_XML, "line")),
               Column  =>
                 Visible_Column_Type'Value
                   (Get_Attribute (From_XML, "column")));
         end if;

      else
         Source := Get_Source_Box_From_MDI (Find_Current_Editor (Kernel));
         if Source /= null then
            Get_Cursor_Position (Get_Buffer (Source), Line, Column);
            return Create_File_Marker
              (Kernel  => Kernel,
               File    => Get_Filename (Source),
               Project => Get_Project (Source),
               Line    => Line,
               Column  => Column);
         end if;
      end if;
      return No_Marker;
   end Load;

   -------------
   -- Similar --
   -------------

   overriding function Similar
     (Left  : not null access File_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Boolean is
   begin
      if Right.all in File_Marker_Data'Class then
         declare
            R : constant File_Marker := File_Marker (Right);
         begin
            Update_Marker_Location (Left);
            Update_Marker_Location (R);
            return R.File = Left.File and then R.Line = Left.Line;
         end;
      else
         return False;
      end if;
   end Similar;

   --------------
   -- Distance --
   --------------

   overriding function Distance
     (Left  : not null access File_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Integer
   is
   begin
      if Right.all in File_Marker_Data'Class then
         declare
            R : constant File_Marker := File_Marker (Right);
         begin
            if Left.File = R.File then
               return Integer (R.Line - Left.Line);
            end if;
         end;
      end if;

      return Integer'Last;
   end Distance;

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Self            : Location_Marker;
      Script          : not null access Scripting_Language_Record'Class)
      return Class_Instance
   is
      F : constant File_Marker := File_Marker (Self.Unchecked_Get);
   begin
      return Proxies.Get_Or_Create_Instance (F.Instances, Self, Script);
   end Get_Or_Create_Instance;

   ------------------------------
   -- Unset_Marker_In_Instance --
   ------------------------------

   procedure Unset_Marker_In_Instance (Self : Class_Instance) is
      Dummy : File_Marker_Proxy;
   begin
      Proxies.Store_In_Instance (Dummy, Self, No_Marker);
      Free (Dummy);
   end Unset_Marker_In_Instance;

   -------------------
   -- From_Instance --
   -------------------

   function From_Instance (Inst : Class_Instance) return Location_Marker is
   begin
      return Proxies.From_Instance (Inst);
   exception
      when No_Data_Set_For_Instance =>
         return No_Marker;
   end From_Instance;

end Src_Editor_Module.Markers;
