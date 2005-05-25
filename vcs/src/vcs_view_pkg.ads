-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with Gtk.Box;                  use Gtk.Box;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;

with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Console;     use GPS.Kernel.Console;

with String_List_Utils;        use String_List_Utils;

with VCS;                      use VCS;
with VFS;                      use VFS;

with Ada.Unchecked_Deallocation;
with HTables;

package VCS_View_Pkg is

   type VCS_View_Record is new Gtk_Hbox_Record with private;
   type VCS_View_Access is access all VCS_View_Record'Class;

   procedure Gtk_New
     (VCS_View : out VCS_View_Access;
      Kernel   : Kernel_Handle := null);
   --  Create a new VCS explorer.

   procedure Initialize
     (VCS_View : access VCS_View_Record'Class;
      Kernel   : Kernel_Handle);
   --  Internal initialization function.

   function Get_Kernel
     (VCS_View : access VCS_View_Record) return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with VCS_View

   procedure Clear (Explorer : VCS_View_Access);
   --  Clear all the files in the model.

   procedure Display_File_Status
     (Kernel         : Kernel_Handle;
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True);
   --  Display Status in the explorer.
   --  Status must be freed by the user after calling this function.
   --  If Override_Cache is set to True, then the cache will be updated
   --  with the new status information, if needed. Otherwise, the values from
   --  the cache will be used for displaying the files.
   --  If Force_Display is True, then files that are not already visible
   --  will be added to the list. Otherwise this function only updates
   --  status for files that are currently visible in the view.
   --  If Clear_Logs is True, then the log files corresponding
   --  to the files that have an "Up_To_Date" status will be deleted,
   --  as well as the corresponding file/log mapping for those files.
   --  If Display is False, do not display the status but only refresh the
   --  cache.

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return String_List.List;
   --  Return the list of files that are selected.

   procedure Push_Message
     (Kernel   : Kernel_Handle;
      M_Type   : Message_Type;
      Message  : String);
   --  Display a message.

   function Get_Current_Dir
     (Context : Selection_Context_Access) return String;
   --  Convenience function to get the current directory.

   function Get_Current_File
     (Context : Selection_Context_Access) return VFS.Virtual_File;
   --  Convenience function to get the current file.

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   function Get_Current_Ref
     (Explorer : access VCS_View_Record) return VCS_Access;
   --  Return the VCS reference currently being viewed in Explorer.

   function Get_Current_Context
     (Explorer : access VCS_View_Record) return Selection_Context_Access;

   procedure Set_Current_Context
     (Explorer : access VCS_View_Record;
      Context  : Selection_Context_Access);

   procedure Refresh_Log
     (Explorer : access VCS_View_Record;
      File     : VFS.Virtual_File);
   --  Refresh the "Log" column for File.

   function Get_Cached_Status
     (Explorer : access VCS_View_Record;
      File     : VFS.Virtual_File;
      Ref      : VCS_Access) return File_Status_Record;
   --  Return the cached status corresponding to File.
   --  User must not free the result.

private
   type Line_Record is record
      Status : File_Status_Record;
      --  The file status.

      Log    : Boolean;
      --  Whether the file is associated with a changelog.
   end record;
   --  The information stored in one line of the VCS explorer.

   No_Data : constant Line_Record :=
               ((VFS.No_File, Unknown, others => String_List.Null_List),
                False);

   procedure Free (X : in out Line_Record);
   --  Free memory associated with X.

   ----------------------
   -- Cache Hash-table --
   ----------------------

   --  This implements a quick way to retrieve an editor which corresponds to
   --  a given file.

   type Header_Num is range 1 .. 5_000;

   type Element is record
      Line : Line_Record;
   end record;

   procedure Free (X : in out Element);

   No_Element : constant Element := (Line => No_Data);

   function Hash (F : Virtual_File) return Header_Num;
   function Equal (F1, F2 : Virtual_File) return Boolean;

   package Status_Hash is new HTables.Simple_HTable
     (Header_Num, Element, Free, No_Element, Virtual_File, Hash, Equal);

   type VCS_Page_Record;
   type VCS_Page_Access is access all VCS_Page_Record;

   type Page_Status_Record is record
      Status  : File_Status;
      Display : Boolean := True;
   end record;

   type Page_Status_Array is array (Natural range <>) of Page_Status_Record;
   type Page_Status_Array_Access is access Page_Status_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Page_Status_Array, Page_Status_Array_Access);

   type VCS_Page_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;

      Reference : VCS_Access;

      Tree   : Gtk_Tree_View;
      Model  : Gtk_Tree_Store;

      Stored_Status   : Status_Hash.HTable;
      Cached_Status   : Status_Hash.HTable;

      Shown : Boolean := False;

      File_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column containing the file names.

      Status_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column containing the file status.

      Log_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column corresponding to the log file indicator.

      Status     : Page_Status_Array_Access;
   end record;

   type VCS_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;
      --  Reference to the kernel that launched the explorer, if any.

      Context : Selection_Context_Access;
      --  The current context being shown / selected in the explorer.

      Notebook   : Gtk_Notebook;
      --  The notebook containing the actual explorer pages.

      Number_Of_Pages : Integer := 0;
      --  The number of pages in the notebook.
   end record;

end VCS_View_Pkg;
