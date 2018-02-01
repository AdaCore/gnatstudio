------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with System;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Strings;               use GNAT.Strings;

with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Glib.Glist;
with Glib.Object;                use Glib.Object;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Event_Box;              use Gtk.Event_Box;
with Gtk.Expander;               use Gtk.Expander;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Image;                  use Gtk.Image;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Paned;                  use Gtk.Paned;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Spin_Button;            use Gtk.Spin_Button;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Table;                  use Gtk.Table;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.File_Selector;       use Gtkada.File_Selector;
with Gtkada.Handlers;            use Gtkada.Handlers;

with Gexpect;                    use Gexpect;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Main_Window;            use GPS.Main_Window;
with GUI_Utils;                  use GUI_Utils;
with String_Utils;               use String_Utils;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Remote.Db;                  use Remote, Remote.Db;
with Remote_Module;              use Remote_Module;

package body Remote.Config_Dialog is

   Me : constant Trace_Handle := Create ("GPS.REMOTE.CONFIG_DIALOG");

   ------------------------
   -- Server list dialog --
   ------------------------

   Name_Col     : constant := 0;

   Invalid_Path : exception;

   Enter_Local_Path_String  : constant String := -"<enter local path here>";
   Enter_Remote_Path_String : constant String := -"<enter remote path here>";

   Synchronisation_String : constant
     array (Synchronisation_Type) of GNAT.OS_Lib.String_Access :=
                              (Never          => new String'("Never"),
                               On_Request     => new String'("Manually"),
                               Always         => new String'("Always"),
                               To_Local  => new String'("To local"),
                               To_Remote => new String'("To remote"));

   type Path_Row_Record is record
      Local_Entry          : Gtk_Entry;
      Local_Browse_Button  : Gtk_Button;
      Local_Frame          : Gtk_Frame;
      Local_Hbox           : Gtk_Hbox;
      Remote_Entry         : Gtk_Entry;
      Remote_Browse_Button : Gtk_Button;
      Remote_Frame         : Gtk_Frame;
      Remote_Hbox          : Gtk_Hbox;
      Sync_Combo           : Gtk_Combo_Box_Text;
      Remove_Button        : Gtk_Button;
   end record;
   type Path_Row is access all Path_Row_Record;
   --  This widget is the graphical representation of a mirror path

   function Convert is new Ada.Unchecked_Conversion (System.Address, Path_Row);
   function Convert is new Ada.Unchecked_Conversion (Path_Row, System.Address);

   type For_Each_Data (Nickname_Length : Natural) is record
      Nickname : String (1 .. Nickname_Length);
      The_Iter : Gtk_Tree_Iter;
      Found    : Boolean;
   end record;
   type For_Each_Data_Access is access all For_Each_Data;

   function For_Each
     (Model     : Gtk_Tree_Model;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      User_Data : For_Each_Data_Access) return Boolean;
   --  CB function for Gtk.Tree_Model.Foreach, searching for the iter
   --  corresponding to the previously selected machine.

   package Foreach is new Gtk.Tree_Model.Foreach_User_Data
     (For_Each_Data_Access);

   package Path_Row_List is new Glib.Glist.Generic_List (Path_Row);

   type Paths_Widget_Record is new Gtk_Frame_Record with record
      Table           : Gtk_Table;
      Add_Path_Button : Gtk_Button;
      List            : Path_Row_List.Glist;
      Nb_Rows         : Guint;
      Dialog          : Gtk_Dialog;
   end record;
   type Paths_Widget is access all Paths_Widget_Record'Class;

   procedure Gtk_New
     (Widget      : out Paths_Widget;
      Dialog      : Gtk_Dialog);

   procedure Set_Path_List
     (Widget : Paths_Widget;
      List   : Mount_Point_Array);
   --  Reset the widget and fills it with the path list

   function Get_Mount_Points
     (Widget : Paths_Widget;
      Host   : String) return Mount_Point_Array;
   --  Retrieve the mirror path list represented by the widget

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Local_Path  : Virtual_File := No_File;
      Remote_Path : Virtual_File := No_File;
      Synchro     : Synchronisation_Type := Synchronisation_Type'First);
   --  Add a new Path row to the Mirror_Path_Widget

   procedure Remove_Path_Row
     (Widget : Paths_Widget;
      Row    : Path_Row);
   --  Remove the row from widget

   function Get_Mount_Point
     (Row  : Path_Row; Host : String) return Mount_Point;
   --  Retrieve the mirror path represented by the widget

   procedure On_Path_Grab_Focus (Widget : access Gtk_Widget_Record'Class);
   --  Called when a path with default string is clicked

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class);
   --  Add_Path button is clicked

   type Path_Cb_Data is new Glib.Object.GObject_Record with record
      Widget : Paths_Widget;
      Row    : Path_Row;
   end record;
   type Path_Cb_Data_Access is access all Path_Cb_Data'Class;

   package Path_Callback is new
     Gtk.Handlers.Callback (Path_Cb_Data);

   procedure On_Remove_Path_Clicked (W : access Path_Cb_Data'Class);
   --  One of the Remove_Path button is clicked

   procedure On_Browse_Local (Widget : access Path_Cb_Data'Class);
   --  Select a local directory

   procedure On_Browse_Remote (Widget : access Path_Cb_Data'Class);
   --  Select a remote directory

   type Server_List_Editor_Record is new Gtk_Dialog_Record with record
      Kernel                : Kernel_Handle;
      Selected_Machine      : String_Access;
      Machine_Tree          : Gtk_Tree_View;
      --  Machine config pannel
      Right_Table           : Gtk_Table;
      Nickname_Event        : Gtk_Event_Box;
      Nickname_Label        : Gtk_Label;
      Nickname_Entry        : Gtk_Entry;
      Network_Name_Entry    : Gtk_Entry;
      Remote_Access_Combo   : Gtk_Combo_Box_Text;
      Remote_Shell_Combo    : Gtk_Combo_Box_Text;
      Remote_Sync_Combo     : Gtk_Combo_Box_Text;
      --  Advanced config panel
      Advanced_Pane         : Gtk.Expander.Gtk_Expander;
      Advanced_Table        : Gtk_Table;
      User_Name_Entry       : Gtk_Entry;
      Max_Nb_Connected_Spin : Gtk_Spin_Button;
      Cr_Lf_Combo           : Gtk_Combo_Box_Text;
      Timeout_Spin          : Gtk_Spin_Button;
      Init_Cmds_View        : Gtk_Text_View;
      Debug_Button          : Gtk_Check_Button;
      --  Mirror Paths config pannel
      Paths_List_Widget     : Paths_Widget;
      --  Add/Remove/Restore buttons
      Add_Machine_Button    : Gtk_Button;
      Restore_Button        : Gtk_Button;
      Remove_Button         : Gtk_Button;
      New_Machine           : Boolean := False;
      Modified              : Boolean := False;
      Applied               : Boolean := False;
      Select_Back           : Boolean := False;
   end record;
   type Server_List_Editor is access all Server_List_Editor_Record'Class;

   procedure Gtk_New
     (Dialog         : out Server_List_Editor;
      Kernel         : Kernel_Handle;
      Default_Server : String);
   --  Creates the server_list_editor dialog

   procedure Set_Machine
     (Dialog   : access Server_List_Editor_Record'Class;
      Nickname : String);
   --  Initialize the different fields of Dialog with the values of Machine.

   procedure Remove_Machine
     (Dialog : access Server_List_Editor_Record'Class;
      Nickname : String);
   --  Remove the machine 'Nickname' from Dialog.

   procedure Select_Back
     (Dialog : access Server_List_Editor_Record'Class);
   --  Cancel the server selection change

   procedure On_Changed
     (W                 : access Gtk_Widget_Record'Class;
      Connection_Params : Boolean);
   --  Called when one of the entries has changed
   --  Connection_Params tells if connection configuration changed. If set, the
   --   machine cannot be browsed until 'Apply' is called.

   package Widget_Boolean_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Boolean);

   function Save_Current
     (Dialog : Server_List_Editor;
      Force  : Boolean) return Boolean;
   --  Saves the currently selected server.

   procedure On_Selection_Changed (W : access Gtk_Widget_Record'Class);
   --  Called when the selected machine has changed

   procedure On_Add_Machine_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the add_machine button is clicked

   procedure On_Restore_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the restore button is clicked

   procedure On_Remove_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the remove button is clicked

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Paths_Widget;
      Dialog : Gtk_Dialog)
   is
      Pix   : Gtk_Image;
      Label : Gtk_Label;
   begin
      Widget := new Paths_Widget_Record;
      Gtk.Frame.Initialize (Widget, -"Path Translations");

      Widget.Dialog      := Dialog;

      Gtk_New (Widget.Table, 1, 6, False);
      Add (Widget, Widget.Table);

      Gtk_New (Label, -"Local Path");
      Attach (Widget.Table, Label, 0, 1, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"Remote Path");
      Attach (Widget.Table, Label, 1, 2, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"Sync");
      Attach (Widget.Table, Label, 2, 3, 0, 1, 0, 0);

      Gtk_New (Widget.Add_Path_Button);
      Set_Name (Widget.Add_Path_Button, "add path button");
      Gtk_New_From_Icon_Name (Pix, "gps-add-symbolic", Icon_Size_Menu);
      Add (Widget.Add_Path_Button, Pix);
      Attach (Widget.Table, Widget.Add_Path_Button, 3, 4, 1, 2, 0, 0);

      Widget.List := Path_Row_List.Null_List;
      Widget_Callback.Object_Connect
        (Widget.Add_Path_Button, Signal_Clicked,
         On_Add_Path_Clicked'Access, Widget);
   end Gtk_New;

   -------------------
   -- Set_Path_List --
   -------------------

   procedure Set_Path_List
     (Widget : Paths_Widget;
      List   : Mount_Point_Array)
   is
      Row    : Path_Row;
      use type Path_Row_List.Glist;

   begin
      loop
         exit when Widget.List = Path_Row_List.Null_List;
         Row := Path_Row_List.Get_Data (Widget.List);
         Remove_Path_Row (Widget, Row);
      end loop;

      Ref (Widget.Add_Path_Button);
      Remove (Widget.Table, Widget.Add_Path_Button);

      Resize (Widget.Table, 1, 4);

      Widget.Nb_Rows := 1;

      for J in List'Range loop
         Add_Path_Row
           (Widget,
            Row_Number  => Widget.Nb_Rows,
            Local_Path  => List (J).Local_Root,
            Remote_Path => List (J).Remote_Root,
            Synchro     => List (J).Sync);
         Widget.Nb_Rows := Widget.Nb_Rows + 1;
      end loop;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);
      Unref (Widget.Add_Path_Button);
   end Set_Path_List;

   ----------------------
   -- Get_Mount_Points --
   ----------------------

   function Get_Mount_Points
     (Widget : Paths_Widget;
      Host   : String) return Mount_Point_Array
   is
      List : Path_Row_List.Glist;
      Row  : Path_Row;
      use type Path_Row_List.Glist;

   begin
      List := Widget.List;

      declare
         Mount_Points : Mount_Point_Array
                          (1 .. Natural (Path_Row_List.Length (List)));
         Idx          : Natural := 1;
      begin

         while List /= Path_Row_List.Null_List loop
            Row := Path_Row_List.Get_Data (List);
            Mount_Points (Idx) := Get_Mount_Point (Row, Host);
            List := Path_Row_List.Next (List);
            Idx  := Idx + 1;
         end loop;

         return Mount_Points;
      end;
   end Get_Mount_Points;

   ------------------
   -- Add_Path_Row --
   ------------------

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Local_Path  : Virtual_File := No_File;
      Remote_Path : Virtual_File := No_File;
      Synchro     : Synchronisation_Type := Synchronisation_Type'First)
   is
      Pix  : Gtk_Image;
      Row  : Path_Row;
      Data : Path_Cb_Data_Access;
   begin
      Row := new Path_Row_Record;

      Gtk_New (Row.Local_Frame);
      Attach (Widget.Table, Row.Local_Frame, 0, 1, Row_Number, Row_Number + 1,
              Fill or Expand or Shrink, 0, 0, 2);

      Gtk_New_Hbox (Row.Local_Hbox, Spacing => 0);
      Add (Row.Local_Frame, Row.Local_Hbox);

      Gtk_New (Row.Local_Entry);
      Set_Name (Row.Local_Entry,
                "local path entry" & Guint'Image (Row_Number));
      Set_Width_Chars (Row.Local_Entry, 16);
      Pack_Start (Row.Local_Hbox, Row.Local_Entry, True, True);
      Set_Tooltip_Text (Row.Local_Entry, -("Enter here the local path"));

      Gtk_New (Row.Local_Browse_Button);
      Gtk_New_From_Icon_Name (Pix, "gps-open-file-symbolic", Icon_Size_Menu);
      Add (Row.Local_Browse_Button, Pix);
      Set_Relief (Row.Local_Browse_Button, Relief_None);
      Set_Border_Width (Row.Local_Browse_Button, 0);
      Set_Can_Focus (Row.Local_Browse_Button, False);
      Set_Can_Default (Row.Local_Browse_Button, False);
      Pack_Start (Row.Local_Hbox, Row.Local_Browse_Button, False, False);
      Set_Tooltip_Text
        (Row.Local_Browse_Button,
         -"Use this button to select a local path with a file explorer");

      Gtk_New (Row.Remote_Frame);
      Attach (Widget.Table, Row.Remote_Frame, 1, 2, Row_Number, Row_Number + 1,
              Fill or Expand or Shrink, 0, 0, 2);

      Gtk_New_Hbox (Row.Remote_Hbox, Spacing => 0);
      Add (Row.Remote_Frame, Row.Remote_Hbox);

      Gtk_New (Row.Remote_Entry);
      Set_Name (Row.Remote_Entry,
                "remote path entry" & Guint'Image (Row_Number));
      Set_Width_Chars (Row.Remote_Entry, 16);
      Pack_Start (Row.Remote_Hbox, Row.Remote_Entry, True, True);
      Set_Tooltip_Text (Row.Remote_Entry, -("Enter here the remote path"));

      Gtk_New (Row.Remote_Browse_Button);
      Gtk_New_From_Icon_Name (Pix, "gps-open-file-symbolic", Icon_Size_Menu);
      Add (Row.Remote_Browse_Button, Pix);
      Set_Relief (Row.Remote_Browse_Button, Relief_None);
      Set_Border_Width (Row.Remote_Browse_Button, 0);
      Set_Can_Focus (Row.Remote_Browse_Button, False);
      Set_Can_Default (Row.Remote_Browse_Button, False);
      Pack_Start (Row.Remote_Hbox, Row.Remote_Browse_Button, False, False);
      Set_Tooltip_Text
        (Row.Remote_Browse_Button,
         -("Use this button to select a remote path with a file explorer. " &
           "Note that the machine configuration shall be properly set and " &
           "applied"));

      Gtk_New (Row.Sync_Combo);
      declare
         Cell : Gtk_Cell_Renderer_Text;
      begin
         Gtk_New (Row.Sync_Combo);
         Gtk_New (Cell);
         Row.Sync_Combo.Pack_Start (Cell, True);
         Row.Sync_Combo.Add_Attribute (Cell, "text", 0);

         for J in Synchronisation_Type'Range loop
            Row.Sync_Combo.Append_Text (Synchronisation_String (J).all);
         end loop;

         Row.Sync_Combo.Set_Active (Synchronisation_Type'Pos (Synchro));
      end;

      Attach (Widget.Table, Row.Sync_Combo, 2, 3, Row_Number, Row_Number + 1,
              0, 0, 0, 2);
      Set_Tooltip_Text
        (Row.Sync_Combo,
         -("Five kinds of path synchronization can be set for each defined " &
           "path:" & ASCII.LF &
           "* Never: no synchronization is required from GPS, the paths " &
           "are shared using an OS mechanism like NFS." & ASCII.LF &
           "* Manually: synchronization is needed, but will only be " &
           "performed manually using the remote view buttons." & ASCII.LF &
           "* Always: the paths are kept synchronised by GPS before and " &
           "after every remote action (e.g. build)." &
           ASCII.LF &
           "* To local/remote: The project's dependencies are" &
           " synchronized once when a remote project is loaded or when a " &
           "local project is set remote. They can be still manually " &
           "synchronized using the Remote View."));

      Gtk_New (Row.Remove_Button);
      Gtk_New_From_Icon_Name (Pix, "gps-remove-symbolic", Icon_Size_Menu);
      Add (Row.Remove_Button, Pix);
      Attach
        (Widget.Table, Row.Remove_Button, 3, 4, Row_Number, Row_Number + 1,
         0, 0, 0, 2);

      Path_Row_List.Append (Widget.List, Row);

      if Local_Path = No_File then
         Set_Text (Row.Local_Entry, Enter_Local_Path_String);
         Widget_Callback.Object_Connect
           (Row.Local_Entry, Signal_Grab_Focus,
            On_Path_Grab_Focus'Access, Row.Local_Entry);
      else
         Set_Text (Row.Local_Entry, Local_Path.Display_Full_Name);
      end if;

      if Remote_Path = No_File then
         Set_Text (Row.Remote_Entry, Enter_Remote_Path_String);
         Widget_Callback.Object_Connect
           (Row.Remote_Entry, Signal_Grab_Focus,
            On_Path_Grab_Focus'Access, Row.Remote_Entry);
      else
         Set_Text (Row.Remote_Entry, Remote_Path.Display_Full_Name);
      end if;

      declare
         List : Gtk_Tree_Model renames Row.Sync_Combo.Get_Model;
         Iter : Gtk_Tree_Iter := Get_Iter_First (List);
      begin
         while Iter /= Null_Iter loop
            if Get_String (List, Iter, 0) =
              Synchronisation_String (Synchro).all
            then
               Row.Sync_Combo.Set_Active_Iter (Iter);
               exit;
            end if;

            Next (List, Iter);
         end loop;
      end;

      Data := new Path_Cb_Data;
      Data.Widget := Widget;
      Data.Row    := Row;

      Widget_Boolean_Callback.Object_Connect
        (Row.Local_Entry, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Local_Browse_Button, Signal_Clicked,
         On_Browse_Local'Access, Data);
      Widget_Boolean_Callback.Object_Connect
        (Row.Remote_Entry, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Remote_Browse_Button, Signal_Clicked,
         On_Browse_Remote'Access, Data);
      Widget_Boolean_Callback.Object_Connect
        (Row.Sync_Combo, Gtk.Combo_Box.Signal_Changed,
         On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Remove_Button, Signal_Clicked,
         On_Remove_Path_Clicked'Access, Data);
      Show_All (Widget.Table);
   end Add_Path_Row;

   ------------------------
   -- On_Path_Grab_Focus --
   ------------------------

   procedure On_Path_Grab_Focus (Widget : access Gtk_Widget_Record'Class) is
      Gentry : constant Gtk_Entry := Gtk_Entry (Widget);
      Str    : constant String := Get_Text (Gentry);
   begin
      if Str = Enter_Local_Path_String
        or else Str = Enter_Remote_Path_String
      then
         Set_Text (Gentry, "");
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Path_Grab_Focus;

   ---------------------
   -- Remove_Path_Row --
   ---------------------

   procedure Remove_Path_Row
     (Widget : Paths_Widget;
      Row    : Path_Row)
   is
   begin
      Remove (Widget.Table, Row.Local_Frame);
      Remove (Widget.Table, Row.Remote_Frame);
      Remove (Widget.Table, Row.Sync_Combo);
      Remove (Widget.Table, Row.Remove_Button);
      Path_Row_List.Remove (Widget.List, Row);
   end Remove_Path_Row;

   ---------------------
   -- Get_Mount_Point --
   ---------------------

   function Get_Mount_Point (Row : Path_Row; Host : String) return Mount_Point
   is
      Local   : constant String := Get_Text (Row.Local_Entry);
      Remote  : constant String := Get_Text (Row.Remote_Entry);
      Loc_Dir : Virtual_File;
      Rem_Dir : Virtual_File;
      Sync    : Synchronisation_Type := Synchronisation_Type'First;
      Dead    : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);

   begin
      if Local = Enter_Local_Path_String
        or else Local = ""
      then
         Dead := Message_Dialog (-"Please enter a valid local path",
                                 Error, Button_OK);
         raise Invalid_Path;
      end if;

      if Remote = Enter_Remote_Path_String
        or else Remote = ""
      then
         Dead := Message_Dialog (-"Please enter a valid remote path",
                                 Error, Button_OK);
         raise Invalid_Path;
      end if;

      Loc_Dir := Create_From_UTF8 (Local);
      Rem_Dir := Create_From_UTF8 (Remote, Host);

      if not Loc_Dir.Is_Absolute_Path then
         Dead := Message_Dialog
           (-"Local path " & Local & (-" needs to be an absolute path"),
            Error, Button_OK);
         raise Invalid_Path;
      end if;

      if not Rem_Dir.Is_Absolute_Path then
         Dead := Message_Dialog
           (-"Remote path " & Remote & (-" needs to be an absolute path"),
            Error, Button_OK);
         raise Invalid_Path;
      end if;

      declare
         Model    : Gtk_Tree_Model renames Row.Sync_Combo.Get_Model;
         Sync_Str : constant String :=
                      Get_String (Model, Row.Sync_Combo.Get_Active_Iter, 0);
      begin
         for J in Synchronisation_Type'Range loop
            if Sync_Str = Synchronisation_String (J).all then
               Sync := J;
               exit;
            end if;
         end loop;
      end;

      if Active (Me) then
         Trace (Me, "Get_Mount_Point : " &
                Loc_Dir.Display_Full_Name & " - " &
                Rem_Dir.Display_Full_Name & " - " &
                Synchronisation_Type'Image (Sync));
      end if;

      return
        (Local_Root  => Loc_Dir,
         Remote_Root => Rem_Dir,
         Sync        => Sync);
   end Get_Mount_Point;

   -------------------------
   -- On_Add_Path_Clicked --
   -------------------------

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class) is
      Widget : Paths_Widget_Record renames Paths_Widget_Record (W.all);
   begin
      Ref (Widget.Add_Path_Button);
      Remove (Widget.Table, Widget.Add_Path_Button);

      Add_Path_Row (Paths_Widget (W), Widget.Nb_Rows);
      Widget.Nb_Rows := Widget.Nb_Rows + 1;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);

   exception
      when E : others => Trace (Me, E);
   end On_Add_Path_Clicked;

   ----------------------------
   -- On_Remove_Path_Clicked --
   ----------------------------

   procedure On_Remove_Path_Clicked (W : access Path_Cb_Data'Class) is
   begin
      Remove_Path_Row (W.Widget, W.Row);
      On_Changed (W.Widget.Dialog, False);

   exception
      when E : others => Trace (Me, E);
   end On_Remove_Path_Clicked;

   ---------------------
   -- On_Browse_Local --
   ---------------------

   procedure On_Browse_Local (Widget : access Path_Cb_Data'Class) is
      Current_Dir : constant String :=
                      Get_Text (Widget.Row.Local_Entry);
      Start_Dir   : Virtual_File := No_File;
   begin
      if Current_Dir /= Enter_Local_Path_String then
         Start_Dir := Create_From_UTF8 (Current_Dir);

         if not Is_Directory (Start_Dir) then
            Start_Dir := Get_Current_Dir;
         end if;
      end if;

      declare
         Dir : constant GNATCOLL.VFS.Virtual_File :=
                 Select_Directory
                   (Base_Directory => Start_Dir,
                    Parent         => Gtk_Window (Widget.Widget.Dialog));
      begin
         if Dir /= No_File then
            Set_Text (Widget.Row.Local_Entry, Display_Full_Name (Dir));
            On_Changed (Widget.Widget.Dialog, False);
         end if;
      end;
   end On_Browse_Local;

   ----------------------
   -- On_Browse_Remote --
   ----------------------

   procedure On_Browse_Remote (Widget : access Path_Cb_Data'Class) is
      Current_Dir : constant String :=
                      Get_Text (Widget.Row.Remote_Entry);
      Start_Dir   : Virtual_File := No_File;
      Dialog      : constant Server_List_Editor :=
                      Server_List_Editor (Widget.Widget.Dialog);
      Gtk_Resp    : Message_Dialog_Buttons;
      pragma Unreferenced (Gtk_Resp);

   begin
      if Dialog.Selected_Machine = null then
         --  Should never happend... however, still preferable to catch
         --  this case !
         Trace (Me, "Dialog.Selected_Machine null while calling " &
                "On_Browse_Remote. This should never happend !");

         return;
      end if;

      if not Dialog.Applied then
         Gtk_Resp := Message_Dialog
           (-"Cannot browse the selected server until Apply button is pressed",
            Dialog_Type => Error,
            Buttons     => Button_OK,
            Parent      => Gtk_Window (Widget.Widget.Get_Toplevel));
         return;
      end if;

      --  Check connection before browsing the remote host
      Start_Dir := Get_Current_Dir (Dialog.Selected_Machine.all);

      if not Is_Directory (Start_Dir) then
         Gtk_Resp := Message_Dialog
           (-"Could not establish communication with selected host",
            Dialog_Type => Error,
            Buttons     => Button_OK);
         return;
      end if;

      --  Determine Start directory
      if Current_Dir /= Enter_Remote_Path_String then
         Start_Dir := Create_From_UTF8
           (Full_Filename => Current_Dir,
            Host          => Dialog.Selected_Machine.all);

         if not Is_Directory (Start_Dir) then
            Start_Dir :=
              Get_Current_Dir (Dialog.Selected_Machine.all);
         end if;
      end if;

      if Start_Dir = No_File then
         Start_Dir := Get_Root
           (Get_Current_Dir (Dialog.Selected_Machine.all));
      end if;

      declare
         Dir : constant GNATCOLL.VFS.Virtual_File :=
                 Select_Directory
                   (Base_Directory => Start_Dir,
                    Parent         => Gtk_Window (Widget.Widget.Dialog));
      begin
         if Dir /= No_File then
            Set_Text (Widget.Row.Remote_Entry, Display_Full_Name (Dir));
            On_Changed (Widget.Widget.Dialog, False);
         end if;
      end;
   end On_Browse_Remote;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog         : out Server_List_Editor;
      Kernel         : Kernel_Handle;
      Default_Server : String)
   is
      Main_Table   : Gtk_Paned;
      Frame        : Gtk_Frame;
      Scrolled     : Gtk_Scrolled_Window;
      Model        : Gtk_Tree_Store;
      Iter         : Gtk_Tree_Iter;
      Tmp          : Gtk_Widget;
      Label        : Gtk_Label;
      Line_Nb      : Guint;
      VBox         : Gtk_Vbox;
      Event        : Gtk_Event_Box;
      Machines     : constant GNAT.Strings.String_List :=
                       Get_Database.Get_Servers;
      Shells       : GNAT.Strings.String_List := Get_Database.Get_Shells;
      Access_Tools : GNAT.Strings.String_List := Get_Database.Get_Access_Tools;
      Sync_Tools   : GNAT.Strings.String_List := Get_Database.Get_Sync_Tools;
      pragma Unreferenced (Tmp);

   begin
      Dialog := new Server_List_Editor_Record;
      Initialize
        (Dialog,
         -"Servers configuration",
         Get_Main_Window (Kernel),
         Modal or Destroy_With_Parent
         or Use_Header_Bar_From_Settings (Get_Main_Window (Kernel)));
      Set_Position (Dialog, Win_Pos_Center_On_Parent);
      Set_Default_Size_From_History (Dialog, "remote", Kernel, -1, 400);

      Dialog.Kernel := Kernel;

      Gtk_New_Hpaned (Main_Table);
      Pack_Start (Get_Content_Area (Dialog), Main_Table);

      Gtk_New_Vbox (VBox, Homogeneous => False);
      Pack1 (Main_Table, VBox, Resize => False, Shrink => False);

      Gtk_New (Frame);
      Pack_Start (VBox, Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scrolled);

      Dialog.Machine_Tree := Create_Tree_View
        (Column_Types       => (Name_Col     => GType_String),
         Column_Names       => (1 => new String'("Servers")),
         Show_Column_Titles => True,
         Selection_Mode     => Selection_Single,
         Sortable_Columns   => True,
         Initial_Sort_On    => 1,
         Hide_Expander      => False);
      Set_Name (Dialog.Machine_Tree, "machine tree");
      Add (Scrolled, Dialog.Machine_Tree);

      --  Add/Restore/Remove buttons
      Gtk_New (Dialog.Add_Machine_Button, -"Add server");
      Set_Tooltip_Text
        (Dialog.Add_Machine_Button,
         -"Add a new server in the servers list");
      Pack_Start (VBox, Dialog.Add_Machine_Button, False, False);
      Gtk_New (Dialog.Restore_Button, -"Remove local changes");
      Set_Tooltip_Text
        (Dialog.Restore_Button,
         -("Reinitialize the selected server's parameters to their " &
             "default values"));
      Pack_Start (VBox, Dialog.Restore_Button, False, False);
      Gtk_New (Dialog.Remove_Button, -"Remove server");
      Set_Tooltip_Text
        (Dialog.Remove_Button,
         -"Remove the selected server from the servers list");
      Pack_Start (VBox, Dialog.Remove_Button, False, False);
      Set_Sensitive (Dialog.Restore_Button, False);
      Set_Sensitive (Dialog.Remove_Button, False);

      --  Machine configuration

      Gtk_New_Vbox (VBox, Homogeneous => False);
      Pack2 (Main_Table, VBox, Resize => True, Shrink => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Pack_Start (VBox, Scrolled);

      Gtk_New (Dialog.Right_Table, Rows => 7, Columns => 2,
               Homogeneous => False);
      Add_With_Viewport (Scrolled, Dialog.Right_Table);

      Line_Nb := 0;
      Create_Blue_Label (Dialog.Nickname_Label,
                         Dialog.Nickname_Event);
      Attach (Dialog.Right_Table, Dialog.Nickname_Event,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 5, 5);

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label, "<span foreground=""red"">*</span>" & (-" Network name:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Network_Name_Entry);
      Set_Name (Dialog.Network_Name_Entry, "network name entry");
      Attach (Dialog.Right_Table, Dialog.Network_Name_Entry,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Network_Name_Entry,
         -("The network name is the name used to connect to this server via " &
           "your network. It can be either an IP address, a host name of " &
           "your local network, or a fully qualified network name."));

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label,
         "<span foreground=""red"">*</span>" & (-" Remote access tool:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Access_Combo);
      Set_Name (Dialog.Remote_Access_Combo, "remote access combo");
      Attach (Dialog.Right_Table, Dialog.Remote_Access_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Remote_Access_Combo,
         -("The remote access tool is the tool used to connect to this " &
           "server."));

      for J of Access_Tools loop
         Dialog.Remote_Access_Combo.Append_Text (J.all);
         Free (J);
      end loop;

      if Access_Tools'Length = 0 then
         declare
            Button : Gtkada.Dialogs.Message_Dialog_Buttons;
            pragma Unreferenced (Button);
         begin
            Button := Gtkada.Dialogs.Message_Dialog
              (-("No suitable remote access tool could be found on your " &
                 "system." & ASCII.LF &
                 "A remote access tool is required to be able to use the " &
                 "GPS remote features. Please install one of the following " &
                 "tools (see the documentation for more details):" & ASCII.LF &
                   " - rlogin" & ASCII.LF &
                   " - ssh" & ASCII.LF &
                   " - telnet"),
               Dialog_Type   => Gtkada.Dialogs.Error,
               Buttons       => Gtkada.Dialogs.Button_OK,
               Justification => Gtk.Enums.Justify_Left,
               Parent        => Kernel.Get_Main_Window);
         end;
      end if;

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup (Label, "<span foreground=""red"">*</span>" & (-" Shell:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Shell_Combo);
      Set_Name (Dialog.Remote_Shell_Combo, "remote shell combo");
      Attach (Dialog.Right_Table, Dialog.Remote_Shell_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Remote_Shell_Combo,
         -"The shell tells GPS what shell runs on the remote server.");

      for J of Shells loop
         Dialog.Remote_Shell_Combo.Append_Text (J.all);
         Free (J);
      end loop;

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label, -"Sync tool:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Sync_Combo);
      Set_Name (Dialog.Remote_Sync_Combo, "remote sync combo");
      Attach (Dialog.Right_Table, Dialog.Remote_Sync_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Remote_Sync_Combo,
         -("The sync tool is used to synchronize remote and local " &
           "filesystems, if these are not shared filesystems."));

      for J of Sync_Tools loop
         Dialog.Remote_Sync_Combo.Append_Text (J.all);
         Free (J);
      end loop;

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label, -"Extra init commands:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label, 0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Init_Cmds_View);
      Set_Wrap_Mode (Dialog.Init_Cmds_View, Wrap_Char);
      Set_Left_Margin (Dialog.Init_Cmds_View, 10);
      Set_Indent (Dialog.Init_Cmds_View, -10);
      Set_Pixels_Below_Lines (Dialog.Init_Cmds_View, 3);
      Gtk_New (Event);
      Add (Event, Dialog.Init_Cmds_View);
      Attach (Dialog.Right_Table, Event, 1, 2,
              Line_Nb, Line_Nb + 1, Fill or Expand, 0);
      Set_Tooltip_Text
        (Event,
         -("The Extra Init Commands field represents initialization commands" &
           " sent to the server upon connection: when GPS connects to your " &
           "remote machine, the chosen shell is launched, and your default " &
           "initialization files are read (e.g. .bashrc file for the bash " &
           "shell). Then GPS sends these extra init commands, allowing you " &
           "for example to specify a compilation toolchain."));

      Line_Nb := Line_Nb + 1;
      Gtk_New (Dialog.Advanced_Pane, -"Advanced configuration");
      Attach (Dialog.Right_Table, Dialog.Advanced_Pane,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10, 10);

      Gtk_New (Dialog.Advanced_Table,
               Rows => 5, Columns => 2, Homogeneous => False);
      Dialog.Advanced_Pane.Add (Dialog.Advanced_Table);

      Gtk_New (Label, -"User name:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 0, 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.User_Name_Entry);
      Attach (Dialog.Advanced_Table, Dialog.User_Name_Entry, 1, 2, 0, 1,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.User_Name_Entry,
         -("The user name specifies the name used to connect to the server. " &
           "If unspecified, the remote access tool will most of the time " &
           "use your current login name. If not, and a user name is " &
           "requested, gps will prompt you for a user name when requested.)"));

      Gtk_New (Label, -"Timeout value (in s):");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 1, 2,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Timeout_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Timeout_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Timeout_Spin, 1, 2, 1, 2,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Timeout_Spin,
         -("The timeout value is used to determine if a connection to a " &
           "remote host is dead. All elementary operations performed on the " &
           "remote host (i.e. operations that are normally almost immediate " &
           "to perform) will use this timeout value. By default, this value " &
           "is set to 10s. If you have a very slow network connection or a " &
           "very overloaded server, set this timeout to a higher value."));

      Gtk_New (Label, -"Max number of connections:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 2, 3,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Max_Nb_Connected_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Max_Nb_Connected_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Max_Nb_Connected_Spin, 1, 2, 2, 3,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Max_Nb_Connected_Spin,
         -("The maximum number of connections determines the maximum number " &
           "of simultaneous connections GPS is allowed to perform to this " &
           "server. In fact, if you want to compile, debug and execute at " &
           "the same time on the machine, GPS will need more that one " &
           "connection to do this. The default value is 3."));

      Gtk_New (Label, -"CR/LF Handling:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 3, 4,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Cr_Lf_Combo);
      Set_Name (Dialog.Cr_Lf_Combo, "crlf handling combo");
      Attach (Dialog.Advanced_Table, Dialog.Cr_Lf_Combo,
              1, 2, 3, 4,
              Fill or Expand, 0);
      Set_Tooltip_Text
        (Dialog.Cr_Lf_Combo,
         -("Indicates what characters the remote host understands as line" &
           " ending: LF, CR/LF, or automatically determine it."));

      for J in Cr_Lf_Handling'Range loop
         Dialog.Cr_Lf_Combo.Append_Text
           (Ada.Characters.Handling.To_Lower (J'Img));
      end loop;

      Gtk_New (Label, -"Debug console:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 4, 5,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Debug_Button);
      Attach (Dialog.Advanced_Table, Dialog.Debug_Button, 1, 2, 4, 5, 0, 0);
      Set_Tooltip_Text
        (Dialog.Debug_Button,
         -("The Debug console allow you to easily debug a remote connection." &
           " If checked, it will open a console reporting all exchanges " &
           "between GPS and the selected server."));

      --  Remote paths configuration
      Line_Nb := Line_Nb + 1;
      Gtk_New
        (Dialog.Paths_List_Widget, Gtk_Dialog (Dialog));
      Attach (Dialog.Right_Table, Dialog.Paths_List_Widget,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10, 0);

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label,
         "<span style=""italic"">" &
         (-" Fields marked by an asterisk (") &
         ("<span foreground=""red"">*</span>") &
         (-") are mandatory") & "</span>");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_End (VBox, Label, False, False, Padding => 5);

      --  Callbacks connections

      Widget_Boolean_Callback.Object_Connect
        (Dialog.Network_Name_Entry, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.User_Name_Entry, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Remote_Access_Combo,
         Gtk.Combo_Box.Signal_Changed, On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Remote_Shell_Combo,
         Gtk.Combo_Box.Signal_Changed, On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Remote_Sync_Combo,
         Gtk.Combo_Box.Signal_Changed, On_Changed'Access, Dialog, False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Max_Nb_Connected_Spin, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Dialog, False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Timeout_Spin, Gtk.Editable.Signal_Changed,
         On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Get_Buffer (Dialog.Init_Cmds_View), Gtk.Text_Buffer.Signal_Changed,
         On_Changed'Access, Dialog, False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Cr_Lf_Combo,
         Gtk.Combo_Box.Signal_Changed, On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Debug_Button, Signal_Clicked,
         On_Changed'Access, Dialog, False);
      Widget_Callback.Object_Connect
        (Get_Selection (Dialog.Machine_Tree),
         Gtk.Tree_Selection.Signal_Changed, On_Selection_Changed'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Add_Machine_Button, Signal_Clicked,
         On_Add_Machine_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Restore_Button, Signal_Clicked,
         On_Restore_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Remove_Button, Signal_Clicked,
         On_Remove_Clicked'Access,
         Dialog);

      --  Fill the tree with already configured machines

      Model := -Get_Model (Dialog.Machine_Tree);
      Iter := Null_Iter;

      for J in Machines'Range loop
         Append (Model, Iter, Null_Iter);
         Model.Set (Iter, Name_Col, Machines (J).all);

         if J = 1 or else Machines (J).all = Default_Server then
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         end if;
      end loop;

      if Machines'Length = 0 then
         Set_Child_Visible (Dialog.Right_Table, False);
      end if;

      Tmp := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);
   end Gtk_New;

   -----------------
   -- Set_Machine --
   -----------------

   procedure Set_Machine
     (Dialog   : access Server_List_Editor_Record'Class;
      Nickname : String)
   is
      Machine : Remote.Db.Machine_Type;
   begin
      if not Get_Database.Is_Configured (Nickname) then
         Machine := New_Machine (Dialog.Kernel, Nickname);
      else
         Machine := Remote.Db.Machine_Type
           (Get_Database.Get_Machine (Nickname).all);
      end if;

      Set_Text (Dialog.Nickname_Label, Machine.Nickname);
      Dialog.Selected_Machine := new String'(Machine.Nickname);

      Set_Text
        (Dialog.Network_Name_Entry,
         Machine.Network_Name);
      Set_Text
        (Dialog.User_Name_Entry,
         Machine.User_Name);

      declare
         Model : Gtk_Tree_Model;
         Iter  : Gtk_Tree_Iter;
      begin
         Model := Dialog.Remote_Access_Combo.Get_Model;
         Iter  := Get_Iter_First (Model);

         while Iter /= Null_Iter loop
            if Get_String (Model, Iter, 0) = Machine.Access_Tool then
               Dialog.Remote_Access_Combo.Set_Active_Iter (Iter);
               exit;
            end if;
            Next (Model, Iter);
         end loop;

         Model := Dialog.Remote_Shell_Combo.Get_Model;
         Iter  := Get_Iter_First (Model);

         while Iter /= Null_Iter loop
            if Get_String (Model, Iter, 0) = Machine.Shell then
               Dialog.Remote_Shell_Combo.Set_Active_Iter (Iter);
               exit;
            end if;
            Next (Model, Iter);
         end loop;

         Model := Dialog.Remote_Sync_Combo.Get_Model;
         Iter  := Get_Iter_First (Model);

         while Iter /= Null_Iter loop
            if Get_String (Model, Iter, 0) = Machine.Sync_Tool then
               Dialog.Remote_Sync_Combo.Set_Active_Iter (Iter);
               exit;
            end if;
            Next (Model, Iter);
         end loop;

         Model := Dialog.Cr_Lf_Combo.Get_Model;
         Iter  := Get_Iter_First (Model);

         while Iter /= Null_Iter loop
            if Get_String (Model, Iter, 0) =
              Ada.Characters.Handling.To_Lower (Machine.Cr_Lf'Img)
            then
               Dialog.Cr_Lf_Combo.Set_Active_Iter (Iter);
               exit;
            end if;
            Next (Model, Iter);
         end loop;
      end;

      Set_Value
        (Dialog.Timeout_Spin,
         Gdouble (Machine.Timeout) / 1000.0);
      Set_Value
        (Dialog.Max_Nb_Connected_Spin,
         Gdouble (Machine.Max_Nb_Connections));
      declare
         Init_Cmds : constant GNAT.Strings.String_List :=
                       Machine.Extra_Init_Commands;
      begin
         Set_Text (Get_Buffer (Dialog.Init_Cmds_View), "");
         for J in Init_Cmds'Range loop
            Insert_At_Cursor
              (Get_Buffer (Dialog.Init_Cmds_View),
               Init_Cmds (J).all & ASCII.LF);
         end loop;
      end;
      Set_Active (Dialog.Debug_Button, Machine.Use_Dbg);

      --  Make sure the current states are correctly set.
      Dialog.Applied := True;
      Dialog.Modified := False;
   end Set_Machine;

   --------------
   -- For_Each --
   --------------

   function For_Each
     (Model     : Gtk_Tree_Model;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      User_Data : For_Each_Data_Access) return Boolean
   is
      pragma Unreferenced (Path);
   begin
      User_Data.The_Iter := Iter;
      User_Data.Found    :=
        Get_String (Model, Iter, Name_Col) = User_Data.Nickname;
      return User_Data.Found;
   end For_Each;

   -----------------
   -- Select_Back --
   -----------------

   procedure Select_Back
     (Dialog : access Server_List_Editor_Record'Class)
   is
      Model     : constant Gtk.Tree_Model.Gtk_Tree_Model :=
                    Get_Model (Dialog.Machine_Tree);

   begin
      if Dialog.Selected_Machine = null then
         return;
      end if;

      Trace (Me, "Select back previous machine "
             & Dialog.Selected_Machine.all);

      declare
         Data      : aliased For_Each_Data :=
                       (Nickname_Length => Dialog.Selected_Machine.all'Length,
                        Nickname        => Dialog.Selected_Machine.all,
                        The_Iter        => Gtk.Tree_Model.Null_Iter,
                        Found           => False);
      begin
         Foreach.Foreach (Model, For_Each'Access, Data'Unchecked_Access);
         if Data.Found then
            Dialog.Select_Back := True;
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Data.The_Iter);
         end if;
      end;
   end Select_Back;

   --------------------
   -- Remove_Machine --
   --------------------

   procedure Remove_Machine
     (Dialog : access Server_List_Editor_Record'Class;
      Nickname : String)
   is
      Model : constant Gtk_Tree_Model := Get_Model (Dialog.Machine_Tree);
      Iter  : Gtk_Tree_Iter;

   begin
      if Active (Me) then
         Trace (Me, "Removing " & Nickname);
      end if;

      if Get_Database.Is_Configured (Nickname) then
         Remote.Db.Remove (Get_Database, Nickname);
      end if;

      Remote_Module.Save_Remote_Config (Dialog.Kernel);

      --  set removed machine as unmodified to prevent save upon
      --  tree model content change (selection change)
      Dialog.Modified := False;

      declare
         Data      : aliased For_Each_Data :=
                       (Nickname_Length => Nickname'Length,
                        Nickname        => Nickname,
                        The_Iter        => Gtk.Tree_Model.Null_Iter,
                        Found           => False);
      begin
         Foreach.Foreach (Model, For_Each'Access, Data'Unchecked_Access);

         if Data.Found then
            Remove (Gtk_Tree_Store'(-Model), Data.The_Iter);
         end if;
      end;

      Iter := Get_Iter_First (Model);

      if Iter = Null_Iter then
         Set_Child_Visible (Dialog.Right_Table, False);
         Set_Sensitive (Dialog.Restore_Button, False);
         Set_Sensitive (Dialog.Remove_Button, False);
      end if;
   end Remove_Machine;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (W                 : access Gtk_Widget_Record'Class;
                         Connection_Params : Boolean)
   is
      Dialog    : Server_List_Editor_Record
                    renames Server_List_Editor_Record (W.all);
   begin
      Dialog.Modified := True;
      if Connection_Params then
         Dialog.Applied := False;
      end if;
   end On_Changed;

   ------------------
   -- Save_Current --
   ------------------

   function Save_Current
     (Dialog : Server_List_Editor;
      Force  : Boolean) return Boolean
   is
      Ret : Message_Dialog_Buttons;

      function Get_Command_List
        (View : Gtk_Text_View) return GNAT.Strings.String_List;
      --  Retrieve the commands from the gtk_text_view

      function Check_Fields
        (Dialog : Server_List_Editor) return Boolean;
      --  Check that the last selected machine has correctly been entered

      ----------------------
      -- Get_Command_List --
      ----------------------

      function Get_Command_List
        (View : Gtk_Text_View) return GNAT.Strings.String_List
      is
         I_Start : Gtk_Text_Iter;
         I_End   : Gtk_Text_Iter;
         Buffer  : constant Gtk_Text_Buffer := Get_Buffer (View);

      begin
         Get_Start_Iter (Buffer, I_Start);
         Get_End_Iter (Buffer, I_End);

         declare
            Str     : constant String := Get_Text (Buffer, I_Start, I_End);
            Idx     : Natural := Str'First;
            Idx_End : Natural := Str'Last;
            N_Lines : Natural;

         begin
            Skip_Blanks (Str, Idx);
            Skip_Blanks_Backward (Str, Idx_End);

            if Idx_End < Idx then
               N_Lines := 0;
            else
               N_Lines := Lines_Count (Str (Idx .. Idx_End));
            end if;

            declare
               Substr : constant String := Str (Idx .. Idx_End);
               List   : GNAT.Strings.String_List (1 .. N_Lines);
            begin
               for J in List'Range loop
                  Idx_End := Line_End (Substr, Idx);
                  List (J) := new String'(Substr (Idx .. Idx_End));
                  Idx := Next_Line (Substr, Idx);
               end loop;

               return List;
            end;
         end;
      end Get_Command_List;

      ------------------
      -- Check_Fields --
      ------------------

      function Check_Fields
        (Dialog : Server_List_Editor) return Boolean
      is
         Nickname         : constant String := Dialog.Selected_Machine.all;
         Has_Network_Name : Boolean;
         Has_Access_Name  : Boolean;
         Has_Shell_Name   : Boolean;
         Error_Str        : Ada.Strings.Unbounded.Unbounded_String;
         use type Ada.Strings.Unbounded.Unbounded_String;

      begin
         Has_Network_Name := Get_Text (Dialog.Network_Name_Entry) /= "";
         Has_Access_Name  :=
           Get_Active_Text (Dialog.Remote_Access_Combo) /= "";
         Has_Shell_Name   :=
           Get_Active_Text (Dialog.Remote_Shell_Combo) /= "";

         if not Has_Network_Name
           or else not Has_Access_Name
           or else not Has_Shell_Name
         then
            Error_Str := Ada.Strings.Unbounded.To_Unbounded_String
              (-"The following items are missing for server ") &
               Nickname & ":";

            if not Has_Network_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Network name");
            end if;

            if not Has_Access_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Remote access");
            end if;

            if not Has_Shell_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Shell");
            end if;

            Ret := Message_Dialog
              (Ada.Strings.Unbounded.To_String (Error_Str),
               Dialog_Type => Error,
               Buttons     => Button_OK,
               Parent      => Gtk_Window (Dialog));

            return False;
         end if;

         return True;
      end Check_Fields;

      Machine : Remote.Db.Machine_Access;

   begin
      if not Dialog.Modified then
         return True;
      end if;

      Trace (Me, "Save " & Dialog.Selected_Machine.all);

      if not Force then
         Ret := Message_Dialog
           (-"The server " & Dialog.Selected_Machine.all &
            (-" has been modified. Do you want to save it before proceeding to"
               & " the next action ?"),
            Dialog_Type => Confirmation,
            Buttons     => Button_OK or Button_Cancel,
            Parent      => Gtk_Window (Dialog));

         if Ret = Button_Cancel then
            if not Get_Database.Is_Configured
              (Dialog.Selected_Machine.all)
            then
               --  we don't want to save a non configured machine.
               --  Let's remove it ...
               Remove_Machine (Dialog, Dialog.Selected_Machine.all);
            end if;

            return True;
         end if;
      end if;

      if not Check_Fields (Dialog) then
         return False;
      end if;

      if Get_Database.Is_Configured (Dialog.Selected_Machine.all) then
         Trace (Me, "retrieving configured machine " &
                Dialog.Selected_Machine.all & " for save");
         Machine := Get_Database.Get_Machine (Dialog.Selected_Machine.all);
      else
         Trace (Me, "creating new machine " &
                Dialog.Selected_Machine.all & " for save");
         Machine := new Remote.Db.Machine_Type'
           (New_Machine (Dialog.Kernel, Dialog.Selected_Machine.all));
      end if;

      Machine.Set_Network_Name
        (Get_Text (Dialog.Network_Name_Entry));
      Machine.Set_Access_Tool
        (Get_Active_Text (Dialog.Remote_Access_Combo));
      Machine.Set_Shell
        (Get_Active_Text (Dialog.Remote_Shell_Combo));
      Machine.Set_Sync_Tool
        (Get_Active_Text (Dialog.Remote_Sync_Combo));
      Machine.Set_Extra_Init_Commands
        (Get_Command_List (Dialog.Init_Cmds_View));
      Machine.Set_User_Name
        (Get_Text (Dialog.User_Name_Entry));
      Machine.Set_Max_Nb_Connections
        (Integer (Get_Value_As_Int (Dialog.Max_Nb_Connected_Spin)));
      Machine.Set_Timeout
        (Integer (Get_Value_As_Int (Dialog.Timeout_Spin)) * 1000);
      Machine.Set_Cr_Lf
        (Cr_Lf_Handling'Value
           (Get_Active_Text (Dialog.Cr_Lf_Combo)));
      Machine.Set_Use_Dbg
        (Get_Active (Dialog.Debug_Button));

      Trace (Me, "Put machine into database.");
      Remote.Db.Add_Or_Replace (Get_Database, Machine);

      --  Now save the paths

      Trace (Me, "Internal save paths");

      begin
         Set_Mount_Points
           (Get_Database,
            Dialog.Selected_Machine.all,
            Get_Mount_Points
              (Dialog.Paths_List_Widget, Dialog.Selected_Machine.all));
      exception
         when Invalid_Path =>
            Trace (Me, "Invalid path detected, selecting back " &
                   Dialog.Selected_Machine.all);

            return False;
      end;

      Dialog.Modified := False;
      Dialog.Applied := True;
      Remote_Module.Save_Remote_Config (Dialog.Kernel);

      return True;
   end Save_Current;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (W : access Gtk_Widget_Record'Class) is
      Dialog    : constant Server_List_Editor :=
        Server_List_Editor (W);
      M         : Gtk_Tree_Model;
      Model     : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Trace (Me, "on selection changed");

      if Dialog.Select_Back then
         --  Do not change dialog values
         Trace (Me, "select change: selecting back");
         Dialog.Select_Back := False;
         return;
      end if;

      --  First try to save the current machine

      if not Save_Current (Server_List_Editor (W), False) then
         --  Select back the old config
         Select_Back (Dialog);
         return;
      end if;

      --  Now reinit the dialog values

      Get_Selected (Get_Selection (Dialog.Machine_Tree), M, Iter);
      Model := -M;

      if Iter /= Null_Iter then
         declare
            Nickname : constant String := Get_String (Model, Iter, Name_Col);
         begin
            if Active (Me) then
               Trace (Me, "Setting dialog values for new selection " &
                      Nickname);
            end if;

            Set_Machine (Dialog, Nickname);

            --  If user defined, look for a system defined descriptor with
            --  same nickname. If found, propose to restore user defined
            --  value with default value.

            if Get_Database.Has_Sys_Default (Nickname)
              and then not Get_Database.Is_Sys_Default (Nickname)
            then
               Set_Sensitive (Dialog.Restore_Button, True);
            else
               Set_Sensitive (Dialog.Restore_Button, False);
            end if;

            Set_Sensitive
              (Dialog.Remove_Button,
               not Get_Database.Has_Sys_Default (Nickname));

            --  Now fill the path list

            Set_Path_List
              (Dialog.Paths_List_Widget,
               Get_Database.Get_Mount_Points (Nickname));

            if Dialog.New_Machine then
               --  We immediately mark newly created machines as unapplied and
               --  modified
               Dialog.Applied := False;
               Dialog.Modified := True;
               Dialog.New_Machine := False;
            end if;

         end;
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Selection_Changed;

   ----------------------------
   -- On_Add_Machine_Clicked --
   ----------------------------

   procedure On_Add_Machine_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog   : Server_List_Editor_Record
                   renames Server_List_Editor_Record (W.all);
      Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter := Null_Iter;
      Ret      : Message_Dialog_Buttons;
      pragma Unreferenced (Ret);

   begin
      --  First save the machines

      if Save_Current (Server_List_Editor (W), False) then
         declare
            Nickname : constant String := Query_User
              (Parent => Gtk_Window (W),
               Prompt => -"Please enter the new machine's nickname",
               Password_Mode => False);

         begin
            if Nickname = "" then
               return;
            end if;

            if Get_Database.Is_Configured (Nickname) then
               Ret := Message_Dialog
                 (-("A server with that name already exists. Please chosse" &
                    " another name."),
                  Dialog_Type => Error,
                  Buttons     => Button_OK,
                  Parent      => Gtk_Window (W));
               return;
            end if;

            Set_Child_Visible (Dialog.Right_Table, True);

            Model := -Get_Model (Dialog.Machine_Tree);
            Append (Model, Iter, Null_Iter);
            Model.Set (Iter, Name_Col, Nickname);

            --  Set the 'New_Machine' state to handle save.
            Dialog.New_Machine := True;

            --  Select this newly created machine
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Add_Machine_Clicked;

   ------------------------
   -- On_Restore_Clicked --
   ------------------------

   procedure On_Restore_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog  : constant Server_List_Editor := Server_List_Editor (W);
      Model   : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Machine : Remote.Db.Machine_Access;

   begin
      Set_Sensitive (Dialog.Restore_Button, False);
      Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Current_Selection : constant String :=
                                  Get_String (Model, Iter, Name_Col);
         begin
            if Active (Me) then
               Trace (Me, "Restoring " & Current_Selection);
            end if;

            Machine := Get_Database.Get_Sys_Default (Current_Selection);
            Get_Database.Add_Or_Replace (Machine);
            Set_Machine (Dialog, Current_Selection);

            Dialog.Applied := True;
            Dialog.Modified := False;
         end;
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Restore_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog    : constant Server_List_Editor := Server_List_Editor (W);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Ret       : Message_Dialog_Buttons;

   begin
      Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Current_Selection : constant String :=
                                  Get_String (Model, Iter, Name_Col);
         begin
            Ret := Message_Dialog
              ((-"Are you sure you want to remove server ") &
               Current_Selection & " ?",
               Dialog_Type => Confirmation,
               Buttons     => Button_OK or Button_Cancel,
               Title       => "Server removal confirmation",
               Parent      => Gtk_Window (W.Get_Toplevel));

            if Ret = Button_Cancel then
               return;
            end if;

            Remove_Machine (Dialog, Current_Selection);

            Iter := Get_Iter_First (Model);

            if Iter /= Null_Iter then
               Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
            else
               Set_Child_Visible (Dialog.Right_Table, False);
               Set_Sensitive (Dialog.Restore_Button, False);
               Set_Sensitive (Dialog.Remove_Button, False);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Remove_Clicked;

   ---------------------------
   -- Configure_Server_List --
   ---------------------------

   procedure Configure_Server_List
     (Kernel         : GPS.Kernel.Kernel_Handle;
      Default_Server : String := "")
   is
      Dialog  : Server_List_Editor;
      Resp    : Gtk_Response_Type;
      Dead    : Boolean;
      pragma Unreferenced (Dead);

   begin
      Gtk_New (Dialog, Kernel, Default_Server);

      loop
         Resp := Run (Dialog);

         --  Apply changes ?

         if Resp = Gtk_Response_OK or else Resp = Gtk_Response_Apply then
            Dead := Save_Current (Dialog, True);
         end if;

         exit when Resp /= Gtk_Response_Apply;
      end loop;

      --  Destroy the widget
      Destroy (Dialog);
   end Configure_Server_List;

end Remote.Config_Dialog;
