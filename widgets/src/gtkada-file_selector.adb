-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;            use Glib;
with Gtk;             use Gtk;
with Gdk;             use Gdk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Arguments;   use Gtk.Arguments;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Stock;       use Gtk.Stock;
with Gtk.Main;
with Gtk.Ctree;       use Gtk.Ctree;
with Gtk.Paned;       use Gtk.Paned;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Toolbar;     use Gtk.Toolbar;
with Gdk.Event;       use Gdk.Event;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.Bitmap;      use Gdk.Bitmap;
with Gdk.Color;       use Gdk.Color;
with GUI_Utils;       use GUI_Utils;

with Gtkada.Types;    use Gtkada.Types;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtkada.Intl;     use Gtkada.Intl;

with GUI_Utils; use GUI_Utils;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Gtkada.File_Selector is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Change_Directory
     (Win : File_Selector_Window_Access;
      Dir : String);
   --  Called every time that the contents of a new directory should be
   --  displayed in the File_Explorer. Dir is the absolute pathname to
   --  that directory, starting with a Directory_Separator.

   procedure Refresh_Files
     (Win : File_Selector_Window_Access);

   function Read_File (Win : in File_Selector_Window_Access) return Boolean;
   --  Read one file from the current directory and insert it in Files.

   function Display_File (Win : in File_Selector_Window_Access) return Boolean;

   --  This function gets one entry from Win.Remaining_Files, applies
   --  a filter to it, and displays the corresponding information in the
   --  file list.

   ---------------
   -- Callbacks --
   ---------------

   procedure Realize
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure Directory_Selected
     (Object : access Gtk_Widget_Record'Class);

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Explorer_Tree_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   function On_File_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Selection_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Dialog : File_Selector_Window_Access)
     return String
   is
   begin
      if Dialog = null
        or else Dialog.Selection_Entry = null
        or else Get_Text (Dialog.Selection_Entry) = ""
      then
         return ("");
      else
         return (Dialog.Current_Directory.all
                 & Get_Text (Dialog.Selection_Entry));
      end if;
   end Get_Selection;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button
     (File_Selection : access File_Selector_Window_Record)
     return Gtk.Button.Gtk_Button
   is
   begin
      return File_Selection.Ok_Button;
   end Get_Ok_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (File_Selection : access File_Selector_Window_Record)
     return Gtk.Button.Gtk_Button
   is
   begin
      return File_Selection.Cancel_Button;
   end Get_Cancel_Button;

   -----------------
   -- Select_File --
   -----------------

   function Select_File (Base_Directory : String := "") return String is
      Filter_A  : Filter_Show_All_Access := new Filter_Show_All;
      File_Selector_Window : File_Selector_Window_Access;
   begin
      Gtk.Main.Init;
      Gtk_New (File_Selector_Window, Base_Directory);

      Register_Filter (File_Selector_Window, Filter_A);

      Show_All (File_Selector_Window);

      Widget_Callback.Connect
        (File_Selector_Window.Ok_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
      Widget_Callback.Connect
        (File_Selector_Window.Cancel_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));

      Gtk.Main.Main;

      return Get_Selection (File_Selector_Window);
   end Select_File;

   -------------
   -- Realize --
   -------------

   procedure Realize
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : File_Selector_Window_Access
        := File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Refresh_Files (Win);
   end Realize;

   ------------------
   -- Display_File --
   ------------------

   function Display_File
     (Win : in File_Selector_Window_Access)
     return Boolean
   is
      Text         : String_Access;
      State        : File_State;
      Pixmap       : Gdk.Pixmap.Gdk_Pixmap;
      Mask         : Gdk.Bitmap.Gdk_Bitmap;
      Current_Row  : Gint;
      Style        : Gtk_Style;
   begin
      if Is_Empty (Win.Remaining_Files) then
         return False;
      end if;

      Use_File_Filter
        (Win.Current_Filter,
         Win,
         Win.Current_Directory.all,
         Head (Win.Remaining_Files),
         State,
         Pixmap,
         Mask,
         Text);

      case State is
         when Invisible =>
            Style := null;
         when Normal =>
            Current_Row := Append (Win.File_List,
                                   "" + "" + "");
            Style := Win.Normal_Style;
         when Highlighted =>
            Current_Row := Append (Win.File_List,
                                   "" + "" + "");
            Style := Win.Highlighted_Style;
         when Insensitive =>
            Current_Row := Append (Win.File_List,
                                   "" + "" + "");
            Style := Win.Insensitive_Style;
            Set_Selectable (Win.File_List,
                            Current_Row,
                            False);
      end case;

      if Style /= null then
         Set_Row_Style (Win.File_List,
                        Current_Row,
                        Style);

         Set_Text (Win.File_List,
                   Current_Row, 1,
                   Head (Win.Remaining_Files));

         if Text /= null then
            Set_Text (Win.File_List,
                      Current_Row, 2,
                      Text.all);
         end if;

         if Pixmap /= Null_Pixmap then
            Set_Pixmap (Win.File_List,
                        Current_Row, 0,
                        Pixmap, Mask);
         end if;
      end if;

      Free (Text);

      Win.Remaining_Files := Tail (Win.Remaining_Files);

      if Is_Empty (Win.Remaining_Files) then
         return False;
      else
         return True;
      end if;
   end Display_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Win : in File_Selector_Window_Access)
     return Boolean
   is
      Buffer       : String (1 .. 256);
      Last         : Natural;

   begin
      if not Win.Current_Directory_Is_Open then
         return False;
      end if;

      Read (Win.Current_Directory_Id.all, Buffer, Last);
      if Last = 0 then
         Close (Win.Current_Directory_Id.all);
         Win.Current_Directory_Is_Open := False;

         Clear (Win.File_List);

         --  Register the function that will fill the list in the background.
         declare
            Id : Idle_Handler_Id;
            function Comparison (Arg1, Arg2 : String) return Boolean;
            --  Simple string comparison used for sorting the files list.

            function Comparison (Arg1, Arg2 : String) return Boolean
            is
            begin
               if Arg1 < Arg2 then
                  return True;
               else
                  return False;
               end if;
            end Comparison;
         begin
            Id := Add (Display_File'Access, Win);
            Sort (Win.Files, Comparison'Unrestricted_Access);
         end;

         Win.Remaining_Files := Win.Files;

         return False;
      end if;

      if Is_Directory
        (Win.Current_Directory.all & Buffer (1 .. Last))
      then
         null;
         --  ??? or should we display directories in the File_List ?
      else
         Prepend (Win.Files, Buffer (1 .. Last));
      end if;

      return True;
   end Read_File;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Win    : File_Selector_Window_Access;
      Filter : access File_Filter_Record'Class)
   is
   begin
      Append (Win.Filters, File_Filter (Filter));
      Add_Unique_Combo_Entry (Win.Filter_Combo, Filter.Label.all);
   end Register_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_All;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access) is
   begin
      State := Normal;
      Mask := Gdk.Bitmap.Null_Bitmap;
      Pixmap := Gdk.Pixmap.Null_Pixmap;
      Text := new String'("");
   end Use_File_Filter;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files
     (Win : File_Selector_Window_Access)
   is
      Dir        : String := Win.Current_Directory.all;
      Filter     : File_Filter := null;
      Id         : Idle_Handler_Id;
   begin
      if Get_Window (Win) = null then
         return;
      end if;

      Set_Busy_Cursor (Get_Window (Win), True, True);
      Clear (Win.File_List);
      Free (Win.Files);

      --  Find out which filter to use.

      declare
         S : String := Get_Text (Win.Filter_Combo_Entry);
         C : Filter_List.List := Win.Filters;
      begin
         while not Is_Empty (C) loop
            if Head (C).Label.all = S then
               Filter := Head (C);
               exit;
            else
               C := Tail (C);
            end if;
         end loop;
      end;

      if Filter = null then
         Set_Busy_Cursor (Get_Window (Win), False, False);
         return;
      end if;

      Insert (Win.File_List, -1, "" + ("Opening ... ") + "");
      Win.Current_Filter := Filter;

      --  Fill the File_List.
      begin
         if Win.Current_Directory_Is_Open then
            Close (Win.Current_Directory_Id.all);
            Win.Current_Directory_Is_Open := False;
         end if;

         GNAT.Directory_Operations.Open (Win.Current_Directory_Id.all, Dir);
         Win.Current_Directory_Is_Open := True;
         Id := Add (Read_File'Access, Win);
      exception
         when Directory_Error =>
            Clear (Win.File_List);
            Insert (Win.File_List, -1, "" + ("Could not open " & Dir) + "");
      end;

      Set_Busy_Cursor (Get_Window (Win), False, False);
   end Refresh_Files;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Win : File_Selector_Window_Access;
      Dir : String)
   is
   begin
      --  If the new directory is not the one currently shown in the File_List,
      --  then update the File_List.
      Set_Text (Win.Selection_Entry, "");

      if Dir /= ""
        and then Dir (Dir'First) = Directory_Separator
        and then Win.Current_Directory.all /= Dir
      then
         Free (Win.Current_Directory);
         Win.Current_Directory := new String' (Dir);

         --  If we are currently moving through the history,
         --  do not append items to the Location_Combo.

         if Win.Moving_Through_History then
            Win.Moving_Through_History := False;
         else
            Push (Win.Past_History,
                  new String'(Get_Text (Win.Location_Combo_Entry)));
            Clear (Win.Future_History);
            Set_Sensitive (Win.Back_Button);
            Set_Sensitive (Win.Forward_Button, False);

            Add_Unique_Combo_Entry
              (Win.Location_Combo, Dir);
         end if;

         if Get_Text (Win.Location_Combo_Entry) /= Dir then
            Set_Text (Win.Location_Combo_Entry, Dir);
         end if;

         --  If the new directory is not the one currently shown
         --  in the Explorer_Tree, then update the Explorer_Tree.

         if Dir /= Get_Selection (Win.Explorer_Tree) then
            Show_Directory (Win.Explorer_Tree, Dir);
         end if;

         Refresh_Files (Win);
      end if;
   end Change_Directory;

   ----------------------------
   -- On_Back_Button_Clicked --
   ----------------------------

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      S   : String_Access;
   begin
      Pop (Win.Past_History, S);

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, False);
      end if;

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button);
      end if;

      Push (Win.Future_History,
            new String'(Get_Text (Win.Location_Combo_Entry)));

      Set_Text (Win.Location_Combo_Entry, S.all);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S.all);
      Free (S);
   end On_Back_Button_Clicked;

   ----------------------------
   -- On_Home_Button_Clicked --
   ----------------------------

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Win.Home_Directory.all);
   end On_Home_Button_Clicked;

   --------------------------
   -- On_Up_Button_Clicked --
   --------------------------

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Show_Parent (Win.Explorer_Tree);
      Change_Directory (Win, Get_Selection (Win.Explorer_Tree));
   end On_Up_Button_Clicked;

   -------------------------------
   -- On_Refresh_Button_Clicked --
   -------------------------------

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Refresh_Files (Win);
   end On_Refresh_Button_Clicked;

   -------------------------------
   -- On_Forward_Button_Clicked --
   -------------------------------

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      S   : String_Access;

   begin
      Pop (Win.Future_History, S);

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button, False);
      end if;

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, True);
      end if;

      Push (Win.Past_History,
            new String'(Get_Text (Win.Location_Combo_Entry)));

      Set_Text (Win.Location_Combo_Entry, S.all);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S.all);

      Free (S);

   exception
      when Stack_Empty =>
         null;

   end On_Forward_Button_Clicked;

   ------------------------
   -- Directory_Selected --
   ------------------------

   procedure Directory_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Change_Directory (Win, Get_Text (Win.Location_Combo_Entry));
   end Directory_Selected;

   ------------------------
   -- Filter_Selected --
   ------------------------

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Refresh_Files (Win);
   end Filter_Selected;

   --------------------------------------
   -- On_Location_Combo_Entry_Activate --
   --------------------------------------

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Get_Text (Win.Location_Combo_Entry));
   end On_Location_Combo_Entry_Activate;

   ---------------------------------
   -- On_Explorer_Tree_Select_Row --
   ---------------------------------

   procedure On_Explorer_Tree_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Get_Selection (Dir_Tree (Object)));
   end On_Explorer_Tree_Select_Row;

   --------------------------------
   -- On_File_List_End_Selection --
   --------------------------------

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class)
   is
      Win      : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      Row_List : constant Gtk.Enums.Gint_List.Glist :=
        Get_Selection (Win.File_List);

   begin
      if Gtk.Enums.Gint_List.Length (Row_List) /= 0 then
         Set_Text (Win.Selection_Entry,
                   Get_Text (Win.File_List,
                             Gtk.Enums.Gint_List.Get_Data (Row_List),
                             1));
      end if;
   end On_File_List_End_Selection;

   --------------------------------
   -- On_Selection_Entry_Changed --
   --------------------------------

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Selection_Entry_Changed;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Main_Quit;
   end On_Ok_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win      : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      if Win /= null
        and then Win.Selection_Entry /= null
      then
         Set_Text (Win.Selection_Entry, "");
      end if;

      Main_Quit;
   end On_Cancel_Button_Clicked;

   ----------------------------------
   -- On_File_List_Key_Press_Event --
   ----------------------------------

   function On_File_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Win      : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

      Event : Gdk_Event := To_Event (Params, 1);
      S     : String := Get_String (Event);
   begin
      if S'Length /= 0
        and then (Is_Alphanumeric (S (S'First))
                  or else Is_Special (S (S'First)))
      then
         declare
            Found : Boolean := False;
         begin
            for J in 0 .. Get_Rows (Win.File_List) - 1 loop
               exit when Found;
               declare
                  T : String := Get_Text (Win.File_List, J, 1);
               begin
                  if T'Length /= 0
                    and then T (T'First) = S (S'First)
                  then
                     Found := True;
                     Select_Row (Win.File_List, J, 1);
                     Moveto (Win.File_List, J, 1, 0.1, 0.1);
                  end if;
               end;
            end loop;
         end;
         return True;
      else
         return False;
      end if;
   end On_File_List_Key_Press_Event;

   ----------------------------------------
   -- On_Selection_Entry_Key_Press_Event --
   ----------------------------------------

   function On_Selection_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Win      : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

      Found    : Boolean := False;
      Event    : Gdk_Event := To_Event (Params, 1);
      G : String := Get_String (Event);
   begin
      if G'Length /= 0
        and then G (G'First) = ASCII.HT
      then
         --  Handle "Tab completion".
         --  The current implementation will fail if there are file names
         --  longer than 512 characters.

         --  Find out what is the biggest common prefix matching the
         --  text in the selection entry.

         declare
            S : String := Get_Text (Win.Selection_Entry);

            First_Match : Gint := -1;
            --  The first column that completely matches S.

            Suffix_Length : Integer := -1;
            --  The length of the biggest common matching prefix.

            Best_Match : String (1 .. 512);
         begin
            for J in 0 .. Get_Rows (Win.File_List) - 1 loop
               declare
                  T : String := Get_Text (Win.File_List, J, 1);
                  K : Natural := 0;
               begin
                  while K < T'Length
                    and then K < S'Length
                    and then T (T'First + K) = S (S'First + K) loop
                     K := K + 1;
                  end loop;

                  --  Does the prefix match S ?
                  if K = S'Length then
                     if Suffix_Length = -1 then
                        First_Match := J;
                        Best_Match (1 .. T'Length) := T;
                        Suffix_Length := T'Length;
                     else
                        --  If there is already a biggest match, try to
                        --  get it.
                        while K < Suffix_Length
                          and then K < T'Length
                          and then T (T'First + K) = Best_Match (K + 1)
                        loop
                           K := K + 1;
                        end loop;

                        Suffix_Length := K;
                     end if;
                  end if;
               end;
            end loop;

            Select_Row (Win.File_List, First_Match, 1);
            Moveto (Win.File_List, First_Match, 1, 0.0, 0.0);
            Set_Text (Win.Selection_Entry, Best_Match (1 .. Suffix_Length));
            Set_Position (Win.Selection_Entry, Gint (Suffix_Length));
         end;

         return True;
      end if;

      for J in 0 .. Get_Rows (Win.File_List) - 1 loop
         exit when Found;
         declare
            T : String := Get_Text (Win.File_List, J, 1);
            S : String := Get_Text (Win.Selection_Entry) & G;
         begin
            if T'Length >= S'Length
              and then T (T'First .. T'First + S'Length - 1)
              = S (S'First .. S'First + S'Length - 1)
            then
               Found := True;
               Moveto (Win.File_List, J, 1, 0.0, 0.0);
            end if;
         end;
      end loop;

      return False;
   end On_Selection_Entry_Key_Press_Event;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Directory            : in String)
   is
   begin
      File_Selector_Window := new File_Selector_Window_Record;

      if Is_Absolute_Path (Directory)
        and then Directory (Directory'Last) = Directory_Separator
      then
         Initialize (File_Selector_Window, Directory);
      else
         Initialize (File_Selector_Window, Get_Current_Dir);
      end if;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Selector_Window : access File_Selector_Window_Record'Class;
      Directory            : in String)
   is
      Toolbar1    : Gtk_Toolbar;

      Label1      : Gtk_Label;

      Hpaned1     : Gtk_Hpaned;

      Hbox1       : Gtk_Hbox;
      Hbox2       : Gtk_Hbox;
      Hbox3       : Gtk_Hbox;
      Hbox4       : Gtk_Hbox;
      Hbox5       : Gtk_Hbox;
      Hbox6       : Gtk_Hbox;
      Hbox7       : Gtk_Hbox;

      Hbuttonbox1 : Gtk_Hbutton_Box;

      Style       : Gtk_Style := Get_Default_Style;
   begin
      File_Selector_Window.Highlighted_Style := Copy (Style);
      File_Selector_Window.Insensitive_Style := Copy (Style);
      File_Selector_Window.Normal_Style      := Copy (Style);

      Set_Foreground (File_Selector_Window.Insensitive_Style,
                      State_Normal,
                      Gdk_Color'(Get_Foreground
                                 (Style, State_Insensitive)));

      File_Selector_Window.Home_Directory := new String' (Directory);

      Gtk_New
        (File_Selector_Window.Explorer_Tree,
         File_Selector_Window.Home_Directory.all);

      Set_Indent (File_Selector_Window.Explorer_Tree, 10);
      Set_Row_Height (File_Selector_Window.Explorer_Tree, 15);

      Gtk.Window.Initialize (File_Selector_Window, Window_Toplevel);
      Set_Default_Size (File_Selector_Window, 600, 500);

      Set_Policy (File_Selector_Window, False, True, False);
      Set_Position (File_Selector_Window, Win_Pos_Center);
      Set_Modal (File_Selector_Window, False);

      Gtk_New_Vbox (File_Selector_Window.File_Selector_Vbox, False, 0);
      Add (File_Selector_Window, File_Selector_Window.File_Selector_Vbox);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (File_Selector_Window.File_Selector_Vbox,
                  Hbox1, False, False, 3);

      Gtk_New_Hbox (Hbox3, False, 0);
      Pack_Start (Hbox1, Hbox3, True, True, 0);

      Gtk_New
        (Toolbar1,
         Orientation_Horizontal, Toolbar_Both);

      Set_Icon_Size (Toolbar1, Icon_Size_Button);
      Set_Style (Toolbar1,  Toolbar_Icons);
      File_Selector_Window.Back_Button := Insert_Stock
        (Toolbar => Toolbar1,
         Stock_Id => Stock_Go_Back,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Back_Button, False);

      Widget_Callback.Connect
        (File_Selector_Window.Back_Button, "clicked",
         On_Back_Button_Clicked'Access);

      Gtk_New (File_Selector_Window.Up_Icon, Stock_Go_Up, Icon_Size_Button);
      File_Selector_Window.Up_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Icon => Gtk_Widget (File_Selector_Window.Up_Icon));
      Widget_Callback.Connect
        (File_Selector_Window.Up_Button, "clicked",
         On_Up_Button_Clicked'Access);

      File_Selector_Window.Forward_Button := Insert_Stock
        (Toolbar => Toolbar1,
         Stock_Id => Stock_Go_Forward,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Forward_Button, False);

      Widget_Callback.Connect
        (File_Selector_Window.Forward_Button, "clicked",
         On_Forward_Button_Clicked'Access);

      Gtk_New
        (File_Selector_Window.Refresh_Icon, Stock_Refresh, Icon_Size_Button);
      File_Selector_Window.Refresh_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Icon => Gtk_Widget (File_Selector_Window.Refresh_Icon));
      Widget_Callback.Connect
        (File_Selector_Window.Refresh_Button, "clicked",
         On_Refresh_Button_Clicked'Access);

      File_Selector_Window.Home_Button := Insert_Stock
        (Toolbar => Toolbar1,
         Stock_Id => Stock_Home,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Home_Button, True);
      Widget_Callback.Connect
        (File_Selector_Window.Home_Button, "clicked",
         On_Home_Button_Clicked'Access);

      Pack_Start (Hbox3, Toolbar1, True, True, 3);

      Gtk_New_Hbox (Hbox2, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         Hbox2, False, False, 3);

      Gtk_New (Label1, -("Exploring :"));
      Pack_Start (Hbox2, Label1, False, False, 3);

      Gtk_New (File_Selector_Window.Location_Combo);
      Set_Case_Sensitive (File_Selector_Window.Location_Combo, True);
      Pack_Start (Hbox2,
                  File_Selector_Window.Location_Combo, True, True, 3);
      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Location_Combo),
         "hide",
         Widget_Callback.To_Marshaller (Directory_Selected'Access),
         File_Selector_Window.Location_Combo);

      File_Selector_Window.Location_Combo_Entry :=
        Get_Entry (File_Selector_Window.Location_Combo);
      Set_Editable (File_Selector_Window.Location_Combo_Entry, False);
      Set_Max_Length (File_Selector_Window.Location_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Location_Combo_Entry, True);
      Widget_Callback.Connect
        (File_Selector_Window.Location_Combo_Entry, "activate",
         Widget_Callback.To_Marshaller
         (On_Location_Combo_Entry_Activate'Access));

      Gtk_New_Hpaned (Hpaned1);
      Set_Position (Hpaned1, 200);
      Set_Handle_Size (Hpaned1, 10);
      Set_Gutter_Size (Hpaned1, 6);

      Gtk_New_Hbox (Hbox7, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         Hbox7, True, True, 3);

      Pack_Start (Hbox7, Hpaned1, True, True, 3);

      Gtk_New (File_Selector_Window.Explorer_Tree_Scrolledwindow);
      Set_Policy
        (File_Selector_Window.Explorer_Tree_Scrolledwindow,
         Policy_Automatic, Policy_Always);

      Add (Hpaned1, File_Selector_Window.Explorer_Tree_Scrolledwindow);

      Set_Column_Width (File_Selector_Window.Explorer_Tree, 0, 80);
      Set_Column_Width (File_Selector_Window.Explorer_Tree, 1, 80);
      Set_Column_Width (File_Selector_Window.Explorer_Tree, 2, 80);
      Widget_Callback.Connect
        (File_Selector_Window.Explorer_Tree, "tree_select_row",
         On_Explorer_Tree_Select_Row'Access);
      Add (File_Selector_Window.Explorer_Tree_Scrolledwindow,
           File_Selector_Window.Explorer_Tree);

      Gtk_New (File_Selector_Window.Files_Scrolledwindow);
      Set_Policy
        (File_Selector_Window.Files_Scrolledwindow,
         Policy_Automatic, Policy_Always);
      Add (Hpaned1, File_Selector_Window.Files_Scrolledwindow);

      Gtk_New (File_Selector_Window.File_List, 3);
      Set_Selection_Mode (File_Selector_Window.File_List, Selection_Single);
      Set_Shadow_Type (File_Selector_Window.File_List, Shadow_In);
      Set_Show_Titles (File_Selector_Window.File_List, True);
      Set_Column_Width (File_Selector_Window.File_List, 0, 20);
      Set_Column_Width (File_Selector_Window.File_List, 1, 180);
      Set_Column_Width (File_Selector_Window.File_List, 2, 80);
      Set_Row_Height (File_Selector_Window.File_List, 15);

      Return_Callback.Connect
        (File_Selector_Window.File_List, "key_press_event",
         On_File_List_Key_Press_Event'Access);

      Widget_Callback.Connect
        (File_Selector_Window.File_List, "select_row",
         Widget_Callback.To_Marshaller (On_File_List_End_Selection'Access));
      Add (File_Selector_Window.Files_Scrolledwindow,
           File_Selector_Window.File_List);

      Gtk_New (File_Selector_Window.File_Icon_Label, -(""));
      Set_Column_Widget
        (File_Selector_Window.File_List, 0,
         File_Selector_Window.File_Icon_Label);

      Gtk_New (File_Selector_Window.File_Name_Label, -("Name"));
      Set_Justify (File_Selector_Window.File_Name_Label, Justify_Left);
      Set_Column_Widget
        (File_Selector_Window.File_List, 1,
         File_Selector_Window.File_Name_Label);

      Gtk_New (File_Selector_Window.File_Text_Label, -("Info"));
      Set_Justify (File_Selector_Window.File_Text_Label, Justify_Left);
      Set_Column_Widget
        (File_Selector_Window.File_List, 2,
         File_Selector_Window.File_Text_Label);

      Gtk_New_Hbox (Hbox4, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         Hbox4, False, False, 3);

      Gtk_New (File_Selector_Window.Filter_Combo);
      Set_Case_Sensitive (File_Selector_Window.Filter_Combo, False);

      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Filter_Combo),
         "hide",
         Widget_Callback.To_Marshaller (Filter_Selected'Access),
         File_Selector_Window.Filter_Combo);

      Pack_Start (Hbox4,
                  File_Selector_Window.Filter_Combo, True, True, 3);

      File_Selector_Window.Filter_Combo_Entry :=
        Get_Entry (File_Selector_Window.Filter_Combo);
      Set_Editable (File_Selector_Window.Filter_Combo_Entry, False);
      Set_Max_Length (File_Selector_Window.Filter_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Filter_Combo_Entry, True);

      Gtk_New_Hbox (Hbox5, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         Hbox5, False, False, 3);

      Gtk_New (File_Selector_Window.Selection_Entry);
      Set_Editable (File_Selector_Window.Selection_Entry, True);
      Set_Max_Length (File_Selector_Window.Selection_Entry, 0);
      Set_Visibility (File_Selector_Window.Selection_Entry, True);
      Pack_Start
        (Hbox5, File_Selector_Window.Selection_Entry, True, True, 3);

      Return_Callback.Connect
        (File_Selector_Window.Selection_Entry,
         "key_press_event", On_Selection_Entry_Key_Press_Event'Access);

      Widget_Callback.Connect
        (File_Selector_Window.Selection_Entry, "changed",
         Widget_Callback.To_Marshaller (On_Selection_Entry_Changed'Access));

      Gtk_New_Hbox (Hbox6, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         Hbox6, False, False, 3);

      Gtk_New (Hbuttonbox1);
      Set_Spacing (Hbuttonbox1, 30);
      Set_Layout (Hbuttonbox1, Buttonbox_Spread);
      Set_Child_Size (Hbuttonbox1, 85, 27);
      Set_Child_Ipadding (Hbuttonbox1, 7, 0);
      Add (Hbox6, Hbuttonbox1);

      Gtk_New_From_Stock (File_Selector_Window.Ok_Button, Stock_Ok);
      Set_Relief (File_Selector_Window.Ok_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Ok_Button, Can_Default);
      Add (Hbuttonbox1, File_Selector_Window.Ok_Button);

      Gtk_New_From_Stock (File_Selector_Window.Cancel_Button, Stock_Cancel);
      Set_Relief (File_Selector_Window.Cancel_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Cancel_Button, Can_Default);
      Add (Hbuttonbox1, File_Selector_Window.Cancel_Button);

      Show_Directory
        (File_Selector_Window.Explorer_Tree,
         File_Selector_Window.Home_Directory.all);

      Widget_Callback.Connect
        (File_Selector_Window, "realize", Realize'Access);
      Widget_Callback.Connect
        (File_Selector_Window, "destroy",
         Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   end Initialize;

end Gtkada.File_Selector;
