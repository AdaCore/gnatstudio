-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Box;               use Gtk.Box;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Combo;             use Gtk.Combo;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Label;             use Gtk.Label;
with Gtk.List;              use Gtk.List;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Table;             use Gtk.Table;
with Gtk.Tooltips;          use Gtk.Tooltips;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Glide_Intl;            use Glide_Intl;
with Glide_Kernel;          use Glide_Kernel;

with Find_Utils;            use Find_Utils;

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.Regexp;           use GNAT.Regexp;
with GNAT.IO;               use GNAT.IO;
with Ada.Exceptions;        use Ada.Exceptions;

with Glide_Kernel.Project;
with Glide_Kernel.Console;
with Basic_Types;           use Basic_Types;
with String_Utils;          use String_Utils;
with Gdk.Color;             use Gdk.Color;
with Gtk.Main;

package body Vsearch_Ext is

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Search_Next (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Find" is clicked.

   procedure On_Search_Replace (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Replace" is clicked.

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Previous" is clicked.

   procedure On_Stop_Search (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Stop" is clicked.

   procedure On_Options_Toggled (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Options" is toggled.

   procedure On_Context_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the entry "Look in" is changed.

   procedure On_Scope_Entry_Changed (Object : access Gtk_Widget_Record'Class);
   --  Called when the entry "Scope" is changed.

   --------------------
   -- On_Search_Next --
   --------------------

   procedure On_Search_Next (Object : access Gtk_Widget_Record'Class) is
      use Glide_Kernel;
      use Glide_Kernel.Project;

      Highlight_File : constant String := "#FF0000000000";
      Vsearch        : constant Vsearch_Extended := Vsearch_Extended (Object);

      S              : Code_Search;
      RE             : Regexp;
      Highlight      : Gdk_Color;
      Sources        : String_Array_Access;
      Scope          : Search_Scope;

      procedure Reset_Search;
      --  Call it before every search using this callback.

      function Callback
        (Match_Found : Boolean;
         File        : String;
         Line_Nr     : Positive    := 1;
         Line_Text   : String      := "";
         Sub_Matches : Match_Array := (0 => No_Match)) return Boolean;
      --  Print every match 'file:line:text'; ignore file calls.
      --  Handle Gtk pending events.

      --------------
      -- Callback --
      --------------

      function Callback
        (Match_Found : Boolean;
         File        : String;
         Line_Nr     : Positive    := 1;
         Line_Text   : String      := "";
         Sub_Matches : Match_Array := (0 => No_Match)) return Boolean
      is
         Dummy : Boolean;
      begin
         if Match_Found then
            Console.Insert
              (Vsearch.Kernel, File & ":" & Image (Line_Nr) & ":" &
               Line_Text);
         end if;

         while Gtk.Main.Events_Pending loop
            Dummy := Gtk.Main.Main_Iteration;
         end loop;

         if Vsearch.Continue then
            return True;
         else
            return False;
         end if;
      end Callback;

      ------------------
      -- Reset_Search --
      ------------------

      procedure Reset_Search is
      begin
         Vsearch.Continue := True;
      end Reset_Search;

   begin
      Highlight := Parse (Highlight_File);
      Alloc (Get_Default_Colormap, Highlight);

      Scope := Search_Scope'Val (Vsearch.Scope);

      if Get_Active (Vsearch.Search_All_Check) then
         case Vsearch.Context is
            when Context_Current_File | Context_Project_Files =>
               if Vsearch.Context = Context_Current_File then
                  --  ??? Sources :=
                  --    (1 => new String' (Get_Current_File (Vsearch.Kernel)));
                  Sources := new String_Array' (1 => new String' (""));

               else
                  Sources := Get_Source_Files (Vsearch.Kernel);
               end if;

               Init_Search
                 (S,
                  Get_Text (Vsearch.Pattern_Entry),
                  Sources,
                  Get_Active (Vsearch.Case_Check),
                  Get_Active (Vsearch.Whole_Word_Check),
                  Get_Active (Vsearch.Regexp_Check),
                  Scope);

            when Context_Files =>
               RE := Compile (Get_Text (Vsearch.Files_Entry), Glob => True);

               Init_Search
                 (S,
                  Get_Text (Vsearch.Pattern_Entry),
                  RE,
                  Get_Text (Vsearch.Directory_Entry),
                  Get_Active (Vsearch.Subdirs_Check),
                  Get_Active (Vsearch.Case_Check),
                  Get_Active (Vsearch.Whole_Word_Check),
                  Get_Active (Vsearch.Regexp_Check),
                  Scope);

            when others =>
               null;
         end case;

         Set_Sensitive (Vsearch.Stop_Button, True);
         Set_Sensitive (Vsearch.Search_Next_Button, False);

         Reset_Search;
         Do_Search (S, Callback'Unrestricted_Access);

         Set_Sensitive (Vsearch.Stop_Button,  False);
         Set_Sensitive (Vsearch.Search_Next_Button, True);
         Free (S);
         Free (Sources);

      else
         --  Search only once
         null;
      end if;

   exception
      when Error_In_Regexp =>
         Put_Line ("--- Bad globbing pattern: '"
                   & Get_Text (Vsearch.Files_Entry)
                   & "'");

      when E : others =>
         Put_Line ("--- Exception >>>");
         Put_Line (Exception_Information (E));
         Put_Line ("--- <<< Exception");
   end On_Search_Next;

   -----------------------
   -- On_Search_Replace --
   -----------------------

   procedure On_Search_Replace (Object : access Gtk_Widget_Record'Class) is
   begin
      null;
   end On_Search_Replace;

   ------------------------
   -- On_Search_Previous --
   ------------------------

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class) is
   begin
      null;
   end On_Search_Previous;

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
   begin
      Vsearch.Continue := False;
   end On_Stop_Search;

   ------------------------
   -- On_Options_Toggled --
   ------------------------

   procedure On_Options_Toggled (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
   begin
      if Get_Active (Vsearch.Options_Toggle) then
         Attach
           (Vsearch.Table, Vsearch.Options_Frame, 0, 2, 5, 6, Fill, 0, 2, 0);
         Unref (Vsearch.Options_Frame);

      else
         Ref (Vsearch.Options_Frame);
         Remove (Vsearch.Table, Vsearch.Options_Frame);
      end if;

      Show_All (Vsearch.Table);
      Queue_Resize (Get_Toplevel (Vsearch.Table));
   end On_Options_Toggled;

   ------------------------------
   -- On_Context_Entry_Changed --
   ------------------------------

   procedure On_Context_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
      List    : constant Gtk_List         := Get_List (Vsearch.Context_Combo);
      Value   : Gint;

   begin
      if Get_Selection (List) /= Widget_List.Null_List then
         Value := Child_Position (List, Get_Data (Get_Selection (List)));

         if Vsearch.Context /= Context_Explorer
           and then Value = Context_Explorer
         then
            Set_Sensitive (Vsearch.Replace_Label, False);
            Set_Sensitive (Vsearch.Replace_Combo, False);
            Set_Sensitive (Vsearch.Search_Replace_Button, False);
            Set_Sensitive (Vsearch.Scope_Label, False);
            Set_Sensitive (Vsearch.Scope_Combo, False);
            Set_Sensitive (Vsearch.Search_All_Check, False);

         elsif Vsearch.Context = Context_Explorer
           and then Value /= Context_Explorer
         then
            Set_Sensitive (Vsearch.Replace_Label, True);
            Set_Sensitive (Vsearch.Replace_Combo, True);
            Set_Sensitive (Vsearch.Search_Replace_Button, True);
            Set_Sensitive (Vsearch.Scope_Label, True);
            Set_Sensitive (Vsearch.Scope_Combo, True);
            Set_Sensitive (Vsearch.Search_All_Check, True);
         end if;

         if Vsearch.Context /= Context_Files
           and then Value = Context_Files
         then
            Attach
              (Vsearch.Table, Vsearch.Files_Frame, 0, 2, 3, 4, Fill, 0, 2, 0);
            Unref (Vsearch.Files_Frame);
            Show_All (Vsearch.Table);
            Queue_Resize (Get_Toplevel (Vsearch.Table));

         elsif Vsearch.Context = Context_Files
           and then Value /= Context_Files
         then
            Ref (Vsearch.Files_Frame);
            Remove (Vsearch.Table, Vsearch.Files_Frame);
            Show_All (Get_Toplevel (Vsearch.Table));
            Queue_Resize (Get_Toplevel (Vsearch.Table));
         end if;

         Vsearch.Context := Value;
      end if;
   end On_Context_Entry_Changed;

   ----------------------------
   -- On_Scope_Entry_Changed --
   ----------------------------

   procedure On_Scope_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
      List    : constant Gtk_List         := Get_List (Vsearch.Context_Combo);

   begin
      if Get_Selection (List) /= Widget_List.Null_List then
         Vsearch.Scope :=
           Child_Position (List, Get_Data (Get_Selection (List)));
      end if;
   end On_Scope_Entry_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Vsearch : out Vsearch_Extended;
      Handle  : Glide_Kernel.Kernel_Handle) is
   begin
      Vsearch := new Vsearch_Extended_Record;
      Vsearch_Ext.Initialize (Vsearch, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Vsearch : access Vsearch_Extended_Record'Class;
      Handle  : Glide_Kernel.Kernel_Handle)
   is
      pragma Suppress (All_Checks);

   begin
      Vsearch_Pkg.Initialize (Vsearch);
      Vsearch.Kernel := Handle;

      Widget_Callback.Object_Connect
        (Vsearch.Context_Entry, "changed",
         Widget_Callback.To_Marshaller (On_Context_Entry_Changed'Access),
         Vsearch);

      Widget_Callback.Object_Connect
        (Vsearch.Scope_Entry, "changed",
         Widget_Callback.To_Marshaller (On_Scope_Entry_Changed'Access),
         Vsearch);

      Ref (Vsearch.Files_Frame);
      Remove (Vsearch.Table, Vsearch.Files_Frame);
      Ref (Vsearch.Options_Frame);
      Remove (Vsearch.Table, Vsearch.Options_Frame);

      Gtk_New_From_Stock (Vsearch.Search_Next_Button, Stock_Find);
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Next_Button, False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Next_Button,
         -"Search next/all occurrence(s)");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Next_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Search_Next'Access), Vsearch);

      Gtk_New (Vsearch.Search_Replace_Button, -"Replace");
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Replace_Button, False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Replace_Button,
         -"Replace next/all occurrence(s)");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Replace_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Search_Replace'Access), Vsearch);

      Gtk_New (Vsearch.Search_Previous_Button, -"Previous");
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Previous_Button,
         False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Previous_Button,
         -"Search previous occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Previous_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Search_Previous'Access), Vsearch);

      Gtk_New (Vsearch.Stop_Button, -"Stop");
      Set_Sensitive (Vsearch.Stop_Button, False);
      Pack_Start (Vsearch.Buttons_Hbox, Vsearch.Stop_Button, False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Stop_Button, -"Stop current search");
      Widget_Callback.Object_Connect
        (Vsearch.Stop_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Stop_Search'Access), Vsearch);

      Gtk_New (Vsearch.Options_Toggle, -"Options>>");
      Set_Active (Vsearch.Options_Toggle, False);
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Options_Toggle, False, False, 0);
      Set_Tip (Get_Tooltips (Handle), Vsearch.Options_Toggle, -
               "Display extended options");
      Widget_Callback.Object_Connect
        (Vsearch.Options_Toggle, "toggled",
         Widget_Callback.To_Marshaller (On_Options_Toggled'Access), Vsearch);
   end Initialize;

end Vsearch_Ext;
