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

with Gtk; use Gtk;
with Glib;            use Glib;
with Gtk.Enums;       use Gtk.Enums;
with Glide_Intl; use Glide_Intl;
with Gtk.Tooltips; use Gtk.Tooltips;
with Glide_Kernel; use Glide_Kernel;

package body Vsearch_Pkg is

   procedure Gtk_New
     (Vsearch : out Vsearch_Access;
      Handle  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Vsearch := new Vsearch_Record;
      Vsearch_Pkg.Initialize (Vsearch, Handle);
   end Gtk_New;

   procedure Initialize
     (Vsearch : access Vsearch_Record'Class;
      Handle  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      pragma Suppress (All_Checks);
      Tooltips : Gtk_Tooltips;
   begin
      Initialize_Vbox (Vsearch, False, 0);

      Gtk_New (Vsearch.Table, 4, 2, False);
      Set_Row_Spacings (Vsearch.Table, 1);
      Pack_Start (Vsearch, Vsearch.Table, False, False, 0);

      Gtk_New (Vsearch.Replace_Label, -("Replace:"));
      Set_Alignment (Vsearch.Replace_Label, 0.0, 0.5);
      Attach (Vsearch.Table, Vsearch.Replace_Label, 0, 1, 1, 2, Fill);

      Gtk_New (Vsearch.Search_For_Label, -("Search for:"));
      Set_Alignment (Vsearch.Search_For_Label, 0.0, 0.5);
      Attach (Vsearch.Table, Vsearch.Search_For_Label, 0, 1, 0, 1, Fill);

      Gtk_New (Vsearch.Search_In_Label, -("Look in:"));
      Set_Alignment (Vsearch.Search_In_Label, 0.0, 0.5);
      Attach (Vsearch.Table, Vsearch.Search_In_Label, 0, 1, 2, 3, Fill);

      Gtk_New (Vsearch.Replace_Combo);
      Set_Case_Sensitive (Vsearch.Replace_Combo, True);
      Disable_Activate (Vsearch.Replace_Combo);
      Attach (Vsearch.Table, Vsearch.Replace_Combo, 1, 2, 1, 2);

      Vsearch.Replace_Entry := Get_Entry (Vsearch.Replace_Combo);
      Set_Size_Request (Vsearch.Replace_Entry, 0, -1);
      Set_Text (Vsearch.Replace_Entry, -"");
      Tooltips := Get_Tooltips (Handle);
      Set_Tip (Tooltips, Vsearch.Replace_Entry,
               -"The text that will replace each match");

      Gtk_New (Vsearch.Context_Combo);
      Set_Case_Sensitive (Vsearch.Context_Combo, False);
      Attach (Vsearch.Table, Vsearch.Context_Combo, 1, 2, 2, 3);

      Vsearch.Context_Entry := Get_Entry (Vsearch.Context_Combo);
      Set_Size_Request (Vsearch.Context_Entry, 0, -1);
      Set_Text (Vsearch.Context_Entry, -"");
      Set_Tip (Tooltips, Vsearch.Context_Entry, -"The context of the search");

      Gtk_New (Vsearch.Pattern_Combo);
      Set_Case_Sensitive (Vsearch.Pattern_Combo, True);
      Attach (Vsearch.Table, Vsearch.Pattern_Combo, 1, 2, 0, 1);

      Vsearch.Pattern_Entry := Get_Entry (Vsearch.Pattern_Combo);
      Set_Size_Request (Vsearch.Pattern_Entry, 0, -1);
      Set_Text (Vsearch.Pattern_Entry, -"");
      Set_Tip (Tooltips, Vsearch.Pattern_Entry,
                -"The searched word or pattern");

      Gtk_New_Hbox (Vsearch.Buttons_Hbox, False, 0);
      Pack_Start (Vsearch, Vsearch.Buttons_Hbox, False, False, 0);

      Gtk_New (Vsearch.Options_Frame, -"Options");
      Set_Shadow_Type (Vsearch.Options_Frame, Shadow_Etched_In);
      Pack_Start (Vsearch, Vsearch.Options_Frame, False, False, 0);

      Gtk_New (Vsearch.Options_Vbox, 3, 2, False);
      Add (Vsearch.Options_Frame, Vsearch.Options_Vbox);

      Gtk_New (Vsearch.Search_All_Check, -"All Occurrences");
      Set_Active (Vsearch.Search_All_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Search_All_Check, 0, 1, 0, 1);

      Gtk_New (Vsearch.Case_Check, -"Case Sensitive");
      Set_Active (Vsearch.Case_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Case_Check, 0, 1, 1, 2);

      Gtk_New (Vsearch.Whole_Word_Check, -"Whole Word");
      Set_Active (Vsearch.Whole_Word_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Whole_Word_Check, 1, 2, 1, 2);

      Gtk_New (Vsearch.Regexp_Check, -"Regular Expression");
      Set_Active (Vsearch.Regexp_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Regexp_Check, 1, 2, 0, 1);

      Gtk_New (Vsearch.Select_Editor_Check, -"Select Window on Match");
      Set_Active (Vsearch.Select_Editor_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Select_Editor_Check, 0, 2, 2, 3);

      Gtk_New (Vsearch.Auto_Hide_Check, -"Close Dialog on Search");
      Set_Active (Vsearch.Auto_Hide_Check, False);
      Attach (Vsearch.Options_Vbox,
              Vsearch.Auto_Hide_Check, 0, 2, 3, 4);
   end Initialize;

end Vsearch_Pkg;
