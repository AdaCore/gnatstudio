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
with Gtk.Button;            use Gtk.Button;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Combo;             use Gtk.Combo;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Label;             use Gtk.Label;
with Gtk.Main;              use Gtk.Main;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Table;             use Gtk.Table;
with Gtk.Toggle_Button;     use Gtk.Toggle_Button;
with Gtk.Tooltips;          use Gtk.Tooltips;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Glide_Intl;            use Glide_Intl;
with Glide_Kernel;          use Glide_Kernel;
with Files_Extra_Info_Pkg;  use Files_Extra_Info_Pkg;

with Find_Utils;            use Find_Utils;
with GUI_Utils;             use GUI_Utils;

with Basic_Types;           use Basic_Types;
with Generic_List;

with Traces; use Traces;

package body Vsearch_Ext is

   Me : Debug_Handle := Create ("Vsearch_Project");

   type Search_Module_Data is record
      Label             : String_Access;
      Mask              : Search_Options_Mask;
      Factory           : Module_Search_Context_Factory;
      Extra_Information : Gtk_Widget;
      Search_Func       : Module_Search_Function;
      Replace_Func      : Module_Replace_Function;
   end record;

   No_Search : constant Search_Module_Data :=
     (Label             => null,
      Mask              => 0,
      Factory           => null,
      Extra_Information => null,
      Search_Func       => null,
      Replace_Func      => null);

   procedure Free (Data : in out Search_Module_Data);

   package Search_Modules_List is new Generic_List (Search_Module_Data);
   use Search_Modules_List;

   Search_Modules : Search_Modules_List.List;
   --  Global variable that contains the list of all registered search
   --  functions.

   type Idle_Search_Data is record
      Search_Func : Module_Search_Function;
      Vsearch : Vsearch_Extended;
      Context : Search_Context_Access;
      Search_Backward : Boolean;
   end record;

   package Search_Idle_Pkg is new Gtk.Main.Idle (Idle_Search_Data);

   function Idle_Search (Data : Idle_Search_Data) return Boolean;
   --  Performs the search in an idle loop, so that the user can still interact
   --  with the rest of Glide2.

   function Find_Module (Name : String) return Search_Module_Data;
   --  Find the module for name Name.

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

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Search_Module_Data) is
   begin
      Free (Data.Label);
      Unref (Data.Extra_Information);
   end Free;

   ------------------------------
   -- Register_Search_Function --
   ------------------------------

   procedure Register_Search_Function
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Label             : String;
      Factory           : Module_Search_Context_Factory;
      Extra_Information : Gtk.Widget.Gtk_Widget := null;
      Mask              : Search_Options_Mask;
      Search_Func       : Module_Search_Function;
      Replace_Func      : Module_Replace_Function := null) is
      Combo             : Gtk_Combo;
   begin
      Prepend (Search_Modules,
               Search_Module_Data'
               (Label             => new String' (Label),
                Mask              => Mask,
                Factory           => Factory,
                Extra_Information => Extra_Information,
                Search_Func       => Search_Func,
                Replace_Func      => Replace_Func));

      if Extra_Information /= null then
         Ref (Extra_Information);
      end if;

      if Get_Search_Module (Kernel) /= null then
         Combo := Vsearch_Extended (Get_Search_Module (Kernel)).Context_Combo;
         Add_Unique_Combo_Entry (Combo,  Label);

         if Get_Text (Get_Entry (Combo)) = "" then
            Set_Text (Get_Entry (Combo), Label);
         end if;
      end if;
   end Register_Search_Function;

   -----------------
   -- Find_Module --
   -----------------

   function Find_Module (Name : String) return Search_Module_Data is
      List : Search_Modules_List.List := Search_Modules;
   begin
      while List /= Null_List loop
         if Head (List).Label.all = Name then
            return Head (List);
         end if;

         List := Next (List);
      end loop;
      return No_Search;
   end Find_Module;

   -----------------
   -- Idle_Search --
   -----------------

   function Idle_Search (Data : Idle_Search_Data) return Boolean is
      C : Search_Context_Access;
   begin
      if Data.Search_Func
        (Data.Vsearch.Kernel, Data.Context, Data.Search_Backward)
        and then Data.Vsearch.Continue
      then
         return True;
      else
         C := Data.Context;
         Free (C);
         Set_Sensitive (Data.Vsearch.Stop_Button, False);
         Set_Sensitive (Data.Vsearch.Search_Next_Button, True);
         Pop_State (Data.Vsearch.Kernel);
         return False;
      end if;
   end Idle_Search;

   --------------------
   -- On_Search_Next --
   --------------------

   procedure On_Search_Next (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
      Options : Search_Options;
      Data    : Search_Module_Data;
      Context : Search_Context_Access;
      All_Occurences : constant Boolean := Get_Active
        (Vsearch.Search_All_Check);
      Has_Next : Boolean;
      Id       : Idle_Handler_Id;

   begin
      Push_State (Vsearch.Kernel, Processing);
      Data := Find_Module (Get_Text (Get_Entry (Vsearch.Context_Combo)));

      if Data.Factory /= null
        and then Get_Text (Vsearch.Pattern_Entry) /= ""
      then
         Context := Data.Factory (Vsearch.Kernel, Data.Extra_Information);

         if Context /= null then
            Options :=
              (Scope          => Search_Scope'Val
                 (Get_Index_In_List (Vsearch.Scope_Combo) + 1),
               Case_Sensitive => Get_Active (Vsearch.Case_Check),
               Whole_Word     => Get_Active (Vsearch.Whole_Word_Check),
               Regexp         => Get_Active (Vsearch.Regexp_Check));
            Set_Context (Context, Get_Text (Vsearch.Pattern_Entry), Options);

            Vsearch.Continue := True;

            if All_Occurences then
               --  Set up Glide during the search. Everything is automatically
               --  put back when the idle loop terminates.
               Push_State (Vsearch.Kernel, Processing);
               Set_Sensitive (Vsearch.Stop_Button, True);
               Set_Sensitive (Vsearch.Search_Next_Button, False);
               Id := Search_Idle_Pkg.Add
                 (Idle_Search'Access,
                  (Search_Func => Data.Search_Func,
                   Vsearch => Vsearch,
                   Context => Context,
                   Search_Backward => False));
            else
               Has_Next := Data.Search_Func
                 (Vsearch.Kernel,
                  Context,
                  Search_Backward => False);
               Free (Context);
            end if;
         end if;
      end if;
      Pop_State (Vsearch.Kernel);
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
   end On_Options_Toggled;

   ------------------------------
   -- On_Context_Entry_Changed --
   ------------------------------

   procedure On_Context_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
      Data    : Search_Module_Data := Find_Module
        (Get_Text (Get_Entry (Vsearch.Context_Combo)));

   begin
      if Data /= No_Search then
         Set_Sensitive (Vsearch.Replace_Label, Data.Replace_Func /= null);
         Set_Sensitive (Vsearch.Replace_Combo, Data.Replace_Func /= null);
         Set_Sensitive
           (Vsearch.Search_Replace_Button, Data.Replace_Func /= null);
         Set_Sensitive (Vsearch.Search_Next_Button, Data.Search_Func /= null);

         Set_Sensitive (Vsearch.Scope_Label, (Data.Mask and Scope_Mask) /= 0);
         Set_Sensitive (Vsearch.Scope_Combo, (Data.Mask and Scope_Mask) /= 0);

         Set_Sensitive
           (Vsearch.Search_All_Check, (Data.Mask and All_Occurences) /= 0);

         Set_Sensitive
           (Vsearch.Case_Check, (Data.Mask and Case_Sensitive) /= 0);
         Set_Sensitive
           (Vsearch.Whole_Word_Check, (Data.Mask and Whole_Word) /= 0);
         Set_Sensitive (Vsearch.Regexp_Check,
                        (Data.Mask and Find_Utils.Regexp) /= 0);

         Set_Sensitive (Vsearch.Search_Previous_Button,
                        (Data.Mask and Search_Backward) /= 0);

         if Vsearch.Extra_Information /= null then
            Remove (Vsearch.Table, Vsearch.Extra_Information);
            Vsearch.Extra_Information := null;
         end if;

         if Data.Extra_Information /= null then
            Attach
              (Vsearch.Table, Data.Extra_Information,
               0, 2, 3, 4, Fill, 0, 2, 0);
            Show_All (Vsearch.Table);
            Vsearch.Extra_Information := Data.Extra_Information;
         end if;
      end if;
   end On_Context_Entry_Changed;

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
      Current : Search_Modules_List.List := Search_Modules;
   begin
      Vsearch_Pkg.Initialize (Vsearch);
      Vsearch.Kernel := Handle;

      Widget_Callback.Object_Connect
        (Vsearch.Context_Entry, "changed",
         Widget_Callback.To_Marshaller (On_Context_Entry_Changed'Access),
         Vsearch);

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

      --  Show the registered modules
      while Current /= Null_List loop
         Add_Unique_Combo_Entry
           (Vsearch.Context_Combo, Head (Current).Label.all);
         Current := Next (Current);
      end loop;

      Set_Search_Module (Handle, Vsearch);
   end Initialize;

   -----------------------------
   -- Register_Default_Search --
   -----------------------------

   procedure Register_Default_Search
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Extra : Files_Extra_Info_Access;
   begin
      Gtk_New (Extra);

      --  Register_Search_Function
      --    (Kernel            => Kernel,
      --     Label             => -"Current File",
      --     Factory           => Current_File_Factory'Access,
      --     Extra_Information => null,
      --     Mask              => All_Options and not Search_Backward,
      --     Search_Func       => Search_Current_File'Access,
      --     Replace_Func      => null);
      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files From Project",
         Factory           => Files_From_Project_Factory'Access,
         Extra_Information => null,
         Mask              => All_Options and not Search_Backward,
         Search_Func       => Search_Files_From_Project'Access,
         Replace_Func      => null);
      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Files...",
         Factory           => Files_Factory'Access,
         Extra_Information => Gtk_Widget (Extra),
         Mask              => All_Options and not Search_Backward,
         Search_Func       => Search_Files'Access,
         Replace_Func      => null);
   end Register_Default_Search;

end Vsearch_Ext;
