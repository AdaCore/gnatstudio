-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            AdaCore                                --
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

with Glib;                  use Glib;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Box;               use Gtk.Box;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Handlers;       use Gtkada.Handlers;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Basic_Types;           use Basic_Types;
with Wizards;               use Wizards;
with Glide_Kernel;          use Glide_Kernel;
with Glide_Kernel.Project;  use Glide_Kernel.Project;
with File_Utils;            use File_Utils;
with Project_Viewers;       use Project_Viewers;
with Project_Properties;    use Project_Properties;
with Projects;              use Projects;

package body Creation_Wizard.Full is

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when a new page is selected in the wizard. We dynamically create
   --  the page if needed.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz               : out Prj_Wizard;
      Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ask_About_Loading : Boolean := False) is
   begin
      Wiz := new Prj_Wizard_Record;
      Creation_Wizard.Full.Initialize (Wiz, Kernel, Ask_About_Loading);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz               : access Prj_Wizard_Record'Class;
      Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ask_About_Loading : Boolean := False)
   is
      Page  : Project_Editor_Page;
      Attr_Count : constant Natural := Attribute_Editors_Page_Count;
      Count : constant Natural := Project_Editor_Pages_Count (Kernel);
      Box           : Gtk_Box;
      Main_Page_Box : Gtk_Box;

   begin
      Creation_Wizard.Initialize
        (Wiz, Kernel, Force_Relative_Dirs => False,
         Ask_About_Loading => Ask_About_Loading,
         Activate_Finish_From_Page => 1);
      Wiz.XML_Pages_Count := 0;

      Main_Page_Box := Gtk_Box (Get_Nth_Page (Wiz, 1));

      --  "+1" here is for the "General" page, which is omitted in the result
      --  of Attribute_Editors_Page_Count

      for E in 1 .. Attr_Count + 1 loop
         Box := Attribute_Editors_Page_Box
           (Kernel           => Kernel,
            Project          => No_Project,
            General_Page_Box => Main_Page_Box,
            Path_Widget      => Wiz.Project_Location,
            Nth_Page         => E,
            Context          => "wizard");
         if Box /= null then
            Wiz.XML_Pages_Count := Wiz.XML_Pages_Count + 1;
            Add_Page (Wiz,
                      Page         => Box,
                      Title        => Attribute_Editors_Page_Name (E),
                      Toc_Contents => Attribute_Editors_Page_Name (E));
         end if;
      end loop;

      for E in 1 .. Count loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);
         Add_Page (Wiz,
                   Page         => Widget_Factory
                     (Page, No_Project,
                      Name_As_Directory (Get_Text (Wiz.Project_Location))
                      & Get_Text (Wiz.Project_Name),
                      Kernel),
                   Title        => Get_Title (Page),
                   Toc_Contents => Get_Toc (Page));
      end loop;

      Widget_Callback.Connect (Wiz, "switch_page", Switch_Page'Access);
   end Initialize;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Wiz : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      W        : constant Prj_Wizard := Prj_Wizard (Wiz);
      Page_Num : constant Guint := To_Guint (Args, 1);
   begin
      if Integer (Page_Num - 1) <= W.XML_Pages_Count then
         null;

      elsif Integer (Page_Num - 1) - W.XML_Pages_Count <=
        Project_Editor_Pages_Count (W.Kernel)
      then
         declare
            Languages : GNAT.OS_Lib.String_List := Get_Current_Value
              (Kernel  => W.Kernel,
               Pkg     => "",
               Name    => "languages",
               Index   => "");
         begin
            Refresh
              (Page         => Get_Nth_Project_Editor_Page
                 (W.Kernel, Integer (Page_Num - 1) - W.XML_Pages_Count),
               Widget       => Get_Nth_Page (W, Integer (Page_Num)),
               Project      => No_Project,
               Languages    => Languages);
            Free (Languages);
         end;
      end if;
   end Switch_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Wiz     : access Prj_Wizard_Record;
      Project : in out Projects.Project_Type)
   is
      Count      : constant Natural := Project_Editor_Pages_Count (Wiz.Kernel);
      Languages      : GNAT.OS_Lib.String_List := Get_Current_Value
        (Kernel  => Wiz.Kernel,
         Pkg     => "",
         Name    => "languages",
         Index   => "");
      Changed, Tmp   : Boolean;
      pragma Unreferenced (Changed, Tmp);

   begin
      Changed := Update_Project_Attributes
        (Project            => Project,
         Scenario_Variables => (1 .. 0 => No_Variable));

      for P in 1 .. Count loop
         --  We are only interested in the side effect of Project_Editor, since
         --  we know for sure that the project will be modified
         Changed := Project_Editor
           (Get_Nth_Project_Editor_Page (Wiz.Kernel, P),
            Project,
            Wiz.Kernel,
            Get_Nth_Page (Wiz, P + Wiz.XML_Pages_Count + 1),
            Languages          => Languages,
            Scenario_Variables => (1 .. 0 => No_Variable),
            Ref_Project => Project);
      end loop;

      Tmp := Save_Single_Project (Wiz.Kernel, Project);
      Free (Languages);
   end Generate_Project;

end Creation_Wizard.Full;
