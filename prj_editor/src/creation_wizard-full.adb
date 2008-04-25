-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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

with Gtk.GEntry;         use Gtk.GEntry;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNATCOLL.Utils;     use GNATCOLL.Utils;
with Wizards;            use Wizards;
with GPS.Kernel;         use GPS.Kernel;
with File_Utils;         use File_Utils;
with Project_Viewers;    use Project_Viewers;
with Project_Properties; use Project_Properties;
with Projects;           use Projects;

package body Creation_Wizard.Full is

   type Project_Editor_Page_Wrapper is new Project_Wizard_Page_Record with
      record
         Page         : Project_Editor_Page;
         Name_And_Loc : Name_And_Location_Page_Access;
         Wiz          : Wizard;
      end record;
   function Create_Content
     (Page : access Project_Editor_Page_Wrapper;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Update_Page (Page : access Project_Editor_Page_Wrapper);
   procedure Generate_Project
     (Page               : access Project_Editor_Page_Wrapper;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   --  See inherited documentation

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Project_Editor_Page_Wrapper;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget is
   begin
      Page.Wiz := Wizard (Wiz);
      return Widget_Factory
        (Page         => Page.Page,
         Project      => Projects.No_Project,
         Full_Project =>
           Name_As_Directory (Get_Text
             (Get_Path_Widget (Page.Name_And_Loc)))
           & Get_Text (Get_Name_Widget (Page.Name_And_Loc)),
         Kernel       => Get_Kernel (Wiz));
   end Create_Content;

   -----------------
   -- Update_Page --
   -----------------

   procedure Update_Page (Page : access Project_Editor_Page_Wrapper) is
      Languages : String_List := Get_Current_Value
        (Kernel => Get_Kernel (Page.Wiz),
         Pkg    => "",
         Name   => "languages");
   begin
      Refresh (Page      => Page.Page,
               Widget    => Get_Content (Page),
               Project   => Projects.No_Project,
               Languages => Languages);
      Free (Languages);
   end Update_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Project_Editor_Page_Wrapper;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      Languages : String_List := Get_Current_Value
        (Kernel => Get_Kernel (Page.Wiz),
         Pkg    => "",
         Name   => "languages");
   begin
      Changed := Changed or Project_Editor
        (Page               => Page.Page,
         Project            => Project,
         Kernel             => Kernel,
         Widget             => Get_Content (Page),
         Languages          => Languages,
         Scenario_Variables => Scenario_Variables,
         Ref_Project        => Projects.No_Project);
      Free (Languages);
   end Generate_Project;

   ---------------------------
   -- Add_Full_Wizard_Pages --
   ---------------------------

   procedure Add_Full_Wizard_Pages
     (Wiz          : access Project_Wizard_Record'Class;
      Name_And_Loc : access Creation_Wizard.Name_And_Location_Page'Class;
      Context      : String)
   is
      P          : Project_Editor_Page;
      Attr_Count : constant Natural := Attribute_Editors_Page_Count;
      Count      : constant Natural :=
        Project_Editor_Pages_Count (Get_Kernel (Wiz));
      Page       : Project_Wizard_Page;

   begin
      --  "+1" here is for the "General" page, which is omitted in the result
      --  of Attribute_Editors_Page_Count

      for E in 1 .. Attr_Count + 1 loop
         Page := Attribute_Editors_Page_Box
           (Kernel      => Get_Kernel (Wiz),
            Wiz         => Wizard (Wiz),
            Project     => No_Project,
            Path_Widget => Get_Path_Widget (Name_And_Loc),
            Nth_Page    => E,
            Context     => Context);
         if Page /= null then
            Add_Page (Wiz,
                      Page        => Page,
                      Description => Attribute_Editors_Page_Name (E),
                      Toc         => Attribute_Editors_Page_Name (E));
         end if;
      end loop;

      for E in 1 .. Count loop
         P := Get_Nth_Project_Editor_Page (Get_Kernel (Wiz), E);
         Page := new Project_Editor_Page_Wrapper'
           (Project_Wizard_Page_Record with
            Page         => P,
            Name_And_Loc => Name_And_Location_Page_Access (Name_And_Loc),
            Wiz          => Wizard (Wiz));
         Add_Page (Wiz,
                   Page        => Page,
                   Description => Get_Title (P),
                   Toc         => Get_Toc (P));
      end loop;
   end Add_Full_Wizard_Pages;

end Creation_Wizard.Full;
