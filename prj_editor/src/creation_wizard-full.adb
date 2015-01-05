------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Gtk.GEntry;         use Gtk.GEntry;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with Wizards;            use Wizards;
with GPS.Kernel;         use GPS.Kernel;
with Project_Viewers;    use Project_Viewers;
with Project_Properties; use Project_Properties;

package body Creation_Wizard.Full is

   type Project_Editor_Page_Wrapper is new Project_Wizard_Page_Record with
      record
         Page         : Project_Editor_Page;
         Name_And_Loc : Name_And_Location_Page_Access;
         Wiz          : Wizard;
      end record;
   overriding function Create_Content
     (Page : access Project_Editor_Page_Wrapper;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   overriding procedure Update_Page
     (Page : access Project_Editor_Page_Wrapper);
   overriding procedure Generate_Project
     (Page               : access Project_Editor_Page_Wrapper;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean);
   --  See inherited documentation

   --------------------
   -- Create_Content --
   --------------------

   overriding function Create_Content
     (Page : access Project_Editor_Page_Wrapper;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Project_Dir : constant Virtual_File :=
                      Create_From_UTF8
                        (Get_Text (Get_Path_Widget (Page.Name_And_Loc)));
      Project_File : constant Virtual_File :=
                       Create_From_Dir
                         (Project_Dir,
                          +Get_Text (Get_Name_Widget (Page.Name_And_Loc)));
   begin
      Page.Wiz := Wizard (Wiz);
      return Widget_Factory
        (Page         => Page.Page,
         Project      => No_Project,
         Full_Project => Project_File,
         Kernel       => Get_Kernel (Wiz));
   end Create_Content;

   -----------------
   -- Update_Page --
   -----------------

   overriding procedure Update_Page
     (Page : access Project_Editor_Page_Wrapper)
   is
      Languages : String_List_Access := Get_Current_Value
        (Kernel => Get_Kernel (Page.Wiz),
         Pkg    => "",
         Name   => "languages");
   begin
      Refresh (Page      => Page.Page,
               Widget    => Get_Content (Page),
               Project   => No_Project,
               Languages => Languages.all);
      Free (Languages);
   end Update_Page;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Project_Editor_Page_Wrapper;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean)
   is
      Languages : String_List_Access := Get_Current_Value
        (Kernel => Get_Kernel (Page.Wiz),
         Pkg    => "",
         Name   => "languages");
   begin
      Changed := Changed or Project_Editor
        (Page               => Page.Page,
         Project            => Project,
         Kernel             => Kernel,
         Widget             => Get_Content (Page),
         Languages          => Languages.all,
         Scenario_Variables => Scenario_Variables,
         Ref_Project        => No_Project);
      Free (Languages);
   end Generate_Project;

   ---------------------------
   -- Add_Full_Wizard_Pages --
   ---------------------------

   procedure Add_Full_Wizard_Pages
     (Wiz          : access Project_Wizard_Record'Class;
      Name_And_Loc : access Creation_Wizard.Name_And_Location_Page'Class;
      Context      : String;
      Allow_Page   : access function (Page : String) return Boolean := null)
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

         if Page /= null
           and then (Allow_Page = null
                     or else Allow_Page (Attribute_Editors_Page_Name (E)))
         then
            Add_Page (Wiz,
                      Page        => Page,
                      Description => Attribute_Editors_Page_Name (E),
                      Toc         => Attribute_Editors_Page_Name (E));
         end if;
      end loop;

      for E in 1 .. Count loop
         P := Get_Nth_Project_Editor_Page (Get_Kernel (Wiz), E);

         if Allow_Page = null or else Allow_Page (P.Get_Toc) then
            Page := new Project_Editor_Page_Wrapper'
              (Project_Wizard_Page_Record with
               Page         => P,
               Name_And_Loc => Name_And_Location_Page_Access (Name_And_Loc),
               Wiz          => Wizard (Wiz));
            Add_Page (Wiz,
                      Page        => Page,
                      Description => Get_Title (P),
                      Toc         => Get_Toc (P));
         end if;
      end loop;
   end Add_Full_Wizard_Pages;

end Creation_Wizard.Full;
