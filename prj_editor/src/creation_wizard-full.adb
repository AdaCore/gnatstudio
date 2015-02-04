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

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with Wizards;            use Wizards;
with GPS.Kernel;         use GPS.Kernel;
with Project_Viewers;    use Project_Viewers;
with Project_Properties; use Project_Properties;
with Switches_Editors;   use Switches_Editors;
with Toolchains_Editor;  use Toolchains_Editor;

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
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget is
   begin
      Page.Wiz := Wizard (Wiz);
      return Gtk.Widget.Gtk_Widget (Page.Page);
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
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Page.Page.Is_Visible (Languages.all);
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
      Changed := Changed or Page.Page.Edit_Project
        (Project            => Project,
         Kernel             => Kernel,
         Languages          => Languages.all,
         Scenario_Variables => Scenario_Variables);
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
      Kernel     : constant Kernel_Handle := Get_Kernel (Wiz);

      procedure Add_Page
        (Title : String;
         Page  : not null access Project_Editor_Page_Record'Class);
      procedure Add_Page
        (Title : String;
         Page  : not null access Project_Editor_Page_Record'Class)
      is
         P : access Wizard_Page_Record'Class;
      begin
         if Allow_Page = null or else Allow_Page (Title) then
            P := new Project_Editor_Page_Wrapper'
              (Project_Wizard_Page_Record with
               Page         => Project_Editor_Page (Page),
               Name_And_Loc => Name_And_Location_Page_Access (Name_And_Loc),
               Wiz          => Wizard (Wiz));
            Add_Page
              (Wiz,
               Page        => P,
               Description => Title,
               Toc         => Title);
         end if;
      end Add_Page;

      P : Project_Editor_Page;
   begin
      P := new Languages_Page_Record;
      P.Initialize (Kernel, Read_Only => False, Project => No_Project);
      Add_Page ("Languages", P);

      For_Each_Project_Editor_Page
        (Kernel,
         Project   => No_Project,
         Path      => Get_Path_Widget (Name_And_Loc),
         Read_Only => False,
         Context   => Context,
         Callback  => Add_Page'Access);

      P := Get_All_Naming_Scheme_Page (Kernel);
      P.Initialize (Kernel, Read_Only => False, Project => No_Project);
      Add_Page ("Naming scheme", P);

      P := Switches_Editor_For_All_Tools_Factory (Kernel);
      P.Initialize (Kernel, Read_Only => False, Project => No_Project);
      Add_Page ("Switches", P);
   end Add_Full_Wizard_Pages;

end Creation_Wizard.Full;
