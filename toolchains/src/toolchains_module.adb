------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Glib.Object;               use Glib.Object;
with XML_Utils;                 use XML_Utils;
with Gtk.Dialog;                use Gtk.Dialog;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;
with GPS.Intl;                  use GPS.Intl;
with GPS.Properties;            use GPS.Properties;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with Toolchains_Old;            use Toolchains_Old;
with Toolchains_Dialog;         use Toolchains_Dialog;
with Builder_Facility_Module;

package body Toolchains_Module is
   Me : constant Trace_Handle := Create ("TOOLCHAINS");

   type Toolchains_Property is new Property_Record with record
      Active           : Boolean;
      Tools_Path       : Virtual_File;
      Use_Xrefs_Subdir : Boolean;
      Compiler_Path    : Virtual_File;
   end record;

   overriding procedure Save
     (Property : access Toolchains_Property;
      Node     : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out Toolchains_Property; From : XML_Utils.Node_Ptr);
   overriding procedure Destroy (Property : in out Toolchains_Property);
   --  See inherited doc.

   pragma Warnings (Off); --  Yes, it's not dispatching and it's expected.
   procedure Apply
     (Property : Toolchains_Property;
      Kernel   : GPS.Kernel.Kernel_Handle);
   pragma Warnings (On);
   --  Applies the property.

   type Toolchains_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Toolchains_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Config menu

   pragma Warnings (Off); --  Yes, it's not dispatching and it's expected.
   function Get_Property return Toolchains_Property;
   pragma Warnings (On);
   --  Retrieve the global property

   type On_GPS_Started is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_GPS_Started;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when GPS is starting

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Toolchains_Property;
      Node     : in out XML_Utils.Node_Ptr)
   is
      Child : XML_Utils.Node_Ptr;
   begin
      if Property.Active then
         Child := new XML_Utils.Node;
         Child.Tag := new String'("active");
         Add_Child (Node, Child);
      end if;

      if Property.Use_Xrefs_Subdir then
         Child := new XML_Utils.Node;
         Child.Tag := new String'("use_xrefs_subdir");
         Add_Child (Node, Child);
      end if;

      if Property.Tools_Path /= No_File then
         Add_File_Child
           (Node, "tools_path", Property.Tools_Path,
            Use_VFS_Prefix => False);
      end if;

      if Property.Compiler_Path /= No_File then
         Add_File_Child
           (Node, "compiler_path", Property.Compiler_Path,
            Use_VFS_Prefix => False);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Toolchains_Property; From : XML_Utils.Node_Ptr)
   is
      Child : XML_Utils.Node_Ptr;
   begin
      Child := Find_Tag (From.Child, "active");
      Property.Active := Child /= null;

      --  Set Use_Xrefs_Subdir on by default when the module is not active.
      --  This has no impact on its actual xrefs_subdir state (Property.Active
      --  state is always checked first), but this will check the corresponding
      --  button in the dialog by default, which is a desirable thing.
      Child := Find_Tag (From.Child, "use_xrefs_subdir");
      Property.Use_Xrefs_Subdir := Child /= null or else not Property.Active;

      Property.Tools_Path :=
        Get_File_Child (From, "tools_path", Use_VFS_Prefix => False);
      Property.Compiler_Path :=
        Get_File_Child (From, "compiler_path", Use_VFS_Prefix => False);
   end Load;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Toolchains_Property) is
   begin
      Property.Tools_Path    := No_File;
      Property.Compiler_Path := No_File;
   end Destroy;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Property : Toolchains_Property;
      Kernel   : GPS.Kernel.Kernel_Handle) is
   begin
      Toolchains_Old.Set_Toolchains_Properties
        (Active               => Property.Active,
         Tool_Search_Path     => Property.Tools_Path,
         Compiler_Search_Path => Property.Compiler_Path);

      if Property.Active
        and then Property.Use_Xrefs_Subdir
      then
         --  ??? .xrefs and mode "xref" should not be string literals, but
         --  stored somewhere instead.
         if not Equal
           (Get_Registry (Kernel).Environment.Xrefs_Subdir, ".xrefs")
         then
            Get_Registry (Kernel).Environment.Set_Xrefs_Subdir (".xrefs");
            GPS.Kernel.Project.Recompute_View (Kernel);
         end if;

         Builder_Facility_Module.Set_Subdir ("xref", ".xrefs");
         Builder_Facility_Module.Activate_Mode ("xref", True);

      else
         if not Equal (Get_Registry (Kernel).Environment.Xrefs_Subdir, "") then
            Get_Registry (Kernel).Environment.Set_Xrefs_Subdir ("");
            GPS.Kernel.Project.Recompute_View (Kernel);
         end if;

         Builder_Facility_Module.Set_Subdir ("xref", ".");
         Builder_Facility_Module.Activate_Mode ("xref", False);
      end if;
   end Apply;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_GPS_Started;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Property : constant Toolchains_Property := Get_Property;
   begin
      Apply (Property, GPS.Kernel.Kernel_Handle (Kernel));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Toolchains_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Property      : Toolchains_Property := Get_Property;
      Prop_Access   : Property_Access;
      Dialog        : Toolchains_Dialog.Dialog;
      Resp          : Gtk_Response_Type;
      Compiler      : constant Filesystem_String :=
                        +Get_Project (Kernel).Attribute_Value
                          (Compiler_Command_Attribute,
                           Default => "gnatmake",
                           Index   => "Ada");
      Default_Path  : Virtual_File;
      Tools_Path    : Virtual_File;
      Compiler_Path : Virtual_File;

   begin
      if Property.Tools_Path = No_File
        or else Property.Compiler_Path = No_File
      then
         declare
            Path : constant Virtual_File := Locate_On_Path (Compiler);
         begin
            if Path /= No_File then
               Default_Path := Dir (Path);
            else
               Default_Path := Get_Root (Get_Current_Dir);
            end if;
         end;
      end if;

      if Property.Tools_Path = No_File then
         Tools_Path := Default_Path;
      else
         Tools_Path := Property.Tools_Path;
      end if;

      if Property.Compiler_Path = No_File then
         Compiler_Path := Default_Path;
      else
         Compiler_Path := Property.Compiler_Path;
      end if;

      Gtk_New
        (Dialog, Kernel, Property.Active,
         Tools_Path, Property.Use_Xrefs_Subdir,
         Compiler_Path);

      Resp := Dialog.Run;

      if Resp = Gtk_Response_OK then
         Property.Active := Get_Active (Dialog);
         Property.Tools_Path := Get_Tools_Path (Dialog);
         Property.Compiler_Path := Get_Compiler_Path (Dialog);
         Property.Use_Xrefs_Subdir := Get_Use_Xrefs_Subdir (Dialog);

         Prop_Access := new Toolchains_Property'(Property);
         Set_Property
           (Kernel,
            Index_Name  => "toolchains_property",
            Index_Value => "toolchains_property",
            Name        => "property",
            Property    => Prop_Access,
            Persistent  => True);

         Apply (Property, Kernel);
      end if;

      Destroy (Dialog);
      return Commands.Success;
   end Execute;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property return Toolchains_Property is
      Property : Toolchains_Property;
      Success  : Boolean;
   begin
      Get_Property
        (Property,
         Index_Name  => "toolchains_property",
         Index_Value => "toolchains_property",
         Name        => "property",
         Found       => Success);

      if not Success then
         return Toolchains_Property'
           (Active           => False,
            Tools_Path       => No_File,
            Use_Xrefs_Subdir => True,
            Compiler_Path    => No_File);
      else
         return Property;
      end if;
   end Get_Property;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Action
        (Kernel, "open toolchains editor", new Toolchains_Command,
         -"Open the toolchains editor (for builds)",
         Category => -"Views");

      --  Load the property after all modules and plug-ins are loaded.
      Gps_Started_Hook.Add (new On_GPS_Started);
   end Register_Module;

end Toolchains_Module;
