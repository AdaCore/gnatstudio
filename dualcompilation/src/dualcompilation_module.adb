-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2008, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with System.OS_Lib;            use System.OS_Lib;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Projects.Registry;
with Traces;                   use Traces;
with GPS.Kernel.Properties;    use GPS.Kernel.Properties;
with GPS.Kernel.Project;

with Dualcompilation; use Dualcompilation;

package body Dualcompilation_Module is

   type Dualcomp_Property is new GPS.Kernel.Properties.Property_Record
   with record
      Active        : Boolean;
      Tools_Path    : String_Access;
      Compiler_Path : String_Access;
   end record;

   overriding procedure Save
     (Property : access Dualcomp_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr);
   overriding procedure Load
     (Property : in out Dualcomp_Property; From : Glib.Xml_Int.Node_Ptr);
   overriding procedure Destroy (Property : in out Dualcomp_Property);
   --  See inherited doc.

   function Get_Property return Dualcomp_Property'Class;
   --  Retrieve the global property

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Dualcomp_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      if Property.Active then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("active");
         Add_Child (Node, Child);
      end if;

      if Property.Tools_Path /= null then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("tools_path");
         Child.Value := new String'(Property.Tools_Path.all);
         Add_Child (Node, Child);
      end if;

      if Property.Compiler_Path /= null then
         Child := new Glib.Xml_Int.Node;
         Child.Tag := new String'("compiler_path");
         Child.Value := new String'(Property.Compiler_Path.all);
         Add_Child (Node, Child);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Dualcomp_Property; From : Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      Child := Find_Tag (From.Child, "active");
      Property.Active := Child /= null;
      Child := Find_Tag (From.Child, "tools_path");
      if Child /= null then
         Property.Tools_Path := new String'(Child.Value.all);
      else
         Property.Tools_Path := new String'("");
      end if;
      Child := Find_Tag (From.Child, "compiler_path");
      if Child /= null then
         Property.Compiler_Path := new String'(Child.Value.all);
      else
         Property.Compiler_Path := new String'("");
      end if;
   end Load;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Dualcomp_Property) is
   begin
      Free (Property.Tools_Path);
      Free (Property.Compiler_Path);
   end Destroy;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property return Dualcomp_Property'Class is
      Property : Dualcomp_Property;
      Success  : Boolean;
   begin
      Get_Property
        (Property,
         Index_Name  => "module",
         Index_Value => "dualcompilation",
         Name        => "configuration",
         Found       => Success);

      if not Success then
         return Dualcomp_Property'
           (Active => False, Tools_Path => null, Compiler_Path => null);
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
      Property : constant Dualcomp_Property'Class := Get_Property;
   begin
      if Property.Tools_Path /= null
        and then Property.Compiler_Path /= null
      then
         Dualcompilation.Set_Dualcompilation_Properties
           (Active               => Property.Active,
            Tool_Search_Path     => Property.Tools_Path.all,
            Compiler_Search_Path => Property.Compiler_Path.all);
      elsif Getenv ("GPS_TEST_DUALCOMPILATION") /= null then
         Projects.Registry.Set_Xrefs_Subdir
           (GPS.Kernel.Project.Get_Registry (Kernel).all,
            "xrefs");
         Dualcompilation.Set_Dualcompilation_Properties
           (Active               => True,
            Tool_Search_Path     => "",
            Compiler_Search_Path => Getenv ("GPS_TEST_DUALCOMPILATION").all);
      else
         Dualcompilation.Set_Dualcompilation_Properties
           (Active               => Property.Active,
            Tool_Search_Path     => "",
            Compiler_Search_Path => "");
      end if;
   end Register_Module;

end Dualcompilation_Module;
