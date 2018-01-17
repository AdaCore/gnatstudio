------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

with GNATCOLL.Traces; use GNATCOLL.Traces;

package body GNATdoc.Backend.HTML.JSON_Builder is

   use GNATCOLL.JSON;

   Me : constant Trace_Handle := Create ("GNATdoc.1-HTML_Backend");

   procedure Set_CSS_Class
     (Object     : in out GNATCOLL.JSON.JSON_Value;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map);
   --  Sets 'class' attribute of object from given event when present

   -------------------
   -- Set_CSS_Class --
   -------------------

   procedure Set_CSS_Class
     (Object     : in out GNATCOLL.JSON.JSON_Value;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map) is
   begin
      if Attributes.Contains ("class") then
         Object.Set_Field ("cssClass", Attributes.Element ("class"));
      end if;
   end Set_CSS_Class;

   ----------------------------
   -- To_JSON_Representation --
   ----------------------------

   function To_JSON_Representation
     (Stream : GNATdoc.Markup_Streams.Event_Vectors.Vector;
      Kernel : not null access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return GNATCOLL.JSON.JSON_Array
   is
      type State_Type is record
         Object          : GNATCOLL.JSON.JSON_Value;
         Children        : GNATCOLL.JSON.JSON_Array;
         Span_Mode       : Boolean := False;
         Span_Attributes : Markup_Streams.Name_Value_Maps.Map;
         --  Span mode means that start of 'span' element tag has been
         --  processed and corresponding attributes has been stored in
         --  Span_Attributes member.
      end record;

      package State_Vectors is
        new Ada.Containers.Vectors (Positive, State_Type);

      State_Stack : State_Vectors.Vector;
      State       : State_Type;
      Object      : GNATCOLL.JSON.JSON_Value;
      Aux         : GNATCOLL.JSON.JSON_Array;
      Number      : Positive;
      Project     : GNATCOLL.Projects.Project_Type;
      Dir         : GNATCOLL.VFS.Virtual_File;
      File        : GNATCOLL.VFS.Virtual_File;
      Success     : Boolean;

   begin
      for Event of Stream loop
         case Event.Kind is
            when GNATdoc.Markup_Streams.Start_Tag =>
               State_Stack.Append (State);
               State :=
                 (Object          => Create_Object,
                  Children        => Empty_Array,
                  Span_Mode       => False,
                  Span_Attributes => <>);

               if Event.Name = "p" then
                  State.Object.Set_Field ("kind", "paragraph");
                  Set_CSS_Class (State.Object, Event.Attributes);

               elsif Event.Name = "pre" then
                  State.Object.Set_Field ("kind", "code");
                  Number := 1;

               elsif Event.Name = "image" then
                  Project := Kernel.Registry.Tree.Root_Project;
                  Dir := Project.Project_Path.Get_Parent;
                  Dir :=
                    GNATCOLL.VFS.Create_From_Base
                      (Filesystem_String
                         (Project.Attribute_Value
                            (Attribute =>
                                 Attribute_Pkg_String'
                               (Build (Pkg_Name, Image_Dir_Name)),
                             Default   => String (Dir.Full_Name.all))),
                       Dir.Full_Name.all);
                  File :=
                    GNATCOLL.VFS.Create_From_Base
                      (Filesystem_String (Event.Attributes.Element ("file")),
                       Dir.Full_Name.all);
                  File.Copy
                    (GNATdoc.Get_Doc_Directory (Kernel).Create_From_Dir
                     ("images/").Full_Name.all,
                     Success);

                  if not Success then
                     Trace
                       (Me, "Unable to copy image " & File.Display_Full_Name);
                  end if;

                  State.Object.Set_Field ("kind", "image");
                  State.Object.Set_Field
                    ("src", "../images/" & String (File.Base_Name));

               elsif Event.Name = "span" then
                  --  Unwind stack
                  State := State_Stack.Last_Element;
                  State_Stack.Delete_Last;
                  State.Span_Mode := True;
                  State.Span_Attributes := Event.Attributes;

               else
                  State.Object.Set_Field ("kind", To_String (Event.Name));

                  if Event.Name = "ul" or else Event.Name = "li" then
                     Set_CSS_Class (State.Object, Event.Attributes);
                  end if;
               end if;

            when GNATdoc.Markup_Streams.End_Tag =>
               if not State.Span_Mode then
                  State.Object.Set_Field ("children", State.Children);
                  Object := State.Object;
                  State := State_Stack.Last_Element;
                  State_Stack.Delete_Last;
                  Append (State.Children, Object);

               else
                  State.Span_Mode := False;
                  State.Span_Attributes.Clear;
               end if;

            when GNATdoc.Markup_Streams.Text =>
               if String'(State.Object.Get ("kind")) = "code" then
                  Object := Create_Object;
                  Object.Set_Field ("kind", "span");
                  Object.Set_Field ("text", To_String (Event.Text));

                  Clear (Aux);
                  Append (Aux, Object);

                  Object := Create_Object;
                  Object.Set_Field ("number", Number);
                  Object.Set_Field ("children", Aux);
                  Number := Number + 1;

               else
                  Object := Create_Object;
                  Object.Set_Field ("kind", "span");
                  Object.Set_Field ("text", To_String (Event.Text) & ASCII.LF);
                  Set_CSS_Class (Object, State.Span_Attributes);

                  if State.Span_Attributes.Contains ("href") then
                     Object.Set_Field
                       ("href", State.Span_Attributes.Element ("href"));
                  end if;
               end if;

               Append (State.Children, Object);
         end case;
      end loop;

      return State.Children;
   end To_JSON_Representation;

end GNATdoc.Backend.HTML.JSON_Builder;
