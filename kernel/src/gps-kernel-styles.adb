-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
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

with Gdk;            use Gdk;
with Gdk.GC;         use Gdk.GC;
with Gtk.Window;     use Gtk.Window;
with Gtk.Widget;     use Gtk.Widget;

with Glib.Xml_Int;   use Glib.Xml_Int;
with XML_Parsers;

with GNAT.OS_Lib;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Traces;         use Traces;

with GPS.Intl;          use GPS.Intl;
with GPS.Kernel.Hooks;  use GPS.Kernel.Hooks;

with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body GPS.Kernel.Styles is

   Me : constant Debug_Handle := Create ("Styles");

   use Style_Htable.String_Hash_Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Free (Color : in out Color_Record);
   --  Free memory associated to Color.

   procedure Allocate_Color (Color : in out Color_Record);
   --  Allocates the low-level structures for Color.

   procedure Initialize_Predefined_Styles (Kernel : Kernel_Handle);
   --  Initialize the GPS predefined styles.

   ----------------------------------
   -- Initialize_Predefined_Styles --
   ----------------------------------

   procedure Initialize_Predefined_Styles (Kernel : Kernel_Handle) is
      procedure Init
        (Style : in out Style_Access;
         Name  : String;
         Desc  : String;
         Fg    : String := "";
         Bg    : String := "");
      --  Initialize one style, if it has never been initialized before.
      --  Otherwise, do nothing.

      procedure Init
        (Style : in out Style_Access;
         Name  : String;
         Desc  : String;
         Fg    : String := "";
         Bg    : String := "") is
      begin
         Style := Get_Or_Create_Style (Kernel, Name, False);

         if Style = null then
            Style := Get_Or_Create_Style (Kernel, Name, True);

            Style.Name := new String'(Name);
            Style.Description := new String'(Desc);

            if Fg /= "" then
               Set_Foreground (Style, Fg);
            end if;

            if Bg /= "" then
               Set_Background (Style, Bg);
            end if;
         end if;
      end Init;
   begin
      --  ??? Should we use the old preferences as reference ?
      Init (Search_Results_Style,
            -"Search results",
            -"Color used to highlight the search results",
            Bg => "light blue");

      Init (Builder_Errors_Style,
            -"Builder results",
            -"Color used to highlight the build errors",
            Bg => "red");

      Init (Builder_Warnings_Style,
            -"Builder warnings",
            -"Color used to highlight the build warnings",
            Bg => "orange");

      Init (Builder_Style_Style,
            -"Style errors",
            -"Color used to highlight the style errors",
            Bg => "yellow");

      Init (Builder_Shadow_Style,
            -"Syntax check",
            -"Color used to highlight the build errors in background builds",
            Bg => "light grey");

      Add_Hook (Kernel, Preferences_Changed_Hook, Preferences_Changed'Access);
   end Initialize_Predefined_Styles;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Set_Background
        (Search_Results_Style,
         Get_Pref (Kernel, Search_Results_Color));

      Set_Background
        (Builder_Errors_Style,
         Get_Pref (Kernel, Error_Src_Highlight));

      Set_Background
        (Builder_Warnings_Style,
         Get_Pref (Kernel, Warning_Src_Highlight));

      Set_Background
        (Builder_Style_Style,
         Get_Pref (Kernel, Style_Src_Highlight));
   end Preferences_Changed;

   --------------------
   -- Allocate_Color --
   --------------------

   procedure Allocate_Color (Color : in out Color_Record) is
      Success : Boolean;
      Widget  : Gtk_Widget;
      Tops    : Gtk.Widget.Widget_List.Glist;
   begin
      if Color.Value = null
        or else Color.Value.all = ""
      then
         Trace (Me, "Color field not filled");
         return;
      end if;

      begin
         Color.Color := Parse (Color.Value.all);
      exception
         when Wrong_Color =>
            Trace (Me, "Could not parse color " & Color.Value.all);
            return;
      end;

      Alloc_Color (Get_Default_Colormap, Color.Color, False, True, Success);

      if not Success then
         Trace (Me, "Could not allocate color " & Color.Value.all);
         return;
      end if;

      Tops := List_Toplevels;
      Widget := Widget_List.Get_Data (Tops);
      Widget_List.Free (Tops);

      if Widget = null
        or else not Realized_Is_Set (Widget)
      then
         Trace (Me, "Cannot create GC: toplevel window not realized");
         return;
      end if;

      Gdk_New (Color.GC, Get_Window (Widget));
      Set_Foreground (Color.GC, Color.Color);
   end Allocate_Color;

   -----------
   -- Reset --
   -----------

   procedure Reset (X : access Style_Htable_Record) is
   begin
      Reset (X.Table);
   end Reset;

   -----------------------
   -- Get_Foreground_GC --
   -----------------------

   function Get_Foreground_GC (Style : Style_Access) return Gdk.GC.Gdk_GC is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return Null_GC;
      end if;

      if Style.Foreground.GC = Null_GC then
         Allocate_Color (Style.Foreground);
      end if;

      return Style.Foreground.GC;
   end Get_Foreground_GC;

   --------------------------
   -- Get_Foreground_Color --
   --------------------------

   function Get_Foreground_Color (Style : Style_Access) return Gdk_Color is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return Null_Color;
      end if;

      if Style.Foreground.Color = Null_Color then
         Allocate_Color (Style.Foreground);
      end if;

      return Style.Foreground.Color;
   end Get_Foreground_Color;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   function Get_Background_GC (Style : Style_Access) return Gdk.GC.Gdk_GC is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return Null_GC;
      end if;

      if Style.Background.GC = null then
         Allocate_Color (Style.Background);
      end if;

      return Style.Background.GC;
   end Get_Background_GC;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   function Get_Background_Color (Style : Style_Access) return Gdk_Color is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return Null_Color;
      end if;

      if Style.Background.Color = Null_Color then
         Allocate_Color (Style.Background);
      end if;

      return Style.Background.Color;
   end Get_Background_Color;

   -----------------
   -- Save_Styles --
   -----------------

   procedure Save_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File)
   is
      Iter : Iterator;
      F    : File_Type;
      Info : Style_Access;

      procedure Write_Style (Style : Style_Access; Name : String);
      --  Write XML node for Style with name Name to F.

      procedure Write_Style (Style : Style_Access; Name : String) is
         function V (S : String_Access) return String;
         pragma Inline (V);

         function V (S : String_Access) return String is
         begin
            if S = null then
               return "";
            end if;
            return S.all;
         end V;

      begin
         --  ??? This implies that fields have to be xml-friendly.
         Put_Line
           (F, "   <style>"
            & ASCII.LF & "      <name>" & Name & "</name>"
            & ASCII.LF & "      <desc>" & V (Style.Description) & "</desc>"
            & ASCII.LF & "      <fg>" & V (Style.Foreground.Value) & "</fg>"
            & ASCII.LF & "      <bg>" & V (Style.Background.Value) & "</bg>"
            & ASCII.LF & "   </style>");
      end Write_Style;

   begin
      Create (F, Out_File, Full_Name (File).all);
      Put_Line (F, "<?xml version=""1.0""?>");
      New_Line (F);
      Put_Line (F, "<Styles>");
      New_Line (F);

      Get_First (Style_Htable_Access (Kernel.Styles).Table, Iter);

      loop
         Info := Get_Element (Iter);
         exit when Info = null;

         Write_Style (Info, Get_Key (Iter));

         Get_Next (Style_Htable_Access (Kernel.Styles).Table, Iter);
      end loop;

      New_Line (F);
      Put_Line (F, "</Styles>");
   end Save_Styles;

   -----------------
   -- Load_Styles --
   -----------------

   procedure Load_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File)
   is
      F, Node : Node_Ptr;
      Err     : GNAT.OS_Lib.String_Access;

      procedure Read_Style (N : Node_Ptr);
      --  Read one Style from N.

      procedure Read_Style (N : Node_Ptr) is
         S     : Style_Record;
         Style : Style_Access;
      begin
         S.Name := new String'(Get_Field (N, "name").all);
         S.Description := new String'(Get_Field (N, "desc").all);
         S.Foreground.Value := new String'(Get_Field (N, "fg").all);
         S.Background.Value := new String'(Get_Field (N, "bg").all);

         Style := Get_Or_Create_Style (Kernel, S.Name.all, True);
         Style.all := S;

      exception
         when E : others =>
            Trace (Exception_Handle,
                   "Unexpected exception: " & Exception_Information (E));
      end Read_Style;

   begin
      if Kernel.Styles = null then
         Kernel.Styles := new Style_Htable_Record;
      end if;

      if Is_Regular_File (File) then
         XML_Parsers.Parse (Full_Name (File).all, F, Err);

         if F /= null then
            Node := F.Child;

            if F.Tag.all = "Styles" then
               Trace (Me, "Loading styles");

               while Node /= null loop
                  if Node.Tag.all = "style" then
                     Read_Style (Node);
                  end if;

                  Node := Node.Next;
               end loop;
            end if;

         else
            Trace (Me, "Error while parsing styles file " & Err.all);
            GNAT.OS_Lib.Free (Err);
         end if;

         Free (F);
      end if;

      Initialize_Predefined_Styles (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Load_Styles;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Style_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Style_Record, Style_Access);
   begin
      if 1 = 1 then
         return;
      end if;
      Free (Style.Name);
      Free (Style.Description);
      Free (Style.Foreground);
      Free (Style.Background);
      Unchecked_Free (Style);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Color : in out Color_Record) is
   begin
      Free (Color.Value);
   end Free;

   -------------------------
   -- Get_Or_Create_Style --
   -------------------------

   function Get_Or_Create_Style
     (Kernel : Kernel_Handle;
      Name   : String;
      Create : Boolean := True) return Style_Access
   is
      Style : Style_Access;
   begin
      Style := Get (Style_Htable_Access (Kernel.Styles).Table, Name);

      if Style = null
        and then Create
      then
         Style := new Style_Record;
         Style.Name := new String'(Name);
         Set (Style_Htable_Access (Kernel.Styles).Table, Name, Style);
      end if;

      return Style;
   end Get_Or_Create_Style;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Style : Style_Access; Color : String) is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return;
      end if;

      if Style.Foreground.Value /= null then
         Free (Style.Foreground.Value);
      end if;

      if Style.Foreground.GC /= Null_GC then
         Unref (Style.Foreground.GC);
      end if;

      Style.Foreground.Color := Null_Color;
      Style.Foreground.Value := new String'(Color);
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Style : Style_Access; Color : String) is
   begin
      if Style = null then
         Trace (Me, "Trying to access null style");
         return;
      end if;

      if Style.Background.Value /= null then
         Free (Style.Background.Value);
      end if;

      if Style.Background.GC /= Null_GC then
         Unref (Style.Background.GC);
      end if;

      Style.Background.Color := Null_Color;
      Style.Background.Value := new String'(Color);
   end Set_Background;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Style : Style_Access) return String is
   begin
      if Style /= null
        and then Style.Name /= null
      then
         return Style.Name.all;
      end if;

      return "";
   end Get_Name;

end GPS.Kernel.Styles;
