-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2005                      --
--                              AdaCore                              --
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

with Language;                  use Language;
with Language.Unknown;          use Language.Unknown;
with Entities;                  use Entities;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Types;                     use Types;
with Projects;                  use Projects;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;
with VFS;                       use VFS;
with Case_Handling;             use Case_Handling;
with GNAT.Bubble_Sort_G;

package body Language_Handlers is

   Me : constant Debug_Handle := Create ("Language_Handlers");

   function Get_Index_From_Language
     (Handler       : access Language_Handler_Record'Class;
      Language_Name : String) return Natural;
   --  Return the index of Language in Handler.Languages, or 0 if no such
   --  language is known.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Info_Array, Language_Info_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Handler_Info_Array, Handler_Info_Access);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handler : out Language_Handler) is
   begin
      --  ??? Never freed, but the handler is never destroyed.
      Handler := new Language_Handler_Record;
   end Gtk_New;

   ------------------
   -- Set_Registry --
   ------------------

   procedure Set_Registry
     (Handler  : access Language_Handler_Record;
      Registry : access Projects.Abstract_Registry'Class) is
   begin
      Handler.Registry := Abstract_Registry_Access (Registry);
   end Set_Registry;

   -----------------------------
   -- Get_Index_From_Language --
   -----------------------------

   function Get_Index_From_Language
     (Handler       : access Language_Handler_Record'Class;
      Language_Name : String) return Natural
   is
      Lang : constant String := To_Lower (Language_Name);
   begin
      Assert (Me, Handler.Languages /= null, "No registered language");

      for Index in Handler.Languages'Range loop
         if To_Lower (Get_Name (Handler.Languages (Index).Lang)) = Lang then
            return Index;
         end if;
      end loop;
      return 0;
   end Get_Index_From_Language;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler         : access Language_Handler_Record;
      Source_Filename : VFS.Virtual_File) return Language.Language_Access
   is
      Index : Natural;
   begin
      Index := Get_Index_From_Language
        (Handler,
         Get_Language_From_File (Handler, Source_Filename));
      if Index /= 0 then
         return Handler.Languages (Index).Lang;
      end if;

      return Unknown_Lang;
   end Get_Language_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler : access Language_Handler_Record;
      Source_Filename : VFS.Virtual_File) return String
   is
      Lang : constant Name_Id := Get_Language_From_File_From_Project
        (Project_Registry'Class (Handler.Registry.all),
         Source_Filename);
   begin
      if Lang = No_Name then
         return "";
      else
         return Get_String (Lang);
      end if;
   end Get_Language_From_File;

   --------------------------
   -- Get_Language_By_Name --
   --------------------------

   function Get_Language_By_Name
     (Handler : access Language_Handler_Record;
      Name    : String) return Language.Language_Access
   is
      Index : constant Natural := Get_Index_From_Language (Handler, Name);
   begin
      if Index = 0 then
         return Unknown_Lang;
      else
         return Handler.Languages (Index).Lang;
      end if;
   end Get_Language_By_Name;

   ----------------------------
   -- Get_LI_Handler_By_Name --
   ----------------------------

   function Get_LI_Handler_By_Name
     (Handler : access Language_Handler_Record;
      Name    : String) return LI_Handler
   is
      Index : constant Natural := Get_Index_From_Language (Handler, Name);
   begin
      if Index = 0 then
         return null;
      else
         return Handler.Languages (Index).Handler;
      end if;
   end Get_LI_Handler_By_Name;

   -----------------------
   -- Register_Language --
   -----------------------

   procedure Register_Language
     (Handler : access Language_Handler_Record;
      Lang    : access Language.Language_Root'Class;
      LI      : LI_Handler)
   is
      N     : constant String := To_Lower (Get_Name (Lang));
      Tmp   : Language_Info_Access;
      Tmp2  : Handler_Info_Access;
      Index : Natural;
   begin
      if Handler.Languages /= null then
         Index := Get_Index_From_Language (Handler, N);
         if Index = 0 then
            Tmp := new Language_Info_Array
              (Handler.Languages'First .. Handler.Languages'Last + 1);
            Tmp (Handler.Languages'Range) := Handler.Languages.all;
            Unchecked_Free (Handler.Languages);
            Handler.Languages := Tmp;
            Index := Handler.Languages'Last;
         end if;
      else
         Handler.Languages := new Language_Info_Array (1 .. 1);
         Index := Handler.Languages'Last;
      end if;

      Handler.Languages (Index) :=
        (Handler => LI,
         Lang    => Language_Access (Lang));

      --  If the name is "", this is a dummy LI handler and we do not need to
      --  register it explicitly
      if LI /= null and then Get_Name (LI) /= "" then
         if Handler.Handlers /= null then
            Index := 0;
            for H in Handler.Handlers'Range loop
               if Handler.Handlers (H) = LI then
                  Index := H;
                  exit;
               end if;
            end loop;

            if Index = 0 then
               Tmp2 := new Handler_Info_Array
                 (Handler.Handlers'First .. Handler.Handlers'Last + 1);
               Tmp2 (Handler.Handlers'Range) := Handler.Handlers.all;
               Unchecked_Free (Handler.Handlers);
               Handler.Handlers := Tmp2;
               Index := Handler.Handlers'Last;
            end if;
         else
            Handler.Handlers := new Handler_Info_Array (1 .. 1);
            Index := Handler.Handlers'Last;
         end if;

         Handler.Handlers (Index) := LI;
      end if;
   end Register_Language;

   ---------------------
   -- Known_Languages --
   ---------------------

   function Known_Languages
     (Handler : access Language_Handler_Record;
      Sorted  : Boolean := False) return GNAT.OS_Lib.Argument_List is
   begin
      if Handler.Languages /= null then
         declare
            Result : Argument_List (1 .. Handler.Languages'Last);

            procedure Move (From, To : Natural);
            function Lt (From, To : Natural) return Boolean;
            package Sort is new GNAT.Bubble_Sort_G (Move => Move, Lt => Lt);

            Tmp : GNAT.OS_Lib.String_Access;

            procedure Move (From, To : Natural) is
            begin
               if From = 0 then
                  Result (To) := Tmp;
               elsif To = 0 then
                  Tmp := Result (From);
               else
                  Result (To) := Result (From);
               end if;
            end Move;

            function Lt (From, To : Natural) return Boolean is
            begin
               return Result (From).all < Result (To).all;
            end Lt;

         begin
            for Index in Result'Range loop
               Result (Index) := new String'
                 (Get_Name (Handler.Languages
                  (Index - 1 + Handler.Languages'First).Lang));
               Mixed_Case (Result (Index).all);
            end loop;

            if Sorted then
               Sort.Sort (Result'Last);
            end if;

            return Result;
         end;
      else
         declare
            Result : Argument_List (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Known_Languages;

   ------------------------------
   -- Get_LI_Handler_From_File --
   ------------------------------

   function Get_LI_Handler_From_File
     (Handler         : access Language_Handler_Record;
      Source_Filename : VFS.Virtual_File)
      return LI_Handler
   is
      Lang : constant String :=
        Get_Language_From_File (Handler, Source_Filename);
      Index : constant Natural := Get_Index_From_Language (Handler, Lang);
   begin
      if Index /= 0
        and then Handler.Languages (Index).Handler /= null
      then
         return Handler.Languages (Index).Handler;
      else
         if Index /= 0 then
            Trace (Me, "No LI_Handler for language "
                   & Full_Name (Source_Filename).all
                   & " Index=" & Index'Img & " lang=" & Lang);
         end if;

         return null;
      end if;
   end Get_LI_Handler_From_File;

   ---------------------
   -- Languages_Count --
   ---------------------

   function Languages_Count (Handler : access Language_Handler_Record)
      return Natural is
   begin
      if Handler.Languages = null then
         return 0;
      else
         return Handler.Languages'Length;
      end if;
   end Languages_Count;

   -----------------------
   -- LI_Handlers_Count --
   -----------------------

   function LI_Handlers_Count (Handler : access Language_Handler_Record)
      return Natural is
   begin
      if Handler.Handlers = null then
         return 0;
      else
         return Handler.Handlers'Length;
      end if;
   end LI_Handlers_Count;

   ---------------------
   -- Get_Nth_Handler --
   ---------------------

   function Get_Nth_Handler
     (Handler : access Language_Handler_Record;
      Num     : Positive) return LI_Handler is
   begin
      if Handler.Handlers = null
        or else Num > Handler.Handlers'Length
      then
         return null;
      else
         return Handler.Handlers (Handler.Handlers'First + Num - 1);
      end if;
   end Get_Nth_Handler;

   ----------------------
   -- Get_Nth_Language --
   ----------------------

   function Get_Nth_Language
     (Handler : access Language_Handler_Record;
      Num     : Positive) return String is
   begin
      if Handler.Languages = null
        or else Num > Handler.Languages'Length
      then
         return "";
      else
         return Get_Name (Handler.Languages
           (Handler.Languages'First + Num - 1).Lang);
      end if;
   end Get_Nth_Language;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out Language_Handler) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Language_Handler_Record'Class, Language_Handler);
   begin
      if Handler.Languages /= null then
         for L in Handler.Languages'Range loop
            Free (Handler.Languages (L).Lang);
         end loop;

         Unchecked_Free (Handler.Languages);
      end if;

      if Handler.Handlers /= null then
         for H in Handler.Handlers'Range loop
            Destroy (Handler.Handlers (H));
         end loop;

         Unchecked_Free (Handler.Handlers);
      end if;

      Unchecked_Free (Handler);
   end Destroy;

end Language_Handlers;
