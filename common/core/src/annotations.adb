------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body Annotations is

   ----------
   -- Free --
   ----------

   procedure Free (Annot : in out Annotation) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (General_Annotation_Record'Class, General_Annotation);
   begin
      if Annot.Kind = Other_Kind then
         Free (Annot.Other_Val.all);
         Internal (Annot.Other_Val);
      end if;
   end Free;

   ------------------------------------
   -- Create_Annotation_Key_Registry --
   ------------------------------------

   function Create_Annotation_Key_Registry return Annotation_Key_Registry is
   begin
      return new Annotation_Key_Registry_Record;
   end Create_Annotation_Key_Registry;

   ----------
   -- Free --
   ----------

   procedure Free (Registry : in out Annotation_Key_Registry) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Annotation_Key_Registry_Record, Annotation_Key_Registry);
   begin
      Internal (Registry);
   end Free;

   ------------------------
   -- Get_Annotation_Key --
   ------------------------

   procedure Get_Annotation_Key
     (Registry : in out Annotation_Key_Registry;
      New_Key  : out Annotation_Key)
   is
   begin
      Registry.Last_Key := Registry.Last_Key + 1;
      New_Key := Registry.Last_Key;
   end Get_Annotation_Key;

   ----------
   -- Free --
   ----------

   procedure Free
     (Container : in out Annotation_Container)
   is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Annotation_Array, Annotation_Array_Access);
   begin
      if Container.Annotations /= null then
         for J in Container.Annotations'Range loop
            if Container.Annotations (J).Kind = Other_Kind then
               Free (Container.Annotations (J));
            end if;
         end loop;

         Internal (Container.Annotations);
      end if;
   end Free;

   --------------------
   -- Get_Annotation --
   --------------------

   procedure Get_Annotation
     (Container : Annotation_Container;
      Key       : Annotation_Key;
      Result    : out Annotation)
   is
   begin
      if Container.Annotations = null
        or else Container.Annotations'Last < Key
      then
         Result := Null_Annotation;
      else
         Result := Container.Annotations (Key);
      end if;
   end Get_Annotation;

   --------------------
   -- Set_Annotation --
   --------------------

   procedure Set_Annotation
     (Container : in out Annotation_Container;
      Key       : Annotation_Key;
      Value     : Annotation)
   is
      Tmp : Annotation_Array_Access;

      procedure Free_Pointer is new Ada.Unchecked_Deallocation
        (Annotation_Array, Annotation_Array_Access);
   begin
      if Container.Annotations = null then
         Container.Annotations := new Annotation_Array (1 .. Key);
         Container.Annotations.all := (others => (Kind => Nothing));
      elsif Container.Annotations'Last < Key then
         Tmp := new Annotation_Array (1 .. Key);
         Tmp (1 .. Container.Annotations'Last) := Container.Annotations.all;
         Tmp (Container.Annotations'Last  + 1 .. Tmp'Last) :=
           (others => (Kind => Nothing));
         Free_Pointer (Container.Annotations);
         Container.Annotations := Tmp;
      end if;

      Free (Container.Annotations (Key));
      Container.Annotations (Key) := Value;
   end Set_Annotation;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Container : Annotation_Container; Key : Annotation_Key)
         return Boolean
   is
   begin
      return Container.Annotations /= null
        and then Container.Annotations'Last >= Key
        and then Container.Annotations (Key).Kind /= Nothing;
   end Is_Set;

   ---------------------
   -- Free_Annotation --
   ---------------------

   procedure Free_Annotation
     (Container : Annotation_Container; Key : Annotation_Key) is
   begin
      if Container.Annotations /= null
        and then Key in Container.Annotations'Range
      then
         Free (Container.Annotations (Key));
         Container.Annotations (Key) := Null_Annotation;
      end if;
   end Free_Annotation;

   ----------
   -- Copy --
   ----------

   function Copy (Obj : Annotation) return Annotation is
      C : Annotation := Obj;
   begin
      if C.Kind = Other_Kind then
         C.Other_Val := new General_Annotation_Record'Class'(C.Other_Val.all);
      end if;

      return C;
   end Copy;

end Annotations;
