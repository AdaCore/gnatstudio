with String_Utils; use String_Utils;
with Unchecked_Deallocation;

package body Src_Info is

   procedure Free is new Unchecked_Deallocation (File_Info, File_Info_Ptr);
   procedure Free is new Unchecked_Deallocation (LI_File, LI_File_Ptr);
   procedure Free is new
     Unchecked_Deallocation (LI_File_Node, LI_File_Node_Ptr);
   procedure Free is new
     Unchecked_Deallocation (E_Reference_Node, E_Reference_List);
   procedure Free is new
     Unchecked_Deallocation (E_Declaration_Info_Node, E_Declaration_Info_List);
   procedure Free is new
     Unchecked_Deallocation (File_Info_Ptr_Node, File_Info_Ptr_List);
   procedure Free is new
     Unchecked_Deallocation
       (Dependency_File_Info_Node, Dependency_File_Info_List);
   --  Memory deallocation routines.

   function Hash is new HTables.Hash (LI_File_HTable_Index);
   --  Hash function for strings.

   function Get_Separate_File_Info
     (LIF : LI_File_Ptr; Unit_Name : String_Access) return File_Info_Ptr;
   --  Return a pointer to the file info which Unit_Name matches the given
   --  Unit_Name. Return null if such unit could not be found.

   ----------------------------
   -- Get_Separate_File_Info --
   ----------------------------

   function Get_Separate_File_Info
     (LIF : LI_File_Ptr; Unit_Name : String_Access) return File_Info_Ptr
   is
      Current_Node : File_Info_Ptr_List := LIF.Separate_Info;
   begin
      while Current_Node /= null loop
         if Current_Node.Value.Unit_Name.all = Unit_Name.all then
            return Current_Node.Value;
         end if;
      end loop;
      --  If we reach this point, this means that the File_Info was not found.
      return null;
   end Get_Separate_File_Info;

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Source_Info : LI_File_Ptr) return Boolean is
   begin
      return not Source_Info.Parsed;
   end Is_Incomplete;

   -----------
   -- Reset --
   -----------

   procedure Reset (LIFL : in out LI_File_List) is
   begin
      Reset (LIFL.Table);
   end Reset;

   ------------
   -- Locate --
   ------------

   function Locate
     (List : LI_File_List;
      LI_Filename : String)
      return LI_File_Ptr is
   begin
      return Get (List.Table, LI_Filename);
   end Locate;

   ------------------------
   -- Locate_From_Source --
   ------------------------

   function Locate_From_Source
     (List            : LI_File_List;
      Source_Filename : String)
      return LI_File_Ptr
   is
      Short_Filename : constant String := Base_File_Name (Source_Filename);
      Current_LI     : LI_File_Ptr;
      Current_Sep    : File_Info_Ptr_List;
      Table : LI_File_HTable.HTable := List.Table;
      --  ??? Make a copy of the table since Get_First and Get_Next need
      --  ??? a Read/Writable HTable. This is temporary since we should stop
      --  ??? using Get_First/Next soon. See ??? comment below.
   begin
      --  ??? The best way of doing this is to convert the filename into the
      --  ??? Library Info filename, and then use the Htable to retrieve the
      --  ??? LI_File. This poses a few problems because the conversion is
      --  ??? language dependent. We might want to play with dispatching
      --  ??? using Language.* later on. For the moment, we do a brutal
      --  ??? search; that'll do for now, and it works fast enough on small
      --  ??? projects.
      Get_First (Table, Current_LI);
      LI_File_Loop :
      while Current_LI /= null loop
         --  See if the filename matches the spec filename
         if Current_LI.Spec_Info /= null
           and then Current_LI.Spec_Info.Source_Filename.all = Short_Filename
         then
            return Current_LI;
         end if;

         --  Check if the filename matches the body filename
         if Current_LI.Body_Info /= null
           and then Current_LI.Body_Info.Source_Filename.all = Short_Filename
         then
            return Current_LI;
         end if;

         --  Finally, check the filenames of the separates
         Current_Sep := Current_LI.Separate_Info;
         Separate_Loop :
         while Current_Sep /= null loop
            if Current_Sep.Value.Source_Filename.all = Short_Filename then
               return Current_LI;
            end if;
            Current_Sep := Current_Sep.Next;
         end loop Separate_Loop;

         --  This LI_File does not match, try the next one in the table...
         Get_Next (Table, Current_LI);
      end loop LI_File_Loop;

      --  If we reach this point, then there is not matching LI_File
      return null;
   end Locate_From_Source;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Source_File) return Boolean is
   begin
      if Left.LI = Right.LI and then Left.Part = Right.Part then
         if Left.Unit_Name = null then
            return Right.Unit_Name = null;
         else
            return Right.Unit_Name /= null
              and then Left.Unit_Name.all = Right.Unit_Name.all;
         end if;
      else
         return False;
      end if;
   end "=";

   function "=" (Left, Right : File_Location) return Boolean is
   begin
      return Left.Line = Right.Line
        and then Left.Column = Right.Column
        and then Left.File = Right.File;
   end "=";

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : LI_File_Node_Ptr; Next : LI_File_Node_Ptr) is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (E : LI_File_Node_Ptr) return LI_File_Node_Ptr is
   begin
      return E.Next;
   end Next;

   ---------------------
   -- Get_LI_Filename --
   ---------------------

   function Get_LI_Filename (E : LI_File_Node_Ptr) return String_Access is
   begin
      return E.Value.LI_Filename;
   end Get_LI_Filename;

   ----------
   -- Hash --
   ----------

   function Hash (F : String_Access) return LI_File_HTable_Index is
   begin
      return Hash (F.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ---------
   -- Add --
   ---------

   procedure Add
     (HT      : in out LI_File_HTable.HTable;
      LIFP    : LI_File_Ptr;
      Success : out Boolean)
   is
      Tmp : constant LI_File_Node_Ptr :=
        LI_File_HTable.Get (HT, LIFP.LI_Filename);
   begin
      --  Make sure no LI_File with the same unit name already exists before
      --  inserting in the table.
      if Tmp /= null then
         Success := False;
         return;
      end if;
      LI_File_HTable.Set (HT, new LI_File_Node'(Value => LIFP, Next => null));
      Success := True;
   end Add;

   -----------
   -- Reset --
   -----------

   procedure Reset (HT : in out LI_File_HTable.HTable) is
      Current_Unit : LI_File_Node_Ptr;
      Next_Unit    : LI_File_Node_Ptr;
   begin
      --  Destroy all elements pointed by the hash-table...
      LI_File_HTable.Get_First (HT, Current_Unit);
      while Current_Unit /= null loop
         LI_File_HTable.Get_Next (HT, Next_Unit);
         Destroy (Current_Unit);
         Current_Unit := Next_Unit;
      end loop;
      --  And finally, reset the hash-table itself...
      LI_File_HTable.Reset (HT);
   end Reset;

   ---------
   -- Get --
   ---------

   function Get
     (HT : LI_File_HTable.HTable; LI_Filename : String) return LI_File_Ptr
   is
      Name : aliased String := LI_Filename;
      Node : constant LI_File_Node_Ptr :=
        LI_File_HTable.Get (HT, Name'Unchecked_Access);
   begin
      if Node = null then
         return No_LI_File;
      else
         return Node.Value;
      end if;
   end Get;

   ---------------
   -- Get_First --
   ---------------

   procedure Get_First
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr)
   is
      Node : LI_File_Node_Ptr;
   begin
      LI_File_HTable.Get_First (HT, Node);
      if Node = null then
         Result := No_LI_File;
      else
         Result := Node.Value;
      end if;
   end Get_First;

   --------------
   -- Get_Next --
   --------------

   procedure Get_Next
     (HT : in out LI_File_HTable.HTable; Result : out LI_File_Ptr)
   is
      Node : LI_File_Node_Ptr;
   begin
      LI_File_HTable.Get_Next (HT, Node);
      if Node = null then
         Result := No_LI_File;
      else
         Result := Node.Value;
      end if;
   end Get_Next;

   ----------------------
   -- Is_File_Location --
   ----------------------

   function Is_File_Location (Location : in File_Location) return Boolean is
   begin
      return Location.File.LI /= null;
   end Is_File_Location;

   -------------------------
   -- Get_Source_Filename --
   -------------------------

   function Get_Source_Filename (File : Source_File) return String is
      FI : constant File_Info_Ptr := Get_File_Info (File);
   begin
      return FI.Source_Filename.all;
   end Get_Source_Filename;

   -------------------
   -- Get_File_Info --
   -------------------

   function Get_File_Info (SF : Source_File) return File_Info_Ptr is
   begin
      case SF.Part is
         when Unit_Spec =>
            return SF.LI.Spec_Info;
         when Unit_Body =>
            return SF.LI.Body_Info;
         when Unit_Separate =>
            return Get_Separate_File_Info (SF.LI, SF.Unit_Name);
      end case;
   end Get_File_Info;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (LIF : in out LI_File) is
   begin
      Free (LIF.LI_Filename);
      Destroy (LIF.Spec_Info);
      Destroy (LIF.Body_Info);
      Destroy (LIF.Separate_Info);
      if LIF.Parsed then
         Destroy (LIF.Dependencies_Info);
      end if;
   end Destroy;

   procedure Destroy (LIFP : in out LI_File_Ptr) is
   begin
      if LIFP /= null then
         Destroy (LIFP.all);
         Free (LIFP);
      end if;
   end Destroy;

   procedure Destroy (SF : in out Source_File) is
   begin
      --  Do not deallocate SF.LI, we are just pointing to it, we did
      --  not allocate it for this object.
      Free (SF.Unit_Name);
   end Destroy;

   procedure Destroy (FL : in out File_Location) is
   begin
      Destroy (FL.File);
   end Destroy;

   procedure Destroy (ER : in out E_Reference) is
   begin
      Destroy (ER.Location);
   end Destroy;

   procedure Destroy (ERL : in out E_Reference_List)
   is
      Current_Node : E_Reference_List renames ERL;
      Next_Node    : E_Reference_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (ED : in out E_Declaration) is
   begin
      Free (ED.Name);
      Destroy (ED.Location);
      Destroy (ED.Parent_Location);
      Destroy (ED.End_Of_Scope);
   end Destroy;

   procedure Destroy (EDI : in out E_Declaration_Info) is
   begin
      Destroy (EDI.Declaration);
      Destroy (EDI.References);
   end Destroy;

   procedure Destroy (EDIL : in out E_Declaration_Info_List) is
      Current_Node : E_Declaration_Info_List renames EDIL;
      Next_Node    : E_Declaration_Info_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (FI : in out File_Info) is
   begin
      Free (FI.Unit_Name);
      Free (FI.Source_Filename);
      Free (FI.Original_Filename);
      Destroy (FI.Declarations);
   end Destroy;

   procedure Destroy (FIP : in out File_Info_Ptr) is
   begin
      if FIP /= null then
         Destroy (FIP.all);
         Free (FIP);
      end if;
   end Destroy;

   procedure Destroy (FIPL : in out File_Info_Ptr_List) is
      Current_Node : File_Info_Ptr_List renames FIPL;
      Next_Node    : File_Info_Ptr_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (DFI : in out Dependency_File_Info) is
   begin
      Destroy (DFI.File);
      Destroy (DFI.Declarations);
   end Destroy;

   procedure Destroy (DFIL : in out Dependency_File_Info_List)
   is
      Current_Node : Dependency_File_Info_List renames DFIL;
      Next_Node    : Dependency_File_Info_List;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   procedure Destroy (LIFNP : in out LI_File_Node_Ptr)
   is
      Current_Node : LI_File_Node_Ptr renames LIFNP;
      Next_Node    : LI_File_Node_Ptr;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Destroy (Current_Node.Value);
         Free (Current_Node);
         Current_Node := Next_Node;
      end loop;
   end Destroy;

   ----------
   -- Copy --
   ----------

   function Copy (SF : Source_File) return Source_File is
      Result : Source_File := SF;
   begin
      if SF.Unit_Name /= null then
         Result.Unit_Name := new String'(SF.Unit_Name.all);
      end if;
      return Result;
   end Copy;

end Src_Info;
