separate (Src_Info.CPP)


------------------------
--  Fu_To_Mi_Handler  --
------------------------

procedure Fu_To_Mi_Handler (Ref : TO_Table) is
   P            : Pair_Ptr;
   Init         : Boolean := True;
   Decl_Info    : E_Declaration_Info_List;
   Overloaded   : Boolean := False;
   Method       : MI_Table;
   Ptr          : E_Declaration_Info_List;
begin
   Info ("Fu_To_Mi_Handler: """
         & Ref.Buffer (Ref.Referred_Class.First .. Ref.Referred_Class.Last)
         & "."
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   --  Lookup all overloaded methods in the same class
   Set_Cursor
     (SN_Table (MI),
      By_Key,
      Ref.Buffer (Ref.Referred_Class.First .. Ref.Referred_Class.Last)
      & Field_Sep &
      Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                  Ref.Referred_Symbol_Name.Last) & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (MI), Next_By_Key);
      exit when P = null;
      Overloaded := not Init;
      if Init then
         Method := Parse_Pair (P.all);
         Init   := False;
      end if;
      Free (P);
      exit when Overloaded; -- once we get to know it, quit...
   end loop;

   if not Overloaded then
      --  If method
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      begin
         if Method.Buffer (Method.File_Name.First .. Method.File_Name.Last)
            = Get_LI_Filename (Global_LI_File) then
            --  this is a function defined in the current file
            Decl_Info := Find_Declaration
              (Global_LI_File,
               Method.Buffer (Method.Name.First .. Method.Name.Last),
               Method.Buffer (Method.Class.First .. Method.Class.Last),
               Method.Start_Position);
         else
            --  this function is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (Global_LI_File,
               Method.Buffer (Method.Name.First .. Method.Name.Last),
               Method.Buffer (Method.Class.First .. Method.Class.Last),
               Method.Buffer (Method.File_Name.First .. Method.File_Name.Last),
               Method.Start_Position);
         end if;
      exception
         when Declaration_Not_Found =>
            pragma Assert (False, "How did we get here?");
            null;
      end;
   else -- overloaded entity
      --  have we already declared it?
      Ptr := Global_LI_File.LI.Body_Info.Declarations;
      while Ptr /= null loop
         Decl_Info := Ptr;
         --  ??? what should we do with class names here?
         --  can confuse global functions and class methods
         --  without matching class name
         exit when Ptr.Value.Declaration.Kind = Overloaded_Entity
            and then Ptr.Value.Declaration.Name.all =
               Method.Buffer (Method.Name.First .. Method.Name.Last);
         Ptr := Ptr.Next;
      end loop;
      if Ptr = null then -- no, we have not. Do it now
         Decl_Info := new E_Declaration_Info_Node'
           (Value =>
              (Declaration => No_Declaration,
               References => null),
            Next => Global_LI_File.LI.Body_Info.Declarations);
         Decl_Info.Value.Declaration.Name :=
            new String'(Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last));
         Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
         Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
      end if;
   end if;

   Free (Method);

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("Method " &
            Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last) &
            " not found");
   return;
end Fu_To_Mi_Handler;

