separate (Src_Info.CPP)


------------------------
--  Fu_To_Fu_Handler  --
------------------------

procedure Fu_To_Fu_Handler (Ref : TO_Table) is
   P            : Pair_Ptr;
   Fn           : FU_Table;
   Decl_Info    : E_Declaration_Info_List;
   Overloaded   : Boolean := False;
   Init         : Boolean := True;
   Ptr          : E_Declaration_Info_List;
begin
   Info ("Fu_To_Fu_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   Set_Cursor
     (SN_Table (FU),
      By_Key,
      Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                  Ref.Referred_Symbol_Name.Last) & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (FU), Next_By_Key);
      exit when P = null;
      Overloaded := not Init;
      if Init then
         Fn   := Parse_Pair (P.all);
         Init := False;
      end if;
      Free (P);
      exit when Overloaded;
   end loop;

   if not Overloaded then
      --  If procedure
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      begin
         if Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last)
            = Get_LI_Filename (Global_LI_File) then
            --  this is a function defined in the current file
            Decl_Info := Find_Declaration
              (Global_LI_File,
               Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
               "",
               Fn.Start_Position);
         else
            --  this function is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (Global_LI_File,
               Fn.Buffer (Fn.Name.First .. Fn.Name.Last),
               "",
               Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last),
               Fn.Start_Position);
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
         exit when Ptr.Value.Declaration.Kind = Overloaded_Entity
            and then Ptr.Value.Declaration.Name.all =
               Fn.Buffer (Fn.Name.First .. Fn.Name.Last);
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
   Free (Fn);

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("Function " &
            Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last) &
            " not found");
   return;
end Fu_To_Fu_Handler;

