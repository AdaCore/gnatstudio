separate (Src_Info.CPP)

------------------------
--  Fu_To_E_Handler  --
------------------------

procedure Fu_To_E_Handler (Ref : TO_Table) is
   Ref_Id : String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
begin
   Info ("Fu_To_E_Handler: """ & Ref_Id & """");
end Fu_To_E_Handler;

