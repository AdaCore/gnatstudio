with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;


package body Html_Output is

   package ASU renames Ada.Strings.Unbounded;
   package ASM renames Ada.Strings.Maps;
   package ASF renames Ada.Strings.Fixed;


   ------------------------
   -- Doc_HTML_Subtitle --
   ------------------------

   procedure Doc_HTML_Subtitle
     (File        : in Ada.Text_IO.File_Type;
      Title       : in String) is
   begin
      Ada.Text_IO.Put_Line (File, "<br>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<h3> <font color=""blue"" >"  & Title & " </font> </h3>");
      Ada.Text_IO.New_Line (File);
   end;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (File : in Ada.Text_IO.File_Type;
      Name        : in String;
      Rename      : in String;
      Description : in String) is
   begin

      Ada.Text_IO.Put_Line (File, " <b> " & Name & "</b> ");
      Ada.Text_IO.Put_Line (File, Rename);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br>");
      Ada.Text_IO.Put_Line (File, "---" & Description);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br> <br>");
      Ada.Text_IO.New_Line (File);

   end Doc_HTML_Exception;



   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
      (File       : in Ada.Text_IO.File_Type;
      Name        : in String;
      Type_Name   : in String;
      Description : in String) is
   begin
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br>");
      Ada.Text_IO.Put_Line (File, " <b> " & Name & "</b>  is ");
      Ada.Text_IO.Put_Line (File, Type_Name);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br>");
      Ada.Text_IO.Put_Line (File, "---" & Description);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br> <br>");
      Ada.Text_IO.New_Line (File);

   end Doc_HTML_Type;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
     (File : in Ada.Text_IO.File_Type;
      Name        : in String;
      Type_Name   : in String;
      Description : in String) is
   begin

      Ada.Text_IO.Put_Line (File, Type_Name & " ");
      Ada.Text_IO.Put_Line (File, " <b> " & Name & "</b> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br>");
      Ada.Text_IO.Put_Line (File, "---" & Description);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<br> <br>");
      Ada.Text_IO.New_Line (File);

   end Doc_HTML_Subprogram;


   ------------
   -- Header --
   ------------

   procedure Header
     (File  : in Ada.Text_IO.File_Type;
      Title : in String)
   is
   begin
      --Ada.Text_IO.Put_Line
      -- (File,
      --   "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01 Transitional//EN"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HTML>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<TITLE>" & Title & "</TITLE>"); --HTMLize
      Ada.Text_IO.Put_Line
        (File,
         "<META NAME=""generator"" CONTENT=""DocGen ");
      --if ASU.Length (Char_Set) /= 0 then
         Ada.Text_IO.Put_Line
           (File,
            "<META http-equiv=""Content-Type"" content=""" &
            "text/html; charset=" & "ISO-8859-1" & """>");
      --end if;
      --Ada.Text_IO.Put_Line
      --  (File, "<LINK REV=""made"" HREF=""" & AD.Version.Get_URL & """>");
      --declare
      --   S : String := ASU.To_String (Style_Sheet);
      --begin
      --   if S'Length > 0 then
      --      Ada.Text_IO.Put_Line
      --        (File,
      --         "<LINK REL=""stylesheet"" HREF=""" & S &
      --         """ TYPE=""text/css"">");
      --   end if;
      --end;

      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.New_Line (File);
      --Ada.Text_IO.Put_Line (File, ASU.To_String (Body_Start));
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.Put_Line (File, " <h1>   <font color=""blue"" > Package </font> <i>" & Title & " </i> </h1>");
      --Ada.Text_IO.Put_Line
      --  (File,
      --   ASU.To_String (AD.HTML.Title (Before)) & HTMLize (Title) &
      --   ASU.To_String (AD.HTML.Title (After)));
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<hr>");
      Ada.Text_IO.New_Line (File);
   end Header;


   ------------
   -- Footer --
   ------------

   procedure Footer
     (File : in Ada.Text_IO.File_Type)
   is
   begin
      Ada.Text_IO.Put_Line (File, "</BODY>");
      Ada.Text_IO.Put_Line (File, "</HTML>");
   end Footer;


end Html_Output;
