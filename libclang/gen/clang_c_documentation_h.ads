pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with clang_c_Index_h;
with clang_c_CXString_h;

package clang_c_Documentation_h is

   type CXComment is record
      ASTNode : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:38
      TranslationUnit : clang_c_Index_h.CXTranslationUnit;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:39
   end record;
   pragma Convention (C_Pass_By_Copy, CXComment);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:40

   --  skipped anonymous struct anon_42

   function clang_Cursor_getParsedComment (C : clang_c_Index_h.CXCursor) return CXComment;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:47
   pragma Import (C, clang_Cursor_getParsedComment, "clang_Cursor_getParsedComment");

   type CXCommentKind is 
     (CXComment_Null,
      CXComment_Text,
      CXComment_InlineCommand,
      CXComment_HTMLStartTag,
      CXComment_HTMLEndTag,
      CXComment_Paragraph,
      CXComment_BlockCommand,
      CXComment_ParamCommand,
      CXComment_TParamCommand,
      CXComment_VerbatimBlockCommand,
      CXComment_VerbatimBlockLine,
      CXComment_VerbatimLine,
      CXComment_FullComment);
   pragma Convention (C, CXCommentKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:54

   type CXCommentInlineCommandRenderKind is 
     (CXCommentInlineCommandRenderKind_Normal,
      CXCommentInlineCommandRenderKind_Bold,
      CXCommentInlineCommandRenderKind_Monospaced,
      CXCommentInlineCommandRenderKind_Emphasized);
   pragma Convention (C, CXCommentInlineCommandRenderKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:165

   type CXCommentParamPassDirection is 
     (CXCommentParamPassDirection_In,
      CXCommentParamPassDirection_Out,
      CXCommentParamPassDirection_InOut);
   pragma Convention (C, CXCommentParamPassDirection);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:191

   function clang_Comment_getKind (Comment : CXComment) return CXCommentKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:213
   pragma Import (C, clang_Comment_getKind, "clang_Comment_getKind");

   function clang_Comment_getNumChildren (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:220
   pragma Import (C, clang_Comment_getNumChildren, "clang_Comment_getNumChildren");

   function clang_Comment_getChild (Comment : CXComment; ChildIdx : unsigned) return CXComment;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:230
   pragma Import (C, clang_Comment_getChild, "clang_Comment_getChild");

   function clang_Comment_isWhitespace (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:241
   pragma Import (C, clang_Comment_isWhitespace, "clang_Comment_isWhitespace");

   function clang_InlineContentComment_hasTrailingNewline (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:249
   pragma Import (C, clang_InlineContentComment_hasTrailingNewline, "clang_InlineContentComment_hasTrailingNewline");

   function clang_TextComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:256
   pragma Import (C, clang_TextComment_getText, "clang_TextComment_getText");

   function clang_InlineCommandComment_getCommandName (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:264
   pragma Import (C, clang_InlineCommandComment_getCommandName, "clang_InlineCommandComment_getCommandName");

   function clang_InlineCommandComment_getRenderKind (Comment : CXComment) return CXCommentInlineCommandRenderKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:273
   pragma Import (C, clang_InlineCommandComment_getRenderKind, "clang_InlineCommandComment_getRenderKind");

   function clang_InlineCommandComment_getNumArgs (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:281
   pragma Import (C, clang_InlineCommandComment_getNumArgs, "clang_InlineCommandComment_getNumArgs");

   function clang_InlineCommandComment_getArgText (Comment : CXComment; ArgIdx : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:291
   pragma Import (C, clang_InlineCommandComment_getArgText, "clang_InlineCommandComment_getArgText");

   function clang_HTMLTagComment_getTagName (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:300
   pragma Import (C, clang_HTMLTagComment_getTagName, "clang_HTMLTagComment_getTagName");

   function clang_HTMLStartTagComment_isSelfClosing (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:308
   pragma Import (C, clang_HTMLStartTagComment_isSelfClosing, "clang_HTMLStartTagComment_isSelfClosing");

   function clang_HTMLStartTag_getNumAttrs (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:315
   pragma Import (C, clang_HTMLStartTag_getNumAttrs, "clang_HTMLStartTag_getNumAttrs");

   function clang_HTMLStartTag_getAttrName (Comment : CXComment; AttrIdx : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:325
   pragma Import (C, clang_HTMLStartTag_getAttrName, "clang_HTMLStartTag_getAttrName");

   function clang_HTMLStartTag_getAttrValue (Comment : CXComment; AttrIdx : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:335
   pragma Import (C, clang_HTMLStartTag_getAttrValue, "clang_HTMLStartTag_getAttrValue");

   function clang_BlockCommandComment_getCommandName (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:343
   pragma Import (C, clang_BlockCommandComment_getCommandName, "clang_BlockCommandComment_getCommandName");

   function clang_BlockCommandComment_getNumArgs (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:351
   pragma Import (C, clang_BlockCommandComment_getNumArgs, "clang_BlockCommandComment_getNumArgs");

   function clang_BlockCommandComment_getArgText (Comment : CXComment; ArgIdx : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:361
   pragma Import (C, clang_BlockCommandComment_getArgText, "clang_BlockCommandComment_getArgText");

   function clang_BlockCommandComment_getParagraph (Comment : CXComment) return CXComment;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:371
   pragma Import (C, clang_BlockCommandComment_getParagraph, "clang_BlockCommandComment_getParagraph");

   function clang_ParamCommandComment_getParamName (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:379
   pragma Import (C, clang_ParamCommandComment_getParamName, "clang_ParamCommandComment_getParamName");

   function clang_ParamCommandComment_isParamIndexValid (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:389
   pragma Import (C, clang_ParamCommandComment_isParamIndexValid, "clang_ParamCommandComment_isParamIndexValid");

   function clang_ParamCommandComment_getParamIndex (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:397
   pragma Import (C, clang_ParamCommandComment_getParamIndex, "clang_ParamCommandComment_getParamIndex");

   function clang_ParamCommandComment_isDirectionExplicit (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:406
   pragma Import (C, clang_ParamCommandComment_isDirectionExplicit, "clang_ParamCommandComment_isDirectionExplicit");

   function clang_ParamCommandComment_getDirection (Comment : CXComment) return CXCommentParamPassDirection;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:414
   pragma Import (C, clang_ParamCommandComment_getDirection, "clang_ParamCommandComment_getDirection");

   function clang_TParamCommandComment_getParamName (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:423
   pragma Import (C, clang_TParamCommandComment_getParamName, "clang_TParamCommandComment_getParamName");

   function clang_TParamCommandComment_isParamPositionValid (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:435
   pragma Import (C, clang_TParamCommandComment_isParamPositionValid, "clang_TParamCommandComment_isParamPositionValid");

   function clang_TParamCommandComment_getDepth (Comment : CXComment) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:451
   pragma Import (C, clang_TParamCommandComment_getDepth, "clang_TParamCommandComment_getDepth");

   function clang_TParamCommandComment_getIndex (Comment : CXComment; Depth : unsigned) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:472
   pragma Import (C, clang_TParamCommandComment_getIndex, "clang_TParamCommandComment_getIndex");

   function clang_VerbatimBlockLineComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:480
   pragma Import (C, clang_VerbatimBlockLineComment_getText, "clang_VerbatimBlockLineComment_getText");

   function clang_VerbatimLineComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:487
   pragma Import (C, clang_VerbatimLineComment_getText, "clang_VerbatimLineComment_getText");

   function clang_HTMLTagComment_getAsString (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:497
   pragma Import (C, clang_HTMLTagComment_getAsString, "clang_HTMLTagComment_getAsString");

   function clang_FullComment_getAsHTML (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:530
   pragma Import (C, clang_FullComment_getAsHTML, "clang_FullComment_getAsHTML");

   function clang_FullComment_getAsXML (Comment : CXComment) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Documentation.h:542
   pragma Import (C, clang_FullComment_getAsXML, "clang_FullComment_getAsXML");

end clang_c_Documentation_h;
