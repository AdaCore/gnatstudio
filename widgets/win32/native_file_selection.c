#include <windows.h>
#include <commdlg.h>
#include <stdlib.h>
#include <stdio.h>


/*static	const char*
get_message_name (UINT uiMsg)
{
	switch (uiMsg)
	{
#define CASE(x) case x: return #x
      CASE (WM_NULL);
      CASE (WM_CREATE);
      CASE (WM_DESTROY);
      CASE (WM_MOVE);
      CASE (WM_SIZE);
      CASE (WM_ACTIVATE);
      CASE (WM_SETFOCUS);
      CASE (WM_KILLFOCUS);
      CASE (WM_ENABLE);
      CASE (WM_SETREDRAW);
      CASE (WM_SETTEXT);
      CASE (WM_GETTEXT);
      CASE (WM_GETTEXTLENGTH);
      CASE (WM_PAINT);
      CASE (WM_CLOSE);
      CASE (WM_QUERYENDSESSION);
      CASE (WM_QUERYOPEN);
      CASE (WM_ENDSESSION);
      CASE (WM_QUIT);
      CASE (WM_ERASEBKGND);
      CASE (WM_SYSCOLORCHANGE);
      CASE (WM_SHOWWINDOW);
      CASE (WM_WININICHANGE);
      CASE (WM_DEVMODECHANGE);
      CASE (WM_ACTIVATEAPP);
      CASE (WM_FONTCHANGE);
      CASE (WM_TIMECHANGE);
      CASE (WM_CANCELMODE);
      CASE (WM_SETCURSOR);
      CASE (WM_MOUSEACTIVATE);
      CASE (WM_CHILDACTIVATE);
      CASE (WM_QUEUESYNC);
      CASE (WM_GETMINMAXINFO);
      CASE (WM_PAINTICON);
      CASE (WM_ICONERASEBKGND);
      CASE (WM_NEXTDLGCTL);
      CASE (WM_SPOOLERSTATUS);
      CASE (WM_DRAWITEM);
      CASE (WM_MEASUREITEM);
      CASE (WM_DELETEITEM);
      CASE (WM_VKEYTOITEM);
      CASE (WM_CHARTOITEM);
      CASE (WM_SETFONT);
      CASE (WM_GETFONT);
      CASE (WM_SETHOTKEY);
      CASE (WM_GETHOTKEY);
      CASE (WM_QUERYDRAGICON);
      CASE (WM_COMPAREITEM);
      // CASE (WM_GETOBJECT);
      CASE (WM_COMPACTING);
      CASE (WM_WINDOWPOSCHANGING);
      CASE (WM_WINDOWPOSCHANGED);
      CASE (WM_POWER);
      CASE (WM_COPYDATA);
      CASE (WM_CANCELJOURNAL);
      CASE (WM_NOTIFY);
      CASE (WM_INPUTLANGCHANGEREQUEST);
      CASE (WM_INPUTLANGCHANGE);
      CASE (WM_TCARD);
      CASE (WM_HELP);
      CASE (WM_USERCHANGED);
      CASE (WM_NOTIFYFORMAT);
      CASE (WM_CONTEXTMENU);
      CASE (WM_STYLECHANGING);
      CASE (WM_STYLECHANGED);
      CASE (WM_DISPLAYCHANGE);
      CASE (WM_GETICON);
      CASE (WM_SETICON);
      CASE (WM_NCCREATE);
      CASE (WM_NCDESTROY);
      CASE (WM_NCCALCSIZE);
      CASE (WM_NCHITTEST);
      CASE (WM_NCPAINT);
      CASE (WM_NCACTIVATE);
      CASE (WM_GETDLGCODE);
      CASE (WM_SYNCPAINT);
      CASE (WM_NCMOUSEMOVE);
      CASE (WM_NCLBUTTONDOWN);
      CASE (WM_NCLBUTTONUP);
      CASE (WM_NCLBUTTONDBLCLK);
      CASE (WM_NCRBUTTONDOWN);
      CASE (WM_NCRBUTTONUP);
      CASE (WM_NCRBUTTONDBLCLK);
      CASE (WM_NCMBUTTONDOWN);
      CASE (WM_NCMBUTTONUP);
      CASE (WM_NCMBUTTONDBLCLK);
      // CASE (WM_NCXBUTTONDOWN);
      // CASE (WM_NCXBUTTONUP);
      // CASE (WM_NCXBUTTONDBLCLK);
      CASE (WM_KEYDOWN);
      CASE (WM_KEYUP);
      CASE (WM_CHAR);
      CASE (WM_DEADCHAR);
      CASE (WM_SYSKEYDOWN);
      CASE (WM_SYSKEYUP);
      CASE (WM_SYSCHAR);
      CASE (WM_SYSDEADCHAR);
      CASE (WM_KEYLAST);
      // CASE (WM_IME_STARTCOMPOSITION);
      // CASE (WM_IME_ENDCOMPOSITION);
      // CASE (WM_IME_COMPOSITION);
      CASE (WM_INITDIALOG);
      CASE (WM_COMMAND);
      CASE (WM_SYSCOMMAND);
      CASE (WM_TIMER);
      CASE (WM_HSCROLL);
      CASE (WM_VSCROLL);
      CASE (WM_INITMENU);
      CASE (WM_INITMENUPOPUP);
      CASE (WM_MENUSELECT);
      CASE (WM_MENUCHAR);
      CASE (WM_ENTERIDLE);
      // CASE (WM_MENURBUTTONUP);
      // CASE (WM_MENUDRAG);
      // CASE (WM_MENUGETOBJECT);
      // CASE (WM_UNINITMENUPOPUP);
      // CASE (WM_MENUCOMMAND);
      // CASE (WM_CHANGEUISTATE);
      // CASE (WM_UPDATEUISTATE);
      // CASE (WM_QUERYUISTATE);
      CASE (WM_CTLCOLORMSGBOX);
      CASE (WM_CTLCOLOREDIT);
      CASE (WM_CTLCOLORLISTBOX);
      CASE (WM_CTLCOLORBTN);
      CASE (WM_CTLCOLORDLG);
      CASE (WM_CTLCOLORSCROLLBAR);
      CASE (WM_CTLCOLORSTATIC);
      CASE (WM_MOUSEMOVE);
      CASE (WM_LBUTTONDOWN);
      CASE (WM_LBUTTONUP);
      CASE (WM_LBUTTONDBLCLK);
      CASE (WM_RBUTTONDOWN);
      CASE (WM_RBUTTONUP);
      CASE (WM_RBUTTONDBLCLK);
      CASE (WM_MBUTTONDOWN);
      CASE (WM_MBUTTONUP);
      CASE (WM_MBUTTONDBLCLK);
      CASE (WM_MOUSEWHEEL);
      // CASE (WM_XBUTTONDOWN);
      // CASE (WM_XBUTTONUP);
      // CASE (WM_XBUTTONDBLCLK);
      CASE (WM_PARENTNOTIFY);
      CASE (WM_ENTERMENULOOP);
      CASE (WM_EXITMENULOOP);
      CASE (WM_NEXTMENU);
      CASE (WM_SIZING);
      CASE (WM_CAPTURECHANGED);
      CASE (WM_MOVING);
      CASE (WM_POWERBROADCAST);
      CASE (WM_DEVICECHANGE);
      CASE (WM_MDICREATE);
      CASE (WM_MDIDESTROY);
      CASE (WM_MDIACTIVATE);
      CASE (WM_MDIRESTORE);
      CASE (WM_MDINEXT);
      CASE (WM_MDIMAXIMIZE);
      CASE (WM_MDITILE);
      CASE (WM_MDICASCADE);
      CASE (WM_MDIICONARRANGE);
      CASE (WM_MDIGETACTIVE);
      CASE (WM_MDISETMENU);
      CASE (WM_ENTERSIZEMOVE);
      CASE (WM_EXITSIZEMOVE);
      CASE (WM_DROPFILES);
      CASE (WM_MDIREFRESHMENU);
      // CASE (WM_IME_SETCONTEXT);
      // CASE (WM_IME_NOTIFY);
      // CASE (WM_IME_CONTROL);
      // CASE (WM_IME_COMPOSITIONFULL);
      // CASE (WM_IME_SELECT);
      // CASE (WM_IME_CHAR);
      // CASE (WM_IME_REQUEST);
      // CASE (WM_IME_KEYDOWN);
      // CASE (WM_IME_KEYUP);
      CASE (WM_MOUSEHOVER);
      CASE (WM_MOUSELEAVE);
      // CASE (WM_NCMOUSEHOVER);
      // CASE (WM_NCMOUSELEAVE);
      CASE (WM_CUT);
      CASE (WM_COPY);
      CASE (WM_PASTE);
      CASE (WM_CLEAR);
      CASE (WM_UNDO);
      CASE (WM_RENDERFORMAT);
      CASE (WM_RENDERALLFORMATS);
      CASE (WM_DESTROYCLIPBOARD);
      CASE (WM_DRAWCLIPBOARD);
      CASE (WM_PAINTCLIPBOARD);
      CASE (WM_VSCROLLCLIPBOARD);
      CASE (WM_SIZECLIPBOARD);
      CASE (WM_ASKCBFORMATNAME);
      CASE (WM_CHANGECBCHAIN);
      CASE (WM_HSCROLLCLIPBOARD);
      CASE (WM_QUERYNEWPALETTE);
      CASE (WM_PALETTEISCHANGING);
      CASE (WM_PALETTECHANGED);
      CASE (WM_HOTKEY);
      CASE (WM_PRINT);
      CASE (WM_PRINTCLIENT);
      // CASE (WM_APPCOMMAND);
      CASE (WM_HANDHELDFIRST);
      CASE (WM_HANDHELDLAST);
      CASE (WM_AFXFIRST);
      CASE (WM_AFXLAST);
      CASE (WM_PENWINFIRST);
      CASE (WM_PENWINLAST);
      CASE (WM_APP);
#undef CASE
	default:
		return "UNKNOWN" ;
	}
}

*/


/*
 * NativeWin32FileSelection
 *
 * opens a common file open dialog
 *
 * returns the selected file
 * user shall free the result
 */
//static UINT_PTR CALLBACK FileSelectionHook (
static	int	FileSelectionHook (
		HWND	hdlg,
		UINT	uiMsg,
		WPARAM	wParam,
		LPARAM	lParam
		)
{
	char	toto [128] ;
	static	POINT*		lpPos ;
	static	RECT		rect ;
	switch (uiMsg)
	{
	case WM_INITDIALOG:
		/* move the window to the proper direction, given as lparam */
		lpPos = (POINT*) (((OPENFILENAME*) lParam)->lCustData) ;
		GetWindowRect ( GetParent (hdlg), &rect) ;
		lpPos->x -= (rect.right - rect.left) / 2 ;
		lpPos->y -= (rect.bottom - rect.top) / 2 ;
		SetWindowPos (GetParent (hdlg), HWND_TOP, 
				lpPos->x, 
				lpPos->y, 
				0, 0, SWP_NOSIZE) ;
		return 0 ;
	default:
	}
	return 0 ;
}


static char*	NativeWin32FileSelection (
		const char*	title,
		const char*	basedir,
		const char*	filepattern,
		const char*	patternname,
		const char*	defaultname,
		unsigned int	style)
{
	static	OPENFILENAME	ofn ;
	static	char		l_Filter [512] ;
	static	char		l_Result [MAX_PATH] ;
	static	POINT		position ;
	RECT			aw_rect ;
	int			l_index = 0 ;
	char*			res ;
	unsigned int		style_flag ;
	HWND			active_window ;

	switch (style)
	{
		case 2:
			/* under mouse */
			GetCursorPos (&position) ;
			style_flag = OFN_ENABLEHOOK ;
			break ;
		case 1:
			/* center */
			active_window = GetActiveWindow () ;
			if (active_window)
			{
				GetWindowRect (active_window, &aw_rect) ;
				position.x = (aw_rect.left + aw_rect.right) / 2 ;
				position.y = (aw_rect.top + aw_rect.bottom) / 2 ;
				style_flag = OFN_ENABLEHOOK ;
				break ;
			}
			/* otherwise, default case */
		case 0:
		default:
			/* nothing */
			style_flag = 0 ;
			break ;
	}
			

	if (patternname != 0)
	{
		l_index += strlen (patternname) ;
		strcpy (l_Filter, patternname) ;
		++l_index ;
	}
	if (filepattern != 0)
	{
		strcat (& (l_Filter [l_index]), filepattern) ;
		l_index += strlen (filepattern) ;
		l_Filter [l_index+1] = '\0' ;
	}

	if (defaultname)
		strcpy (l_Result, defaultname) ;
	else
		l_Result [0] = '\0' ;
	
	ofn.lStructSize		= sizeof (OPENFILENAME) ;
	ofn.hwndOwner		= NULL ;
	ofn.hInstance		= NULL ;
	ofn.lpstrFilter		= l_Filter ;
	ofn.lpstrCustomFilter	= NULL ;
	ofn.nMaxCustFilter	= 0 ;
	ofn.nFilterIndex	= 0 ;
	ofn.lpstrFile		= l_Result ;
	ofn.nMaxFile		= MAX_PATH ;
	ofn.lpstrFileTitle	= NULL ;
	ofn.nMaxFileTitle	= 0 ;
	ofn.lpstrInitialDir	= basedir ;
	ofn.lpstrTitle		= title ;
	ofn.Flags		= OFN_CREATEPROMPT | OFN_EXPLORER | style_flag ;
	ofn.nFileOffset		= 0 ;
	ofn.nFileExtension	= 0 ;
	ofn.lpstrDefExt		= "ads" ;
	ofn.lCustData		= &position ;
	ofn.lpfnHook		= FileSelectionHook ;
	ofn.lpTemplateName	= NULL ;

	GetOpenFileName (&ofn) ;

	/* copy the result into a well sized string */
	res = malloc (sizeof (char) * (strlen (l_Result) + 1)) ;
	strcpy (res, l_Result) ;

	return res ;
}

/*
 *style	:
 *	0 : nothing
 *	1 : center
 *	2 : under mouse
 */



char*	NativeFileSelection (
		const char*	title,
		const char*	basedir,
		const char*	filepattern,
		const char*	patternname,
		const char*	defaultname,
		unsigned int	style)
{
	return NativeWin32FileSelection (
			title,
			basedir,
			filepattern,
			patternname,
			defaultname,
			style) ;
}
