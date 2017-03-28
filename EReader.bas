'BBasic
'����С˵�Ķ���
'���ߣ�Clyde
'֧��ԭ9588����ǩ
'֧���Լ�������ǩ
'֧�ֺ�����ת��
'֧�ִ�����ҳ
'֧�ַ�ҳ����
'֧����Ӣ����ʾ

'===========��������������
Dim g_TextFileName$'��Ŵ򿪵��ı��ļ�����
Dim g_BookMarkFileName$'��ǩ�ļ�����
Dim g_TextPageNum'С˵ҳ������
Dim g_TextPageNow'��ǰҳ��
Dim g_TextLineNum
Dim g_TextLineNow
Dim g_TextPos'��ǰҳ��ƫ��
'Dim g_TextPosNext'��һҳ��ƫ��
Dim g_Text$(14)'��ȡ���ı�����
Dim g_DelayTime'��ҳʱ����
Dim g_BookMarkName$(4)
Dim g_BookMarkAddress(4)
Dim g_ReadLineFilePos'�����õ��ļ�ƫ�ƹ�������

Dim g_FontRes(9)'��9��ͼƬ����ֿ�
Dim g_MainPic'������ͼƬ
dim g_MainPic2
Dim g_MsgPic1'��Ϣ���ͼƬ
Dim g_MsgPic2
Dim g_MsgPic3
Dim g_InputMethodPic1
Dim g_InputMethodPic2
Dim g_BgPic
Dim g_BgPicId
Dim g_BookSelectMenuPic1'�鱾ѡ��������ͼƬ1
Dim g_MainPage'��ʾҳ��
Dim g_MsgPage'��Ϣ���õ���ʱҳ��
Dim g_AnimationPage'����ҳ��
Dim g_TempPage'��ҳ�����������õ���ʱҳ��
Dim g_Ext$'��Դ�ļ��ĺ�׺��
Dim g_MsTime
Dim g_FullScreen'ȫ���Ķ�ģʽ��1�ǣ�0����
'===========

'===========����������
declare sub main()
declare sub ShowTextPage()'�ѵ�ǰҳ����ı����������ȥ
declare sub Controler()'�õ�����������ִ��
declare sub LoadText()'��������
declare Sub LoadTextPos()
declare sub LoadDataToString()
declare Sub ClearTextString()
declare Sub MsgBox(Text$)
declare sub LastPage()
declare sub NextPage()
declare sub SaveBookMark()
declare sub ProgramExit()
declare sub JumpPage()
declare sub showhelp()
declare sub SaveConfig()
declare sub AutoTurnPage()
declare sub SetBookMark()
declare sub PageSlide(Page1,Page2,x1,y1,x2,y2,Mode)
declare sub SaveBookMarkId(BookMarkId)
declare sub LoadBookMarkId(BookMarkId)


declare function InputMethod(Mode$,defaultNum,MinNum,MaxNum,Text$)'���뷨  ģʽ��Ĭ��ֵ,��Сֵ ���ֵ ��ʾ
declare function InitRes()'���������Ҫ����Դ
declare function GetTextFileName()'�õ��ı��ļ����ļ���
declare function LoadBookMark(FileName$)'ͨ���ı��ļ�����Ѱ�Ҳ���������ǩ
declare Function ShowStr(BgPage,x,y,Text$)'�ı��������
declare function InBox(x,y,x1,y1,x2,y2)
declare function ReadLineFromText$(IntPos)'���ܣ����ı��ĵ��ж���һ�У����������е��ַ���  ���������еĳ�ʼλ�ã������ʼλ��Ϊ-1�����ʾ�ӵ�ǰ�ļ��������
declare function fgetc(add)
declare function StrToInt(Text$)
declare Function BookSelectMenu()

'===========

'===========����������


sub SaveBookMarkId(BookMarkId)
	'MsgBox(Str$(BookMarkId))
	
	g_BookMarkAddress(BookMarkId) = g_TextPos
	g_BookMarkName$(BookMarkId) = Mid$(g_Text$(0),0,12)
	
	'Msgbox(g_BookMarkName$(BookMarkId))
	'Msgbox(g_BookMarkFileName$)
	Open g_BookMarkFileName$ For Binary As #1
	
	'������ǩ��ַ
	Seek #1,32 + BookMarkId * 4
	Put #1,g_BookMarkAddress(BookMarkId)
	'������ǩ����
	Seek #1,52 + BookMarkId * 20
	Put #1,g_BookMarkName$(BookMarkId)
	
	Close #1
end sub

sub LoadBookMarkId(BookMarkId)
	'MsgBox(Str$(BookMarkId))
	Dim i,j,m,DwordData,WhileEscape
	Open g_BookMarkFileName$ For Binary As #1
	'������ǩ��ַ
	Seek #1,32 + BookMarkId * 4
	Get #1,DwordData
	if DwordData = 0 then
		g_TextPos = DwordData
		g_TextPageNow = 1
	else if DwordData = -1 then
		Msgbox("����ǩ�����ڣ�")
	else
		g_TextPos = DwordData
		'���ݵ�ַ�ö��ַ�����ҳ��
		i = 160
		j = lof(1) - 4
		WhileEscape = 1
		While (i <= j and WhileEscape = 1)
			'm = (j + i) / 2
			m = ((i - 160) / 4 + (j - 160) / 4) / 2 * 4 + 160
			Seek #1,m
			Get #1,DWordData
			if DwordData = g_TextPos then
				g_TextPageNow = (m - 160) / 4 + 2
				WhileEscape = 0
			else if DwordData > g_TextPos then
				j = m - 4
			else if DwordData < g_TextPos then
				i = m + 4
			end if
		Wend
		'˵��û���ҵ�һ���ģ�ֻ��Ѱ����ӽ���
		if i > j then
			g_TextPageNow = (m - 160) / 4 + 2
		end if
	end if
	Close #1
	LoadText()
end sub

'���ܣ���ʾҳ���л�����
'������ԭҳ�棬Ŀ��ҳ�棬������ʾ����X1Y1,X2Y2�ľ�������,�л�ģʽ
'ע�⣺ģʽ��1�������ң�2�����ҵ���3�����ϵ��£�4�����µ���
sub PageSlide(Page1,Page2,x1,y1,x2,y2,Mode)
	Dim PosX1,PosY1,PosX2,PosY2'1ԭҳ�����꣬2Ŀ��ҳ������
	Dim VX,VY,AX,AY'X������ٶȣ�Y������ٶȣ����ٶ�
	PosX1 = x1
	PosY1 = y1
	if Mode = 1 Then
		'������
		PosX2 = x1 - (x2 - x1 + 1)
		PosY2 = y1
		VX = 7
		VY = 0
		AX = 7
		AY = 0
	else if Mode = 2 Then
		'���ҵ���
		PosX2 = x2
		PosY2 = y1
		VX = -7
		VY = 0
		AX = -7
		AY = 0
	else if Mode = 3 Then
		'���ϵ���
		PosX2 = x1
		PosY2 = y1 - (y2 - y1 + 1)
		VX = 0
		VY = 7
		AX = 0
		AY = 7
	else if Mode = 4 Then
		'���µ���
		PosX2 = x1
		PosY2 = y2
		VX = 0
		VY = -7
		AX = 0
		AY = -7
	end if
	'Print PosX2
	'Print PosY2
	'WaitKey()
	'FlipPage(Page1)
	'WaitKey()
	'FlipPage(Page2)
	'WaitKey()

	While not(PosX1 > 240 or PosX1 < -240 or PosY1 < -320 or PosY1 > 320)
		StretchBltPage(0,0,g_TempPage,Page1)
		StretchBltPageEx(PosX1,PosY1,x2 - x1 + 1,y2 - y1 + 1,x1,y1,g_TempPage,Page1)
		StretchBltPageEx(PosX2,PosY2,x2 - x1 + 1,y2 - y1 + 1,x1,y1,g_TempPage,Page2)
		'ShowStr(g_TempPage,0,0,Str$(PosX2))
		'ShowStr(g_TempPage,20,0,Str$(PosY2))
		FlipPage(g_TempPage)
		'WaitKey()
		msdelay(g_MsTime)
		PosX1 += VX
		PosX2 += VX
		PosY1 += VY
		PosY2 += VY
		VX += AX
		VY += AY
	Wend
	'STRETCHBLTPAGE
	'StretchBltPageEx(
	'STRETCHBLTPAGEEX(X,Y,WID,HGT,CX,CY,DEST,SRC)
end sub

'���ܣ����ַ���ת��������
'�������ַ���
'���أ�����
'˵�����滻Val����������BUG
function StrToInt(Text$)
	Dim StrLen
	Dim MyChar$
	Dim Num
	Dim i
	StrLen = Len(Text$)
	i = 1
	Num = 0
	While (i <= StrLen)
		MyChar$ = Mid$(Text$,i - 1,1)
		Num = Num * 10 + Val(MyChar$)
		i ++
	Wend
	'Print Num
	StrToInt = Num
end function

'���ܣ����ļ�1�ж�ȡaddƫ���µ�byte����
'�������ļ�ƫ��add
'���أ�word����ֵ��0��255��
function fgetc(add)
	Dim TempData
	seek #1,add
	get #1,TempData
	TempData = TempData mod 256
	if (TempData < 0) then
		TempData += 256
	end if
	fgetc = TempData
end function

'���ܣ����ļ�λ��1���ı��ĵ��ж���һ�У����������е��ַ���
'���������еĳ�ʼλ�ã������ʼλ��Ϊ-1�����ʾ�ӵ�ǰ�ļ�ƫ�ƴ�����
'���أ��ַ���
function ReadLineFromText$(IntPos)
	Dim TempString$'��ʱ������Ҫ���ص��ַ������ַ�������
	Dim TempData'������ʱ��BYTE���ݣ���ҪMOD 256֮��ſ���ʹ��
	Dim FilePos'�ļ�ƫ��
	Dim FileLen'�ļ�����
	Dim WhileEscape
	
	'�ַ�����ʼ��
	TempString$ = ""
	
	'���ó�ʼƫ��
	if IntPos > -1 then
		FilePos = IntPos
	else
		FilePos = g_ReadLineFilePos
	end if
	
	'ѭ�����ַ�ֱ�������س����߶����ļ�Ϊֹ
	FileLen = lof(1)
	WhileEscape = 1
	while WhileEscape
		TempData = fgetc(FilePos)
		'print TempData
		'waitkey()
		if (TempData = 13) then
			FilePos ++
			WhileEscape = 0
		Else if (FilePos >= FileLen) then
			'Print TempData
			WhileEscape = 0
		Else
			FilePos ++
			TempString$ = "" + TempString$ + Chr$(TempData)'��ֹ�������
		end if
	wend
	g_ReadLineFilePos = FilePos + 1
	'������з���Ĭ��ΪWINDOWS����ı��ĵ�
	ReadLineFromText$ = TempString$
end function


'���ܣ��鱾ѡ��˵�
'��������
'���أ�ѡ������ID������-1�����˳�
Function BookSelectMenu()
	Dim TempString$
	Dim Key,PenPosX,PenPosY
	Dim BookSelectMenuWhileEscape,DeleteMode
	Dim BookNum$'�鱾��Ŀ
	Dim BookFileName$(9)'Ŀǰ��Ϊ���Ϊ9���飬ÿ������ļ���
	Dim BookPicture(9)'�ֱ����ǰ��ʾ��9���鱾�ķ���
	Dim i
	Dim BookSelected
	Dim BookPicShadow,BookPicDefault
	
	'��ʼ���鱾���ļ���
	i = 0
	While i < 9
		BookFileName$ = ""
		i ++
	Wend
	
	'�����鱾��ӰͼƬ
	TempString$ = "С˵�Ķ�\BookPic" + g_Ext$
	BookPicShadow = LoadRes(TempString$,1)
	'�����鱾Ĭ�Ϸ���
	BookPicDefault = LoadRes(TempString$,2)
	
	'��ȡ�鱾��Ϣ
	Open "С˵�Ķ�\\BooksConfig.txt" for Binary as #1
	'����鱾��Ŀ
	BookNum$ = ReadLineFromText$(0)
	i = 0
	While i < Val(BookNum$)
		BookFileName$(i) = ReadLineFromText$(-1)
		'Print BookFileName$(i)
		i ++
	Wend
	'WaitKey()
	'����鱾���ƺͷ��棬�����������Ϊ-1������Ĭ�Ϸ������
	'Ŀǰ��û�д�������Զ������
	i = 0
	While i < 9
		BookPicture(i) = BookPicDefault
		i ++
	Wend
	
	Close #1
	
	
	BookSelectMenuWhileEscape = 1
	DeleteMode = 0
	while BookSelectMenuWhileEscape = 1
		'��ʾ������
		ShowPic(g_MainPage,g_BookSelectMenuPic1,0,0,240,320,0,0,1)
		'��ʾ�鱾ͼƬ1��9�����9��
		i = 0
		While i < Val(BookNum$)
			'����ͼ��Ӱ��
			ShowPic(g_MainPage,BookPicShadow,25 + (i Mod 3) * 72,50 + (i / 3) * 88,54,66,0,0,1)
			'����ͼ����ͼ
			ShowPic(g_MainPage,BookPicture(i),25 + (i Mod 3) * 72,50 + (i / 3) * 88,47,59,0,0,1)
			'����ͼ������
			ShowStr(g_MainPage,25 + (i Mod 3) * 72,120 + (i / 3) * 88,Mid$(BookFileName$(i),0,8))
			
			i ++
		Wend
		
		flippage(g_MainPage)
		Key = WaitKey()
		if Key > 0 then
			if Key = KEY_ESCAPE then
				BookSelectMenuWhileEscape = 0
			end if
		else if Key < 0 then
			PenPosX = GetPenPosX(Key)
			PenPosY = GetPenPosY(Key)
			'���ذ�ť
			if inbox(PenPosX,PenPosY,10,0,60,30) then
				BookSelectMenuWhileEscape = 0
				BookSelected = -1
			'ѡ���鱾
			else if inbox(PenPosX,PenPosY,25,50,223,304) then
				BookSelected = (PenPosY - 50) / 85 * 3 + (PenPosX - 25) / 66
				If (BookSelected < Val(BookNum$)) Then
					BookSelectMenuWhileEscape = 0
					g_TextFileName$ = "С˵�Ķ�\Books\" + BookFileName$(BookSelected) + ".txt"
				End If
				'Print BookSelected
				'Waitkey()
			end if
		end if
	wend
	FreeRes(BookPicShadow)
	FreeRes(BookPicDefault)
	i = 0
	While i < 9
		if (BookPicture(i) <> BookPicDefault) Then
			FreeRes(BookPicture(i))
		End if
		i ++
	Wend
	BookSelectMenu = BookSelected
end Function

sub SetBookMark()
	'Msgbox("��ǩ����Ŀǰ������ʹ�á�")
	Dim BookMarkPic1,BookMarkPic2
	Dim TempString$,WhileEscape
	Dim Key,i,PenPosX,PenPosY
	Dim BookMarkId
	TempString$ = "С˵�Ķ�\Res" + g_Ext$
	BookMarkPic1 = LoadRes(TempString$,8)
	BookMarkPic2 = LoadRes(TempString$,9)
	BitBltPage(g_TempPage,-1)
	WhileEscape = 1
	While WhileEscape
		BitBltPage(g_MainPage,g_TempPage)
		ShowPic(g_MainPage,BookMarkPic1,110,54,GetPicWid(BookMarkPic1),GetPicHgt(BookMarkPic2),0,0,1)
		i = 0
		While i < 4
			ShowStr(g_MainPage,144,69 + i * 22,g_BookMarkName$(i))
			i ++
		Wend
		FlipPage(g_MainPage)
		Key = WaitKey()
		if (Key < 0) then
			PenPosX = GetPenPosX(Key)
			PenPosY = GetPenPosY(Key)
			if inbox(PenPosX,PenPosY,111,60,230,157) then
				if inbox(PenPosX,PenPosY,120,67,230,145) then
					'MsgBox(Str$(PenPosY))
					BookMarkId = (PenPosY - 67) / 22
					'MsgBox(Str$(BookMarkId))
					'�������ֵ�ͼ�괦
					if inbox(PenPosX,PenPosY,120,67,140,149) then
						ShowPic(-1,BookMarkPic2,118,67 + (BookMarkId) * 22,24,22,8,13 + (BookMarkId) * 22,1)
						MsDelay(300)
						SaveBookMarkId(BookMarkId)
					'���ִ�
					else
						LoadBookMarkId(BookMarkId)
						WhileEscape = 0
					end if
				end if
			else
				WhileEscape = 0
			end if
		end if
	Wend
	FreeRes(BookMarkPic1)
	FreeRes(BookMarkPic2)
end sub

sub AutoTurnPage()
	Dim OldTick,TickNow
	OldTick = GetTick()
	while (Not Keypress(Key_Escape)) and g_TextPageNow < g_TextPageNum
		ShowTextPage()
		showpic(g_MainPage,g_MainPic2,101,23,140 - 101 + 1,50 - 23 + 1,101,23,1)
		flippage(g_MainPage)
		TickNow = GetTick()
		if TickNow - OldTick > g_DelayTime then
			NextPage()
			OldTick = TickNow
		end if
	wend
	ShowTextPage()
	showpic(g_MainPage,g_MainPic2,122,293,166 - 122 + 1,316 - 293 + 1,122,293,1)
	flippage(g_MainPage)
	MsDelay(300)
	MsgBox("�Զ���ҳ�Ѿ�������")
end sub

sub SaveConfig()
	Open "С˵�Ķ�\config.cfg" for binary as #1
	seek #1,0
	put #1,g_BgPicId
	put #1,g_DelayTime
	Close #1
end sub

function InputMethod(Mode$,defaultNum,MinNum,MaxNum,Text$)
	'g_MsgPage
	Dim IM_WhileEscape,TempString$,Key,penposx,penposy,TempNum,Mid_Len
	BITBLTPAGE(g_AnimationPage,-1)
	TempString$ = Str$(defaultNum)
	IM_WhileEscape = 1
	While IM_WhileEscape = 1
		showpic(g_MsgPage,g_InputMethodPic1,0,0,240,320,0,0,1)
		showstr(g_MsgPage,29,35,Text$)
		showstr(g_MsgPage,13,75,TempString$)
		flippage(g_MsgPage)
		Key = Waitkey()
		if Key < 0 then
			penposx = getpenposx(key)
			penposy = getpenposy(key)
			if Inbox(penposx,penposy,5,277,48,315) then
				showpic(-1,g_InputMethodPic2,5,277,48 - 5 + 1,315 - 277 + 1,5,277,1)
				Msdelay(300)
				IM_WhileEscape = 0
				TempNum = -1
			else if Inbox(penposx,penposy,162,68,219,101) then
				showpic(-1,g_InputMethodPic2,162,68,58,34,162,68,1)
				Msdelay(300)
				if val(TempString$) >= MinNum and Val(TempString$) <= MaxNum then
					IM_WhileEscape = 0
					TempNum = Val(TempString$)
				else
					Msgbox("��������ֲ�����ȷ��Χ�ڣ�")
				end if
			else if Inbox(penposx,penposy,52,122,110,170) then
				showpic(-1,g_InputMethodPic2,52,122,110 - 52 + 1,170 - 122 + 1,52,122,1)
				Msdelay(300)
				TempString$ = TempString$ + "1"
			else if Inbox(penposx,penposy,115,122,172,170) then
				showpic(-1,g_InputMethodPic2,115,122,172 - 115 + 1,170 - 122 + 1,115,122,1)
				Msdelay(300)
				TempString$ = TempString$ + "2"
			else if Inbox(penposx,penposy,177,122,234,170) then
				showpic(-1,g_InputMethodPic2,177,122,234 - 177 + 1,170 - 122 + 1,177,122,1)
				Msdelay(300)
				TempString$ = TempString$ + "3"
			else if Inbox(penposx,penposy,52,174,110,222) then
				showpic(-1,g_InputMethodPic2,52,174,110 - 52 + 1,222 - 174 + 1,52,174,1)
				Msdelay(300)
				TempString$ = TempString$ + "4"
			else if Inbox(penposx,penposy,115,174,172,222) then
				showpic(-1,g_InputMethodPic2,115,174,172 - 115 + 1,222 - 174 + 1,115,174,1)
				Msdelay(300)
				TempString$ = TempString$ + "5"
			else if Inbox(penposx,penposy,177,174,234,222) then
				showpic(-1,g_InputMethodPic2,177,174,234 - 177 + 1,222 - 174 + 1,177,174,1)
				Msdelay(300)
				TempString$ = TempString$ + "6"
			else if Inbox(penposx,penposy,52,226,110,273) then
				showpic(-1,g_InputMethodPic2,52,226,110 - 52 + 1,273 - 226 + 1,52,226,1)
				Msdelay(300)
				TempString$ = TempString$ + "7"
			else if Inbox(penposx,penposy,115,226,172,273) then
				showpic(-1,g_InputMethodPic2,115,226,172 - 115 + 1,273 - 226 + 1,115,226,1)
				Msdelay(300)
				TempString$ = TempString$ + "8"
			else if Inbox(penposx,penposy,177,226,234,273) then
				showpic(-1,g_InputMethodPic2,177,226,234 - 177 + 1,273 - 226 + 1,177,226,1)
				Msdelay(300)
				TempString$ = TempString$ + "9"
			else if Inbox(penposx,penposy,115,278,172,314) then
				showpic(-1,g_InputMethodPic2,115,278,172 - 115 + 1,314 - 278 + 1,115,278,1)
				Msdelay(300)
				TempString$ = TempString$ + "0"
			else if Inbox(penposx,penposy,177,278,234,314) then
				showpic(-1,g_InputMethodPic2,177,278,234 - 177 + 1,314 - 278 + 1,177,278,1)
				Msdelay(300)
				Mid_Len = len(TempString$) - 1
				TempString$ = Mid$(TempString$,0,Mid_Len)
			end if
		else if key > 0 then
			if key = key_escape then
				IM_WhileEscape = 0
				TempNum = -1
			else if key = key_enter then
				if val(TempString$) >= MinNum and Val(TempString$) <= MaxNum then
					IM_WhileEscape = 0
					TempNum = Val(TempString$)
				else
					Msgbox("��������ֲ�����ȷ��Χ�ڣ�")
				end if
			end if
		end if
		if len(TempString$) > 20 then
			Msgbox("�ַ������Ѿ��ﵽ���")
			TempString$ = Mid$(TempString$,0,20)
		end if
	wend
	flippage(g_AnimationPage)
	InputMethod = TempNum
end function

sub showhelp()
	MsgBox("���ߣ�Clyde �뵽BBK9588���ɹ�ע����")
end sub

sub JumpPage()'��תҳ��
	Dim Temp_Page
	Temp_Page = InputMethod("Number",g_TextPageNow,1,g_TextPageNum,"������Ҫ��ת��ҳ�棺")'���뷨  ģʽ����Сֵ ���ֵ
	If Temp_Page <> -1 then
		if Temp_Page > g_TextPageNow then
			g_TextPageNow = Temp_Page - 1
			NextPage()
		else if Temp_Page < g_TextPageNow then
			g_TextPageNow = Temp_Page + 1
			LastPage()
		else if Temp_Page = g_TextPageNow then
			MsgBox("��ҳ���Ѿ���Ŀ��ҳ�档")
		end if
	end if
end sub

function InBox(x,y,x1,y1,x2,y2)
	inbox = x > x1 and x < x2 and y > y1 and y < y2
end function

Sub LastPage()
	Dim v,a,PosY,i
	if g_TextPageNow > 1 then
		BITBLTPAGE(g_AnimationPage,-1)
		g_TextPageNow --
		LoadText()
		if g_FullScreen = 0 Then
			ShowTextPage()
			PageSlide(g_AnimationPage,g_MainPage,0,52,230,290,1)
			ShowTextPage()
			FlipPage(g_MainPage)
		else
			ShowTextPage()
			PageSlide(g_AnimationPage,g_MainPage,0,0,230,320,3)
			ShowTextPage()
			FlipPage(g_MainPage)
		end if
	else
		MsgBox("�Ѿ��������ǰ��")
	end if
end sub

Sub NextPage()
	Dim v,a,PosY,i
	if g_TextPageNow < g_TextPageNum then
		BITBLTPAGE(g_AnimationPage,-1)
		g_TextPageNow ++
		LoadText()
		if g_FullScreen = 0 Then
			'showpic(g_MsgPage,g_MainPic,5,60,216,224,5,60,1)
			'showpic(g_MsgPage,g_BgPic,0,52,240,238,0,0,1)
			'i = 0
			'while i < 14
			'	showstr(g_MsgPage,5,60 + i * 16,g_Text$(i))
			'	i ++
			'wend
			'v = 5
			'a = 5
			'PosY = 290
			'While PosY > 52
			'	STRETCHBLTPAGEEX(5,PosY,216,290 - PosY,5,52,g_MainPage,g_MsgPage)
			'	STRETCHBLTPAGEEX(5,52,216,PosY - 52,5,290 - PosY + 52 ,g_MainPage,g_AnimationPage)
			'	flippage(g_MainPage)
			'	msdelay(g_MsTime)
			'	PosY -= v
			'	v += a
			'Wend
			ShowTextPage()
			PageSlide(g_AnimationPage,g_MainPage,0,52,230,290,2)
			ShowTextPage()
			FlipPage(g_MainPage)
		else
			ShowTextPage()
			PageSlide(g_AnimationPage,g_MainPage,0,0,230,320,4)
			ShowTextPage()
			FlipPage(g_MainPage)
		end if
	else
		MsgBox("�Ѿ���������")
	end if
end sub

Sub MsgBox(Text$)
	Dim width,PixelStart,PixelPos,PosY,x,y,key,while_Escape
	
	BITBLTPAGE(g_MsgPage,-1)
	width = len(Text$) * 6 + 20
	PixelStart = (240 - Width) / 2
	PosY = (320 - getpichgt(g_MsgPic1) - getpichgt(g_MsgPic2) - getpichgt(g_MsgPic3)) / 2
	
	'���ƴ���
	PixelPos = PixelStart
	while PixelPos < PixelStart + Width
		showpic(g_MsgPage,g_MsgPic1,PixelPos,PosY,GetPicWid(g_MsgPic1),getpichgt(g_MsgPic1),0,0,1)
		showpic(g_MsgPage,g_MsgPic2,PixelPos,PosY + getpichgt(g_MsgPic1),GetPicWid(g_MsgPic2),getpichgt(g_MsgPic2),0,0,1)
		showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),GetPicWid(g_MsgPic3),getpichgt(g_MsgPic3),0,0,1)
		PixelPos += GetPicWid(g_MsgPic1)
	wend
	PixelPos -= GetPicWid(g_MsgPic1)
	showpic(g_MsgPage,g_MsgPic1,PixelPos,PosY,PixelStart + Width - PixelPos,getpichgt(g_MsgPic1),0,0,1)
	showpic(g_MsgPage,g_MsgPic2,PixelPos,PosY + getpichgt(g_MsgPic1),PixelStart + Width - PixelPos,getpichgt(g_MsgPic2),0,0,1)
	showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),PixelStart + Width - PixelPos,getpichgt(g_MsgPic3),0,0,1)
	showstr(g_MsgPage,96,PosY + 6,"��ܰ��ʾ")
	showstr(g_MsgPage,PixelStart + 10,PosY + getpichgt(g_MsgPic1) + 10,Text$)
	showstr(g_MsgPage,108,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2) + 4,"ȷ��")
	flippage(g_MsgPage)
	
	'����������Ӧ
	while_Escape = 1
	while while_Escape
		key = waitkey()
		if key > 0 then
			if key = key_enter or key = key_escape then
				while_Escape = 0
			end if
		else if key < 0 then
			x = getpenposx(key)
			y = getpenposy(key)
			if Inbox(x,y,PixelStart,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),PixelStart + Width,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2) + getpichgt(g_MsgPic3)) then while_Escape = 0
		end if
	wend
	
	'�˳��ӳ�ͼƬ��ʾ
	PixelPos = PixelStart
	while PixelPos < PixelStart + Width
		showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),GetPicWid(g_MsgPic3),getpichgt(g_MsgPic3),0,0,1)
		PixelPos += GetPicWid(g_MsgPic1)
	wend
	PixelPos -= GetPicWid(g_MsgPic1)
	showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),PixelStart + Width - PixelPos,getpichgt(g_MsgPic3),0,0,1)
	showstr(g_MsgPage,110,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2) + 6,"ȷ��")
	flippage(g_MsgPage)
	msdelay(300)
end sub

sub Controler()'�õ�����������ִ��
	Dim key,PenPosX,PenPosY,Temp_String$,TempTimeDelay
	key = Waitkey()
	if Key < 0 Then
		PenPosX = getpenposx(key)
		PenPosY = getpenposy(key)
		if g_FullScreen = 0 then
			if inbox(penposx,penposy,0,290,120,320) then
				showpic(-1,g_MainPic2,0,290,120 - 0,320 - 290,0,290,1)
				msdelay(300)
				LastPage()
			else if inbox(penposx,penposy,120,290,240,320) then
				showpic(-1,g_MainPic2,120,290,240 - 120,320 - 290,120,290,1)
				msdelay(300)
				NextPage()
			else if inbox(penposx,penposy,200,5,216,20) then
				showpic(-1,g_MainPic2,200,5,17,16,200,5,1)
				msdelay(300)
				showhelp()
			else if inbox(penposx,penposy,173,23,198,50) then'��ҳ
				showpic(-1,g_MainPic2,173,23,198 - 173,50 - 23,173,23,1)
				msdelay(300)
				JumpPage()
			else if inbox(penposx,penposy,47,23,72,50) then'�˳�
				showpic(-1,g_MainPic2,47,23,72 - 47,50 - 23,47,23,1)
				msdelay(300)
				ProgramExit()
			else if inbox(penposx,penposy,6,23,35,50) then'��
				showpic(-1,g_MainPic2,6,23,35 - 6,50 - 23,6,23,1)
				msdelay(300)
				SaveBookMark()
				GetTextFileName()'�õ��ı��ļ����ļ���
				LoadBookMark(g_TextFileName$)'ͨ���ı��ļ�����Ѱ�Ҳ���������ǩ
				LoadText()'������ҳ���е����ݣ�Ϊ��ʾ��׼��
			else if inbox(penposx,penposy,202,23,230,50) then'Ч��
				showpic(-1,g_MainPic2,202,23,230 - 202,50 - 23,202,23,1)
				msdelay(300)
				freeres(g_BgPic)
				if g_BgPicId < 5 then
					g_BgPicId ++
				else
					g_BgPicId = 1
				end if
				Temp_String$ = "С˵�Ķ�\Bg" + g_Ext$
				g_BgPic = LoadRes(Temp_String$, g_BgPicId)
			else if inbox(penposx,penposy,142,23,169,50) then'��ǩ
				showpic(-1,g_MainPic2,142,23,169 - 142 + 1,50 - 23 + 1,142,23,1)
				msdelay(300)
				SetBookMark()
				'Msgbox("��ǩ����Ŀǰ������ʹ�á�")
			else if inbox(penposx,penposy,101,23,140,50) then'�Զ���ҳ
				showpic(-1,g_MainPic2,101,23,140 - 101 + 1,50 - 23 + 1,101,23,1)
				msdelay(300)
				AutoTurnPage()
				'Msgbox("�Զ���ҳ����Ŀǰ������ʹ�á�")
			else if inbox(penposx,penposy,75,23,100,50) then'ʱ������
				showpic(-1,g_MainPic2,75,23,100 - 75 + 1,50 - 23 + 1,75,23,1)
				msdelay(300)
				TempTimeDelay = InputMethod("Number",g_DelayTime / 1000,1,90,"�����뷭ҳʱ����,��λΪ��(S):")
				if TempTimeDelay <> -1 then
					g_DelayTime = TempTimeDelay * 1000
				end if
			else if inbox(penposx,penposy,0,50,240,290) then
				g_FullScreen = 1
			end if
		else
			if inbox(penposx,penposy,0,10,240,110) then
				LastPage()
			else if inbox(penposx,penposy,0,110,240,210) then
				g_FullScreen = 0
			else if inbox(penposx,penposy,0,210,240,310) then
				NextPage()
			end if
		end if
	else if Key > 0 Then
		if Key = Key_Up then
			LastPage()
		else if key = key_left then
			LastPage()
		else if key = key_right then
			NextPage()
		else if key = key_down then
			NextPage()
		end if
	end if
end sub

sub ProgramExit()
	SaveBookMark()
	SaveConfig()
	end
end sub

sub SaveBookMark()
	Dim DwordData
	
	Open g_BookMarkFileName$ For Binary As #1
	
	Seek #1,8
	Put #1,g_TextPageNum
	
	DwordData = g_TextPageNow - 1
	Seek #1,16
	Put #1,DwordData
	
	g_TextLineNow = (g_TextPageNow - 1) * 14 + 1
	DwordData = g_TextLineNow - 1
	Seek #1,20
	Put #1,DwordData
	
	Seek #1,24
	Put #1,g_TextPos
	
	Close #1
end sub

Sub ShowTextPage()'�ѵ�ǰҳ����ı����������ȥ
	Dim i,Temp_String$,ProgressBarLength
	If g_FullScreen = 0 Then
		showpic(g_MainPage,g_MainPic,0,0,240,320,0,0,1)
		showpic(g_MainPage,g_BgPic,0,52,240,238,0,52,1)
		'��ʾҳ��
		Temp_String$ = Str$(g_TextPageNow) + "/" + Str$(g_TextPageNum) + "ҳ"
		showstr(g_MainPage,230 - Len(Temp_String$) * 6,8,Temp_String$)
		'��ʾ����
		showstr(g_MainPage,10,8,Mid$(g_TextFileName$,15,Len(g_TextFileName$) - 4 - 15))
		i = 0
		while i < 14
			showstr(g_MainPage,5,60 + i * 16,g_Text$(i))
			'pixlocate(5,60 + i * 12)
			'print g_Text$(i)
			i ++
		wend
		'��ʾ������
		ProgressBarLength = 1 * 222 / g_TextPageNum 
		if ProgressBarLength < 10 Then ProgressBarLength = 10
		fillpage(g_MainPage,232,60,3,224,{214C72})'BGR
		fillpage(g_MainPage,231,61 + (222 - ProgressBarLength) * (g_TextPageNow - 1) / (g_TextPageNum - 1),5,ProgressBarLength,{0F419C})
	else
		'���Ʊ���
		ShowPic(g_MainPage,g_BgPic,0,0,240,320,0,0,1)
		'FillPage(g_MainPage,0,0,240,320,0)
		'��������
		i = 0
		while i < 14
			showstr(g_MainPage,5,20 + i * 21,g_Text$(i))
			i ++
		wend
		'���ƽ�����
		ProgressBarLength = 1 * 280 / g_TextPageNum
		if ProgressBarLength < 10 Then ProgressBarLength = 12
		fillpage(g_MainPage,232,18,3,282,{214C72})'BGR
		fillpage(g_MainPage,231,19 + (280 - ProgressBarLength) * (g_TextPageNow - 1) / (g_TextPageNum - 1),5,ProgressBarLength,{0F419C})
	end if
end sub

sub LoadText()'��������
	LoadTextPos()
	LoadDataToString()
end sub

Sub ClearTextString()
	Dim i
	i = 0
	while i < 14
		g_Text$(i) = ""
		i ++
	wend
End Sub

function LoadDataToString()
	Dim FilePos,DwordData,ByteData,i,ChineseState'��ǰ���ֵĴ�ӡ״̬
	ClearTextString()
	FilePos = g_TextPos
	ChineseState = 1
	Open g_TextFileName$ For Binary As #1
	i = 0
	while (i < 14) and (FilePos < Lof(1))
		Seek #1,FilePos
		Get #1,DwordData
		if DwordData > 0 then
			ByteData = DwordData Mod 256
		else
			ByteData = DwordData Mod 256
			ByteData += 256
		end if
		'print ByteData
		'waitkey()
		if ByteData = 13 Then
			i ++
			FilePos ++'0D0AҪ���һ��
		else if ByteData > 128 then
			if Len(g_Text$(i)) = 35 and ChineseState = 1 then
				i ++
				if i < 14 then g_Text$(i) = g_Text$(i) + Chr$(ByteData)
			else if Len(g_Text$(i)) = 36 then
				i ++
				if i < 14 then g_Text$(i) = g_Text$(i) + Chr$(ByteData)
			else
				g_Text$(i) = g_Text$(i) + Chr$(ByteData)
			end if
			if ChineseState = 1 then
				ChineseState = 2
			else if ChineseState = 2 then
				ChineseState = 1
			end if
		else if ByteData < 128 Then
			if Len(g_Text$(i)) < 36 then
				g_Text$(i) = g_Text$(i) + Chr$(ByteData)
			else
				i ++
				if i < 14 then g_Text$(i) = g_Text$(i) + Chr$(ByteData)
			end if
			ChineseState = 1
		end if
		FilePos ++
	wend
	Close #1
	LoadDataToString = FilePos
end function

Sub LoadTextPos()
	if g_TextPageNow > 1 then
		Open g_BookMarkFileName$ for binary as #1
		Seek #1,(g_TextPageNow - 2) * 4 + 160
		Get #1,g_TextPos
		Close #1
	else
		g_TextPos = 0
	end if
	
end sub

function LoadBookMark(FileName$)'ͨ���ı��ļ�����Ѱ�Ҳ���������ǩ
	Dim DwordData,Temp_I
	g_BookMarkFileName$ = MID$(FileName$,0,Len(FileName$) - 3) + "cfg"
	
	Open g_BookMarkFileName$ For Binary As #1
	Seek #1,4
	Get #1,DwordData
	if DwordData <> 0 then
		Msgbox("��ǩ���������µ���ͼ�顣")
		End
	End if
	
	Seek #1,8
	Get #1,DwordData
	g_TextPageNum = DwordData
	
	Seek #1,12
	Get #1,g_TextLineNum
	
	Seek #1,16
	Get #1,DwordData
	g_TextPageNow = DwordData + 1
	
	Seek #1,20
	Get #1,DwordData
	g_TextLineNow = DwordData + 1
	
	Seek #1,24
	Get #1,g_TextPos
	
	Temp_I = 0
	While Temp_I < 4
		Seek #1,32 + Temp_I * 4
		Get #1,DwordData
		g_BookMarkAddress(Temp_I) = DwordData
		if g_BookMarkAddress(Temp_I) = -1 Then
			g_BookMarkName$(Temp_I) = ""
		else
			Seek #1,52 + Temp_I * 20
			Get #1,g_BookMarkName$(Temp_I)
		end if
		'MsgBox(g_BookMarkName$(Temp_I))
		'print g_BookMarkAddress(Temp_I)
		'print g_BookMarkName$
		Temp_I ++
	Wend
	'waitkey()
	Close #1
	
end function

function InitRes()'���������Ҫ����Դ
	dim i,Temp_String$
	
	SetLCD(240,320)
	
	if GetENV!() = Env_Sim then
		g_Ext$ = ".Rlb"
		g_MsTime = 30
	else
		g_Ext$ = ".Lib"
		g_MsTime = 0
	end if
	
	g_ReadLineFilePos = 0'���к����ļ�ƫ�Ƴ�ʼ��
	g_FullScreen = 0
	
	'�����ֿ�
	i = 0
	while i < 9
		Temp_String$ = "С˵�Ķ�\String" + g_Ext$
		g_FontRes(i) = LoadRes(Temp_String$,i + 1)
		i ++
	wend
	
	'����������ͼƬ
	Temp_String$ = "С˵�Ķ�\Res" + g_Ext$
	g_MainPic = LoadRes(Temp_String$, 1)
	g_MainPic2 = LoadRes(Temp_String$, 5)
	
	'������Ϣ��ͼƬ
	g_MsgPic1 = LoadRes(Temp_String$, 2)
	g_MsgPic2 = LoadRes(Temp_String$, 3)
	g_MsgPic3 = LoadRes(Temp_String$, 4)
	
	'�������뷨��ͼƬ
	g_InputMethodPic1 = LoadRes(Temp_String$, 6)
	g_InputMethodPic2 = LoadRes(Temp_String$, 7)
	
	'�����鱾ѡ���������ͼƬ
	g_BookSelectMenuPic1 = LoadRes(Temp_String$,10)
	
	g_MainPage = Createpage()
	g_AnimationPage = CreatePage()
	g_MsgPage = CreatePage()
	g_TempPage = CreatePage()
	
	Open "С˵�Ķ�\Config.cfg" for binary as #1
	seek #1,0
	get #1,g_BgPicId
	get #1,g_DelayTime
	Close #1
	
	if g_BgPicId > 0 and g_BgPicId < 6 Then
		g_BgPicId = g_BgPicId
	else
		g_BgPicId = 1
	end if
	
	if g_DelayTime = 0 Then 
		g_DelayTime = 9000
	End if
	
	Temp_String$ = "С˵�Ķ�\Bg" + g_Ext$
	g_BgPic = LoadRes(Temp_String$, g_BgPicId)
	
	
end function

'�ı��������
Function ShowStr(BgPage,x,y,Text$)
	dim Str_n,Str_len,Str_byte,Str_byte2,p1,p2,temlen,x0,y0
	dim tmpstr$
	temlen = 0
	Str_n = 1
	Str_len = len(Text$)
	While Str_n <= Str_len
		tmpstr$=mid$(Text$, Str_n-1, 1)
		Str_byte = asc(tmpstr$)
		Str_n ++
		if Str_byte < 128 and Str_byte > 30 then
			ShowPic(BgPage,g_FontRes(0),x,y,6,12,(str_byte Mod 32) * 6,(str_byte / 32 - 1) * 12 ,1)
			x += 6
			temlen++
		else
			tmpstr$=mid$(Text$, Str_n-1, 1)
			Str_byte2 = asc(tmpstr$)
			str_byte += 256
			str_byte2 += 256
			str_n ++
			p1 = str_byte -160
			p2 = str_byte2 - 161
			if p1 >12 then
				p2= 96 * ((p1-1) mod 12) + p2
				p1=(p1 - 1 )/12 + 1
			else
				p2 = 96 * p1 - (96 - p2 )
				p1 = 1
			end if
			if p1 <= 8 and p1>=1 then
				x0=( p2 mod 32 ) * 12
				y0=(p2 / 32 ) * 12
				Showpic(BgPage,g_FontRes(p1),x,y,12,12,x0,y0,1)
			end if
			x += 12
			temlen += 2
		end if
	Wend
	showstr=temlen
End Function

'�õ��ı��ļ����ļ���
function GetTextFileName()
	GetTextFileName = BookSelectMenu()'ѡ���鱾�Ĳ˵�
end function

'===========

'===========������
sub main()
	InitRes()'���������Ҫ����Դ
	If (GetTextFileName() <> -1) Then'�õ��ı��ļ����ļ���
		LoadBookMark(g_TextFileName$)'ͨ���ı��ļ�����Ѱ�Ҳ���������ǩ
		LoadText()'������ҳ���е����ݣ�Ϊ��ʾ��׼��
		while 1
			ShowTextPage()'�ѵ�ǰҳ����ı����������ȥ
			flippage(g_MainPage)
			Controler()'�õ�����������ִ��
		Wend
	End If
	
end sub

'===========

main()

end 


'===========�������
