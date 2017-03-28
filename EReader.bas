'BBasic
'亦云小说阅读器
'作者：Clyde
'支持原9588的书签
'支持自己生成书签
'支持横竖屏转换
'支持触摸翻页
'支持翻页动画
'支持中英文显示

'===========公共变量定义区
Dim g_TextFileName$'存放打开的文本文件名字
Dim g_BookMarkFileName$'书签文件名字
Dim g_TextPageNum'小说页面总数
Dim g_TextPageNow'当前页面
Dim g_TextLineNum
Dim g_TextLineNow
Dim g_TextPos'当前页面偏移
'Dim g_TextPosNext'下一页的偏移
Dim g_Text$(14)'读取的文本内容
Dim g_DelayTime'翻页时间间隔
Dim g_BookMarkName$(4)
Dim g_BookMarkAddress(4)
Dim g_ReadLineFilePos'读行用的文件偏移公共变量

Dim g_FontRes(9)'由9张图片存放字库
Dim g_MainPic'主界面图片
dim g_MainPic2
Dim g_MsgPic1'消息框的图片
Dim g_MsgPic2
Dim g_MsgPic3
Dim g_InputMethodPic1
Dim g_InputMethodPic2
Dim g_BgPic
Dim g_BgPicId
Dim g_BookSelectMenuPic1'书本选择主界面图片1
Dim g_MainPage'显示页面
Dim g_MsgPage'消息框用的临时页面
Dim g_AnimationPage'动画页面
Dim g_TempPage'翻页动画和其他用的临时页面
Dim g_Ext$'资源文件的后缀名
Dim g_MsTime
Dim g_FullScreen'全屏阅读模式，1是，0不是
'===========

'===========函数定义区
declare sub main()
declare sub ShowTextPage()'把当前页面的文本内容输出出去
declare sub Controler()'得到操作，并且执行
declare sub LoadText()'载入文字
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


declare function InputMethod(Mode$,defaultNum,MinNum,MaxNum,Text$)'输入法  模式，默认值,最小值 最大值 提示
declare function InitRes()'载入程序需要的资源
declare function GetTextFileName()'得到文本文件的文件名
declare function LoadBookMark(FileName$)'通过文本文件名来寻找并且载入书签
declare Function ShowStr(BgPage,x,y,Text$)'文本输出函数
declare function InBox(x,y,x1,y1,x2,y2)
declare function ReadLineFromText$(IntPos)'功能：从文本文档中读入一行，并返回这行的字符串  参数：读行的初始位置，如果初始位置为-1，则表示从当前文件句柄读行
declare function fgetc(add)
declare function StrToInt(Text$)
declare Function BookSelectMenu()

'===========

'===========函数代码区


sub SaveBookMarkId(BookMarkId)
	'MsgBox(Str$(BookMarkId))
	
	g_BookMarkAddress(BookMarkId) = g_TextPos
	g_BookMarkName$(BookMarkId) = Mid$(g_Text$(0),0,12)
	
	'Msgbox(g_BookMarkName$(BookMarkId))
	'Msgbox(g_BookMarkFileName$)
	Open g_BookMarkFileName$ For Binary As #1
	
	'存入书签地址
	Seek #1,32 + BookMarkId * 4
	Put #1,g_BookMarkAddress(BookMarkId)
	'存入书签名字
	Seek #1,52 + BookMarkId * 20
	Put #1,g_BookMarkName$(BookMarkId)
	
	Close #1
end sub

sub LoadBookMarkId(BookMarkId)
	'MsgBox(Str$(BookMarkId))
	Dim i,j,m,DwordData,WhileEscape
	Open g_BookMarkFileName$ For Binary As #1
	'搜索书签地址
	Seek #1,32 + BookMarkId * 4
	Get #1,DwordData
	if DwordData = 0 then
		g_TextPos = DwordData
		g_TextPageNow = 1
	else if DwordData = -1 then
		Msgbox("该书签不存在！")
	else
		g_TextPos = DwordData
		'根据地址用二分法查找页面
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
		'说明没有找到一样的，只能寻找最接近的
		if i > j then
			g_TextPageNow = (m - 160) / 4 + 2
		end if
	end if
	Close #1
	LoadText()
end sub

'功能：显示页面切换动画
'参数：原页面，目标页面，动画显示坐标X1Y1,X2Y2的矩形区域,切换模式
'注意：模式，1：从左到右，2：从右到左，3：从上到下，4：从下到上
sub PageSlide(Page1,Page2,x1,y1,x2,y2,Mode)
	Dim PosX1,PosY1,PosX2,PosY2'1原页面坐标，2目标页面坐标
	Dim VX,VY,AX,AY'X方向的速度，Y方向的速度，加速度
	PosX1 = x1
	PosY1 = y1
	if Mode = 1 Then
		'从左到右
		PosX2 = x1 - (x2 - x1 + 1)
		PosY2 = y1
		VX = 7
		VY = 0
		AX = 7
		AY = 0
	else if Mode = 2 Then
		'从右到左
		PosX2 = x2
		PosY2 = y1
		VX = -7
		VY = 0
		AX = -7
		AY = 0
	else if Mode = 3 Then
		'从上到下
		PosX2 = x1
		PosY2 = y1 - (y2 - y1 + 1)
		VX = 0
		VY = 7
		AX = 0
		AY = 7
	else if Mode = 4 Then
		'从下到上
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

'功能：把字符串转换成数字
'参数：字符串
'返回：整数
'说明：替换Val函数，测试BUG
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

'功能：从文件1中读取add偏移下的byte数据
'参数：文件偏移add
'返回：word但是值在0～255内
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

'功能：从文件位置1的文本文档中读入一行，并返回这行的字符串
'参数：读行的初始位置，如果初始位置为-1，则表示从当前文件偏移处读行
'返回：字符串
function ReadLineFromText$(IntPos)
	Dim TempString$'临时储存需要返回的字符串的字符串变量
	Dim TempData'储存临时的BYTE数据，需要MOD 256之后才可以使用
	Dim FilePos'文件偏移
	Dim FileLen'文件长度
	Dim WhileEscape
	
	'字符串初始化
	TempString$ = ""
	
	'设置初始偏移
	if IntPos > -1 then
		FilePos = IntPos
	else
		FilePos = g_ReadLineFilePos
	end if
	
	'循环读字符直到读到回车或者读完文件为止
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
			TempString$ = "" + TempString$ + Chr$(TempData)'防止编译错误
		end if
	wend
	g_ReadLineFilePos = FilePos + 1
	'清楚换行符，默认为WINDOWS风格文本文档
	ReadLineFromText$ = TempString$
end function


'功能：书本选择菜单
'参数：无
'返回：选择的书的ID，返回-1代表退出
Function BookSelectMenu()
	Dim TempString$
	Dim Key,PenPosX,PenPosY
	Dim BookSelectMenuWhileEscape,DeleteMode
	Dim BookNum$'书本数目
	Dim BookFileName$(9)'目前认为最多为9本书，每本书的文件名
	Dim BookPicture(9)'分别代表当前显示的9本书本的封面
	Dim i
	Dim BookSelected
	Dim BookPicShadow,BookPicDefault
	
	'初始化书本的文件名
	i = 0
	While i < 9
		BookFileName$ = ""
		i ++
	Wend
	
	'载入书本阴影图片
	TempString$ = "小说阅读\BookPic" + g_Ext$
	BookPicShadow = LoadRes(TempString$,1)
	'载入书本默认封面
	BookPicDefault = LoadRes(TempString$,2)
	
	'获取书本信息
	Open "小说阅读\\BooksConfig.txt" for Binary as #1
	'获得书本数目
	BookNum$ = ReadLineFromText$(0)
	i = 0
	While i < Val(BookNum$)
		BookFileName$(i) = ReadLineFromText$(-1)
		'Print BookFileName$(i)
		i ++
	Wend
	'WaitKey()
	'获得书本名称和封面，如果封面载入为-1，则用默认封面代替
	'目前还没有打算加入自定义封面
	i = 0
	While i < 9
		BookPicture(i) = BookPicDefault
		i ++
	Wend
	
	Close #1
	
	
	BookSelectMenuWhileEscape = 1
	DeleteMode = 0
	while BookSelectMenuWhileEscape = 1
		'显示主界面
		ShowPic(g_MainPage,g_BookSelectMenuPic1,0,0,240,320,0,0,1)
		'显示书本图片1～9，最多9张
		i = 0
		While i < Val(BookNum$)
			'绘制图本影子
			ShowPic(g_MainPage,BookPicShadow,25 + (i Mod 3) * 72,50 + (i / 3) * 88,54,66,0,0,1)
			'绘制图书贴图
			ShowPic(g_MainPage,BookPicture(i),25 + (i Mod 3) * 72,50 + (i / 3) * 88,47,59,0,0,1)
			'绘制图书名字
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
			'返回按钮
			if inbox(PenPosX,PenPosY,10,0,60,30) then
				BookSelectMenuWhileEscape = 0
				BookSelected = -1
			'选择书本
			else if inbox(PenPosX,PenPosY,25,50,223,304) then
				BookSelected = (PenPosY - 50) / 85 * 3 + (PenPosX - 25) / 66
				If (BookSelected < Val(BookNum$)) Then
					BookSelectMenuWhileEscape = 0
					g_TextFileName$ = "小说阅读\Books\" + BookFileName$(BookSelected) + ".txt"
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
	'Msgbox("书签功能目前还不能使用。")
	Dim BookMarkPic1,BookMarkPic2
	Dim TempString$,WhileEscape
	Dim Key,i,PenPosX,PenPosY
	Dim BookMarkId
	TempString$ = "小说阅读\Res" + g_Ext$
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
					'保存数字的图标处
					if inbox(PenPosX,PenPosY,120,67,140,149) then
						ShowPic(-1,BookMarkPic2,118,67 + (BookMarkId) * 22,24,22,8,13 + (BookMarkId) * 22,1)
						MsDelay(300)
						SaveBookMarkId(BookMarkId)
					'文字处
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
	MsgBox("自动翻页已经结束！")
end sub

sub SaveConfig()
	Open "小说阅读\config.cfg" for binary as #1
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
					Msgbox("输入的数字不在正确范围内！")
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
					Msgbox("输入的数字不在正确范围内！")
				end if
			end if
		end if
		if len(TempString$) > 20 then
			Msgbox("字符长度已经达到最大！")
			TempString$ = Mid$(TempString$,0,20)
		end if
	wend
	flippage(g_AnimationPage)
	InputMethod = TempNum
end function

sub showhelp()
	MsgBox("作者：Clyde 请到BBK9588贴吧关注更新")
end sub

sub JumpPage()'跳转页面
	Dim Temp_Page
	Temp_Page = InputMethod("Number",g_TextPageNow,1,g_TextPageNum,"请输入要跳转的页面：")'输入法  模式，最小值 最大值
	If Temp_Page <> -1 then
		if Temp_Page > g_TextPageNow then
			g_TextPageNow = Temp_Page - 1
			NextPage()
		else if Temp_Page < g_TextPageNow then
			g_TextPageNow = Temp_Page + 1
			LastPage()
		else if Temp_Page = g_TextPageNow then
			MsgBox("该页面已经是目标页面。")
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
		MsgBox("已经浏览至最前！")
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
		MsgBox("已经浏览至最后！")
	end if
end sub

Sub MsgBox(Text$)
	Dim width,PixelStart,PixelPos,PosY,x,y,key,while_Escape
	
	BITBLTPAGE(g_MsgPage,-1)
	width = len(Text$) * 6 + 20
	PixelStart = (240 - Width) / 2
	PosY = (320 - getpichgt(g_MsgPic1) - getpichgt(g_MsgPic2) - getpichgt(g_MsgPic3)) / 2
	
	'绘制窗口
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
	showstr(g_MsgPage,96,PosY + 6,"温馨提示")
	showstr(g_MsgPage,PixelStart + 10,PosY + getpichgt(g_MsgPic1) + 10,Text$)
	showstr(g_MsgPage,108,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2) + 4,"确定")
	flippage(g_MsgPage)
	
	'按键操作反应
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
	
	'退出延迟图片显示
	PixelPos = PixelStart
	while PixelPos < PixelStart + Width
		showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),GetPicWid(g_MsgPic3),getpichgt(g_MsgPic3),0,0,1)
		PixelPos += GetPicWid(g_MsgPic1)
	wend
	PixelPos -= GetPicWid(g_MsgPic1)
	showpic(g_MsgPage,g_MsgPic3,PixelPos,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2),PixelStart + Width - PixelPos,getpichgt(g_MsgPic3),0,0,1)
	showstr(g_MsgPage,110,PosY + getpichgt(g_MsgPic1) + getpichgt(g_MsgPic2) + 6,"确定")
	flippage(g_MsgPage)
	msdelay(300)
end sub

sub Controler()'得到操作，并且执行
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
			else if inbox(penposx,penposy,173,23,198,50) then'跳页
				showpic(-1,g_MainPic2,173,23,198 - 173,50 - 23,173,23,1)
				msdelay(300)
				JumpPage()
			else if inbox(penposx,penposy,47,23,72,50) then'退出
				showpic(-1,g_MainPic2,47,23,72 - 47,50 - 23,47,23,1)
				msdelay(300)
				ProgramExit()
			else if inbox(penposx,penposy,6,23,35,50) then'打开
				showpic(-1,g_MainPic2,6,23,35 - 6,50 - 23,6,23,1)
				msdelay(300)
				SaveBookMark()
				GetTextFileName()'得到文本文件的文件名
				LoadBookMark(g_TextFileName$)'通过文本文件名来寻找并且载入书签
				LoadText()'载入在页面中的数据，为显示做准备
			else if inbox(penposx,penposy,202,23,230,50) then'效果
				showpic(-1,g_MainPic2,202,23,230 - 202,50 - 23,202,23,1)
				msdelay(300)
				freeres(g_BgPic)
				if g_BgPicId < 5 then
					g_BgPicId ++
				else
					g_BgPicId = 1
				end if
				Temp_String$ = "小说阅读\Bg" + g_Ext$
				g_BgPic = LoadRes(Temp_String$, g_BgPicId)
			else if inbox(penposx,penposy,142,23,169,50) then'书签
				showpic(-1,g_MainPic2,142,23,169 - 142 + 1,50 - 23 + 1,142,23,1)
				msdelay(300)
				SetBookMark()
				'Msgbox("书签功能目前还不能使用。")
			else if inbox(penposx,penposy,101,23,140,50) then'自动翻页
				showpic(-1,g_MainPic2,101,23,140 - 101 + 1,50 - 23 + 1,101,23,1)
				msdelay(300)
				AutoTurnPage()
				'Msgbox("自动翻页功能目前还不能使用。")
			else if inbox(penposx,penposy,75,23,100,50) then'时间设置
				showpic(-1,g_MainPic2,75,23,100 - 75 + 1,50 - 23 + 1,75,23,1)
				msdelay(300)
				TempTimeDelay = InputMethod("Number",g_DelayTime / 1000,1,90,"请输入翻页时间间隔,单位为秒(S):")
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

Sub ShowTextPage()'把当前页面的文本内容输出出去
	Dim i,Temp_String$,ProgressBarLength
	If g_FullScreen = 0 Then
		showpic(g_MainPage,g_MainPic,0,0,240,320,0,0,1)
		showpic(g_MainPage,g_BgPic,0,52,240,238,0,52,1)
		'显示页数
		Temp_String$ = Str$(g_TextPageNow) + "/" + Str$(g_TextPageNum) + "页"
		showstr(g_MainPage,230 - Len(Temp_String$) * 6,8,Temp_String$)
		'显示书名
		showstr(g_MainPage,10,8,Mid$(g_TextFileName$,15,Len(g_TextFileName$) - 4 - 15))
		i = 0
		while i < 14
			showstr(g_MainPage,5,60 + i * 16,g_Text$(i))
			'pixlocate(5,60 + i * 12)
			'print g_Text$(i)
			i ++
		wend
		'显示进度条
		ProgressBarLength = 1 * 222 / g_TextPageNum 
		if ProgressBarLength < 10 Then ProgressBarLength = 10
		fillpage(g_MainPage,232,60,3,224,{214C72})'BGR
		fillpage(g_MainPage,231,61 + (222 - ProgressBarLength) * (g_TextPageNow - 1) / (g_TextPageNum - 1),5,ProgressBarLength,{0F419C})
	else
		'绘制背景
		ShowPic(g_MainPage,g_BgPic,0,0,240,320,0,0,1)
		'FillPage(g_MainPage,0,0,240,320,0)
		'绘制文字
		i = 0
		while i < 14
			showstr(g_MainPage,5,20 + i * 21,g_Text$(i))
			i ++
		wend
		'绘制进度条
		ProgressBarLength = 1 * 280 / g_TextPageNum
		if ProgressBarLength < 10 Then ProgressBarLength = 12
		fillpage(g_MainPage,232,18,3,282,{214C72})'BGR
		fillpage(g_MainPage,231,19 + (280 - ProgressBarLength) * (g_TextPageNow - 1) / (g_TextPageNum - 1),5,ProgressBarLength,{0F419C})
	end if
end sub

sub LoadText()'载入文字
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
	Dim FilePos,DwordData,ByteData,i,ChineseState'当前汉字的打印状态
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
			FilePos ++'0D0A要多加一个
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

function LoadBookMark(FileName$)'通过文本文件名来寻找并且载入书签
	Dim DwordData,Temp_I
	g_BookMarkFileName$ = MID$(FileName$,0,Len(FileName$) - 3) + "cfg"
	
	Open g_BookMarkFileName$ For Binary As #1
	Seek #1,4
	Get #1,DwordData
	if DwordData <> 0 then
		Msgbox("书签错误，请重新导入图书。")
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

function InitRes()'载入程序需要的资源
	dim i,Temp_String$
	
	SetLCD(240,320)
	
	if GetENV!() = Env_Sim then
		g_Ext$ = ".Rlb"
		g_MsTime = 30
	else
		g_Ext$ = ".Lib"
		g_MsTime = 0
	end if
	
	g_ReadLineFilePos = 0'读行函数文件偏移初始化
	g_FullScreen = 0
	
	'载入字库
	i = 0
	while i < 9
		Temp_String$ = "小说阅读\String" + g_Ext$
		g_FontRes(i) = LoadRes(Temp_String$,i + 1)
		i ++
	wend
	
	'载入主界面图片
	Temp_String$ = "小说阅读\Res" + g_Ext$
	g_MainPic = LoadRes(Temp_String$, 1)
	g_MainPic2 = LoadRes(Temp_String$, 5)
	
	'载入消息框图片
	g_MsgPic1 = LoadRes(Temp_String$, 2)
	g_MsgPic2 = LoadRes(Temp_String$, 3)
	g_MsgPic3 = LoadRes(Temp_String$, 4)
	
	'载入输入法的图片
	g_InputMethodPic1 = LoadRes(Temp_String$, 6)
	g_InputMethodPic2 = LoadRes(Temp_String$, 7)
	
	'载入书本选择界面的相关图片
	g_BookSelectMenuPic1 = LoadRes(Temp_String$,10)
	
	g_MainPage = Createpage()
	g_AnimationPage = CreatePage()
	g_MsgPage = CreatePage()
	g_TempPage = CreatePage()
	
	Open "小说阅读\Config.cfg" for binary as #1
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
	
	Temp_String$ = "小说阅读\Bg" + g_Ext$
	g_BgPic = LoadRes(Temp_String$, g_BgPicId)
	
	
end function

'文本输出函数
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

'得到文本文件的文件名
function GetTextFileName()
	GetTextFileName = BookSelectMenu()'选择书本的菜单
end function

'===========

'===========主函数
sub main()
	InitRes()'载入程序需要的资源
	If (GetTextFileName() <> -1) Then'得到文本文件的文件名
		LoadBookMark(g_TextFileName$)'通过文本文件名来寻找并且载入书签
		LoadText()'载入在页面中的数据，为显示做准备
		while 1
			ShowTextPage()'把当前页面的文本内容输出出去
			flippage(g_MainPage)
			Controler()'得到操作，并且执行
		Wend
	End If
	
end sub

'===========

main()

end 


'===========程序结束
