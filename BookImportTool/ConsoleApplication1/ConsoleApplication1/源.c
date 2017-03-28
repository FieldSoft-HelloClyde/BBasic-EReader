#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct BookFileList
{
	int BookNum;
	char* BookFileName[9];
}BOOKFILELIST;

BOOKFILELIST MyBookFileList;

void ReadBookNameFromFile(char* FileName);
void ShowBookFileName();
void MakeBookMark(char* BookName);
long NextPage(FILE *fp,long *add,long *line);
void MakeConfig(void);

void main(void)
{
	int i;
	printf("BBK9588 贴吧 小说阅读器 PC导入程序 V1.0\n作者：HelloClyde\n正在载入当前目录下的TXT文件列表....\n");
	remove("..//file_list.txt");
	system("DIR /b *.txt >> ..//file_list.txt");
	//把文件列表中的文件读取到数组里
	ReadBookNameFromFile("..//file_list.txt");
	remove("..//file_list.txt");
	printf("找到%d本小说\n",MyBookFileList.BookNum);
	//根据TXT文件名进行书签生成。
	for (i = 0;i < MyBookFileList.BookNum;i ++)
	{
		printf("正在生成%s的书签...\n",MyBookFileList.BookFileName[i]);
		MakeBookMark(MyBookFileList.BookFileName[i]);
	}
	printf("正在重新生成配置文件...\n");
	MakeConfig();
	printf("导入完毕！\n");
	//ShowBookFileName();
	getchar();
}

void MakeConfig(void)
{
	FILE *CfgFp;
	int i;

	CfgFp = fopen("..//BooksConfig.txt","wb");
	fputc(MyBookFileList.BookNum + '0',CfgFp);
	fputc(13,CfgFp);
	fputc(10,CfgFp);
	for (i = 0;i < MyBookFileList.BookNum;i ++)
	{
		MyBookFileList.BookFileName[i][strlen(MyBookFileList.BookFileName[i]) - 4] = '\0';
		fputs(MyBookFileList.BookFileName[i],CfgFp);
		fputc(13,CfgFp);
		fputc(10,CfgFp);
	}
	fclose(CfgFp);
}

void ReadBookNameFromFile(char* FileName)
{
	FILE *fp = fopen(FileName,"rb");
	char TempString[1000];
	char i;

	if (fp == NULL)
	{
		printf("打开%s文件错误！\n",FileName);
		getchar();
		exit(0);
	}

	i = 0;
	while (!feof(fp))
	{
		TempString[0] = '\0';
		fgets(TempString,1000,fp);
		if (TempString[strlen(TempString) - 2] == 13)
			TempString[strlen(TempString) - 2] = '\0';
		if (strcmp(TempString,"") != 0)
		{
			MyBookFileList.BookFileName[i] = (char*)malloc(strlen(TempString) + 1);
			strcpy(MyBookFileList.BookFileName[i],TempString);
			i ++;
		}
		if (i >= 9)
		{
			printf("目录中的小说多于9本，导入程序只会处理前9本小说，尽请谅解。\n");
			break;
		}
	}

	MyBookFileList.BookNum = i;
}

void ShowBookFileName()
{
	int i;
	printf("%d\n",MyBookFileList.BookNum);
	for (i = 0; i < 9; i++)
	{
		printf("%s\n",MyBookFileList.BookFileName[i]);
	}
}

void MakeBookMark(char* BookName)
{
	char BookMarkName[256];
	char BookMarkInitData[0x9C] = {
		0x4B,0x4F,0x4F,0x42,//标识符
		0,0,0,0,//字体大小
		0,0,0,0,//总页面
		0,0,0,0,//总行数
		0,0,0,0,//当前页数-1
		0,0,0,0,//当前行数-1
		0,0,0,0,//当前行的偏移
		0,0,0,0,//文件大小
		0xFF,0xFF,0xFF,0xFF,//书签1偏移
		0xFF,0xFF,0xFF,0xFF,//书签2偏移
		0xFF,0xFF,0xFF,0xFF,//书签3偏移
		0xFF,0xFF,0xFF,0xFF,//书签4偏移
		0xFF,0xFF,0xFF,0xFF,//书签5偏移
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//书签1名字
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//书签2名字
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//书签3名字
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//书签4名字
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0//书签5名字
	};
	long FilePos = 0;
	long PageAddress;
	long PageNum;
	long LineNum;
	long FileLength;
	int OldPrecent,NowPrecent;
	FILE *fp = NULL;
	FILE *TxtFp = NULL;

	strcpy(BookMarkName,BookName);
	BookMarkName[strlen(BookMarkName) - 4] = '\0';
	strcat(BookMarkName,".cfg");
	//printf("书签名为%s\n",BookMarkName);

	fp = fopen(BookMarkName,"wb");
	if (fp == NULL)
	{
		printf("打开%s文件错误！\n",BookMarkName);
		return;
	}

	TxtFp = fopen(BookName,"rb");
	if (TxtFp == NULL)
	{
		printf("打开%s文件错误！\n",BookName);
		return;
	}

	/*
	for (i = 0;i < 0x9C;i ++)
	{
		if (i % 4 == 0)
			printf("\n");
		printf("%2X ",BookMarkInitData[i]);
		
	}
	*/

	printf("==================\n");

	//获取文件长度
	fseek(TxtFp,0,SEEK_SET);
	fseek(TxtFp,0,SEEK_END);
	FileLength = ftell(TxtFp);

	//Init File
	fseek(fp,FilePos,0);
	fwrite(BookMarkInitData,0x9C,1,fp);

	//
	PageNum = 0;
	LineNum = 0;
	PageAddress = 0;
	OldPrecent = 0;
	while (!feof(TxtFp))
	{
		fseek(fp,0x9C + PageNum * 4,0);
		fwrite(&PageAddress,4,1,fp);
		NextPage(TxtFp,&PageAddress,&LineNum);
		NowPrecent = PageAddress * 100 / FileLength;
		if (NowPrecent - OldPrecent > 10)
		{
			printf("**");
			OldPrecent = NowPrecent;
		}
		//printf("%d %d\n",PageAddress,LineNum);
		PageNum ++;
	}

	printf("\n");

	//文件大小
	fseek(fp,0x1C,0);
	fwrite(&PageAddress,4,1,fp);
	//页数
	fseek(fp,0x8,0);
	fwrite(&PageNum,4,1,fp);
	//行数
	fseek(fp,0xC,0);
	fwrite(&LineNum,4,1,fp);

	fclose(fp);
	fclose(TxtFp);
}

long NextPage(FILE *fp,long *add,long *line)
{
	unsigned char TempChar;
	int LinePos;
	int OldLine;

	OldLine = *line;
	LinePos = 0;
	while (!feof(fp))
	{
		fseek(fp,*add,0);
		fread(&TempChar,1,1,fp);
		//printf("ox%x %x\n",*add,TempChar);
		if (TempChar > 128)
		{
			if (LinePos < 35)
			{
				(*add) += 2;
				LinePos += 2;
			}
			else
			{
				(*line) ++;
				if ((*line) - OldLine >= 14)
					return *add;
				(*add) += 2;
				LinePos = 2;
			}
		}
		else
		{
			if (TempChar == 13)
			{
				(*add) += 2;
				LinePos = 0;
				(*line) ++;
				if ((*line) - OldLine >= 14)
					return *add;
			}
			else if (TempChar == 10)
			{
				(*add) ++;
			}
			else
			{
				if (LinePos < 36)
				{
					(*add) ++;
					LinePos ++;
				}
				else
				{
					(*line) ++;
					if ((*line) - OldLine >= 14)
						return *add;
					(*add) ++;
					LinePos = 1;
				}
			}
		}
	}
	return *add;
}