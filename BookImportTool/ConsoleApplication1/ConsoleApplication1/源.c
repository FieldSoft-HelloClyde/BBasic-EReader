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
	printf("BBK9588 ���� С˵�Ķ��� PC������� V1.0\n���ߣ�HelloClyde\n�������뵱ǰĿ¼�µ�TXT�ļ��б�....\n");
	remove("..//file_list.txt");
	system("DIR /b *.txt >> ..//file_list.txt");
	//���ļ��б��е��ļ���ȡ��������
	ReadBookNameFromFile("..//file_list.txt");
	remove("..//file_list.txt");
	printf("�ҵ�%d��С˵\n",MyBookFileList.BookNum);
	//����TXT�ļ���������ǩ���ɡ�
	for (i = 0;i < MyBookFileList.BookNum;i ++)
	{
		printf("��������%s����ǩ...\n",MyBookFileList.BookFileName[i]);
		MakeBookMark(MyBookFileList.BookFileName[i]);
	}
	printf("�����������������ļ�...\n");
	MakeConfig();
	printf("������ϣ�\n");
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
		printf("��%s�ļ�����\n",FileName);
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
			printf("Ŀ¼�е�С˵����9�����������ֻ�ᴦ��ǰ9��С˵�������½⡣\n");
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
		0x4B,0x4F,0x4F,0x42,//��ʶ��
		0,0,0,0,//�����С
		0,0,0,0,//��ҳ��
		0,0,0,0,//������
		0,0,0,0,//��ǰҳ��-1
		0,0,0,0,//��ǰ����-1
		0,0,0,0,//��ǰ�е�ƫ��
		0,0,0,0,//�ļ���С
		0xFF,0xFF,0xFF,0xFF,//��ǩ1ƫ��
		0xFF,0xFF,0xFF,0xFF,//��ǩ2ƫ��
		0xFF,0xFF,0xFF,0xFF,//��ǩ3ƫ��
		0xFF,0xFF,0xFF,0xFF,//��ǩ4ƫ��
		0xFF,0xFF,0xFF,0xFF,//��ǩ5ƫ��
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//��ǩ1����
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//��ǩ2����
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//��ǩ3����
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,//��ǩ4����
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0//��ǩ5����
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
	//printf("��ǩ��Ϊ%s\n",BookMarkName);

	fp = fopen(BookMarkName,"wb");
	if (fp == NULL)
	{
		printf("��%s�ļ�����\n",BookMarkName);
		return;
	}

	TxtFp = fopen(BookName,"rb");
	if (TxtFp == NULL)
	{
		printf("��%s�ļ�����\n",BookName);
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

	//��ȡ�ļ�����
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

	//�ļ���С
	fseek(fp,0x1C,0);
	fwrite(&PageAddress,4,1,fp);
	//ҳ��
	fseek(fp,0x8,0);
	fwrite(&PageNum,4,1,fp);
	//����
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