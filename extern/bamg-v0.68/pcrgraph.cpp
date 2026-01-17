#include <iostream>
#include <windows.h>
#include <commdlg.h>
#include <direct.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <setjmp.h>

#include <new.h>
#include <iostream.h>
#include <fstream.h>
//#include <memory.h>

using namespace std; //introduces namespace std


#include "rgraph.h"

template<class T> inline T Min (const T &a,const T &b){return a < b ? a : b;}
template<class T> inline T Max (const T &a,const T & b){return a > b ? a : b;}

void 	out_of_memory ();
void 	NEW_HANDLER (void);
void 	myexit();
float 	scali(int i);
float 	scalj(int j);
void 	execute(char* what);
char 	Getijc(int & x,int & y);
char 	Getxyc(float &x,float &y);
int 	getcolor();
void	putpixel(int ix,int iy, int couleur);
int 	scalx(float x);
int 	scaly(float y);
void 	compile (char *);
void 	rattente (int);
void 	inittext();
BOOL 	ShowOpenDialogBox(char *fileName);

char szProgName[] = "freefem+"; 
char errbuf[255], myiobuf[255];
float rayon;
static float aspx, aspy, echx,echy,ech,rxmin,rxmax,rymin,rymax;  
static int Ax,Ay,Lx,Ly,currx,curry, carre, lacouleur;
int NbPlotTotal, jh=20,jv=10;
char shortName[256],fullName[256];                     
HWND        hWnd;
WNDCLASS    rClass;
HDC hdc;
static void* StdCHandle=0;

void out_of_memory ()
{
  cout << " error: operator new failed; not enough memory" << endl;
  myexit (-1);
}

void erreur(char *s)
{
 char se[256],prj[256];
 strcpy(se,shortName);
 strcat(se," : Syntax Error\n");
 strcat(se,s);
 cout << se << endl;
 //MessageBox(NULL,se,"FREEFEM SOLVER",MB_OK|MB_SYSTEMMODAL|MB_ICONEXCLAMATION);
 rattente(0);
  myexit(0);
}

void SetColorTable(int n){}
void raffpoly(int n, float *poly){}
int getprog(char* fn,int argc, char** argvptr){  return 0;}
void execute(char* what){}
void couleur(int c){}
void penthickness(int pepais){}
void showgraphic(){}
void myexit(int i){  rattente(0); exit(0);}

void NEW_HANDLER (void){  set_new_handler (&out_of_memory);}
								
void getcadre(float &xmin,float &xmax,float &ymin,float &ymax)
{
  xmin = rxmin;
  xmax = rxmax;
  ymin = rymin;
  ymax = rymax;

}
void cadre(float xmin,float xmax,float ymin,float ymax)
{
RECT rc;
int a,b,le,Ex,Ey;
le=20;
float px,py;
GetClientRect(hWnd, &rc);
rxmin = xmin;
rxmax = xmax;
rymin = ymin;
rymax = ymax;
px=xmax-xmin;
py=ymax-ymin; 
 if ( px > py)
    {/* Our Object larger than long */
     Ax=le; Lx=((rc.right-rc.left)-2*le);
     Ex=Lx; Ey=(int)(Ex*(py/px));
     b=((rc.bottom-rc.top)-Ey-2*le)/2;
     Ay=b+le; Ly=Ey;
     if (Ey>(rc.bottom-rc.top)-2*le){Ay=le; Ly=((rc.bottom-rc.top)-2*le);
     						          Ey=Ly; Ex=(int)(Ey*(px/py));
									  a=((rc.right-rc.left)-Ex-2*le)/2;
     								  Ax=a+le; Lx=Ex;
                                     }
    }
else
    {/* Our Object longer than large */
     Ay=le; Ly=((rc.bottom-rc.top)-2*le);
     Ey=Ly; Ex=(int)(Ey*(px/py));
     a=((rc.right-rc.left)-Ex-2*le)/2;
     Ax=a+le; Lx=Ex;                     
     if (Ex>(rc.right-rc.left)-2*le){Ax=le; Lx=((rc.right-rc.left)-2*le);
     								 Ex=Lx; Ey=(int)(Ex*(py/px));
     								 b=((rc.bottom-rc.top)-Ey-2*le)/2;
     								 Ay=b+le; Ly=Ey;
     								}
     
    }
echx=1/(xmax-xmin);
echy=1/(ymax-ymin); 
    
}

int scalx(float x)
{
 int test;
 test=(int)(Ax+Lx*(x-rxmin)*echx);
 return test;
}

int scaly(float y)
{
int test;
 test=(int)(Ay+Ly*(rymax-y)*echy);
 return test;                                                  
}

void rmoveto(float x, float y)
{
 currx = scalx(x);
 curry = scaly(y);
 //MoveTo(hdc,scalx(x), scaly(y));
}				  

void rlineto(float x, float y)
{
  int newx = scalx(x), newy = scaly(y);
  MoveToEx(hdc,currx,curry,NULL);
  LineTo(hdc,newx,newy);
  currx = newx; curry = newy;
}

void cadreortho(float centrex, float centrey, float ray)
{								  ///
  cadre(centrex-ray, centrex+ray, centrey-ray, centrey+ray);
}

int InRecScreen(float x1, float y1,float x2, float y2)
{  
  float xi = Min(x1,x2),xa=Max(x1,x2);
  float yi = Min(y1,y2),ya=Max(y1,y2);
  return (xa >= rxmin) && (xi <= rxmax) && (ya >= rymin) && (yi <= rymax);
}

int InPtScreen( float x, float y)
{
  return (x >= rxmin) && (x <= rxmax) && (y >= rymin) && (y <= rymax);
}

float scali(int i){  return i/echx  + rxmin;}
float scalj(int j) {  return -j/echy  + rymax;}
char Getijc(int & x,int & y){ char char1=' '; return char1;}

char Getxyc(float &x,float &y)
{ 
  char c=' ';
  int i=0,j=0;
 // c = Getijc( i,j);
  x = scali(i);
  y = scalj(j);
  return c;
}

void initgraphique(void)       
{ 
 RECT rc;
 GetClientRect(hWnd, &rc);
 aspx = (float)(rc.right - rc.left);
 aspy = (float)(rc.bottom - rc.top);
 carre = aspx == aspy;
}

void closegraphique(void)
{	  
	ReleaseDC(hWnd,hdc); 
}


void rattente(int waitm)
{
	 char c=' ';
//    you may prefer to use carriage return to move to the next graph
//	 getc(stdin);             
	
	if (waitm>1) return;
	TextOut(hdc,0,0,"                     ",20);
	TextOut(hdc,0,0,"Click to continue...",19);
	MSG msg;      
	if (!waitm)
		do
		{
			GetMessage(&msg,hWnd,WM_MOUSEFIRST,WM_MOUSELAST);
			if (msg.message==WM_RBUTTONDOWN)
		     myexit(0); 
		}
		while (msg.message!=WM_LBUTTONDOWN); 
	
	TextOut(hdc,0,0,"                     ",20);
	TextOut(hdc,0,0,"FreeFem works...",16);
}

void reffecran(void)
{     
 HBRUSH hbr;
 RECT rc;

 GetClientRect(hWnd, &rc);
 hbr = CreateSolidBrush(RGB(255, 255, 255));
 FillRect(hdc,&rc,hbr);
 DeleteObject(hbr);
}

BOOL ShowOpenDialogBox(char *fileName)
{
	OPENFILENAME ofn; 
	char szDirName[256];   
	char *strFilter="PCgFEM Files (*.edp)\0*.edp\0All Files (*.*)\0*.*\0\0"; 
	
	memset(&ofn, 0, sizeof(OPENFILENAME));
	getcwd(szDirName,sizeof(szDirName));
	ofn.lStructSize = sizeof(OPENFILENAME);
	ofn.hwndOwner = NULL;
	ofn.lpstrFilter = strFilter;
	ofn.lpstrFileTitle = fileName;
	ofn.nMaxFileTitle = 80;
	ofn.lpstrInitialDir=szDirName;
	ofn.lpstrTitle ="Choose you freefem '*.edp' File";
	ofn.Flags=OFN_SHOWHELP|OFN_PATHMUSTEXIST|OFN_FILEMUSTEXIST;
	
	return GetOpenFileName(&ofn);
}    

int main ( void )
{
	
	cout << "Start freefem" <<endl;
	hWnd = GetForegroundWindow();
	hdc=GetDC(hWnd);
   	if (ShowOpenDialogBox(shortName)==FALSE) 
      myexit(0);
      
    strcpy(fullName,shortName);
	compile(fullName);
	return 0;
}
