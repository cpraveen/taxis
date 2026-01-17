/**************DO NOR REMOVE THIS BANNER***************/
/*  FreeFEM : Language for a Finite Element Method    */
/*  -------    Release 1.0:  June 1994.               */
/*  Authors: D. Bernardi, Y. Darmaillac F. Hecht,     */
/*           O. Pironneau                             */
/*  You may copy freely these files and use it for    */
/* teaching or research. These or part of these may   */
/* not be sold or used for a commercial purpose with- */
/* out our consent : fax (33)1 44 27 44 11            */
/* (fax)    Olivier.Pironneau@ann.jussieu.fr          */
/******************************************************/
#include <sioux.h>
#include <SIOUXGlobals.h> //OP my hack
#include <assert.h>
#include <fstream.h>
#include <iostream.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#define fill thequikdrawfill
#include "rgraph.h"
#include <Windows.h>
#include <Fonts.h>
#include <SegLoad.h>
#include <StandardFile.h>
#include <setjmp.h>
#undef fill

static FILE *psfile = 0;
static FILE *psfile_save = 0;


#ifdef FREEFEM
//  pour imprimer la version   FH 
#define STRING(i) #i
#include <new.h>

jmp_buf environ;
static int  myenviron = 0;

TEHandle TESioux;

void out_of_memory ();
void NEW_HANDLER (void);
void compile(char *fname);
float scali(int i);
float scalj(int j);
int pStrCopy (StringPtr p1, char * p2);
void execute(char* what);
int DoMouseDown (int windowPart, WindowPtr whichWindow, EventRecord *myEvent);
char Getijc(int & x,int & y);
 
void  viderbuff(){;}//for unix only

void out_of_memory ()
{
  cout << "FreeFEM error: operator new failed; not enough memory" << endl;
  if (myenviron)
   longjmp(environ,1);
}

void NEW_HANDLER (void){  set_new_handler (&out_of_memory);}
#endif

#define	ours(w)		(w==grafWindow0)

template<class T> inline T Min (const T &a,const T &b){return a < b ? a : b;}
template<class T> inline T Max (const T &a,const T & b){return a > b ? a : b;}

static  int cube6[7][3] ={ {65534,0,0},{65534,65534,0},{0,65534,0},{0,65534,65534},{0,0,65534}
     , {65534,0,65534},{65534,0,0} }; 

char errbuf[255];
static int INITGRAPH=0;
static float aspx, aspy, echx,echy,ech,rxmin,rxmax,rymin,rymax;
static int carre, lacouleur;
static CWindowRecord wgRecord0;
static WindowPtr	 grafWindow0;
static	Rect		boundsRect;
static int nbcolor;
static CursHandle  CrossCurseur ;
static CursHandle  WatchCurseur ;
static int ncolortable;
static int LastColor; // LastColor=1 => Noir et Blanc 

static int width,height;
static RGBColor * colortable;
int getcolor();
void putpixel(int ix,int iy, int couleur);
int scalx(float x);
int scaly(float y);
void thisexit();

#ifdef FREEFEM

void coutmode(short i) 
{ 
   cout <<  flush;
   cerr <<  flush;
 //  if(i)(**(SIOUXTextWindow->edit)).txFace = 0;
 //  else (**(SIOUXTextWindow->edit)).txFace = 1;
;}

void myexit(int err)
{
 if (INITGRAPH)
  {
    rattente(0);
    closegraphique();
  }
 if (err !=0)
    cout << "Error: freefem+ has end with error code " <<err<<endl;
 else cout << "Normal exit 0" << endl;
  if (myenviron)
   longjmp(environ,1);
}
void thisexit(){ myexit();}

int main (int argc, char **argv)
{
  char       *prog;
  char       *fname;
  
	InitGraf(&qd.thePort);
	InitFonts();
	FlushEvents(everyEvent, 0L);
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(0L);
	InitCursor();
	MaxApplZone();
	MoreMasters();
  fname = new char[256];
	
  short bas = qd.screenBits.bounds.bottom;
  short droit = qd.screenBits.bounds.right;
  SIOUXSettings.initializeTB = 0; // else SIOUX initialize the toolbox for us
  SIOUXSettings.toppixel = 45;
  SIOUXSettings.leftpixel = 15; 
//  SIOUXSettings.fontface = bold + italic;// or normal
  SIOUXSettings.asktosaveonclose = 0;
  SIOUXSettings.columns = (short)(2.+(float)droit/8.);
  SIOUXSettings.rows = (short)(10. + (float)bas/18.);
  SIOUXSetTitle("\pfreefem+ v1.1.16 line output"); //marche pas!!
  SIOUXSettings.fontface = bold;
  SIOUXSettings.fontid = 22;// courier;
  SIOUXSettings.fontsize = 10;
  cout << "Welcome to freefem+ version " << STRING(FREEFEM) << endl; //Screen size = " <<bas<<" x "<<droit<<endl;
  (**(SIOUXTextWindow->edit)).txFace = 0; // from bold to normal
  argc = getprog (fname, argc, argv);
  atexit(thisexit);
  NEW_HANDLER (); // see dependent system files ({pc,x,mac}rgraph.{h,cpp})
  

  int OPTION = 0;
  if (argc == 2)
    {
        initgraphique();
        if(0==setjmp(environ))
         {  myenviron=1;
	   compile (fname);
	   cout << "No Error" << endl;
	 }
	myenviron = 0;			
    }
  else
    printf ("To launch freefem you must type freefem  and a file name\n");
  return 0;
}
#endif

void message(char *s)
{ 
   printf("%s	\n", errbuf);
}

void erreur(char *s)
{
    cout  << endl;
    cerr << "##Fatal error  :" << s << endl << "exit(1)" <<  endl;
    exit(1);
}

void *safecalloc(long nb, long size)
{
  void* p=NULL;
  p = calloc(nb, size);
  if (p == NULL) 
     erreur("Out of Memory!\n");
  return p;
}

void safefree(void** f)
{
  if(*f)
  { 
    free(*f); 
    *f=NULL;
  }
}

void initgraphique(void)
{
        
	INITGRAPH = 1;
	boundsRect.top = 45;
	boundsRect.left = 15 +0.35 * qd.screenBits.bounds.right;
	boundsRect.bottom = qd.screenBits.bounds.bottom - 25;
	boundsRect.right =  qd.screenBits.bounds.right-25;
	if((boundsRect.bottom - boundsRect.top) < (boundsRect.right - boundsRect.left))
		boundsRect.right = boundsRect.left + boundsRect.bottom - boundsRect.top;
	else
		boundsRect.bottom = boundsRect.top + boundsRect.right - boundsRect.left;
	grafWindow0=NewCWindow(&wgRecord0, &boundsRect, "\pFreeFem Graphics",true, 8, NULL, true, 0);
	ShowWindow(grafWindow0);
	BringToFront(grafWindow0);
	SelectWindow(grafWindow0);
	SetPort(grafWindow0);
	height = boundsRect.bottom - boundsRect.top - 10;
	width = boundsRect.right - boundsRect.left -10;
	aspx = boundsRect.right - boundsRect.left -10;
	aspy = boundsRect.bottom - boundsRect.top - 10;
	carre = aspx == aspy;
	lacouleur = getcolor();
	CrossCurseur = GetCursor(crossCursor);
	WatchCurseur = GetCursor(watchCursor);
	if( (**(wgRecord0.port.portPixMap)).pixelSize>7)
		nbcolor= 256; 
	else 
		nbcolor= 2;


	ncolortable =0;
	LastColor=2;// En couleur pas defaul
	colortable=0;
	SetColorTable(2+6);
  //    TextFont(fontNum);
	TextSize(9); // small size 

}


void SetColorTable(int nb)
{
   if (ncolortable == nb) return;// optim
   if (nbcolor && nb>2) 
     { 
       if(colortable) delete [] colortable;
       colortable = new RGBColor[nb];
       ncolortable = nb;
       if(LastColor>1) LastColor=nb-1;
       int k=0;
       colortable[k].red=65534;
       colortable[k].green=65534;
       colortable[k].blue=65534;
       k++;
       colortable[k].red=0;
       colortable[k].green=0;
       colortable[k].blue=0;
       k++;
       nb = nb -2;
       for (long i0=0;i0<nb;i0++,k++)
         {  
      //     long  i1 = nb - i0;
           long  i6 = i0*6;
           long  j0 = i6/nb;// in 0..6
           long  j1 = j0+1;// in 1..6
           long  k0 = i0 - (nb*j0)/6L;
           long  k1 = (nb*j1)/6L-i0;
           long  kk = k0+k1;
           //cout << "\t\t" << i0 << " " << j0 << " " << j1 << " " << k0 << " " << k1  << " "<<kk<<endl;
         	if(kk<=0) kk=1;
         // assert(kk);
           colortable[k].red   = (cube6[j1][0]*k0+cube6[j0][0]*k1)/kk;
           colortable[k].green = (cube6[j1][1]*k0+cube6[j0][1]*k1)/kk;
           colortable[k].blue  = (cube6[j1][2]*k0+cube6[j0][2]*k1)/kk;
           assert(k<ncolortable);
           
          }
  /*    for (k=0;k<ncolortable;k++)
           cout << " color"  << k 
                <<" r = " << colortable[k].red 
                <<" g = " << colortable[k].green
                <<" b = " << colortable[k].blue << endl;
  */    
         
       }
     else 
      ncolortable  =0;
}
void closegraphique(void)
{
  if(INITGRAPH)  CloseWindow(grafWindow0);
  INITGRAPH=0;
}
void showgraphic()
{
  if (grafWindow0 != FrontWindow())
   { 
	ShowWindow(grafWindow0);
	BringToFront(grafWindow0);
	SelectWindow(grafWindow0);
	SetPort(grafWindow0); }

	
}

void reffecran(void)
{
  EraseRect(&(grafWindow0->portRect));
}

int getcolor(void)
{ return 0;
}

void putpixel(int ix,int iy, int couleur)
{
}

 void plotstring(const char *s)
{  DrawText(s,0,strlen(s));
 if(psfile) fprintf(psfile,"(%s) S\n",s);
} 

int LaCouleur(){return lacouleur;}

void couleur(int c)
{ 
  if ( lacouleur == c) // small optim
    return;
  c= c > LastColor ? 1 : c; // c=Min(c,LastColor); pour noir et blanc
  lacouleur =c;
  if ( c == 0 )
    ForeColor(30);
  else if (ncolortable>3 && c < ncolortable && c >=0 ) 
    RGBForeColor(colortable+c);
  else 
   ForeColor(33);
 if (psfile)
  {
    float r=1,g=1,b=1;
    if (colortable) {
      if (c>0 && c < ncolortable)
	{
	  r =  (float) colortable[c].red /65535.;
	  g =  (float) colortable[c].green /65535.;
	  b =  (float) colortable[c].blue /65535.;
	}
    }
    else if (c!=0)
      r=g=b=0;
    
    fprintf(psfile,"%.3f %.3f %.3f C\n",r,g,b);
  }
   
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

void penthickness(int pepais)
{
  PenSize(pepais,pepais);
  if (psfile) fprintf(psfile,"%d setlinewidth\n",pepais);
}
void cadre(float xmin,float xmax,float ymin,float ymax)
{
  rxmin = xmin;
  rxmax = xmax;
  rymin = ymin;
  rymax = ymax;
  echx = aspx / (xmax - xmin);
  echy = aspy / (ymax - ymin);
}
void getcadre(float &xmin,float &xmax,float &ymin,float &ymax)
{
  xmin = rxmin;
  xmax = rxmax;
  ymin = rymin;
  ymax = rymax;

}

void cadreortho(float centrex, float centrey, float rayon)
{
  int xasp,yasp, getmaxx, getmaxy;
  
	getmaxx = xasp =aspx;	getmaxy = yasp = aspy;
	
  if (getmaxx * (float)xasp > getmaxy * (float)yasp)
  {
    rymin = centrey - rayon;
    rymax = centrey + rayon;
    echy= getmaxy / (2 * rayon);
    echx= (echy * xasp) / yasp;
    rxmin= centrex - getmaxx / (2 * echx);
    rxmax= centrex + getmaxx / (2 * echx);
  }
  else
  {
    rxmin = centrex - rayon;
    rxmax = centrex + rayon;
    echx = getmaxx / (2 * rayon);
    echy = (echx * yasp) / xasp;
    rymin = centrey - getmaxy / (2 * echy);
    rymax = centrey + getmaxy / (2 * echy);
  }
}

int scalx(float x)
{
  return (x - rxmin) * echx;
}

int scaly(float y)
{
  return (rymax - y) * echy;
}

float scali(int i)
{
  return i/echx  + rxmin;
}
float scalj(int j)
{
  return -j/echy  + rymax;
}

void pointe(float x, float y)
{
  putpixel(scalx(x), scaly(y), lacouleur);
}

void rmoveto(float x, float y)
{
  int newx = scalx(x), newy = scaly(y);
  MoveTo(newx,newy);
  if (psfile) 
   fprintf(psfile,"%d %d M\n", newx, height-newy);
  
}

void rlineto(float x, float y)
{
  int newx = scalx(x), newy = scaly(y);
  LineTo(newx,newy);
   if (psfile) 
    fprintf(psfile,"%d %d L\n", newx,height-newy);
  
}

void raffpoly(int n, float *poly)
{
  PolyHandle thePoly;
  int i;
  thePoly =OpenPoly();
   rmoveto(poly[0], poly[1]);
    for(i=1; i<n; i++)
    rlineto(poly[2*i], poly[2*i+1]);
    ClosePoly();
    FillPoly(thePoly,&qd.white);
    FramePoly(thePoly);
    KillPoly(thePoly);
}
void fillpoly(int n, float *poly)
{
  PolyHandle thePoly;
  int i;
  thePoly =OpenPoly();
   rmoveto(poly[0], poly[1]);
    for(i=1; i<n; i++)
    rlineto(poly[2*i], poly[2*i+1]);
    ClosePoly();
    FillPoly(thePoly,&qd.black);
    FramePoly(thePoly);
    KillPoly(thePoly);
}

int pStrCopy (StringPtr p1, char * p2)
/* copies a pascal string `p1 into a C string */
{
	int len,i;
	
	len = (*p1++) %256;
	for(i=1;i<=len;i++) *p2++=*p1++;
	*p2 = 0;
	return 0;
}



int getprog(char* fn,int argc, char** argvptr)
{
    Point SFGwhere = { 90, 82 };
 	SFReply reply;
	SFTypeList	myTypes;
	
	InitCursor();
	reply.fName[0] = 0;  // empty string
	cout << "\t\tStart Program freefem+ 1.0 "<<endl;
	myTypes[0] = 'TEXT';	
	myTypes[1] = 'TEXT';	
	myTypes[2] = 'TEXT';	
	myTypes[3] = 'TEXT';	
	SFGetFile(SFGwhere, NULL, NULL,1, myTypes, NULL, &reply );
	if (!reply.good) return (-1);
	SetVol(0, reply.vRefNum);
	pStrCopy(reply.fName, fn);
	return (2);
}

void execute(char* what)
{
		// exec(what); link with unix lib
}

int DoMouseDown (int windowPart, WindowPtr whichWindow, EventRecord *myEvent)
{
  int wasactive;
	switch (windowPart) {
		case inGoAway:
			if (ours(whichWindow))
				if (TrackGoAway(whichWindow, myEvent->where))
					 HideWindow(whichWindow);
			break;

		case inZoomIn:
			if (ours(whichWindow))
				{
					//SetCursor(&waitCursor);
					 SetPort(whichWindow);
					EraseRect(&(whichWindow->portRect));
					ZoomWindow(whichWindow, inZoomIn, true);
					InitCursor();
				}
			break;

		case inZoomOut:
/*			if (ours(whichWindow))
				{
					SetCursor(&waitCursor); SetPort(whichWindow);
					EraseRect(&(whichWindow->portRect));
					ZoomWindow(whichWindow, inZoomOut, true);
					if(whichWindow == editWindow) 
						 MyZoomWindow(whichWindow);
					InitCursor();
				}*/
			break;

		case inMenuBar:
//			return(DoCommand(MenuSelect(myEvent->where)));
            break;

		case inSysWindow:
			SystemClick(myEvent, whichWindow);
			break;

		case inDrag:
			if (ours(whichWindow))
				{
					SetPort(whichWindow);
		//			DragWindow(whichWindow, myEvent->where, &dragRect);
				}
			break;

		case inGrow:
			//if (ours(whichWindow))
			//	{MyGrowWindow(whichWindow, myEvent->where);}
			break;

		case inContent:
			wasactive = (whichWindow == FrontWindow()); 
	     if(!wasactive) { SelectWindow(whichWindow);
	//	    if (ours(whichWindow) && MacReDraw ) (* MacReDraw)();
		   }
		  else if (ours(whichWindow))
			{ SetPort(whichWindow);
			   while (Button()) ;
			   return 0;
			}
			break;
	}
return 1;
}

char Getijc(int & x,int & y)
{   char char1;
//void MacGetXYC_(integer * b, integer * x, integer *y)
    EventRecord		myEvent;
	WindowPtr		whichWindow=NULL;
	short			windowPart;
	unsigned short theCode;
	int flag=1;
	Point Pt;
	HLock( (Handle) WatchCurseur);
	SetCursor(*CrossCurseur);
	HUnlock( (Handle) WatchCurseur);
   while (flag) {
	SystemTask();

	if (GetNextEvent(everyEvent, &myEvent) /* ,OxFFFFFFFF,h)*/) 
	{
	  switch (myEvent.what) {
		case mouseDown:
  		windowPart = FindWindow(myEvent.where, &whichWindow);
	    flag =  DoMouseDown(windowPart, whichWindow, &myEvent);
	    char1=  251;
	    break; 

//
//
		case keyDown:
		case keyUp:
		case autoKey: 
			{
//			register char	theChar;
			 windowPart = FindWindow(myEvent.where, &whichWindow);
			if((whichWindow==grafWindow0) /* && (inContent == windowPart)*/)
				{ if  (grafWindow0 !=  FrontWindow()) { 
				     SelectWindow(whichWindow); 
		  //           if ( MacReDraw ) (* MacReDraw)();
		             SetPort(whichWindow);
		             }
				   char1 = (myEvent.message & 127L);
//				   cout << "cccc= " << (int) char1 << "'" << char1 << "'" << endl;
//				   printf(" cccc '%d' '%ld'\n",char1,myEvent.message );
				  flag =0;
/*                 return;*/
				}
			break;}
	   case updateEvt: 
	     if (ours((WindowPtr) myEvent.message)) {
	       	BeginUpdate((WindowPtr) myEvent.message);
//			if ( MacReDraw ) { 
//			     SetCursor(*WatchCurseur);
//			     (* MacReDraw)();	
//			     SetCursor(*CrossCurseur);
//			  }
		    EndUpdate((WindowPtr) myEvent.message);
	     }
	    break;

}}}
/*   Pt := myEvent.where;*/
   GlobalToLocal( & myEvent.where);
   x = myEvent.where.h;
   y = myEvent.where.v;
	HLock( (Handle) WatchCurseur);
	SetCursor(*WatchCurseur);
	HUnlock( (Handle) WatchCurseur);

 //   printf("\t\t x = %d y = %d  c=%d\n", x,y,char1);
  return char1;
    
    

}

char Getxyc(float &x,float &y)
{ 
  char c;
  int i,j;
  c = Getijc( i,j);
  x = scali(i);
  y = scalj(j);
  return c;
}


void rattente(int waitm)
{ int i,j;
 char   c;
 if(waitm)  c = Getijc( i,j);
/*    you may prefer to use carriage return to move to the next graph */
/*	 getc(stdin);
*/
// if(waitm) while(!Button()){ };
}

void GetSizeScreen(int & ix,int &iy);
void GetSizeScreen(int & ix,int &iy)
{
  	ix = width ;
  	iy = height;
}



void openPS(const char *filename )
{ 
  if(psfile_save) closePS();
  time_t *timer,t_loc;
  float s=0.5;
  char  username[10];
  time(&t_loc);
  printf("\t\t Save Postscript in file '%s'\n",filename?filename:"freefem.ps"),
  psfile=fopen(filename?filename:"freefem.ps","w");
  if(psfile==0) {printf("Erreur %s errno %d\d",filename,errno);exit(1);}
  if(psfile) {
  fprintf(psfile,"%%!\n%%%%Creator: %s\n%%%%Title: FremFem+\n","user");
  fprintf(psfile,"%%%%CreationDate: %s",ctime(&t_loc));
  fprintf(psfile,"%%%%Pages: 1\n");
  fprintf(psfile,"%%%%BoundingBox:       0 0 %d %d\n",int(50+width*s),int(50+height*s));
  fprintf(psfile,"%%%%EndComments\n");
  fprintf(psfile," /L {  lineto currentpoint stroke newpath moveto} def\n");
  fprintf(psfile," /M {  moveto } def\n");
  fprintf(psfile," /C {setrgbcolor} def\n");
  fprintf(psfile," /rec {newpath 4 copy 8 1 roll moveto 3 -1 roll lineto 4 2 roll exch lineto lineto closepath} def\n");
  fprintf(psfile," 50 50  translate \n");
  fprintf(psfile," %f %f  scale \n",s,s);
  fprintf(psfile," 0 %d 0 %d rec clip newpath\n",int(width),int(height));
  fprintf(psfile," /Helvetica findfont 16 scalefont setfont\n");
  fprintf(psfile," /S { show} def\n");
  psfile_save=psfile;
  }
}
void closePS(void)
{
  if(psfile_save)   {
    fprintf(psfile,"showpage\n");
    fclose(psfile);
    }
    
  psfile=0;
  psfile_save=0;
  
}

  void Commentaire(const char * c)  
  {
  if(psfile)   {
    fprintf(psfile,"%% %s\n",c);
   }
  };
  void NoirEtBlanc(int NB)
  {
    if(NB) LastColor=1;
    else LastColor=ncolortable?ncolortable:2;
  }
 
  void MettreDansPostScript(int in)
   {
     if(in)  psfile=psfile_save;     
     else    psfile=0;
   }

static void     FillRect(float x0,float y0, float x1, float y1)
 {
     float r[8];
     r[0]=x0;r[1]=y0;
     r[2]=x1;r[3]=y0;
     r[4]=x1;r[5]=y1;
     r[6]=x0;r[7]=y1;
     fillpoly(4,r);
 }

float  GetHeigthFont()
{ 
  FontInfo 	MyFontInfo;
  GetFontInfo(&MyFontInfo);   				
 int interligne = MyFontInfo.ascent + MyFontInfo.descent + MyFontInfo.leading;
 return interligne/echy;
}



int PutLevel(int lineno, float xf, int col)
{
  float xmin,xmax,ymin,ymax;
  getcadre(xmin,xmax,ymin,ymax);
  float xleft = xmax - (xmax-xmin)*0.1;
  float ytop  = ymax;
  float ydelta = (ymax-ymin)/40;
  ydelta=GetHeigthFont();
  xleft = xmax - 6*ydelta;  
  ytop -= ydelta*(col+2);
  couleur(col);
  FillRect(xleft+ydelta/8.,ytop+ydelta/8.,xleft+ydelta*7./8.,ytop+ydelta*7./8.);
  rmoveto(xleft+ydelta*1.4,ytop+ydelta/4);
  char buf[30];
  sprintf(buf,"%g",xf);
  couleur(1);
  plotstring(buf);

   return lineno;
}

class Grid;

void SaveMesh(Grid &t){}
void SavePlot(int D, Grid& t, float *f){}

