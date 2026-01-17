/**************DO NOR REMOVE THIS BANNER***************/
/*  FreeFEM : Language for a Finite Element Method    */
/*  -------    Release 1.0:  June 1994.               */
/*  Authors: D. Bernardi, Y. Darmaillac F. Hecht,     */
/*           O. Pironneau                             */
/*  You may copy freely these files and use it for    */
/* teaching or research. These or part of these may   */
/* not be sold or used for a commercial purpose with- */
/* out our consent : fax (33)1 44 27 44 11            */
/*  modified for bamg by F Hecht  dec 1997            */
/* add of  InPtScreen   and   InRecScreen             */
/* (e-mail)    Olivier.Pironneau@ann.jussieu.fr       */
/* (e-mail)    Frederic.Hecht@inria.fr                */
/******************************************************/
#ifndef RGRAPH_H_
#define RGRAPH_H_
#ifdef __cplusplus
extern "C" {
#endif
  int getprog(char* fn,int , char** argvptr);
  void message(char *s);
  void erreur(char *s);
  void *safecalloc(long nb, long size);
  void initgraphique();
  void closegraphique();
  void showgraphic();
  void rattente(int waitm);
  void cadre(float xmin,float xmax, float ymin, float ymax);
  void getcadre(float &xmin,float &xmax, float &ymin, float &ymax);
  void cadreortho(float centrex, float centrey, float rayon);
  void couleur(int c);
  int  LaCouleur();
  void pointe(float x, float y);
  int  InPtScreen(float x, float y);
  int  InRecScreen(float x1, float y1,float x2, float y2);
  void plotstring(const char *s);
  void rmoveto(float x, float y);
  void rlineto(float x, float y);
  void penthickness(int );
  void cercle(float centrex, float centrey, float rayon);
  void execute(const char* s);
  void safefree(void** f);
  void reffecran();
  void raffpoly(int n, float *poly);
  void fillpoly(int n, float *poly);
  char Getxyc(float &x,float &y);
  void SetColorTable(int nb);
  void GetScreenSize(int &ix,int &iy);
  void openPS(const char * );
  void closePS(void);
  void coutmode(short i);
  void myexit(int err=0);
  void  viderbuff();
  void Commentaire(const char *);
  void NoirEtBlanc(int NB);
  void MettreDansPostScript(int in);//  oui=1 ou non=0      
#ifdef __cplusplus
	   }
/*
class myio{ public:
  myio& operator << (const char *s) ;
  myio& operator << (long int s); 
  myio& operator << (double s);
};

extern myio mcout;

#define endl "\n"
*/
#endif

#endif
