// ********** DO NOT REMOVE THIS BANNER **********
//
// SUMMARY: Bamg: Bidimensional Anisotrope Mesh Generator
// RELEASE: 0 
// USAGE  : You may copy freely these files and use it for    
//          teaching or research. These or part of these may   
//          not be sold or used for a commercial purpose with- 
//          out our consent : fax (33) 1 39 63 55 14       
//
// AUTHOR:   F. Hecht,    
// ORG    :  INRIA
// E-MAIL :   Frederic.Hecht@Inria.fr   
//
// ORIG-DATE:     Dec 97


#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

inline void *operator new(size_t, void *place) { return place; }
// patterne debile pour verification 
static int kerr=0;
void * mymalloc(size_t l)
{
   char *p = (char*)malloc(l+16);
   for( int i = 0; i < 8 ; i++)
     p[i] = 'a'+i,p[i+l+8] = 'z'-i; // put a marque before 
   return (void*) (p+8);
}
void myfree(char *p,size_t l=0) 
{
  if(p) {
    p -= 8;
    int k =0;
    for( int i = 0; i < 8 ; i++)
      {
	if (p[i] != 'a' +i)     k++;
	if(l && (p[i+l+8] != 'z' -i)) k++;       
      }
    if(!k) free(p);
    else {
      if (kerr++<20) 
	printf("@@@@@@@@@@@@@@@@@ Erreur myfree p= %lx   l=%d\n",p,l);
      exit(1);
    }
  }
}
void *operator new(size_t);
void operator delete(void * pp );

//#define SHOWALLOC


const int N100 = 100;
class  AllocData;

class AllocExtern {
  public:
class OneAlloc {public:
  void * p;
  size_t l;
  long n;
};

class AllocData {public:
  OneAlloc *a;
  AllocData * next;
  AllocData();
  ~AllocData();
};

private:

  static size_t AllocSize ;
  static size_t MaxUsedSize;
  static AllocData * AllocHead ;  
  static long NbAlloc;
  static void * NextFree;

  AllocData * NewAllocData();
  OneAlloc *Alloc();
  public:

  void * MyNewOperator(size_t ll);
  void MyDeleteOperator(void * pp);
  AllocExtern();
  ~AllocExtern();
  void ShowAlloc(char *s) ;

};

static AllocExtern AllocExternData;

   size_t AllocExtern::AllocSize =0;
   size_t AllocExtern::MaxUsedSize =0;
   AllocExtern::AllocData * AllocExtern::AllocHead =0;  
   long AllocExtern::NbAlloc =0;
   void * AllocExtern::NextFree =0;


AllocExtern::AllocData * AllocExtern::NewAllocData()
  { AllocExtern::AllocData * ad = (AllocData *) mymalloc(sizeof(AllocData));
    ad->a = (OneAlloc*) mymalloc(sizeof(OneAlloc)*N100);
    for (int i=0;i<N100;i++)
      ad->a[i].l=0,ad->a[i].p=NextFree,NextFree = & ad->a[i];
    ad->next = AllocHead;
    AllocHead = ad;
#ifdef SHOWALLOC    
    printf("\t\tCheckPtr: OneAlloc[100] %lx\n",this);
#endif    
    return ad;
  }



AllocExtern::OneAlloc * AllocExtern::Alloc()
  {  OneAlloc * f =  (OneAlloc *) NextFree;
     if (!f) 
        AllocHead = NewAllocData();
     f =(OneAlloc *) NextFree;
     if (!f) exit(1);
     NextFree =   f->p;
     return f;
  }


 void * AllocExtern::MyNewOperator(size_t ll)
{ 
  AllocExtern::OneAlloc * a = Alloc();
  a->p = mymalloc(ll);
  a->l = ll;
  a->n = ++NbAlloc;
  AllocSize += ll;
#ifdef SHOWALLOC    
  printf( "\t\tCheckPtr: New Alloc %ld %lx when %ld\n ", ll, a->p, a->n);
#endif
  MaxUsedSize = AllocSize < MaxUsedSize ? MaxUsedSize :  AllocSize;
  if( !ll &&  !a->p)
   {
     printf("\t\tCheckPtrMem Full Exit(10) New Alloc %ld %lx when %ld\n ", ll, a->p, a->n);
     printf ("\t\tCheckPtr:Max Memory used %10.3f kbytes " ,  MaxUsedSize/1024. );
     printf (" Memory undelete %ld \n" , AllocSize);
     exit(10);
   }
  return (void*) ((char*)a->p);
}

 void AllocExtern::MyDeleteOperator(void * pp)
{
  if (AllocHead)
  {
   AllocExtern::AllocData *p = AllocHead;
   while (p)
    {
      for (int i=0;i<N100;i++)
	if((p->a[i].l > 0) && (p->a[i].p == pp))
	  {
#ifdef SHOWALLOC    	  
	    printf("\t\tCheckPtr: delete  Alloc %ld %lx when %ld \n",p->a[i].l,  p->a[i].p, p->a[i].n);
#endif
        int ll = p->a[i].l;
	    for (int kkk=0;kkk< p->a[i].l;kkk++) 
	      ((char *) pp)[kkk]=18;
	      
	    myfree((char*)pp,ll);

	    AllocSize -= p->a[i].l;
	    p->a[i].l=0;
	    p->a[i].p = NextFree;
	    p->a[i].n =0;
	    NextFree = & p->a[i].p;
	    return;}
      p = p->next;
    }
  if(pp) 
    printf( "\t\tCheckPtr: delete of bad pointer %lx -----------\n",pp);

  } else 
      myfree((char*)pp); 
}

AllocExtern::AllocExtern()
 {
//OP ca m'empeche de regler SIOUX   printf ("\t\tCheckPtr:Init AllocExtern %ld \n",NbAlloc);
 }

 AllocExtern::~AllocExtern()
 {
     AllocData * p=AllocHead;
     int k=0;
    while (p) {int i=N100;
      while(i--)
	if (p->a[i].l >0 &&  k++<10)
	  printf ("\t\tCheckPtr:Undelete pointer %d  %lx size %ld  when %ld\n",i, p->a[i].p,p->a[i].l,p->a[i].n);
      myfree((char*)p->a);
      AllocData * pold = p;
      p = p->next;
      myfree((char*)pold);
    }
     if(k)  printf ("\t\tCheckPtr:Nb of undelete pointer is %d\n",k);
     printf ("\t\tCheckPtr:Max Memory used %10.3f kbytes " ,  MaxUsedSize/1024. );
     printf (" Memory undelete %ld \n" , AllocSize);
     AllocHead=0;
 }
 // ------------------


  void *operator new(size_t ll )
{ void * p =  AllocExternData.MyNewOperator(ll);
  if (ll && !p) { printf("EXIT BECAUSE MEMORY FULL \n");
  					exit(-1);};
  return p;}

void operator delete(void * pp)
{  AllocExternData.MyDeleteOperator(pp);}

//  cas de tableau depend des compilateurs
#if  !defined(__hpux)
void *operator new[](size_t ll )
{
  void * p =  AllocExternData.MyNewOperator(ll);
  if (ll && !p) { printf("EXIT BECAUSE MEMORY FULL \n");
  exit(-1);};
  return p;
}
void operator delete[](void * pp)
{
  AllocExternData.MyDeleteOperator(pp);
}
#endif

void AllocExtern::ShowAlloc(char *s) {
  AllocExtern::AllocData * p=AllocExtern::AllocHead;
  int i=N100-1;
   printf ("C\t\theckPtr:-----%s------ %d Undelete pointer  %lx size %ld  when %ld\n",s,i, p->a[i].p,p->a[i].l,p->a[i].n);
}

void ShowAlloc(char *s);
void ShowAlloc(char *s){  AllocExternData.ShowAlloc(s);}

