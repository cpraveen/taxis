#
# Author: Nisha C
#
END {
BFLAG=NOSLIP;
Pi=3.14159265358979;

x0=0;
y0=0;
y1=0;
x1=0.5;
x2=1.5;
H=1;
delta =10; #angle of the wedge
y2 = (x2-x1)*sin(delta*Pi/180)/cos(delta*Pi/180);

#specifying the number of points
#np_in = 50;
#np_out = 50;
#np_top = 100;
#np_bot = 80;
#np_wedge = 80;
np_in = 10;
np_out = 10;
np_top = 20;
np_bot = 10;
np_wedge = 10;


print "Dimension",2;
print "Angle of Wedge",delta;
print "Vertices",np_out + np_in + np_top + np_bot + np_wedge;


# the vertices on bottom surface (counter clockwise)
 for (i=0; i<np_bot; i++) 
 {
   x = x0 + i*(x1-x0)/np_bot;
   y=y0;
   print x,y,100001;
 }
 
# the vertices on the wedge (counter clockwise)
  lw = sqrt((x2-x1)*(x2-x1) + y2*y2); #length of the wedge
  for (i=0; i<np_wedge; i++) 
  {
    x= x1 + cos(delta*Pi/180)/np_wedge*i*lw;
    y = y1 + sin((delta*Pi/180))/np_wedge*i*lw;
    print x,y,100001; 
  }

# the vertices on the outlet (counter clockwise)
  for (i=0; i<np_out; i++)
  {
    x = x2;
    y = y2 + i*(H - y2)/np_out;
    print x,y,100002;
  }

#the vertices on the top (counter clockwise)
  for (i=0; i<np_top; i++)
  {
    x = x2 - i*(x2-x0)/np_top;
    y = H;
    print x,y,100003;
  }
# the vertices on the inlet (counter clockwise)
  for (i=0; i<np_in; i++)
  {
    x = x0;
    y = H - i*(H-y0)/np_in;
    print x,y,100004;
  }

 t_e =  np_bot + np_wedge + np_out + np_top + np_in; #Total number of edges 
 print "Edges", t_e ;

#  edges (counter clockwise) 
  k = 1
  for (i=1;i<t_e;i++) 
  { 
    if(i <= np_bot + np_wedge + np_out + np_top)
      if(i <= np_bot + np_wedge + np_out)
        if(i <= np_bot + np_wedge)
          if(i <= np_bot)
              print i,i+k,100001; #bottom edges
          else
              print i,i+k,100001; #edges on the wedge
        else
            print i,i+k,100002; #edges on the outlet
      else
          print i,i+k,100003; #edges on the top
    else
      print i,i+k,100004; #edges on the inlet
  
  } # previous, current vertex
  print i,1,100004; #The closing edge

  print "SubDomain",1;
  print 2,1,1,0;
  
  c1 = 1
  c2 = c1 + np_bot
  c3 = c2 + np_wedge
  c4 = c3 + np_out
  c5 = c4 + np_top
  print "Corners   5";
  print c1, c2, c3, c4, c5
}
