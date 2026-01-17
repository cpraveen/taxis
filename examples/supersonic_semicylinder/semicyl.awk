END {
SLIP=2;
NOSLIP=3;
BFLAG=NOSLIP;
Pi=3.14159265358979;
i20=60;
i8=120;
i10=60;
b = 2.0;
a = 1.1;
r = 0.5;
c0x = 0.0;
c0y = 0.0;
alpha = 10.0;
print "Dimension",2;
print "MaximalAngleOfCorner 46";

# Convert to radians
alpha = alpha*Pi/180;

print "Vertices",i20+i10+i8+i10;

# the vertex on naca012  cylinder (counter clock wise on cylinder)
dtheta = (Pi-2*alpha)/(i20-1);
theta  = -Pi/2-alpha;
 for (i=1;i<=i20;i++) {
   x = r*cos(theta);
   y = r*sin(theta);
   print x,y,1;
   theta = theta - dtheta;
   }
 
psi = atan2(a*sin(Pi/2+alpha),b*cos(Pi/2+alpha));
x1 = a*cos(psi);
y1 = b*sin(psi);
ds = (sqrt(x1*x1+y1*y1) - r)/(i10 + 1);

rr=r;
for(i=1; i<=i10; i++){
   rr= rr + ds;
   x=rr*cos(Pi/2+alpha);
   y=rr*sin(Pi/2+alpha);
   print x,y,6;
	}

dtheta = (Pi-2*alpha)/(i8-1);
theta  = Pi/2+alpha;
 for (i=1;i<=i8;i++) {
   psi = atan2(a*sin(theta), b*cos(theta));
   x = a*cos(psi);
   y = b*sin(psi);
   print x,y,5;
   theta = theta + dtheta;
   }

#x1 = a*cos(3*Pi/2-alpha);
#y1 = b*sin(3*Pi/2-alpha);
rr=sqrt(x1*x1 + y1*y1);
for(i=1; i<=i10; i++){
   rr = rr - ds;
   x=rr*cos(3*Pi/2-alpha);
   y=rr*sin(3*Pi/2-alpha);
    print x,y,6;
	}

print "Edges",i20+i10+i8+i10;
for(i=1; i<i20; i++){ #on cylinder
    print i, i+1, 100001;
	}
for(i=i20; i<=i20+i10; i++){ #supersonic outlet
    print i, i+1, 100002;
	}
for(i=i20+i10+1; i<i20+i10+1+i8; i++){ #constant outer boundary
    print i, i+1, 100003;
	}
for(i=i20+i10+1+i8; i<i20+i10+i8+i10; i++){ #supersonic outlet
    print i, i+1, 100002;
	}
print i20+i10+i8+i10, 1, 100002;

#   one subdomain, region on left side of the wing
#   because clock wise sens.
 print "SubDomain",1;
 print 2,1,1,0;

 print "Corners   4";
 print 1, i20, i20+i10+1, i20+i10+i8;
}
