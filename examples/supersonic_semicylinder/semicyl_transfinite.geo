// Mach = 2
//a = 1.5; b = 2.5;

// Mach = 20
a = 1.1; b = 1.5;

r = 0.5;

n1 = 40;
n2 = 40;

alpha = 10;

alpha = alpha*Pi/180.0;

lc = 0.02;

x1 = r*Cos(-Pi/2-alpha);
y1 = r*Sin(-Pi/2-alpha);

psi = Atan2(a*Sin(-Pi/2-alpha),b*Cos(-Pi/2-alpha));
x2 = a*Cos(psi);
y2 = b*Sin(psi);

Point(1) = {0, 0, 0, lc};
Point(2) = {x1, y1, 0, lc};
Point(3) = {-r, 0, 0, lc};
Point(4) = {x1, -y1, 0, lc};
Point(5) = {x2, -y2, 0, lc};
Point(6) = {-a, 0, 0, lc};
Point(7) = {x2, y2, 0, lc};

Point(8) = {0, 1, 0};

Circle(1) = {2, 1, 3};
Circle(2) = {3, 1, 4};
Line(3)   = {4, 5};
Ellipse(4) = {5, 1, 8, 6};
Ellipse(5) = {6, 1, 8, 7};
Line(6) = {7, 2};
Line(7) = {3, 6};

Line Loop(1) = {1,7,5,6};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {2,3,6,7};

Line Loop(2) = {2,3,4,-7};
Ruled Surface(2) = {2};
Transfinite Surface(2) = {3,4,5,6};

Transfinite Line{1,-5,2,4} = n1;
Transfinite Line{7,-6,3} = n2;

Physical Surface(100000) = {1,2};
Physical Line(100001) = {1,2}; // cylinder
Physical Line(100002) = {3,6}; // outlet
Physical Line(100003) = {4,5}; // inlet
