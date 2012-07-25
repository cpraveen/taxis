a = 1.1;
b = 2.0;
r = 0.5;

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

Line Loop(1) = {1,2,3,4,5,6};
Plane Surface(1) = {1};

Physical Surface(100000) = {1};
Physical Line(100001) = {1,2}; // cylinder
Physical Line(100002) = {3,6}; // outlet
Physical Line(100003) = {4,5}; // inlet
