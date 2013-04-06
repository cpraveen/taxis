n1 = 50;
t  = 0.12; // thickness
h1 = 0.01;
h2 = 1.0;
R  = 10.0;

t = t/2.0;

Point(1) = {0,    0, 0, h1};
Point(2) = {0.5, -t, 0, h1};
Point(3) = {1.0,  0, 0, h1};
Point(4) = {0.5,  t, 0, h1};

Point(5) = { R,  0, 0, h2};
Point(6) = { 0,  R, 0, h2};
Point(7) = {-R,  0, 0, h2};
Point(8) = { 0, -R, 0, h2};

Point(9) = {0, 0, 0, h1};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};

Circle(5) = {5,9,6};
Circle(6) = {6,9,7};
Circle(7) = {7,9,8};
Circle(8) = {8,9,5};

Line Loop(1) = {1,2,3,4};
Line Loop(2) = {8,7,6,5};
Plane Surface(1) = {2,1};

Transfinite Line{1,2,3,4} = n1 Using Bump 0.1;

Physical Surface(100000) = {1};
Physical Line(100001) = {1,2,3,4}; // airfoil
Physical Line(100002) = {5,6,7,8}; // farfield

//Physical Surface("100000") = {1};
//Physical Line("100001") = {1,2,3,4}; // airfoil
//Physical Line("100002") = {5,6,7,8}; // farfield
