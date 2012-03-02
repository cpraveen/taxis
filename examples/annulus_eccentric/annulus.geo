r1 = 0.1;
n1 = 400; // no. of points on outer cylinder
cellSize=2*Pi*r1/n1;
Printf("cellSize on inner/outer cylinders = %e\n", cellSize);
r = 0.04;
xc= -0.02;
yc= 0.0;
nc=400; // no. of points on small cylinder
h2=2*Pi*r/nc;
Printf("cellSize on small stationary cylinder = %e\n", h2);
n_p = 100;
p = 0.05;
// create inner 1/8 shell
Point(1) = {0, 0, 0, cellSize};
Point(2) = {-r1,0, 0, cellSize};
Point(3) = {0, r1, 0, cellSize};
Point(4) = {r1,0, 0, cellSize};
Point(5) = {0, -r1, 0, cellSize};
Point(10) = {xc, yc, 0, h2};
Point(11) = {xc-r,yc, 0, h2};
Point(12) = {xc, yc+r, 0, h2};
Point(13) = {xc+r, yc, 0, h2};
Point(14) = {xc, yc-r, 0, h2};


Circle(1) = {2, 1, 3};
Circle(2) = {3, 1, 4};
Circle(3) = {4, 1, 5};
Circle(4) = {5, 1, 2};


Circle(9) = {11, 10, 12};
Circle(10) = {12, 10, 13};
Circle(11) = {13, 10, 14};
Circle(12) = {14, 10, 11};


Line(25) = {11,2};
Line(26) = {13,4};
Line(27) = {12,3};
Line(28) = {14,5};

Line Loop(1) = {25,-4,-28,12};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {2,5,11,14};
Transfinite Line{28,25} = n_p Using Bump p ;
Transfinite Line{4,12} = n_p;

Line Loop(2) = {28,-3,-26,11};
Ruled Surface(2) = {2};
Transfinite Surface(2) = {5,4,13,14};
Transfinite Line{28,26} = n_p Using Bump p ;
Transfinite Line{11,3} = n_p;

Line Loop(3) = {26,-2,-27,10};
Ruled Surface(3) = {3};
Transfinite Surface(3) = {3,4,12,13};
Transfinite Line{27,26} = n_p Using Bump p ;
Transfinite Line{2,10} = n_p;

Line Loop(4) = {27,-1,-25,9};
Ruled Surface(4) = {4};
Transfinite Surface(4) = {2,3,11,12};
Transfinite Line{27,25} = n_p Using Bump p ;
Transfinite Line{9,1} = n_p;

Physical Line(100001) = {1,2,3,4}; // outer cylinder
Physical Line(100003) = {9, 10, 11, 12}; // small cylinder
Physical Surface(100000) = {1,2,3,4};

Geometry.Normals = 100;
