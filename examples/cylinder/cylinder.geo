D    =  0.1;      // diameter
H    =  0.41;     // half channel height
L    = 2.2;       // length of domain
xmin = -0.20;     // left end
xmax =  L + xmin; // right end
r    = D/2.0;     // radius

h1   =  0.002; // on cylinder
h2   =  0.005; // on boundary
h3   =  0.01; // on outlet


Point(1)  = {xmin,  0, 0, h2};
Point(2)  = {xmax,  0, 0, h3};
Point(3)  = {xmax,  H, 0, h3};
Point(4)  = {xmin,  H, 0, h2};

Point(5) = {-r,   H/2, 0, h1};
Point(6) = { 0,   r+H/2, 0, h1};
Point(7) = { r,   H/2, 0, h1};
Point(8) = { 0,  -r+H/2, 0, h1};

Point(9) = {0, H/2, 0, h1};

Line(1)   = {1, 2};
Line(2)   = {2, 3};
Line(3)   = {3, 4};
Line(4)   = {4, 1};

Circle(5) = {5, 9, 6};
Circle(6) = {6, 9, 7};
Circle(7) = {7, 9, 8};
Circle(8) = {8, 9, 5};

Line Loop(1) = {1,2,3,4};
Line Loop(2) = {5,6,7,8};
Plane Surface(1) = {1,2};

Physical Surface(1000000) = {1};

Physical Line(1000001) = {4};       // Inlet
Physical Line(1000002) = {5,6,7,8}; // cylinder
Physical Line(1000003) = {2};       // outlet
Physical Line(1000004) = {1,3};     // top and bottom walls
