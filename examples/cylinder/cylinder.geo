D    = 0.1;   // diameter
r    = D/2.0; // radius

h1   =  0.001; // on cylinder
h2   =  0.01; // on boundary
xmin = -1.5;  // left end
xmax =  2.2;  // right end
H    =  0.2;  // half channel height

Point(1)  = {xmin, -H, 0, h2};
Point(2)  = {xmax, -H, 0, h2};
Point(3)  = {xmax,  H, 0, h2};
Point(4)  = {xmin,  H, 0, h2};

Point(5) = {-r,   0, 0, h1};
Point(6) = { 0,   r, 0, h1};
Point(7) = { r,   0, 0, h1};
Point(8) = { 0,  -r, 0, h1};

Point(9) = {0, 0, 0, h1};

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

Field[1] = MathEval;
Field[1].F = Sprintf("2*%g*(1-(y/%g)^2)+%g",h2,H,h2);

Field[2] = MathEval;
Field[2].F = Sprintf("%g*(x^2 + y^2)/(%g)^2",h1,r);

Field[3] = Min;
Field[3].FieldsList = {1,2};

Background Field = 3;

Physical Surface(1000000) = {1};

Physical Line(1000001) = {4};       // Inlet
Physical Line(1000002) = {5,6,7,8}; // cylinder
Physical Line(1000003) = {2};       // outlet
Physical Line(1000004) = {1,3};     // top and bottom walls
