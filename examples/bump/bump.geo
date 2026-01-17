// Run from terminal as follows:
//   gmsh -3 -optimize bump.geo
// This creates file bump.msh

L = 0.5; // extrusion in third direction
R = 2.0; // radius of arc
h1 = 0.01; // point density on bump
h = 0.1; // point density
l1= 1*R;
l2=1*R;

// Center of arc
C= Sqrt(R^2-0.5^2);

// Height
H=1*R;

Point(1) = {0,0,0,h1};

Point(2) = {1,0,0,h1};
Point(3) = {1+l2,0,0,h};
Point(4) = {1+l2,H,0,h};
Point(5) = {-l1,H,0,h};

Point(6) = {-l1,0,0,h};
Point(7) = {0.5,-C,0};

Circle(1) = {1,7,2};

Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,5};
Line(5) = {5,6};
Line(6) = {6,1};


Line Loop(1) = {1,2,3,4,5,6};

Plane Surface(2) = {1};

Physical Surface(100000) = {2};  // volume

Physical Line(100001) = {5};        // inlet
Physical Line(100002) = {3};        // outlet
Physical Line(100003) = {1,2,4,6};  // solid wall
