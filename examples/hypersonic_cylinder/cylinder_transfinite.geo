Ri=0.1524;
Ro=10*Ri;

ntheta=100;
nr=100;
r=1.05;

Point(1) = {0,0,0};
Point(2) = {-Ri,0,0};
Point(3) = {0,Ri,0};
Point(4) = {Ri,0,0};
Point(5) = {Ro,0,0};
Point(6) = {0,Ro,0};
Point(7) = {-Ro,0,0};

Circle(1) = {2,1,3};
Line(2) = {3,6};
Circle(3) = {6,1,7};
Line(4) = {7,2};

Circle(5) = {3,1,4};
Line(6) = {4,5};
Circle(7) = {5,1,6};

Line Loop(1) = {1,2,3,4};
Ruled Surface(1) = {1};

Line Loop(2) = {5,6,7,-2};
Ruled Surface(2) = {2};

Transfinite Line{1,5,3,7} = ntheta;
Transfinite Line{-4,2,6} = nr Using Progression r;

Transfinite Surface(1) = {2,3,6,7};
Transfinite Surface(2) = {3,4,5,6};

Physical Surface(100000) = {1,2};

Physical Line(100001) = {1,5}; // cylinder
Physical Line(100002) = {4,6}; // symmetry
Physical Line(100003) = {3,7}; // farfield

