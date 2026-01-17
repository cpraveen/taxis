Ri=0.1524;
Ro=10*Ri;
L=10*Ri;

cl1=Pi*Ri/400;
cl2=Ro/50;
cl3=Pi*Ro/100;

Point(1) = {0,0,0,cl1};
Point(2) = {-Ri,0,0,cl1};
Point(3) = {0,Ri,0,cl1};
Point(4) = {Ri,0,0,cl1};
Point(5) = {L,0,0,cl2};
Point(6) = {L,Ro,0,cl2};
Point(7) = {0,Ro,0,cl2};
Point(8) = {-0.5,0,0,cl2};

Circle(1) = {2,1,3};
Circle(2) = {3,1,4};
Line(3) = {4,5};
Line(4) = {5,6};
Line(5) = {6,7};
Ellipse(6) = {7,1,7,8};
Line(7) = {8,2};

Line Loop(1) = {1,2,3,4,5,6,7};
Plane Surface(1) = {1};

Physical Surface(100000) = {1};

Physical Line(100001) = {1,2}; // cylinder
Physical Line(100002) = {3,7}; // symmetry
Physical Line(100003) = {5,6}; // farfield
Physical Line(100004) = {4};   // outlet

