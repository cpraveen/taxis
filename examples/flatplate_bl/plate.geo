l = 0.5; // initial length
L = 1.0; // plate length
H = 0.25;// height

n1 = 50;  // normal to BL
n2 = 50;  // along plate
p  = 1.1; // geometric progression

n3 = 20;

// First cell height
dh = H*(p - 1)/(p^n1 - 1);
Printf("First cell height = %e\n", dh);

Point(1) = {0,0,0};
Point(2) = {L,0,0};
Point(3) = {L,H,0};
Point(4) = {0,H,0};
Point(5) = {-l,0,0};
Point(6) = {-l,H,0};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};
Line(5) = {4,6};
Line(6) = {6,5};
Line(7) = {5,1};

Line Loop(1) = {1,2,3,4};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {1,2,3,4};

Line Loop(2) = {-4,5,6,7};
Ruled Surface(2) = {2};
Transfinite Surface(2) = {5,1,4,6};

Transfinite Line{2,-4,-6} = n1 Using Progression p;
Transfinite Line{1,-3} = n2 Using Progression 1.05;

Transfinite Line{-7,5} = n3 Using Progression 1.15;

Physical Surface(100000) = {1,2};

Physical Line(100001) = {6};   // inlet
Physical Line(100002) = {7};   // bottom
Physical Line(100003) = {1};   // plate
Physical Line(100004) = {2};   // outlet
Physical Line(100005) = {3,5}; // top
