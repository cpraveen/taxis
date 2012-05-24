L = 5.0;// channel length
H = 1.0; // height

n1 = 25;  // normal to BL
n2 = 25*L;  // along plate
p  = 1.1; // geometric progression

// First cell height
dh = 0.5*H*(p - 1)/(p^(n1/2) - 1);
Printf("First cell height = %e\n", dh);

Point(1) = {0,0,0};
Point(2) = {L,0,0};
Point(3) = {L,H,0};
Point(4) = {0,H,0};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};

Line Loop(1) = {1,2,3,4};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {1,2,3,4};

Transfinite Line{2,-4} = n1;
Transfinite Line{1,-3} = n2;

Physical Surface(100000) = {1};

Physical Line(100001) = {4}; // inlet
Physical Line(100002) = {2}; // outlet
Physical Line(100003) = {1}; // bottom
Physical Line(100004) = {3}; // top
