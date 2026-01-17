//-----------------------------------------------------------------------------
// Author: Ashish Garg
//-----------------------------------------------------------------------------
r1 = 0.05;
r2 = 0.1;
n1 = 400; // no. of points on inner cylinder
cellSize=2*Pi*r1/n1;
Printf("cellSize on inner/outer cylinders = %e\n", cellSize);
r = 0.01;
r3 = 1.1*r1;
r4 = 0.92*r2;
r11 = 1.3*r;
xc= 0.0;
yc= 0.075;
nc=200; // no. of points on small cylinder
h2=2*Pi*r/nc;
theta = 50*Pi/180;
theta1 = 5*Pi/180;
theta2= 40*Pi/180;
p=1.15;
n_p = n1/4;
Printf("cellSize on small stationary cylinder = %e\n", h2);

// create inner 1/8 shell
Point(1) = {0, 0, 0, cellSize};
Point(2) = {-r1,0, 0, cellSize};
Point(3) = {0, r1, 0, cellSize};
Point(4) = {r1,0, 0, cellSize};
Point(5) = {0, -r1, 0, cellSize};
Point(6) = {-r2,0 , 0, cellSize};
Point(7) = {0, r2, 0, cellSize};
Point(8) = {r2,0, 0, cellSize};
Point(9) = {0, -r2, 0, cellSize};

Point(10) = {xc, yc, 0, h2};
Point(11) = {xc-r,yc, 0, h2};
Point(12) = {xc, yc+r, 0, h2};
Point(13) = {xc+r, yc, 0, h2};
Point(14) = {xc, yc-r, 0, h2};
//Point(14) = {xc+r*Cos(Pi - theta1 + theta2), yc+r*Sin(Pi - theta1 + theta2), 0, h2};


//Point(15) = {r_av*Cos(Pi - theta),r_av*Sin(Pi -theta), 0, cellSize};
//Point(16) = {r_av*Cos(theta),r_av*Sin(theta), 0, cellSize};
Point(17) = {xc-r11,yc, 0, h2};
Point(18) = {xc, yc+r11, 0, h2};
Point(19) = {xc+r11, yc, 0, h2};
Point(20) = {xc, yc-r11, 0, h2};


Point(21) = {-r3,0, 0, cellSize};
Point(22) = {0, r3, 0, cellSize};
Point(23) = {r3,0, 0, cellSize};
Point(24) = {0, -r3, 0, cellSize};
Point(25) = {-r4,0 , 0, cellSize};
Point(26) = {0, r4, 0, cellSize};
Point(27) = {r4,0, 0, cellSize};
Point(28) = {0, -r4, 0, cellSize};


Circle(1) = {2, 1, 3};
Circle(2) = {3, 1, 4};
Circle(3) = {4, 1, 5};
Circle(4) = {5, 1, 2};
Circle(5) = {6, 1, 7};
Circle(6) = {7, 1, 8};
Circle(7) = {8, 1, 9};
Circle(8) = {9, 1, 6};

Circle(9) = {11, 10, 12};
Circle(10) = {12, 10, 13};
Circle(11) = {13, 10, 14};
Circle(12) = {14, 10, 11};

Circle(21) = {17, 10, 18};
Circle(22) = {18, 10, 19};
Circle(23) = {19, 10, 20};
Circle(24) = {20, 10, 17};

Line(25) = {11,17};
Line(26) = {12,18};
Line(27) = {13,19};
Line(28) = {14,20};



Circle(29) = {21, 1, 22};
Circle(30) = {22, 1, 23};
Circle(31) = {23, 1, 24};
Circle(32) = {24, 1, 21};
Circle(33) = {25, 1, 26};
Circle(34) = {26, 1, 27};
Circle(35) = {27, 1, 28};
Circle(36) = {28, 1, 25};

Line(37) = {2,21};
Line(38) = {3,22};
Line(39) = {4,23};
Line(40) = {5,24};

Line(41) = {25,6};
Line(42) = {26,7};
Line(43) = {27,8};
Line(44) = {28,9};



Line Loop(1) = {9,26,-21,-25};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {18,17,11,12};
Transfinite Line{9,21} = n_p ;
Transfinite Line{25,26} = n_p Using Progression p;

Line Loop(2) = {10,27,-22,-26};
Ruled Surface(2) = {2};
Transfinite Surface(2) = {12,13,18,19};
Transfinite Line{10,22} = n_p ;
Transfinite Line{27,26} = n_p Using Progression p;

Line Loop(3) = {11,28,-23,-27};
Ruled Surface(3) = {3};
Transfinite Surface(3) = {13,14,19,20};
Transfinite Line{11,23} = n_p ;
Transfinite Line{28,27} = n_p Using Progression p;

Line Loop(4) = {12,25,-24,-28};
Ruled Surface(4) = {4};
Transfinite Surface(4) = {20,17,11,14};
Transfinite Line{12,24} = n_p ;
Transfinite Line{25,28} = n_p Using Progression p;



Line Loop(5) = {1,38,-29,-37};
Ruled Surface(5) = {5};
Transfinite Surface(5) = {2,3,21,22};
Transfinite Line{1,29} = n_p;
Transfinite Line{37,38} = n_p Using Progression p;

Line Loop(6) = {2,39,-30,-38};
Ruled Surface(6) = {6};
Transfinite Surface(6) = {3,22,4,23};
Transfinite Line{2,30} = n_p ;
Transfinite Line{39,38} = n_p Using Progression p;

Line Loop(7) = {3,40,-31,-39};
Ruled Surface(7) = {7};
Transfinite Surface(7) = {4,5,23,24};
Transfinite Line{31,3} = n_p ;
Transfinite Line{40,39} = n_p Using Progression p;

Line Loop(8) = {4,37,-32,-40};
Ruled Surface(8) = {8};
Transfinite Surface(8) = {2,21,24,5};
Transfinite Line{4,32} = n_p ;
Transfinite Line{37,40} = n_p Using Progression p;


Line Loop(9) = {33,42,-5,-41};
Ruled Surface(9) = {9};
Transfinite Surface(9) = {6,7,25,26};
Transfinite Line{-33,-5} = n_p ;
Transfinite Line{-42,-41} = n_p Using Progression p;

Line Loop(10) = {34,43,-6,-42};
Ruled Surface(10) = {10};
Transfinite Surface(10) = {7,8,26,27};
Transfinite Line{-34,-6} = n_p ;
Transfinite Line{-43,-42} = n_p Using Progression p;

Line Loop(11) = {35,44,-7,-43};
Ruled Surface(11) = {11};
Transfinite Surface(11) = {8,9,27,28};
Transfinite Line{-35,-7} = n_p ;
Transfinite Line{-44,-43} = n_p Using Progression p;

Line Loop(12) = {36,41,-8,-44};
Ruled Surface(12) = {12};
Transfinite Surface(12) = {9,28,6,25};
Transfinite Line{-36,-8} = n_p ;
Transfinite Line{-41,-44} = n_p Using Progression p;


Line(45) = {21,25};
Line(46) = {22,20};
Line(47) = {23,27};
Line(48) = {24,28};
Line(49) = {18,26};


Line Loop(13) = {31,48,-35,-47};
Plane Surface(13) = {13};

Line Loop(14) = {32,45,-36,-48};
Plane Surface(14) = {14};

Line Loop(15) = {29,46,24,21,49,-33,-45};
Plane Surface(15) = {15};

Line Loop(16) = {30,47,-34,-49,22,23,-46};
Plane Surface(16) = {16};

Physical Line(100001) = {1, 2, 3, 4}; // inner cylinder
Physical Line(100002) = {5, 6, 7, 8}; // outer cylinder
Physical Line(100003) = {9, 10, 11, 12}; // small cylinder
Physical Surface(100000) = {1,2,3,4,5,6,7,8,9,10,11,12, 13,14,15,16 };


