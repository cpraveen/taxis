r1 = 0.05;
r2 = 0.1;
n1 = 200; // no. of points on inner cylinder
cellSize=2*Pi*r1/n1;
Printf("cellSize on inner/outer cylinders = %e\n", cellSize);

r = 0.01;
xc= 0.0;
yc= 0.075;
nc=100; // no. of points on small cylinder
h2=2*Pi*r/nc;
Printf("cellSize on small stationary cylinder = %e\n", h2);

// create inner 1/8 shell
Point(1) = {0, 0, 0, cellSize};
Point(2) = {-r1, 0, 0, cellSize};
Point(3) = {0, r1, 0, cellSize};
Point(4) = {r1, 0, 0, cellSize};
Point(5) = {0, -r1, 0, cellSize};
Point(6) = {-r2, 0, 0, cellSize};
Point(7) = {0, r2, 0, cellSize};
Point(8) = {r2, 0, 0, cellSize};
Point(9) = {0, -r2, 0, cellSize};

Point(10) = {xc, yc, 0, h2};
Point(11) = {xc-r, yc, 0, h2};
Point(12) = {xc, yc+r, 0, h2};
Point(13) = {xc+r, yc, 0, h2};
Point(14) = {xc, yc-r, 0, h2};

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

Line Loop(1) = {-4, -3, -2, -1};
Line Loop(2) = {-8, -7, -6, -5};
Line Loop(3) = {9, 10, 11, 12};

Plane Surface(10) = {2,1,3};

Physical Surface(100000) = {10};

Physical Line(100001) = {1, 2, 3, 4}; // inner cylinder
Physical Line(100002) = {5, 6, 7, 8}; // outer cylinder
Physical Line(100003) = {9, 10, 11, 12}; // small cylinder
