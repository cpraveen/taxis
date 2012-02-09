r1 = 1.0;
r2 = 1.5;
cellSize=0.02;

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
Circle(1) = {2, 1, 3};
Circle(2) = {3, 1, 4};
Circle(3) = {4, 1, 5};
Circle(4) = {5, 1, 2};
Circle(5) = {6, 1, 7};
Circle(6) = {7, 1, 8};
Circle(7) = {8, 1, 9};
Circle(8) = {9, 1, 6};

//Line Loop(1) = {1, 2, 3, 4};
Line Loop(1) = {-4, -3, -2, -1};
//Line Loop(2) = {5, 6, 7, 8};
Line Loop(2) = {-8, -7, -6, -5};
Plane Surface(10) = {2,1};

Physical Surface(100000) = {10};

Physical Line(100001) = {1, 2, 3, 4}; // inner cylinder
Physical Line(100002) = {5, 6, 7, 8}; // outer cylinder
