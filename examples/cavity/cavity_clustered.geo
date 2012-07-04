h = 0.005;
H = 0.1;

Point(1) = {0,   0, 0, h};
Point(2) = {0.5, 0, 0, h};
Point(3) = {1, 0, 0, h};
Point(4) = {1, 0.5, 0, h};
Point(5) = {1, 1, 0, h};
Point(6) = {0.5, 1, 0, h};
Point(7) = {0, 1, 0, h};
Point(8) = {0, 0.5, 0, h};
Point(9) = {0.5, 0.5, 0, H};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,5};
Line(5) = {5,6};
Line(6) = {6,7};
Line(7) = {7,8};
Line(8) = {8,1};
Line(9) = {2,9};
Line(10) = {9,6};
Line(11) = {8,9};
Line(12) = {9,4};

Line Loop(1) = {1,9,-11,8};
Plane Surface(1) = {1};

Line Loop(2) = {2,3,-12,-9};
Plane Surface(2) = {2};

Line Loop(3) = {4,5,-10,12};
Plane Surface(3) = {3};

Line Loop(4) = {6,7,11,10};
Plane Surface(4) = {4};

Physical Surface(100000) = {1,2,3,4};

Physical Line(100001) = {1};
Physical Line(100002) = {2};
Physical Line(100003) = {3};
Physical Line(100004) = {4};
