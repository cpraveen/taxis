xmin = -15;
xmax = +15;
ymin = 0;
ymax = 15;

nx = 100;
ny = 100;
r  = 1.02;

Point(1) = {xmin, ymin, 0};
Point(2) = {xmax, ymin, 0};
Point(3) = {xmax, ymax, 0};
Point(4) = {xmin, ymax, 0};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};

Line Loop(1) = {1,2,3,4};
Ruled Surface(1) = {1};
Transfinite Surface(1) = {1,2,3,4};

Transfinite Line{1,3} = nx;
Transfinite Line{2,-4} = ny Using Progression r;

Physical Surface(100000) = {1};
Physical Line(100001) = {1}; // bottom wall
Physical Line(100002) = {2}; // right side wall
Physical Line(100003) = {3}; // top side wall
Physical Line(100004) = {4}; // left side wall
